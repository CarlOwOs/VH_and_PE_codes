import argparse
import sys
import pandas as pd

def table_data(df, table):
    script = ""
    path = ""
    for _, row in df.iterrows():
        for t in str(row.Target_File).split(","):
            if table in t.split("/"):
                script = row.Name_Script
                path = t

    return script, path

def table_impact(df, table, temp):

    to_check = [table]
    impacted_tables = {""}
    impacted_scripts = {""}

    while len(to_check) != 0:
        check = to_check.pop(0)
        for _, row in df.iterrows():
            # Si la tabla está en las sources
            if check in str(row.Source_Table).split(",") or check in str(row.DB_table).split(","):
                targets = str(row.Target_File).split(",")
                # Añadimos script impactado
                impacted_scripts.add(row.Name_Script)
                for target in targets:
                    if target != "NAN":
                        # Si no la hemos visitado hacemos paso recursivo 
                        if not target in impacted_tables: to_check.append(target)
                        # Añadimos al conjunto de visitadas
                        if temp or not "TEMP" in target: impacted_tables.add(target)

    impacted_tables.remove("")
    impacted_scripts.remove("")

    return impacted_tables, impacted_scripts

def table_linaje(df, table, temp):

    to_check = [table]
    linaje_tables = {""}
    linaje_scripts = {""}
    source_files = {""}

    while len(to_check) != 0:
        check = to_check.pop(0)
        for _, row in df.iterrows():
            # Si la tabla está en las targets
            if check in str(row.Target_File).split(","):

                for source in str(row.DB_table).split(","):
                    if source != "NAN":
                        source_files.add(source)

                linaje_scripts.add(row.Name_Script)
                for source in str(row.Source_Table).split(","):
                    if source != "NAN":
                        if not source in linaje_tables: to_check.append(source)
                        if temp or not "TEMP" in source: linaje_tables.add(source)
                        


    linaje_tables.remove("")
    linaje_scripts.remove("")
    source_files.remove("")
    return linaje_tables, linaje_scripts, source_files


def table_trace(df, table, temp):

    script, path = table_data(df, table)
    path = path if path != "" else table # table is from DB
    print(path)
    impact_tables, impact_scripts = table_impact(df, path, temp) #
    linaje_tables, linaje_scripts, linaje_source = table_linaje(df, path, temp) #


    return (script, path), (impact_tables, impact_scripts), (linaje_tables, linaje_scripts, linaje_source)

def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument('-table', default="", type=str)
    parser.add_argument('-temp', default=False, type=str)
    #parser.add_argument('-script', default="", type=str)
    args = parser.parse_args()
    args.table = args.table.upper()

    df = pd.read_excel("./audit_valleh.xlsx")
    df = df.apply(lambda x: x.astype(str).str.upper())

    if args.table != "":
        if len(args.table) != 0:
            print("Showing trace of table:", args.table)
            aux = table_trace(df, args.table, args.temp)
            print("TABLE:")
            print("    Table loaded in script '", aux[0][0], "' and saved in path '", aux[0][1], "'", sep="")
            print("    IMPACT:")
            print("        Impacts the tables:")
            print("                         ",aux[1][0])
            print("        Impacts the scripts:")
            print("                         ",aux[1][1])
            print("    LINAJE:")
            print("        Depends on tables:")
            print("                         ",aux[2][0])
            print("        Depends on scripts:")
            print("                         ",aux[2][1])
            print("        Depends on database tables:")
            print("                         ",aux[2][2])
    

    return None


if __name__ == "__main__":
    sys.exit(main())