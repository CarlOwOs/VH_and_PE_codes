
#alarmes <- read.csv("ICTUS_CONSTANTALARM_202104231033.csv")
#personas <- read.csv("ICTUS_PATIENTWEIGHT_202104231033.csv")
gr_multialarmes <- function(alarmes, personas){
  
  alarmes$DALARM_DATE <- as.Date(as.POSIXct(alarmes$DALARM_DATE/1000,origin="1970-01-01"))
  
  alarms_per_person <- alarmes %>%
    mutate(month = format(DALARM_DATE, "%m"), year = format(DALARM_DATE, "%Y")) %>%
    group_by(month, year, SPAT_NHC) %>%
    count()
  
  personas$DPW_DATE <- as.Date(as.POSIXct(personas$DPW_DATE/1000,origin="1970-01-01"))
  
  personas1 <- personas %>%
    mutate(month = format(DPW_DATE, "%m"), year = format(DPW_DATE, "%Y")) %>%
    group_by(month, year, SPAT_NHC) %>%
    summarise(Unique_Elements = n_distinct(SPAT_NHC), .groups = 'drop' ) %>%
    group_by(month, year) %>%
    count() 
  
  names(personas1)[names(personas1) == "n"] <- "ppersonas"
  
  alarms_per_person$group<-sapply(alarms_per_person$n, function(x) 
    if (x >= 8) "multi_alarm"
    else "standard") 
  
  alarms_per_person$group <- as.factor(alarms_per_person$group)
  
  alarms_per_person_superalarmers <- alarms_per_person %>%
    group_by(group, year, month) %>%
    summarise(n = sum(n), .groups = 'drop')
  
  alarms_per_person_superalarmers$time <- paste(alarms_per_person_superalarmers$year,
                                                alarms_per_person_superalarmers $month, sep ='-')
  alarms_per_person_superalarmers$time <- as.factor(alarms_per_person_superalarmers$time)
  #personas mensuales alarmistas por alarmadas
  
  chart <- ggplot(alarms_per_person_superalarmers, aes(fill=group, y=n, x=time)) + 
    geom_bar(position="stack", stat="identity") + scale_fill_discrete(name = "Patient Type", labels = c("Multialarm", "Standard"))
  return (chart)
}


#alarmes <- read.csv("ICTUS_CONSTANTALARM_202104231033.csv")
#personas <- read.csv("ICTUS_PATIENTWEIGHT_202104231033.csv")
gr_multialarmes_standard <- function(alarmes, personas){
  
  alarmes$DALARM_DATE <- as.Date(as.POSIXct(alarmes$DALARM_DATE/1000,origin="1970-01-01"))
  
  alarms_per_person <- alarmes %>%
    mutate(month = format(DALARM_DATE, "%m"), year = format(DALARM_DATE, "%Y")) %>%
    group_by(month, year, SPAT_NHC) %>%
    count()
  
  personas$DPW_DATE <- as.Date(as.POSIXct(personas$DPW_DATE/1000,origin="1970-01-01"))
  
  personas1 <- personas %>%
    mutate(month = format(DPW_DATE, "%m"), year = format(DPW_DATE, "%Y")) %>%
    group_by(month, year, SPAT_NHC) %>%
    summarise(Unique_Elements = n_distinct(SPAT_NHC), .groups = 'drop') %>%
    group_by(month, year) %>%
    count() 
  
  names(personas1)[names(personas1) == "n"] <- "ppersonas"
  
  alarms_per_person$group<-sapply(alarms_per_person$n, function(x) 
    if (x >= 8) "multi_alarm"
    else "standard") 
  alarms_per_person$group <- as.factor(alarms_per_person$group)
  
  alarms_per_person_superalarmers <- alarms_per_person %>%
    group_by(group, year, month) %>%
    summarise(n = sum(n), .groups = 'drop')
  alarms_per_person_superalarmers$time <- paste(alarms_per_person_superalarmers$year,
                                                alarms_per_person_superalarmers $month, sep ='-')
  alarms_per_person_superalarmers$time <- as.factor(alarms_per_person_superalarmers$time)
  #personas mensuales alarmistas por alarmadas
  
  alarms_per_person_norm <- alarms_per_person_superalarmers %>%
    inner_join (personas1, by = c("year", "month"))
  alarms_per_person_norm$normalized <- alarms_per_person_norm$n/alarms_per_person_norm$ppersonas
  chart <- ggplot(alarms_per_person_norm, aes(fill=group, y=normalized, x=time)) + 
    geom_bar(position="stack", stat="identity")
  return(chart)
}

#alarmes <- read.csv("ICTUS_CONSTANTALARM_202104231033.csv")
gr_alarmes_tipus <- function(alarmes){
  alarmes$DALARM_DATE <- as.Date(as.POSIXct(alarmes$DALARM_DATE/1000,origin="1970-01-01"))
  
  alarms_per_type <- alarmes %>%
    mutate(month = format(DALARM_DATE, "%m"), year = format(DALARM_DATE, "%Y")) %>%
    group_by(month, year, NAME) %>%
    count()
  
  alarms_per_type$time <- paste(alarms_per_type$year,
                                alarms_per_type$month, sep ='-')
  chart <- ggplot(alarms_per_type, aes(fill=NAME, y=n, x=time)) + 
    geom_bar(position="fill", stat="identity") + scale_fill_discrete(name = "Type", labels = c("Diastolic", "Glucose","No Constants","Oxigen","Pulse","Systolic","Weight"))
  return (chart)
  
}