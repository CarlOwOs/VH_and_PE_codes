############## Assigna colors depenent dels anys
colors <- function(vec) {
  colorss <- c("#5A160F","#A5392D","#C2665B","#DC8659" ,"#C56430" ,"#D99541","#FEF071","#CBFF70" ,"#77B956", "#2B8A5C" ,"#48B17E")
  n<-length(vec) # nombre d'anys
  colvec <- rep(NA, n)
  any_ini <- 2016  # any minim
  for (i in 1:n) {
    any_i <- as.integer(vec[i]) 
    colvec[i] <- colorss[any_i-any_ini]
  }
  return(colvec)
}


############## retorna un vector que contingui tots els mesos (des de la mínima data fins a la màxima)
cada1_mesos <- function(dd) {
  mes_any_min <- as.Date(paste(min(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_any_max <- as.Date(paste(max(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes <- mes_any_min
  cada1_mesos <- c(substr(mes_any_min, 1,7)) 
  while(mes != mes_any_max){
    mes <- mes %m+% months(1)
    cada1_mesos <- c(cada1_mesos,substr(mes, 1,7))
  }
  
  return(cada1_mesos)
}


############## retorna un vector amb les dates separades cada 3 mesos
cada3_mesos <- function(dd) {
  mes_any_min <- as.Date(paste(min(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_min <- as.integer(substr(mes_any_min, 6,7))
  if (mes_min %% 3 == 1) {
    mes_any_min <- mes_any_min %m-% months(1)
  } else if (mes_min %% 3 == 2) {
    mes_any_min <- mes_any_min %m+% months(1)
  }
  
  mes_any_max <- as.Date(paste(max(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_max <- as.integer(substr(mes_any_max, 6,7))
  if (mes_max %% 3 == 1) {
    mes_any_max <- mes_any_max %m-% months(1)
  } else if (mes_max %% 3 == 2) {
    mes_any_max <- mes_any_max %m+% months(1)
  }
  
  mes <- mes_any_min
  cada3_mesos <- c(substr(mes_any_min, 1,7)) 
  while(mes != mes_any_max){
    mes <- mes %m+% months(3)
    cada3_mesos <- c(cada3_mesos,substr(mes, 1,7))
  }
  
  return(cada3_mesos)
}

############# retorna un vector amb les dates separades cada 6 mesos
cada6_mesos <- function(dd) {
  mes_any_min <- as.Date(paste(min(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_min <- as.integer(substr(mes_any_min, 6,7))
  mod <- mes_min %% 12
  if (mod %in% c(1,2)) {
    mes_any_min <- mes_any_min %m-% months(mod)
  } else if (mod %in% c(9,10,11)) {
    mes_any_min <- mes_any_min %m+% months(12-mod)
  }
  
  mes_any_max <- as.Date(paste(max(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_max <- as.integer(substr(mes_any_max, 6,7))
  mod <- mes_max %% 12
  if (mod %in% c(1,2)) {
    mes_any_max <- mes_any_max %m-% months(mod)
  } else if (mod %in% c(9,10,11)) {
    mes_any_max <- mes_any_max %m+% months(12-mod)
  }
  
  mes <- mes_any_min
  cada6_mesos <- c(substr(mes_any_min, 1,7)) 
  while(mes != mes_any_max){
    mes <- mes %m+% months(6)
    cada6_mesos <- c(cada6_mesos,substr(mes, 1,7))
  }
  
  return(cada6_mesos)
}


############# retorna un vector amb les dates separades cada 12 mesos
cada12_mesos <- function(dd) {
  mes_any_min <- as.Date(paste(min(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_min <- as.integer(substr(mes_any_min, 6,7))
  mod <- mes_min %% 12
  if (mod %in% c(1,2)) {
    mes_any_min <- mes_any_min %m-% months(mod)
  } else if (mod %in% c(9,10,11)) {
    mes_any_min <- mes_any_min %m+% months(12-mod)
  }
  
  mes_any_max <- as.Date(paste(max(dd$data), '-01', sep = ""), format = "%Y-%m-%d")
  mes_max <- as.integer(substr(mes_any_max, 6,7))
  mod <- mes_max %% 12
  if (mod %in% c(1,2)) {
    mes_any_max <- mes_any_max %m-% months(mod)
  } else if (mod %in% c(9,10,11)) {
    mes_any_max <- mes_any_max %m+% months(12-mod)
  }
  
  mes <- mes_any_min
  cada12_mesos <- c(substr(mes_any_min, 1,7)) 
  while(mes != mes_any_max){
    mes <- mes %m+% months(12)
    cada12_mesos <- c(cada12_mesos,substr(mes, 1,7))
  }
  
  return(cada12_mesos)
  
}

############## separa en dues files diferents si la duracio cau en dos mesos diferents 
#(cal que la columna amb dates inicials es digui DATA_INICI i finals DATA_FI)
separar <- function(dd){
  dd <- dd[!is.na(dd$DATA_INICI) & !is.na(dd$DATA_FI), ]
  for (i in 1:nrow(dd)) {
    df <- data.frame()
    
    while (substr(dd$DATA_INICI[i], 1,7) !=  substr(dd$DATA_FI[i], 1,7)) {
      newr <- dd[i,]
      
      # crear nova fila amb lultim mes
      data_ini <- dd$DATA_FI[i]
      day(data_ini) <- 1
      newr$DATA_INICI <- data_ini
      
      # modificar fila inicial
      dd$DATA_FI[i] <- dd$DATA_FI[i] %m-% months(1)
      day(dd$DATA_FI[i]) <- days_in_month(dd$DATA_FI[i])
      
      # afegir la nova fila
      dd <- rbind(dd, newr)
      
    }
  }
  return (dd)
}


############## agrupar per mes 
agrupar <- function (dd, tipus, preu, nom) {
  dd <- dd[!is.na(dd$DATA_INICI) & !is.na(dd$DATA_FI), ]
  df <- data.frame()
  for (i in 1:length(tipus)) {
    d_tipus <- dd[dd$tipus_event == tipus[i], ]
    d_tipus$cost <- preu[i]
    df <- rbind(df, d_tipus)
  }
  
  df$freq <- as.integer(df$DATA_FI - df$DATA_INICI) + 1 # per comptar tant l'entrada com la sortida 
  df$cost <- df$cost * df$freq
  df$data <- substr(df$DATA_INICI, 1, 7)
  
  df <- df %>% group_by(data)
  df <- df %>% dplyr::summarise(dies = sum(freq), persones = n(), cost = sum(cost), tipus = nom)
  df <- as.data.frame(df)
  return(df)
}


############## preparar el dataframe de events
events.f <- function(events) {
  events$data_visita <- as.Date(events$data_visita, format = "%Y-%m-%d")
  events$alta_event <- as.Date(events$alta_event, format = "%Y-%m-%d")
  names(events)[names(events) == 'data_visita'] <- 'DATA_INICI'
  names(events)[names(events) == 'alta_event'] <- 'DATA_FI'
  events <- events[!is.na(events$DATA_INICI) & !is.na(events$DATA_FI), ]
  events <- separar(events)
  return(events)
}

############## preparar el dataframe de famacia
farmacia.f <- function(farmacia) {
  # modificacions per poder aplicar la funcio (agrupar) de creacio del dataframe
  farmacia$DATA_INICI <- as.Date(farmacia$DATA_INICI, format = "%Y-%m-%d")
  farmacia$DATA_FI <- as.Date(farmacia$DATA_FI, format = "%Y-%m-%d")
  farmacia$tipus_event <- 'farmacia'
  #farmacia <- separar(farmacia)
  farmacia <- agrupar(farmacia, 'farmacia' , 20, 'farmacia')
  return(farmacia)
}

############## preparar el dataframe de curs clinic
hdia_cc.f <- function(cursclinic) { 
  # modificacions per poder aplicar la funcio (agrupar) de creacio del dataframe
  names(cursclinic)[names(cursclinic) == 'UT_DESC'] <- 'tipus_event'
  cursclinic$DATA_CURS <- as.Date(cursclinic$DATA_CURS, format = "%Y-%m-%d")
  names(cursclinic)[names(cursclinic) == 'DATA_CURS'] <- 'DATA_INICI'
  cursclinic$DATA_FI <- cursclinic$DATA_INICI
  
  hdia_cc <- agrupar(cursclinic, 'Hdia Àrea del cor', 340, 'hdia_cc')
  return(hdia_cc)
}

############## preparar el dataframe d'urgencies
urgencies.f <- function(urgencies) {
  # modificacions per poder aplicar la funcio (agrupar) de creacio del dataframe
  urgencies <- urgencies[urgencies$OUN_DESCR == 'CARDIOLOGIA' | urgencies$OUN_DESCR_1== 'CARDIOLOGIA' , ]
  urgencies$tipus_event <- 'CARDIOLOGIA'
  urgencies$FECHA_ENTRADA <- as.Date(urgencies$FECHA_ENTRADA, format = "%Y-%m-%d")
  names(urgencies)[names(urgencies) == 'FECHA_ENTRADA'] <- 'DATA_INICI'
  urgencies$FECHA_SALIDA <- as.Date(urgencies$FECHA_SALIDA, format = "%Y-%m-%d")
  names(urgencies)[names(urgencies) == 'FECHA_SALIDA'] <- 'DATA_FI'
  
  urgencies <- separar(urgencies)
  urgencies <- agrupar(urgencies, 'CARDIOLOGIA', 275, 'urgencies')
  
  return(urgencies)
}


############## preparar el dataframe d'hospitalitzacions
hospitalitzacions.f <- function(hospitalitzacions, diagnostic) {
  descompensacio <- c('CIM10', 'I50.1', 'I50.20', 'I50.21', 'I50.22', 'I50.23', 'I50.30', 'I50.31', 'I50.32', 'I50.33', 'I50.40', 'I50.41', 'I50.42', 'I50.43', 'I50.9', 'I11.0', 'I13.0', 'I13.2', 'I97.130', 'I97.131', 'P29.0')
  
  hospitalitzacions <- hospitalitzacions[hospitalitzacions$DIAG_PPAL_EPI %in% descompensacio,]
  hospitalitzacions <- hospitalitzacions[hospitalitzacions$CLASE_ALTA == 1, ]
  
  hospitalitzacions <- merge(diagnostic, hospitalitzacions, by='ID_EPISODI')
  
  hospitalitzacions$DATA_INICI_HOSPOSPITALITZACIO <- as.Date(hospitalitzacions$DATA_INICI_HOSPOSPITALITZACIO, format = "%Y-%m-%d")
  names(hospitalitzacions)[names(hospitalitzacions) == 'DATA_INICI_HOSPOSPITALITZACIO'] <- 'DATA_INICI'
  hospitalitzacions$DATA_ALTA <- as.Date(hospitalitzacions$DATA_ALTA, format = "%Y-%m-%d")
  names(hospitalitzacions)[names(hospitalitzacions) == 'DATA_ALTA'] <- 'DATA_FI'
  hospitalitzacions$tipus_event <- 'hospitalitzacions'
  
  hospitalitzacions <- separar(hospitalitzacions)
  hospitalitzacions <- agrupar(hospitalitzacions, 'hospitalitzacions', 330, 'hospitalitzacions')
  
  return(hospitalitzacions)
}


############## preparar el dataframe de visites
visites.f <- function(visites) { 
  names(visites)[names(visites) == 'data_visita'] <- 'DATA_INICI'
  visites$DATA_INICI <- as.Date(visites$DATA_INICI, format = "%Y-%m-%d")
  visites$DATA_FI <- visites$DATA_INICI
  visites$tipus_event <- 'visita'
  visites <- agrupar(visites, 'visita', 64, 'visites')
  return(visites) 
}
  

############## preparar el dataframe de ecocardiogrames
ecocardiogrames.f <- function(ecocardiogrames) { 
  names(ecocardiogrames)[names(ecocardiogrames) == 'data_eco_seguim'] <- 'DATA_INICI'
  ecocardiogrames$DATA_INICI <- as.Date(ecocardiogrames$DATA_INICI, format = "%Y-%m-%d")
  ecocardiogrames$DATA_FI <- ecocardiogrames$DATA_INICI
  ecocardiogrames$tipus_event <- 'ecocardiogrames'
  ecocardiogrames <- agrupar(ecocardiogrames, 'ecocardiogrames', 21, 'ecocardiogrames')
  return(ecocardiogrames) 
}


############## preparar el dataframe de analitiques
analitiques.f <- function(analitiques) { 
  names(analitiques)[names(analitiques) == 'data_eco_seguim'] <- 'DATA_INICI'
  analitiques$DATA_INICI <- as.Date(analitiques$DATA_INICI, format = "%Y-%m-%d")
  analitiques$DATA_FI <- analitiques$DATA_INICI
  analitiques$tipus_event <- 'analitiques'
  analitiques <- agrupar(analitiques, 'analitiques', 21, 'analitiques')
  return(analitiques) 
}

############## preparar el dataframe de proves
proves_tipus.f <- function (unified) {
  analitiques <- analitiques.f(unified)
  ecocardiogrames <- ecocardiogrames.f(unified)
  proves_tipus <- rbind(ecocardiogrames, analitiques)
  return(proves_tipus)
}

proves_tipus.plot <- function(proves_tipus) {
  proves_tipus$data <- substr(proves_tipus$data, 1, 4)
  proves_tipus<- proves_tipus %>% group_by(data, tipus)
  proves_tipus <- proves_tipus %>% dplyr::summarise(dies = sum(dies), persones = sum(persones), cost = sum(cost), .groups = 'drop')
  proves_tipus <- as.data.frame(proves_tipus)
  
  hc <- proves_tipus %>% 
    hchart('column', hcaes(x = 'data', y = 'cost', group = 'tipus'),
           stacking = "normal"
    )
  
  return(hc)
}

############## agrupar tots els tipus de proves en un dataframe
proves.f <- function(proves) { 
  proves <- proves %>% group_by(data)
  proves <- proves %>% dplyr::summarise(dies = sum(dies), persones = sum(persones), cost = sum(cost), tipus = 'proves')
  proves <- as.data.frame(proves)
  return(proves)
}

############## unifica tots els dataframes en un de sol
linies <- function (dataframes, names) {
  dd <- data.frame(data = c(0))
  for (i in 1:length(dataframes)) {
    df <- as.data.frame(dataframes[i])
    df <- subset(df, select=-c(tipus))
    s <- names[i]
    colnames(df) <- c('data', paste0('dies_',s), paste0('persones_',s), paste0('cost_',s))
    
    dd <- merge(x = dd, y = df, by = 'data', all = TRUE)
  }
  dd[is.na(dd)] <- 0
  return (dd[dd$data != 0,])
}

############## per omplir les dates que no tenen dades
omplir <- function(dd_linies) {
  # crear un dataframe amb tots els mesos i freqüencia 0 als tres camps
  m <- matrix(0, ncol = ncol(dd_linies), nrow = length(cada1_mesos(dd_linies)))
  aux <- as.data.frame(m)
  colnames(aux) <- colnames(dd_linies)
  aux$data <- cada1_mesos(dd_linies)
  dd_linies<-rbind(dd_linies,aux)
  
  # agrupar de nou les dades per unir els dataframes de tal manera que sumi les freqüencies d'un i de l'altre
  # (sempre sumara 0 al dataframe original, ja que les frequencies de aux son 0 i aixi no afecten a les dates que ja estaven)
  dd_linies <- dd_linies %>% group_by(data)
  dd_linies <- dd_linies %>% dplyr::summarise_each(list(sum))
  dd_linies <- as.data.frame(dd_linies)
  
  return (dd_linies)
}


############## retorna els eixos més adequats segons l'interval que es representi
eixos <- function(ini, fi, dd) {
  # els eixos només tenen en compte si existeixen dades
  i <- max(as.Date(paste(min(dd$data), '-01', sep = "")), as.Date(paste(ini, '-01', sep = "")))
  f <- min(as.Date(paste(max(dd$data), '-01', sep = "")), as.Date(paste(fi, '-01', sep = "")))
  time <- as.numeric(f-i)
  if (time < 300) return(cada1_mesos(dd))
  else if (time < 1006) return(cada3_mesos(dd))
  else if (time < 3000) return(cada6_mesos(dd))
  else return(cada12_mesos(dd))
}


############## fer el grafic corresponent segons les caselles marcades per l'usuari
dibuixa <- function(interval, choice, axis, start_title, yaxis, cols) {
  plot <- ggplot(interval, aes(x=data, group=1)) + xlab('Time (month-year)') + ylab(yaxis) + theme_minimal() + scale_x_discrete(breaks = axis)
  colors <- c()
  title <- c()
  if (grepl("A", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[1]],color = 'Hdia'))
    colors <- append(colors, "orangered2")
    title <- append(title, "hdia")
  }
  if (grepl("B", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[2]],color = 'Hospitalitzacions'))
    colors <- append(colors, "deepskyblue3")
    title <- append(title, "hospitalitzacions")
  }
  if (grepl("C", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[3]],color = 'Urgències'))
    colors <- append(colors, "darkolivegreen3")
    title <- append(title, "urgències")
  }
  if (grepl("D", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[4]],color = 'Proves'))
    colors <- append(colors, "purple")
    title <- append(title, "proves")
  }
  if (grepl("E", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[5]],color = 'Visites'))
    colors <- append(colors, "pink")
    title <- append(title, "visites")
  }
  if (grepl("F", choice)) {
    plot <- plot + geom_line(aes(y=interval[,cols[6]],color = 'Farmàcia'))
    colors <- append(colors, "orange")
    title <- append(title, "farmàcia")
  }
  
  
  plot <- plot +scale_color_manual(values=colors)
  
  l <- length(title)
  if (l != 0) {
    aux <- paste(start_title, title[1], sep=" ")
    if (l > 1) {
      if (l > 2) {
        for (i in 3:l-1) {
          aux <- paste(aux, title[i], sep=", ")
        }
      }
      aux <- paste(aux, "and", title[l], sep=" ")
    }
    aux <- paste(aux, "per month/year", sep=" ")
    plot <- plot + ggtitle(aux) + theme(plot.title = element_text(hjust = 0.5)) +  labs(color = "Plot Selection") 
  }
  
}


############## per visualitzar el plot de linies(part que canvia depenent de l'interval triat) 
economic_lin <- function(dd, input, choice, type) {
  ini <- substr(as.character(input[1]), 1, 7)
  fi <- substr(input[2], 1, 7)
  interval <- dd[(dd$data >= ini  & dd$data <= fi),]
  axis <- eixos(ini, fi, dd)
  
  if (type) plot <- dibuixa(interval, choice, axis, "Cost (in euros) of the", "Cost (€)", seq(4, 19, 3))
  else plot <- dibuixa(interval, choice, axis, "Number of", "Frequency", seq(2, 19, 3))
  
  
  return(plot)
}


############## preparar el dataframe pel grafic de barres
costs.f <- function (costs) {
  costs$data <- substr(costs$data, 1, 4)
  costs <- costs %>% group_by(data, tipus)  
  costs <- costs %>% dplyr::summarise(cost = sum(cost), dies = sum(dies), persones = sum(persones), .groups = 'drop')

  # calcular el percentatge
  costs_persones_total <- costs
  costs_persones_total <- costs_persones_total %>% group_by(data)
  costs_persones_total <- costs_persones_total %>% dplyr::summarise(cost_total = sum(cost), freq_total = sum(dies))
  costs_persones_total <- as.data.frame(costs_persones_total)
  
  costs <- merge(x = costs, y = costs_persones_total, by = "data", all = TRUE)
  costs$percentatge <- base::round(costs$cost / costs$cost_total * 100, 2)
  costs$cost_estandaritzat <-  base::round(costs$cost / costs$persones, 2)
  
  return(costs)
  
}
  

############## per visualitzar el plot de barres 
economic_bar <- function(costs) {
  costs$tipus[costs$tipus == "urgencies"] = "Urgències"
  costs$tipus[costs$tipus == "hospitalitzacions"] = "Hospitalitzacions"
  costs$tipus[costs$tipus == "hdia_cc"] = "Hospital de dia"
  costs$tipus[costs$tipus == "visites"] = "Visites"
  
  #costs <- costs[costs$tipus != "farmacia" & costs$tipus != "proves",]
  colorsss <- colors(2017:2020)
  hc <- costs %>% 
    hchart('column', hcaes(x = 'data', y = 'cost', group = 'tipus'),tooltip = list(pointFormat = "{point.tipus} : {point.percentatge} %"),
           stacking = "normal")  %>% 
    hc_colors(colorsss) %>%
    hc_title(text = "Cost (in euros) per year", style = list(fontSize = "12px"), align = "center") %>%
    # hc_plotOptions(
    #   series = list (
    #     events = list(
    #       click = JS("function(event) {Shiny.onInputChange('clicked', event.point.tipus);}")
    #     )
    #   )
    # )
  
    #hc_colors(colorsss) %>%
    #hc_title(text = "Cost (in euros) per year", style = list(fontSize = "12px"), align = "center") %>%    
    #hc_xAxis(title = list(text = "Time (year)")) %>%
    #hc_yAxis(title = list(text = "Cost (€)"))
  
  return(hc)
  
}

############## per visualitzar el plot de barres 
economic_bar_stnd <- function(costs) {
  costs$tipus[costs$tipus == "urgencies"] = "Urgències"
  costs$tipus[costs$tipus == "hospitalitzacions"] = "Hospitalitzacions"
  costs$tipus[costs$tipus == "hdia_cc"] = "Hospital de dia"
  costs$tipus[costs$tipus == "visites"] = "Visites"
  colorsss <- colors(2017:2020)
  hc <- costs %>% 
    hchart('column', hcaes(x = 'data', y = 'cost_estandaritzat', group = 'tipus'),tooltip = list(pointFormat = "{point.tipus} : {point.percentatge} %"),
           stacking = "normal")  %>% 
    hc_colors(colorsss) %>%
    hc_title(text = "Cost (in euros) per year", style = list(fontSize = "12px"), align = "center") %>%
    #hc_xAxis(title = list(text = "Time (year)"), categories = costs$data) %>%
    #hc_yAxis(title = list(text = "Cost (€)"))c_xAxis(title = list(text = "Time (year)")) %>%
  #hc_yAxis(title = list(text = "Cost (€)"))
  
  return(hc)
  
}





  
  