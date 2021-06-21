library(xts)
library(forecast)
library(openxlsx)
library(tsoutliers)
library(ggplot2)

timestamp_to_date <- function(timestamp) {
  return (as.Date(as.POSIXct(timestamp/1e3, origin="1970-01-01 00:00:00", tz="CET")))
}

prep.dd.al <- function(dd, nhc) {
  # La columna de la date te diferents noms però és la última sempre
  colnames(dd)[ncol(dd)] <- "Date"
  
  # Convertim les dates
  dd$Date <- timestamp_to_date(dd$Date)
  
  # Seleccionem un pacient
  dd <- dd %>% filter(SPAT_NHC == nhc)
  
  # Mirem si l'NHC existeix en el data frame.
  if (nrow(dd) == 0){
    return (NULL)
  }
  else{
    # Ordenem i afegim index per posterior merge
    dd <- dd[order(dd$Date),]
    dd$ind <- seq.int(nrow(dd))
    
    return(dd)
  }
}

prep.ts <- function(dd) {
  if (!is.null(dd)){
    # ts
    ts_dd <- dd[,"Value"]
    
    # Agrupar les alarmes
    serie_dd = ts(data=ts_dd, frequency = 365)
    return(serie_dd)
  }
  else return (NULL)
}


# Computes the t.test: equality of the eman of two sequences
alarm_ma <- function(serie) {
  ALARM = c()
  # Window de 90 dies, 30 dies i 14 dies
  windows = c(90, 30, 14)
  for (w in windows) {
    if (length(serie) > 2*w) {
      aux = tail(serie, 2*w)
      recent = aux[1:w]
      later = aux[(w+1):(2*w)]
      ttest = t.test(recent, later)
      if(ttest$p.value < 0.05){
        ALARM=c(ALARM,w)
      }
    }
  }
  ALARM
}

alarm <- function(dd,value){
  if (length(dd) != 0){
    dd = c(dd,value)
    serie=ts(data=dd, frequency = 365) #asumimos que son datos diarios
    tryCatch(
      {
        product.outlier<-tso(serie,types=c("AO","LS","TC"),cval = 3.5)
        num_obs <- length(dd) # numero de observacion
        output = ""
        windows=alarm_ma(serie)
        if(num_obs %in% c(product.outlier$outliers[,2]) | length(windows)>0) output = '<h1 style="text-align:center; color:red; font-family: Lucida Bright,Georgia,serif"> &#x1F6A8 <b>ALARM!</b></h1>'
        else output = ''
        values = product.outlier$outlier
        values$time = NULL
        values = values[,c(2,1,3,4)]
        values$tstat = abs(values$tstat)
        return(list(values,output))
        
      },
      error = function(e){
       return(list(NULL,NULL))
      }
    )
  }
  else return(list(NULL,NULL))
}

extract_outliers_graphic=function(out,dd,out_type,v){
  dd = c(dd,v)
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df) <- c("x","y","ABS_L")
  for (i in 1:nrow(out)){
    if(out[i,2] == out_type){
      x_val = out[i,1]
      aux = cbind(x_val,dd[x_val],out[i,4])
      df[nrow(df) + 1,] = aux
    }
  }
  return(df)
}

extract_all_outliers = function(dd,values,v){
  dd = c(dd,v)
  # Extracting the value of the ABS_L outliers.
  ABS_L <- rep(0,length(dd))
  for (i in 1:nrow(values)){
    ABS_L[values[i,1]] <- values[i,4]
  }
  if (sum(ABS_L) != 0) {
    # Extracting the outlier type.
    AO <- extract_outliers_graphic(values,dd,"AO",v)
    TC <- extract_outliers_graphic(values,dd,"TC",v)
    LS <- extract_outliers_graphic(values,dd,"LS",v)
    return(list(ABS_L,AO,TC,LS))
  }
  return(list(ABS_L,c(),c(),c()))
}

alarm_graphic <- function(dd,outliers,value,wAO,wTC,wLS,type){
  y_axis_labels = c("SPO2","BPM","Kg","mmHg")
  tittles = c("Oxigen Monitoring","Heart Rate Monitoring","Weight Monitoring","Blood Pressure Monitoring")
  # Including the new value.
  dd <- c(dd,value)
  
  # Selecting the variables.
  ABS_L = outliers[[1]]
  AO = outliers[[2]]
  TC = outliers[[3]]
  LS = outliers[[4]]

  for (i in 2:length(ABS_L)){
    if (ABS_L[i] != 0){
      limit = min(6,i-1)
      for (j in 1:limit){
        ABS_L[i-j] = (limit+1-j)*ABS_L[i]/limit
      }
    }
  }

  # Doing the main graphic.
  line <- data.frame(y = dd, x = seq(1,length(dd)), ABS_L = ABS_L) 
  if (!is.null(nrow(AO)) | !is.null(nrow(TC)) | !is.null(nrow(LS))) {
    aux = ggplot(data = line, aes(x = x ,y = y, color = ABS_L)) + scale_color_distiller(palette = "Spectral") + 
          labs(color = "Outlier conf.") +  geom_line(size = 1.1)
  }else aux = ggplot(data = line, aes(x = x ,y = y)) +  geom_line(size = 1.1, colour = '#0089c2')
  plot <- aux +
    xlab('Measurements') + ylab(y_axis_labels[type]) + ggtitle(tittles[type]) +
    theme_minimal() 

  
  # Plotting the desired points. 
  if (wAO && !is.null(nrow(AO)) && nrow(AO) != 0) {
   plot <- plot + geom_point(data= AO, aes(x = x, y = y), size= 5, shape = 21, color = "black")  + 
     geom_point(data= AO, aes(x = x, y = y), size= 4.2)
  }
  if (wTC && !is.null(nrow(TC)) && nrow(TC) != 0) {
    plot <- plot + geom_point(data= TC, aes(x = x, y = y), size= 5, color = "black", shape = 17) + 
    geom_point(data= TC, aes(x = x, y = y), size= 4, shape = 17)
  }
  if (wLS && !is.null(nrow(LS)) && nrow(LS) != 0){
    plot <- plot + geom_point(data= LS, aes(x = x, y = y), size= 7, color = "black", shape = 18) + 
    geom_point(data= LS, aes(x = x, y = y), size= 6, shape = 18)
  }
  return(plot)
}

# merge
merge.alarms <- function(out_t, out_f, out_w, out_p, t, f, w, p,v_t,v_f,v_w,v_p) {
  if (!is.null(out_t) && nrow(out_t) >0){
    t <- rbind(t, data.frame(SPAT_NHC=t$SPAT_NHC[1], IPO_ID = NA, Value=v_t,IPO_PAT_ID=NA,Date=Sys.Date(), ind=nrow(t)+1))
    out_t$Type <- "OXIMETER"
    out_t <- merge(out_t, t, by="ind")
    out_t <- out_t[,c('ind', 'Type', 'Date')]
  }
  if (!is.null(out_w) && nrow(out_w) >0){
    w <- rbind(w, data.frame(SPAT_NHC=w$SPAT_NHC[1],IPW_ID = NA,IPW_PAT_ID = NA, Value=v_w, Date=Sys.Date(), ind=nrow(w)+1))
    out_w["Type"] = "WEIGHT"
    out_w <- merge(out_w, w, by="ind")
    out_w <- out_w[,c('ind','Type','Date')]
  }
  if (!is.null(out_p) && nrow(out_p) >0){
    p <- rbind(p, data.frame(SPAT_NHC=p$SPAT_NHC[1], IPRE_ID= NA, IPAT_ID= NA, DPRE_LOW= NA, DPRE_HIGH= NA,Value=v_p, Date=Sys.Date(), ind=nrow(p)+1))
    out_p["Type"] = "PRESSURE"
    out_p <- merge(out_p, p, by="ind")
    out_p <- out_p[,c('ind','Type','Date')]
  }
  if (!is.null(out_f) && nrow(out_f) >0){
    f <- rbind(f, data.frame(SPAT_NHC=f$SPAT_NHC[1],IPFC_ID = NA, Value=v_f,IPFC_PAT_ID = NA, Date=Sys.Date(), ind=nrow(f)+1))
    out_f["Type"] = "CARDIAC FREQ"
    out_f <- merge(out_f, f, by="ind")
    out_f <- out_f[,c('ind','Type','Date')]
  }
  out <- rbind(out_t, out_w, out_f)
  if(!is.null(out) && nrow(out) > 0){
    out <-  out[order(out$Date, decreasing = T),]
    return (out)
  }
  else return(NULL)
}


print.alarms <- function(out_t, out_f, out_w, out_p, t, f, w, p, v_t, v_f, v_w, v_p) {
  out <- merge.alarms(out_t, out_f, out_w, out_p, t, f, w, p,v_t, v_f, v_w, v_p)
  
  if(!is.null(out)){
    date <- out[1,"Date"]
    cat(out[1,"Type"])
    i <- 2
    while (i <= nrow(out)){
      if (date == out[i,"Date"]){
        if (out[i,"Type"] != out[i-1,"Type"]){
          cat(" AND", out[i, "Type"])
        }
      }
      else{
        cat(" ALARM ON", as.character(out[i-1, "Date"]), "\n")
        cat(out[i, "Type"])
        date = out[i,"Date"]
      }
      i <- i+1
    }
    cat(" ALARM ON", as.character(out[i-1, "Date"]), "\n")
  }
  else cat("No alarms found for this patient.")
}
