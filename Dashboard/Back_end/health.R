llindar <- function(mesura, talls){
  t <- c(0:(length(talls)))
  talls <- c(-Inf, talls, Inf)
  return(cut(mesura, breaks = unique(talls), labels = t))
}

extensio_N0 <- function(dd2, l){
  nhcs <- unique(dd2$NHC)
  dd2$Nivell <- 0
  for (nhc in nhcs){
    n0 <- dd2 %>%
      filter(NHC == nhc)%>%
      filter(mesos == 0)%>%
      select(SCORE)
    N0 <- 0
    if(nrow(n0) == 0) N0 <- NA
    else N0 <- n0[1,]
    dd2[dd2$NHC == nhc, "Nivell"] <- N0
  }
  dd2$Nivell <- llindar(dd2$Nivell, l)
  return(dd2$Nivell)
}

agrupament_fluvial <- function(dd){
  dd$Nivell <- as.numeric.factor(dd$Nivell)
  dd$Nivell0 <- as.numeric.factor(dd$Nivell0)
  dd <- dd %>%
    group_by(NHC,mesos) %>%
    dplyr::summarise(
      mes_0 = (mesos == 0)*1*Nivell,
      mes_1 = (mesos == 1)*1*Nivell,
      mes_6 = (mesos == 6)*1*Nivell,
      mes_12 = (mesos == 12)*1*Nivell,
      Nivell0,
      .groups = 'drop'
    )
  dd <- dd %>%
    group_by(NHC) %>%
    dplyr::summarise(
      mes_0 = sum(mes_0),
      mes_1 = sum(mes_1),
      mes_6 = sum(mes_6),
      mes_12 = sum(mes_12),
      Nivell0 = mean(Nivell0),
      .groups = 'drop'
    )
  return(dd)
}

graph_fluvial <- function(dd,nhc,t){
  if (nhc != "") dd1 <- dd[dd$NHC == nhc,]
  else dd1 <- dd
  cols <- colnames(dd)
  dd <- dd %>% mutate_at(cols, funs(factor(.)))
  dd <- dd %>%
    group_by(mes_0, mes_1, mes_6, mes_12) %>%
    count()
  m <- c(dd1[1,]$mes_0, dd1[1,]$mes_1, dd1[1,]$mes_6, dd1[1,]$mes_12)
  dd <- as.data.frame(dd)
  
  if (nrow(dd1) > 0) dd$sel <- ifelse(dd$mes_0 ==m[1] & dd$mes_1 == m[2] & dd$mes_6 == m[3] & dd$mes_12 == m[4] , 10, 0)
  else dd$sel <- 0
  f <- ggplot(dd,
         aes(axis1 = mes_0,
             axis2 = mes_1,
             axis3 = mes_6,
             axis4 = mes_12,
             y = n, 
         )) +
    geom_alluvium(data = dd,aes(fill = mes_0)) +
    theme_minimal() +
    geom_stratum(width = 1/6,fill = "grey", color= "white") +
    geom_label(stat = "stratum", 
               color = "#333333ff",
               aes(label = after_stat(stratum))) +
    scale_x_discrete(limits = c("Month 0", "Month 1", "Month 6", "Month 12"),
                     expand = c(.05, .05, .05, .05)) +
    scale_fill_viridis_d() +
    labs(title = t,
         subtitle = "Per month and basal",
         y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none")
    if (nhc != "") {
      f <- f + geom_line(stat = "alluvium", aes(alpha = sel, color= mes_0), size= 1) +
        stat_alluvium(geom = "pointrange", aes(alpha = sel, color=mes_0)) 
    }
  
  return(f)

}

as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}


###  Graphics EQ5DL
#Read and compute scores
dd_EQ5DL.f <- function(dd) {
  scores.df <- data.frame(
    MO=dd$MOVI, SC=dd$CUID,
    UA=dd$COTI, PD=dd$DOL, AD=dd$ANSI
  )
  dd['SCORE'] <- eq5d(scores.df, country="Spain", version="5L", type="VT", ignore.invalid=TRUE)
  #Extending levels
  dd$Nivell <- extensio_N0(dd, c(0.5, 0.75))
  dd$Nivell0 <-dd$Nivell
  dd$Nivell <- llindar(dd$SCORE, c(0.5, 0.75))
  return(dd)
}

#Nivells 
fluv_EQ5DL <- function(dd, nhc) {
  dd2_short_og <- dd[,c(2:9,11,18,19)]
  dd2_short_og$Nivell0 <- dd2_short_og$Nivell
  dd2_short_og$Nivell <- llindar(dd2_short_og$SCORE, c(0.50, 0.75))
  
  #Alluvium graph
  dd2_short_og <- agrupament_fluvial(dd2_short_og)
  f <- graph_fluvial(dd2_short_og, nhc, "Quality of Life: EQ5DL Score")
  return(f)
}


agrupacio_radar_mes_nivell <- function(dd, kansas){
  dd <- dd %>%
    gather( key=score_type, value = scoreT, -NHC, -mesos, -Nivell0) %>%
    group_by(mesos, score_type, Nivell0) %>%
    summarize(
      scoreT = mean(scoreT, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(Nivell0)) %>%
    spread( key = score_type, value = scoreT, fill = NA, convert = FALSE)
  if (kansas) {
    dd <- dd %>% mutate(
      PL = ifelse( 100-PL >= 0, 100-PL, 0 ),
      QL = ifelse( 100-QL >= 0, 100-QL, 0 ),
      SF = ifelse( 100-SF >= 0, 100-SF, 0 ),
      SL = ifelse( 100-SL >= 0, 100-SL, 0 )
    )
  }
  return(dd)
}


### Kansas

fluv_kansas <- function(dd3, nhc) {
  dd3$Nivell <- extensio_N0(dd3, c(50,75))
  dd3$Nivell0 <-dd3$Nivell
  
  dd3$Nivell <- llindar(dd3$SCORE, c(50, 75))
  
  dd3_short <- agrupament_fluvial(dd3)
  f <- graph_fluvial(dd3_short, nhc, "Qualiy of Life: Kansas Score")
  return (f)
}

radar_graph_individual <- function(dd, nhc, mes, t, kansas){
  if(kansas) dd_post <- dd[c("NHC","mesos", "Nivell", "PL", "QL", "SF", "SL")]
  else dd_post <- dd[c("NHC","mesos", "Nivell","MOVI","CUID","COTI", "DOL","ANSI")]
  if (nhc != "") dd_post <- dd_post[dd_post$NHC == nhc,]
  if (mes != "All") dd_post <- dd_post[dd_post$mesos == mes,]
  dd_post$mesos <- as.factor(dd_post$mesos)
  dd_post <- dd_post %>%
    select(-NHC)
  
  levels(dd_post$Nivell) <- c("Bad (0)", "Mid (1)", "Well (2)")

    if (kansas) {
      dd_post <- dd_post %>% mutate(
      PL = ifelse( 100-PL >= 0, 100-PL, 0 ),
      QL = ifelse( 100-QL >= 0, 100-QL, 0 ),
      SF = ifelse( 100-SF >= 0, 100-SF, 0 ),
      SL = ifelse( 100-SL >= 0, 100-SL, 0 )
      )
      limit <- 100
    } else limit <- 5
  ggRadar(dd_post, aes(color=Nivell, facet = mesos), interactive = TRUE,rescale=FALSE, ylim=c(0,limit),legend.position = "right")
}

radar_graph_app <- function(dd, app = 0, t, kansas){
  if(kansas) dd_post <- dd[c("NHC","mesos", "Nivell", "Nivell0", "PL", "QL", "SF", "SL", "APP_INCL")]
  else dd_post <- dd[c("NHC","mesos", "Nivell", "Nivell0", "MOVI","CUID","COTI", "DOL","ANSI", "APP_INCL")]
  dd_post <- dd_post[dd_post$APP_INCL == app,]
  dd_post <- dd_post %>% select(-APP_INCL, -Nivell)
  dd_post <- agrupacio_radar_mes_nivell(dd_post, kansas)
  if(kansas) limit <- 100
  else limit <- 5
  levels(dd_post$Nivell0) <- c("Bad (0)", "Mid (1)", "Well (2)")
  ggRadar(dd_post, aes(color=Nivell0, facet = mesos), interactive = TRUE,rescale=FALSE, ylim=c(0,limit),legend.position = "right")

}


dd_kansas.f <- function(dd3) {
  dd3$Nivell <- extensio_N0(dd3, c(50,75))
  dd3$Nivell0 <-dd3$Nivell
  dd3$Nivell <- llindar(dd3$SCORE, c(50, 75))
  return(dd3)
}

# dd3 <- read.csv("./Desktop/GCED/Tercer/Q2/PE/Graphics/full_EQ5DL.csv")
#  # nhc <- 6970569
# dd3 <- dd_EQ5DL.f(dd3)
# 
# #fluv_EQ5DL(dd3, nhc)
# 
# #radar_graph_individual(dd3, "", "All", "Radar Chart", F)
# #radar_graph_app(dd3, 1, "Radar Chart", T)
# app.incl <- T
# rad.app.incl.type <- "EQ5DL"
# 
# if (app.incl) {app <- 1
# } else {app <- 0}
# 
# if (rad.app.incl.type == "Kansas") {plot <- radar_graph_app(dd3, app, "Radar Chart", T)
# }else plot <- radar_graph_app(dd3, app, "Radar Chart", F)
# 
# plot
