llindar <- function(mesura, talls){
  t <- c(0:(length(talls)))
  talls <- c(-Inf, talls, Inf)
  return(cut(mesura, breaks = unique(talls), labels = t))
}

general <- function(dd_visites) {
  
  dd_visites<- dd_visites %>% filter(!is.na(edat_visita))
  var_vis <- c("nhc", "sexe", "data_visita", "ritme_base_seguim", "amplada_qrs_seguim", "trastorn_conduccio_t_v_1", "feve_seguim", "dtdve_seguim", "tapse_seguim", "insuf_mitral_seguim", "pes_seguim", "talla_seguim", "pas_seguim", "pad_seguim", "fc_seguim", "classe_funcional_seguim", "hb_seguim", "fge_seguim", "sodi_seguim", "potassi_seguim", "prot_seguim", "albu_seguim", "cole_seguim", "colehdl_segui", "coleldl_segui", "albuorin_seguim", "prot_creorin_seguim", "albu_crorin_seguim", "probnp_seguim", "st2_seguim", "tni_seguim", "ca125_seguim", "ttm_seguim___1", "ttm_seguim___2", "ttm_seguim___3", "ttm_seguim___4", "ttm_seguim___5", "ttm_seguim___6", "ttm_seguim___7", "ttm_seguim___8", "ttm_seguim___9", "ttm_seguim___10", "ttm_seguim___11", "ttm_seguim___12", "ttm_seguim___13", "ttm_seguim___14", "ttm_seguim___15", "ttm_seguim___16", "ttm_seguim___17", "ttm_seguim___18", "ttm_seguim___19", "ttm_seguim___20", "ttm_seguim___21", "ttm_seguim___22", "ttm_seguim___23", "ttm_seguim___24", "ttm_seguim___25", "ttm_seguim___26", "ttm_seguim___27", "ttm_seguim___28", "ttm_seguim___29", "ttm_seguim___30", "ttm_seguim___31", "diur_segui___1", "diur_segui___2", "diur_segui___3", "mg_diur_segui", "tiaz_seguim___1", "tiaz_seguim___2", "tiaz_seguim___3", "mg_tiaz_seguim", "antial_seguim___1", "antial_seguim___2", "antial_seguim___3", "mg_anti_seguim", "ieca_seguim___1", "ieca_seguim___2", "ieca_seguim___3", "ieca_seguim___4", "mg_ieca_seguim", "ara2_seguim___1", "ara2_seguim___2", "ara2_seguim___3", "ara2_seguim___4", "mg_ara2_seguim", "beta_seguim___1", "beta_seguim___2", "beta_seguim___3", "beta_seguim___4", "beta_seguim___5", "mg_beta_seguim", "mg_sacu_seguim", "islgt2_seguim___1", "islgt2_seguim___2", "islgt2_seguim___3", "islgt2_seguim___4", "mg_islgt2_seguim", "naco_seguim___1", "naco_seguim___2", "naco_seguim___3", "naco_seguim___4", "naco_seguim___5", "mg_naco_seguim", "edat_visita", "days_since_start", "IMC" , "estacio_visita" )
  
  dd_visites_salut <- dd_visites[var_vis]
  
  return(dd_visites_salut)
}

freq_card <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$freq_card_nivell <- llindar(dd_visites_salut$fc_seguim, c(65,80))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$freq_card_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$freq_card_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_fc <- dd_visites_salut
  dd_fc<- dd_visites_salut %>% filter(!is.na(fc_seguim))
  dd_fc<- dd_fc %>% filter(!is.na(days_since_start))
  
  freq_card_seguim <- dd_fc$fc_seguim
  temps_visita <- dd_fc$days_since_start
  freq_card_nivell <- dd_fc$freq_card_nivell
  seguiment_salut_no_na <- data.frame(freq_card_seguim,temps_visita,freq_card_nivell)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=freq_card_seguim, color=freq_card_nivell)) +
    geom_point() + geom_smooth(formula = y ~ x, method=lm, aes(fill=freq_card_nivell))
  
  return(p)
  
}

imc <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$IMC_nivell <- llindar(dd_visites_salut$IMC, c(20,35,50))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$IMC_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$IMC_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_imc <- dd_visites_salut
  dd_imc <- dd_visites_salut %>% filter(!is.na(IMC))
  dd_imc <- dd_imc %>% filter(!is.na(days_since_start))
  
  IMC <- dd_imc$IMC
  temps_visita <- dd_imc$days_since_start
  IMC_nivells <- dd_imc$IMC_nivell
  seguiment_salut_no_na <- data.frame(IMC, temps_visita, IMC_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=IMC, color=IMC_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=IMC_nivells))
  
  return(p)
}

feve <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$feve_nivell <- llindar(dd_visites_salut$feve_seguim, c(27,35,42))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$feve_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$feve_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_feve <- dd_visites_salut
  dd_feve <- dd_visites_salut %>% filter(!is.na(feve_seguim))
  dd_feve <- dd_feve %>% filter(!is.na(days_since_start))
  
  feve_seguim <- dd_feve$feve_seguim
  temps_visita <- dd_feve$days_since_start
  feve_nivells <- dd_feve$feve_nivell
  seguiment_salut_no_na <- data.frame(feve_seguim, temps_visita, feve_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=feve_seguim, color=feve_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=feve_nivells))
  
  return(p)
  
}

qrs <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$ampla_qrs_nivell <- llindar(dd_visites_salut$amplada_qrs_seguim, c(90,100,140))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$ampla_qrs_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$ampla_qrs_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_qrs <- dd_visites_salut
  dd_qrs<- dd_visites_salut %>% filter(!is.na(amplada_qrs_seguim))
  dd_qrs<- dd_qrs %>% filter(!is.na(days_since_start))
  
  amplada_qrs_seguim <- dd_qrs$amplada_qrs_seguim
  temps_visita <- dd_qrs$days_since_start
  ampla_qrs_nivell <- dd_qrs$ampla_qrs_nivell
  seguiment_salut_no_na <- data.frame(amplada_qrs_seguim,temps_visita,ampla_qrs_nivell)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=amplada_qrs_seguim, color=ampla_qrs_nivell)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=ampla_qrs_nivell))
  
  return(p)
  
}

hemoglobine <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$hb_nivell <- llindar(dd_visites_salut$hb_seguim, c(11.7,13.3,14.5))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$hb_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$hb_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_hb <- dd_visites_salut
  dd_hb <- dd_visites_salut %>% filter(!is.na(hb_seguim))
  dd_hb <- dd_hb %>% filter(hb_seguim < 100)
  dd_hb <- dd_hb %>% filter(!is.na(days_since_start))
  
  hb_seguim <- dd_hb$hb_seguim
  temps_visita <- dd_hb$days_since_start
  hb_nivells <- dd_hb$hb_nivell
  seguiment_salut_no_na <- data.frame(hb_seguim, temps_visita, hb_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=hb_seguim, color=hb_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=hb_nivells))
  
  return(p)
  
}

glom_filtr <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$fge_nivell <- llindar(dd_visites_salut$fge_seguim, c(45,68,90))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$fge_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$fge_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_fge <- dd_visites_salut
  dd_fge <- dd_visites_salut %>% filter(!is.na(fge_seguim))
  dd_fge <- dd_fge %>% filter(!is.na(days_since_start))
  
  fge_seguim <- dd_fge$fge_seguim
  temps_visita <- dd_fge$days_since_start
  fge_nivells <- dd_fge$fge_nivell
  seguiment_salut_no_na <- data.frame(fge_seguim, temps_visita, fge_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=fge_seguim, color=fge_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=fge_nivells))
  
  return(p)
  
}

sap <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$pas_nivell <- llindar(dd_visites_salut$pas_seguim, c(109,123,139))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$pas_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$pas_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_pas <- dd_visites_salut
  dd_pas <- dd_visites_salut %>% filter(!is.na(pas_seguim))
  dd_pas <- dd_pas %>% filter(!is.na(days_since_start))
  
  pas_seguim <- dd_pas$pas_seguim
  temps_visita <- dd_pas$days_since_start
  pas_nivells <- dd_pas$pas_nivell
  seguiment_salut_no_na <- data.frame(pas_seguim, temps_visita, pas_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=pas_seguim, color=pas_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=pas_nivells))
  
  return(p)
  
}

dap <- function(dd) {
  dd_visites_salut <- general(dd)
  dd_visites_salut$pad_nivell <- llindar(dd_visites_salut$pad_seguim, c(66,75,83))
  nhcs <- unique(dd_visites_salut$nhc)
  for(i in nhcs){
    freqs <- array(dd_visites_salut$pad_nivell[dd_visites_salut$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_visites_salut$pad_nivell[dd_visites_salut$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_pad <- dd_visites_salut
  dd_pad <- dd_visites_salut %>% filter(!is.na(pad_seguim))
  dd_pad <- dd_pad %>% filter(!is.na(days_since_start))
  
  pad_seguim <- dd_pad$pad_seguim
  temps_visita <- dd_pad$days_since_start
  pad_nivells <- dd_pad$pad_nivell
  seguiment_salut_no_na <- data.frame(pad_seguim, temps_visita, pad_nivells)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=pad_seguim, color=pad_nivells)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=pad_nivells))
  
  return(p)
  
}


freq_card_nb <- function(dd_visites) {

  dd_visites<- dd_visites %>% filter(!is.na(edat_visita))
  
  betas<- dd_visites[c("nhc","data_visita","beta_seguim___1","beta_seguim___2","beta_seguim___3","beta_seguim___4","beta_seguim___5")]
  
  betas_long <- gather(betas, beta, valor, c("beta_seguim___1" ,"beta_seguim___2", "beta_seguim___3" ,"beta_seguim___4","beta_seguim___5"),factor_key=TRUE)
  betas_long = betas_long[betas_long$valor == "Checked",]
  betas_long = betas_long[c("nhc", "beta", "data_visita")]
  
  nhc_total <- dd_visites$nhc
  nhc_beta <- betas_long$nhc
  nhc_sensebeta <- setdiff(nhc_total, nhc_beta)
  
  dd_visites_salut <- general(dd_visites)
  
  dd_vis_sense <- dd_visites_salut
  dd_vis<- dd_vis_sense %>% filter(is.element(nhc, nhc_sensebeta))
  
  dd_vis$freq_card_nivell <- llindar(dd_vis$fc_seguim, c(65,80))
  nhcs <- unique(dd_vis$nhc)
  for(i in nhcs){
    freqs <- array(dd_vis$freq_card_nivell[dd_vis$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_vis$freq_card_nivell[dd_vis$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_fc <- dd_vis
  dd_fc<- dd_vis %>% filter(!is.na(fc_seguim))
  dd_fc<- dd_fc %>% filter(!is.na(days_since_start))
  
  freq_card_seguim <- dd_fc$fc_seguim
  temps_visita <- dd_fc$days_since_start
  freq_card_nivell <- dd_fc$freq_card_nivell
  seguiment_salut_no_na <- data.frame(freq_card_seguim,temps_visita,freq_card_nivell)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=freq_card_seguim, color=freq_card_nivell)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=freq_card_nivell))
  
  return(p)
}

freq_card_b <- function(dd_visites) {
  
  dd_visites<- dd_visites %>% filter(!is.na(edat_visita))
  
  betas<- dd_visites[c("nhc","data_visita","beta_seguim___1","beta_seguim___2","beta_seguim___3","beta_seguim___4","beta_seguim___5")]
  betas_long <- gather(betas, beta, valor, c("beta_seguim___1" ,"beta_seguim___2", "beta_seguim___3" ,"beta_seguim___4","beta_seguim___5"),factor_key=TRUE)
  betas_long = betas_long[betas_long$valor == "Checked",]
  betas_long =betas_long[c("nhc", "beta", "data_visita")]
  
  nhc_total <- dd_visites$nhc
  nhc_beta <- betas_long$nhc
  nhc_sensebeta <- setdiff(nhc_total, nhc_beta)
  
  dd_vis_amb <- general(dd_visites)
  dd_vis<- dd_vis_amb %>% filter(is.element(nhc, nhc_beta))
  
  dd_vis$freq_card_nivell <- llindar(dd_vis$fc_seguim, c(65,80))
  nhcs <- unique(dd_vis$nhc)
  for(i in nhcs){
    freqs <- array(dd_vis$freq_card_nivell[dd_vis$nhc == i])
    j = 1
    while(j <= length(freqs) && is.na(freqs[j])) j = j + 1
    if(j <=  length(freqs) ) dd_vis$freq_card_nivell[dd_vis$nhc == i] <- rep(freqs[j],length(freqs))
  }
  
  dd_fc <- dd_vis
  dd_fc<- dd_vis %>% filter(!is.na(fc_seguim))
  dd_fc<- dd_fc %>% filter(!is.na(days_since_start))
  
  freq_card_seguim <- dd_fc$fc_seguim
  temps_visita <- dd_fc$days_since_start
  freq_card_nivell <- dd_fc$freq_card_nivell
  seguiment_salut_no_na <- data.frame(freq_card_seguim,temps_visita,freq_card_nivell)
  
  p <- ggplot(seguiment_salut_no_na, aes(x=temps_visita, y=freq_card_seguim, color=freq_card_nivell)) +
    geom_point() + 
    geom_smooth(formula = y ~ x, method=lm, aes(fill=freq_card_nivell))
  
  return(p)
}

tractament <- function(dd_visites) {
  
  dd_visites<- dd_visites %>% filter(!is.na(edat_visita))
  tractaments <- dd_visites[c("nhc","data_visita","days_since_start","ttm_seguim___1", "ttm_seguim___2", "ttm_seguim___3", "ttm_seguim___4", "ttm_seguim___5", "ttm_seguim___6", "ttm_seguim___7", "ttm_seguim___8", "ttm_seguim___9", "ttm_seguim___10", "ttm_seguim___11", "ttm_seguim___12", "ttm_seguim___13", "ttm_seguim___14", "ttm_seguim___15", "ttm_seguim___16", "ttm_seguim___17", "ttm_seguim___18", "ttm_seguim___19", "ttm_seguim___20", "ttm_seguim___21", "ttm_seguim___22", "ttm_seguim___23", "ttm_seguim___24", "ttm_seguim___25", "ttm_seguim___26", "ttm_seguim___27", "ttm_seguim___28", "ttm_seguim___29", "ttm_seguim___30", "ttm_seguim___31")]
  tractaments_long <- gather(tractaments, ttm, valor, c("ttm_seguim___1", "ttm_seguim___2", "ttm_seguim___3", "ttm_seguim___4", "ttm_seguim___5", "ttm_seguim___6", "ttm_seguim___7", "ttm_seguim___8", "ttm_seguim___9", "ttm_seguim___10", "ttm_seguim___11", "ttm_seguim___12", "ttm_seguim___13", "ttm_seguim___14", "ttm_seguim___15", "ttm_seguim___16", "ttm_seguim___17", "ttm_seguim___18", "ttm_seguim___19", "ttm_seguim___20", "ttm_seguim___21", "ttm_seguim___22", "ttm_seguim___23", "ttm_seguim___24", "ttm_seguim___25", "ttm_seguim___26", "ttm_seguim___27", "ttm_seguim___28", "ttm_seguim___29", "ttm_seguim___30", "ttm_seguim___31"),factor_key=TRUE)
  tractaments_long = tractaments_long[tractaments_long$valor == "Checked",]
  tractaments_long = tractaments_long[c("nhc", "ttm", "data_visita", "days_since_start")]
  
  noms <-c(1:31)
  names(noms) <- c("ttm_seguim___1", "ttm_seguim___2", "ttm_seguim___3", "ttm_seguim___4", "ttm_seguim___5", "ttm_seguim___6", "ttm_seguim___7", "ttm_seguim___8", "ttm_seguim___9", "ttm_seguim___10", "ttm_seguim___11", "ttm_seguim___12", "ttm_seguim___13", "ttm_seguim___14", "ttm_seguim___15", "ttm_seguim___16", "ttm_seguim___17", "ttm_seguim___18", "ttm_seguim___19", "ttm_seguim___20", "ttm_seguim___21", "ttm_seguim___22", "ttm_seguim___23", "ttm_seguim___24", "ttm_seguim___25", "ttm_seguim___26", "ttm_seguim___27", "ttm_seguim___28", "ttm_seguim___29", "ttm_seguim___30", "ttm_seguim___31")
  
  b <- c()
  for (i in tractaments_long$ttm){
    c <-noms[[i]]
    b = cbind(b, c)
  }
  tractaments_long$num_ttm <- b[1,]
  
  tractaments_long <- tractaments_long[c('nhc', 'data_visita','days_since_start', 'num_ttm')]
  
  tractaments_long$data_visita <- ymd(format(as.Date(tractaments_long$data_visita), "%Y-%m-%d"))
  tractaments_agg= (tractaments_long %>% 
                      group_by(num_ttm, month=floor_date(data_visita, "month")) %>%
                      tally()
  )
  tractaments_agg$num_ttm <- as.factor(tractaments_agg$num_ttm)
  
  p1 <- ggplot(tractaments_agg, aes(x = month, y = n, fill=num_ttm))+
    geom_bar(position="stack", stat="identity")
  p2 <- ggplot(tractaments_agg, aes(x = month, y = n, fill=num_ttm))+
    geom_bar(position="fill", stat="identity")
  
  tractaments_agg$num_ttm <- as.factor(tractaments_agg$num_ttm)
  p3 <- ggplot(tractaments_agg, aes(x = month, y = n, fill = num_ttm) ) + geom_area()
  
  return(list(p1, p2, p3))
  
  
}
