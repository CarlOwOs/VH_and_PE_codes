## ----echo = T, results = 'hide'----------------------------------------------------
library(dplyr)
library(naivebayes)
library(MASS)
library(klaR)
library(nnet)
library(caret)
library(randomForest)
library(nnet)
library(rpart)
library(cowplot)
library(missForest)
set.seed(123)


## ----------------------------------------------------------------------------------
set.seed(1234)
dd=read.csv("./Temp/continuous_CS5.csv")
nhc = dd[,1]
dd = dd[,-1]
targets=c("target_1","target_3","target_6","target_12")
dd[targets]=lapply(dd[targets],factor)


## ----------------------------------------------------------------------------------
aux=colnames(dd)
no_factor = c("edat_visita","days_since_start","pes_seguim","talla_seguim","pas_seguim","pad_seguim","fc_seguim","data_visita_day","data_visita_year","data_visita_month","data_visita_week","diff_anal_seguim","days_since_diagnostic","pes_seguim","pas_seguim","fc_seguim","days_since_start","amplada_qrs_seguim","feve_seguim","dtdve_seguim","tiv_seguim","paret","auricula_seguim","ona_e_seguim","ona_a_seguim","temps_desac_seguim","ona_e_prima_seguim","insuf_mitral_seguim","tapse_seguim","diff_anal_seguim","hb_seguim","ferritina_seguim","sat_transf_seguim","ha1c_seguim","creat_seguim","fge_seguim","urat_seguim","sodi_seguim","potassi_seguim","cloro_seguim","tni_seguim","probnp_seguim","cole_seguim","colehdl_segui","coleldl_segui","trigli_seguim","prot_seguim","albu_seguim","ca125_seguim","st2_seguim","albuorin_seguim","prot_creorin_seguim","paps_seguim","relacio_seguim","amplada_qrs_seguim","albu_crorin_seguim")
aux = setdiff(aux,no_factor)
dd[aux]<-lapply(dd[aux],factor)
head(dd)


## ----echo = T, results = 'hide',include= F-----------------------------------------
dd$edat_visita = as.numeric(dd$edat_visita)
dd$days_since_start = as.numeric(dd$days_since_start)
dd$amplada_qrs_seguim = as.factor(dd$amplada_qrs_seguim)
dd$pes_seguim = as.numeric(dd$pes_seguim)
dd$talla_seguim = as.numeric(dd$talla_seguim)
dd$pas_seguim = as.numeric(dd$pas_seguim)
dd$pad_seguim = as.numeric(dd$pad_seguim)
dd$fc_seguim = as.numeric(dd$fc_seguim)
dd$data_visita_day = as.numeric(dd$data_visita_day)
dd$data_visita_year = as.numeric(dd$data_visita_year)
dd$data_visita_month = as.numeric(dd$data_visita_month)
dd$data_visita_week = as.numeric(dd$data_visita_week)
dd$diff_anal_seguim = as.numeric(dd$diff_anal_seguim)
dd$days_since_diagnostic = as.numeric(dd$days_since_diagnostic)
dd$days_since_start=as.numeric(dd$days_since_start)
dd$feve_seguim = as.numeric(dd$feve_seguim)
dd$dtdve_seguim = as.numeric(dd$dtdve_seguim)
dd$tiv_seguim = as.numeric(dd$tiv_seguim)
dd$paret = as.numeric(dd$paret)
dd$auricula_seguim = as.numeric(dd$auricula_seguim)
dd$ona_e_seguim = as.numeric(dd$ona_e_seguim)
dd$ona_a_seguim = as.numeric(dd$ona_a_seguim)
dd$temps_desac_seguim = as.numeric(dd$temps_desac_seguim)
dd$ona_e_prima_seguim = as.numeric(dd$ona_e_prima_seguim)
dd$tapse_seguim = as.numeric(dd$tapse_seguim)
dd$diff_anal_seguim = as.numeric(dd$diff_anal_seguim)
dd$hb_seguim = as.numeric(dd$hb_seguim)
dd$ferritina_seguim = as.numeric(dd$ferritina_seguim)
dd$sat_transf_seguim = as.numeric(dd$sat_transf_seguim)
dd$ha1c_seguim = as.numeric(dd$ha1c_seguim)
dd$creat_seguim = as.numeric(dd$creat_seguim)
dd$fge_seguim = as.numeric(dd$fge_seguim)
dd$urat_seguim = as.numeric(dd$urat_seguim)
dd$sodi_seguim = as.numeric(dd$sodi_seguim)
dd$potassi_seguim = as.numeric(dd$potassi_seguim)
dd$cloro_seguim = as.numeric(dd$cloro_seguim)
dd$tni_seguim = as.numeric(dd$tni_seguim)
dd$probnp_seguim = as.numeric(dd$probnp_seguim)
dd$cole_seguim = as.numeric(dd$cole_seguim)
dd$colehdl_segui = as.numeric(dd$colehdl_segui)
dd$coleldl_segui = as.numeric(dd$coleldl_segui)
dd$trigli_seguim = as.numeric(dd$trigli_seguim)
dd$prot_seguim = as.numeric(dd$prot_seguim)
dd$albu_seguim = as.numeric(dd$albu_seguim)
dd$ca125_seguim = as.numeric(dd$ca125_seguim)
dd$st2_seguim = as.numeric(dd$st2_seguim)
dd$albuorin_seguim = as.numeric(dd$albuorin_seguim)
dd$prot_creorin_seguim = as.numeric(dd$prot_creorin_seguim)
dd$paps_seguim = as.numeric(dd$paps_seguim)
dd$relacio_seguim = as.numeric(dd$relacio_seguim)
dd$amplada_qrs_seguim = as.numeric(dd$amplada_qrs_seguim)
dd$albu_crorin_seguim = as.numeric(dd$albu_crorin_seguim)


## ----------------------------------------------------------------------------------
dd[dd == ""] <- NA


## ----echo = T, results = 'hide',include= F-----------------------------------------
X.imp <- missForest(xmis = dd, maxiter = 10, mtry = 10, ntree = 200)
Y.imp <- X.imp$ximp


## ----------------------------------------------------------------------------------
Y.imp = cbind(nhc,Y.imp)

## ----------------------------------------------------------------------------------
write.csv(Y.imp, "./Temp/imputed_R.csv")

