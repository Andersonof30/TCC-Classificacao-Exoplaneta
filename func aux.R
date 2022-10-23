coef_bot = as.data.frame(matrix(NA, ncol = 7, nrow = iteracoes))
coef_bay = coef_bot
coef_bay.DL = coef_bay

coef_bay.DLP = as.data.frame(matrix(NA, ncol = 8, nrow = iteracoes))
coef_bay.DLRP = coef_bay.DLP


#############
# pegando os valores 
vt_qnt.1 = seq(1,14,2)
vt_qnt.2 = seq(2,14,2)

qnt_bot = as.data.frame(matrix(NA, ncol = 7*2, nrow = iteracoes))
qnt_bay = qnt_bot
qnt_bay.DL = qnt_bay


qnt_bay.DLP = as.data.frame(matrix(NA, ncol = 8*2, nrow = iteracoes))
qnt_bay.DLRP = qnt_bay.DLP

vt_qnt.1P = seq(1,16,2)
vt_qnt.2P = seq(2,16,2)
############# 


met_bay = as.data.frame(matrix(NA, ncol = 4, nrow = iteracoes))
colnames(met_bay) = c("VN", "FP", "FN", "VP")

met_bay.1 = met_bay

met_c = met_bay
met_c.1 = met_bay

met_bay.DL = met_bay
met_bay.DL.1 = met_bay

met_bay.DLP = met_bay
met_bay.DLP.1 = met_bay

met_bay.DLRP = met_bay
met_bay.DLRP.1 = met_bay

#######################

smote = function(exo.train = exo.train, sementes = sementes, i = i){
  set.seed(sementes[i])
  #### SMOTE
  exo.smote.15 = smotefamily::SMOTE(exo.train[, -1], 
                                    exo.train[, 1], dup_size = 22)
  return(exo.smote.15)
}
down_sample = function(exo.smote.15 = exo.smote.15, sementes = sementes, i = i){
  ### down sample
  set.seed(sementes[i])
  exo.downS = caret::downSample(x = exo.smote.15$data[-3198],
                                y = as.factor(exo.smote.15$data$class),
                                yname = 'class')
  return(exo.downS)
}

pca_dt = function(exo.downS = exo.downS, exo.test = exo.test,
                     sementes = sementes, i = i){
  zx = rbind(exo.downS[,-3198], exo.test[-1]) 
  zx['class'] = as.factor(c(exo.downS[,3198], exo.test[1]))
  set.seed(sementes[i])
  pca.dt <- zx |> select(-class) |>
    prcomp(,
           center = TRUE,
           scale. = TRUE)
  return(pca.dt)
}  

#prob

prob_bay = function(fit_RL = fit_RL, pca.test = pca.test){
  ##### bayesiano
  xbeta = model.matrix(~as.matrix(as.matrix(pca.test[,-7])))%*%as.matrix(fit_RL)
  predict.RL_prob = exp(xbeta)/(1+exp(xbeta))
  return(predict.RL_prob)
}

prob_bay.DL = function(fit_DL = fit_DL, pca.test = pca.test){
  ##### bayesiano
  xbeta = model.matrix(~as.matrix(as.matrix(pca.test[,-7])))%*%as.matrix(fit_DL)
  predict.RL_prob = 0.5+0.5*sign(xbeta)*(1-0.5*(2+abs(xbeta))*exp(-abs(xbeta))) 
  return(predict.RL_prob)
}

prob_bay.DLP = function(fit_DLP = fit_DLP, pca.test = pca.test){
  xbeta = model.matrix(~as.matrix(as.matrix(pca.test[,-7])))%*%as.matrix(fit_DLP[-8])
  predict.DLP_prob = (0.5+0.5*sign(xbeta)*(1-0.5*(2+abs(xbeta)
  )*exp(-abs(xbeta))))^fit_DLP[8]
  return(predict.DLP_prob)
}

prob_bay.DLRP = function(fit_DLRP = fit_DLRP, pca.test = pca.test){
  xbeta = model.matrix(~as.matrix(as.matrix(pca.test[,-7])))%*%as.matrix(fit_DLRP[-8])
  predict.DLRP_prob = 1-((0.5+
 0.5*sign(-xbeta)*(1-0.5*(2+abs(-xbeta))*exp(-abs(-xbeta))))^fit_DLRP[8])
  return(predict.DLRP_prob)
}
