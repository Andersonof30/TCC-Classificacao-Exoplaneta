source('packages.R')

setwd("C:/Users/Anderson ssd/Desktop/TCC")

exo.test = read_csv("exoTest.csv")
exo.test = mutate(exo.test, LABEL = ifelse(LABEL == 2, 1, 0))
exo.train = read_csv("exoTrain.csv")
exo.train = mutate(exo.train, LABEL = ifelse(LABEL == 2, 1, 0))

iteracoes = 50

source('func aux.R')

i = 1

set.seed(123)
sementes = sample(1:10000000, iteracoes)

while(i <= iteracoes){
  print(Sys.time())
  #SMOTE
  exo.smote.15 = smote(exo.train = exo.train, 
                      sementes = sementes, i = i)
  #DownSample
  exo.downS = down_sample(exo.smote.15 = exo.smote.15, 
                          sementes = sementes, i = i)
  ### PCA
  pca.dt = pca_dt(exo.downS = exo.downS, exo.test = exo.test,
                  sementes = sementes, i = i)
  
  pca.train = data.frame(pca.dt$x[1:1702,1:6], 
                         'class' = exo.downS$class) |> as_tibble()
  
  pca.train = mutate(pca.train, class = ifelse(class == 1,1,0))
  
  pca.test = data.frame( pca.dt$x[1703:2272,1:6], 
                         'class' = exo.test$LABEL) |> as_tibble()
  pca.test = mutate(pca.test, class = as.factor(ifelse(class == 1,1,0)))
  
  
  m_gl = glm(class ~ ., data = pca.train, family = binomial)
  coef_bot[i,] = m_gl$coefficients
  
  ### classico
  predict.prob_c = predict(m_gl, pca.test[-7], type = 'response')
  predict.RL_c = as.factor(ifelse(predict.prob_c > 0.5, 1, 0) )
  mc_c = caret::confusionMatrix(data = predict.RL_c, reference = pca.test$class)$table
  VN_c=mc_c[1,1]; FP_c=mc_c[2,1]; FN_c=mc_c[1,2]; VP_c=  mc_c[2,2]
  met_c.1[i,] = c(VN_c, FP_c, FN_c, VP_c)
  
  predict.RL_c = as.factor(ifelse(predict.prob_c > 
                                    predict.prob_c |> mean(na.rm = T) , 1, 0) )
  mc_c = caret::confusionMatrix(data = predict.RL_c, reference = pca.test$class)$table
  VN_c=mc_c[1,1]; FP_c=mc_c[2,1]; FN_c=mc_c[1,2]; VP_c=  mc_c[2,2]
  met_c[i,] = c(VN_c, FP_c, FN_c, VP_c)
  
  #####################
  
  fit_RL = data.frame(0)
  while(dim(fit_RL)[2] == 1){
    #set.seed(sementes[i])
    fit.reg <- stan(file = 'RL.stan',
                        data = list(Y=pca.train$class,
                                    N=dim(pca.train)[1],
                                    X1=pca.train$PC1, 
                                    X2=pca.train$PC2,
                                    X3=pca.train$PC3,
                                    X4=pca.train$PC4,
                                    X5=pca.train$PC5,
                                    X6=pca.train$PC6),
                        iter = 5000,
                        #init = g.init,
                        chains = 1,
                        refresh =0)
    
    
    fit_RL = as.data.frame(fit.reg)
  }
  
  qnt_bay[i,vt_qnt.1] = fit_RL[,1:7]|> apply(2, quantile, 0.025)
  qnt_bay[i,vt_qnt.2] = fit_RL[,1:7]|> apply(2, quantile, 0.975)
  
  fit_RL = fit_RL[,1:7]|> apply(2, mean)
  coef_bay[i,] = fit_RL 
  
  
  predict.RL_prob = prob_bay(fit_RL = fit_RL, pca.test = pca.test)
  predict.RL = as.factor(ifelse(predict.RL_prob > 0.5, 1, 0))
  
  
  mc=caret::confusionMatrix(data = predict.RL, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.1[i,] = c(VN, FP, FN, VP)
  #met_bay.1 é o limiar é de 0.5
  
  
  thr_roc = pROC::coords(pROC::roc(pca.train$class, 
     prob_bay(fit_RL = fit_RL, pca.test = pca.train)), 
                         "best",ret=c("threshold"))$threshold
  
  predict.RL = as.factor(ifelse(predict.RL_prob > thr_roc[1]
                                  , 1, 0))
  
  
  mc=caret::confusionMatrix(data = predict.RL, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay[i,] = c(VN, FP, FN, VP)

  
  #############
  
  fit_DL = data.frame(0)
  while(dim(fit_DL)[2] == 1){
    #set.seed(sementes[i])
    fit.reg.DL <- stan(file = 'DL.stan',
                                           data = list(Y=pca.train$class,
                                                       N=dim(pca.train)[1],
                                                       X1=pca.train$PC1, 
                                                       X2=pca.train$PC2,
                                                       X3=pca.train$PC3,
                                                       X4=pca.train$PC4,
                                                       X5=pca.train$PC5,
                                                       X6=pca.train$PC6),
                                           iter = 5000,
                                           #init = g.init,
                                           chains = 1,
                                           refresh =0)
    
    
    fit_DL = as.data.frame(fit.reg.DL)
  }
  
  qnt_bay.DL[i,vt_qnt.1] = fit_DL[,1:7]|> apply(2, quantile, 0.025)
  qnt_bay.DL[i,vt_qnt.2] = fit_DL[,1:7]|> apply(2, quantile, 0.975)
  
  fit_DL = fit_DL[,1:7] |> apply(2, mean)
  coef_bay.DL[i,] = fit_DL 
  
  
  predict.DL_prob = prob_bay.DL(fit_DL = fit_DL, pca.test = pca.test)
  predict.DL = as.factor(ifelse(predict.DL_prob > 0.5, 1, 0))
  
  
  mc=caret::confusionMatrix(data = predict.DL, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DL.1[i,] = c(VN, FP, FN, VP)
  #met_bay.DL.1 é o limiar é de 0.5

  thr_roc = pROC::coords(pROC::roc(pca.train$class, 
  prob_bay.DL(fit_DL = fit_DL, pca.test = pca.train)), 
  "best",ret=c("threshold"))$threshold
  
  
  predict.DL = as.factor(ifelse(predict.DL_prob > thr_roc[1], 1, 0))
  
  
  mc=caret::confusionMatrix(data = predict.DL, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DL[i,] = c(VN, FP, FN, VP)
  
  print('metade')
  
  ############ 
  fit_DLP = data.frame(0)
  while(dim(fit_DLP)[2] == 1){
    #set.seed(sementes[i])
    fit.reg.DLP <- stan(file = 'DLP.stan',
                                         data = list(Y=pca.train$class,
                                                     N=dim(pca.train)[1],
                                                     X1=pca.train$PC1, 
                                                     X2=pca.train$PC2,
                                                     X3=pca.train$PC3,
                                                     X4=pca.train$PC4,
                                                     X5=pca.train$PC5,
                                                     X6=pca.train$PC6),
                                         iter = 5000,
                                         #init = c(DLP.init),
                                         chains = 1,
                                         refresh =0)
    
    fit_DLP = as.data.frame(fit.reg.DLP)
  }
  
  qnt_bay.DLP[i,vt_qnt.1P] = fit_DLP[,1:8]|> apply(2, quantile, 0.025)
  qnt_bay.DLP[i,vt_qnt.2P] = fit_DLP[,1:8]|> apply(2, quantile, 0.975)
  
  fit_DLP = fit_DLP[,1:8] |> apply(2, mean)
  coef_bay.DLP[i,] = fit_DLP 
  
  
  predict.DLP_prob = prob_bay.DLP(fit_DLP = fit_DLP, pca.test = pca.test)
  
  predict.DLP = as.factor(
    ifelse(predict.DLP_prob > 0.5, 1, 0)) 
  
  mc=caret::confusionMatrix(data = predict.DLP, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DLP.1[i,] = c(VN, FP, FN, VP)
  
  ##### média
  thr_roc = pROC::coords(pROC::roc(pca.train$class, 
  prob_bay.DLP(fit_DLP = fit_DLP, pca.test = pca.train)), 
  "best",ret=c("threshold"))$threshold
  
  predict.DLP = as.factor(
    ifelse(predict.DLP_prob > thr_roc[1], 1, 0)) 
  
  mc=caret::confusionMatrix(data = predict.DLP, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DLP[i,] = c(VN, FP, FN, VP)
  
  
  
  ############ 
  
  fit_DLRP = data.frame(0)
  while(dim(fit_DLRP)[2] == 1){
    
    #set.seed(sementes[i])
    fit.reg.DLRP <- stan(file = 'DLRP.stan',
                       data = list(Y=pca.train$class,
                                   N=dim(pca.train)[1],
                                   X1=pca.train$PC1, 
                                   X2=pca.train$PC2,
                                   X3=pca.train$PC3,
                                   X4=pca.train$PC4,
                                   X5=pca.train$PC5,
                                   X6=pca.train$PC6),
                       iter = 5000,
                       #init = c(dlrp.init),
                       chains = 1,                         
                       refresh = 0)
  
  fit_DLRP = as.data.frame(fit.reg.DLRP)
  }
  
  qnt_bay.DLRP[i,vt_qnt.1P] = fit_DLRP[,1:8]|> apply(2, quantile, 0.025)
  qnt_bay.DLRP[i,vt_qnt.2P] = fit_DLRP[,1:8]|> apply(2, quantile, 0.975)
  
  fit_DLRP = fit_DLRP[,1:8] |> apply(2, mean)
  coef_bay.DLRP[i,] = fit_DLRP 
  
         
  predict.DLRP_prob = prob_bay.DLRP(fit_DLRP = fit_DLRP, pca.test = pca.test)
  
  predict.DPRP = as.factor(
    ifelse(predict.DLRP_prob > 0.5, 1, 0)) 
  
  mc=caret::confusionMatrix(data = predict.DPRP, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DLRP.1[i,] = c(VN, FP, FN, VP)
  
##### thr
  thr_roc = pROC::coords(pROC::roc(pca.train$class, 
 prob_bay.DLRP(fit_DLRP = fit_DLRP, pca.test = pca.train)), 
                         "best",ret=c("threshold"))$threshold
  
  predict.DPRP = as.factor(
    ifelse(predict.DLRP_prob > thr_roc[1], 1, 0)) 
  
  mc=caret::confusionMatrix(data = predict.DPRP, reference = pca.test$class)$table
  VN=mc[1,1]; FP=mc[2,1]; FN=mc[1,2]; VP=mc[2,2]
  met_bay.DLRP[i,] = c(VN, FP, FN, VP)
  

  cat('iteração:', i)
  #print(Sys.time())
  i = i + 1
  rm(list = c('pca.dt','exo.downS', 'exo.smote.15',
    'fit.reg','fit.reg.DL', 'fit.reg.DLP', 'fit.reg.DLRP'))
  save.image("Resultado repitacoes.RData")
  
}


#require('rstanarm')

#md1 = rstanarm::stan_glm(class ~ .,
#                   family = binomial(),
#                   data = pca.train,
#                   chains = 1)
#summary(md1) 
#md1$coefficients

