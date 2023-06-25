library(uwot)


setwd("C:/Users/Anderson ssd/Desktop/TCC")
source('packages.R')

exo.test = read_csv("exoTest.csv")
exo.test = mutate(exo.test, LABEL = ifelse(LABEL == 2, 1, 0))
exo.train = read_csv("exoTrain.csv")
exo.train = mutate(exo.train, LABEL = ifelse(LABEL == 2, 1, 0))

iteracoes = 50
n_va = 6
source('App/40 x 60/func aux 40 ica.R')

i = 1


set.seed(123)
sementes = sample(1:10000000, iteracoes)
z = 0

while(i <= iteracoes){
  
  print(Sys.time())
  #SMOTE
  exo.smote.15 = smote(exo.train = exo.train, 
                      sementes = sementes, i = i)
  
  #pca.dt = pca_dt(exo.smote.15$data, exo.test = exo.test,
           #       sementes = sementes, i = i)
  
  #DownSample
  exo.downS = down_sample(exo.smote.15 = exo.smote.15, 
                          sementes = sementes, i = i)
  ### PCA
  pca.dt = pca_dt(exo.downS = exo.downS, exo.test = exo.test,
                  sementes = sementes, i = i)
  #pca.train = data.frame(predict(pca.dt, exo.downS[-3198]))[1:n_va]
  pca.train = data.frame(umap_transform(X = exo.downS[-3198], model = pca.dt))[1:n_va]
  
  pca.train = data.frame(pca.train, 
                         'class' = exo.downS$class) |> as_tibble()
  
  pca.train = mutate(pca.train, class = ifelse(class == 1,1,0))
  names(pca.train) = c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'PC6', 'class')
  
  #pca.test = data.frame( pca.dt$x[1420:1989,1:n_va], 
                        # 'class' = exo.test$LABEL) |> as_tibble()
  #pca.test = data.frame(predict(pca.dt, exo.test[-1]))[1:n_va]
  #pca.test = data.frame(predict.ica(exo.test[-1], pca.dt))[1:n_va]
  pca.test = data.frame(umap_transform(X = exo.test[-1], model = pca.dt))[1:n_va]
                   
  pca.test = data.frame(pca.test, 'class' = exo.test$LABEL) |> as_tibble()
  pca.test = mutate(pca.test, class = as.factor(ifelse(class == 1,1,0)))
  names(pca.test) = c('PC1', 'PC2', 'PC3', 'PC4', 'PC5', 'PC6', 'class')
  
  m_gl = glm(class ~ ., data = pca.train, family = binomial)
  coef_bot[i,] = m_gl$coefficients
  
  ### classico
  predict.prob_c = predict(m_gl, pca.test[-(n_va + 1)], type = 'response')
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
  print('bayes')
  
  fit_RL = data.frame(0)
  while(dim(fit_RL)[2] == 1){
    #set.seed(sementes[i])
    fit.reg <- stan(file = 'App/modelos/RL lasso.stan',
                        data = list(Y = pca.train$class,
                                    N = dim(pca.train)[1],
                                    X = pca.train[1:n_va],
                                    tau = 5
                                   ),
                        iter = 1200,
                        #init = g.init,
                        chains = 1,
                        refresh = 0,
                        cores = 7)
    
    
    fit_RL = as.data.frame(fit.reg)
  }
  
  qnt_bay[i,vt_qnt.1] = fit_RL[,1:(n_va + 1)]|> apply(2, quantile, 0.025)
  qnt_bay[i,vt_qnt.2] = fit_RL[,1:(n_va + 1)]|> apply(2, quantile, 0.975)
  
  fit_RL = fit_RL[,1:(n_va + 1)]|> apply(2, getmode)
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
  aic_bay[i, ] = loo::waic(loo::extract_log_lik(fit.reg,
                                                parameter_name = 'lp__'))$waic
  
  #############
  
  fit_DL = data.frame(0)
  while(dim(fit_DL)[2] == 1){
    #set.seed(sementes[i])
    fit.reg.DL <- stan(file = 'DL lasso.stan',
                                           data = list(Y=pca.train$class,
                                                       N=dim(pca.train)[1],
                                                       X = pca.train[1:n_va],
                                                       tau = 5
                                                       ),
                                           iter = 1200,
                                           #init = g.init,
                                           chains = 1,
                                           refresh = 0, 
                                           cores = 7)
    
    
    fit_DL = as.data.frame(fit.reg.DL)
  }
  
  qnt_bay.DL[i,vt_qnt.1] = fit_DL[,1:(n_va + 1)]|> apply(2, quantile, 0.025)
  qnt_bay.DL[i,vt_qnt.2] = fit_DL[,1:(n_va + 1)]|> apply(2, quantile, 0.975)
  
  fit_DL = fit_DL[,1:(n_va + 1)] |> apply(2, getmode)
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
  aic_bay.DL[i, ] = loo::waic(loo::extract_log_lik(fit.reg.DL,
                                                   parameter_name = 'lp__'))$waic
  
  cat('metade')
  ############ 
  fit_DLP = data.frame(0)
  while(dim(fit_DLP)[2] == 1){
    #set.seed(sementes[i])
    fit.reg.DLP <- stan(file = 'App/modelos/DLP lasso.stan',
                                         data = list(Y=pca.train$class,
                                                     N=dim(pca.train)[1],
                                                     X = pca.train[1:n_va],
                                                     tau = 5),
                                         iter = 1200,
                                         #init = c(DLP.init),
                                         chains = 1,
                                         refresh = 0,
                                         cores = 7)
    
    fit_DLP = as.data.frame(fit.reg.DLP)
  }
  
  qnt_bay.DLP[i,vt_qnt.1P] = fit_DLP[,1:(n_va + 2)]|> apply(2, quantile, 0.025)
  qnt_bay.DLP[i,vt_qnt.2P] = fit_DLP[,1:(n_va + 2)]|> apply(2, quantile, 0.975)
  
  fit_DLP = fit_DLP[,1:(n_va +2)] |> apply(2, getmode)
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
  aic_bay.DLP[i, ] = loo::waic(loo::extract_log_lik(fit.reg.DLP,
                                                    parameter_name = 'lp__'))$waic
  
  
  ############ 
  z = 0
  fit_DLRP = data.frame(0)
  while(dim(fit_DLRP)[2] == 1){
    
    #set.seed(sementes[i])
    fit.reg.DLRP <- stan(file = 'App/modelos/DLRP.stan',
                       data = list(Y=pca.train$class,
                                   N=dim(pca.train)[1],
                                   X1 = pca.train$PC1,
                                   X2 = pca.train$PC2,
                                   X3 = pca.train$PC3,
                                   X4 = pca.train$PC4,
                                   X5 = pca.train$PC5,
                                   X6 = pca.train$PC6,
                                   tau = 5),
                       iter = 1200,
                       #init = c(dlrp.init),
                       chains = 1,                         
                       refresh = 0, 
                       cores = 7)
  
  fit_DLRP = as.data.frame(fit.reg.DLRP)
  z = z + 1
  if(z%%100 == 0){
    print('tá df')
  }
  }
  
  qnt_bay.DLRP[i,vt_qnt.1P] = fit_DLRP[,1:(n_va + 2)]|> apply(2, quantile, 0.025)
  qnt_bay.DLRP[i,vt_qnt.2P] = fit_DLRP[,1:(n_va + 2)]|> apply(2, quantile, 0.975)
  
  fit_DLRP = fit_DLRP[,1:(n_va + 2)] |> apply(2, getmode)
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
  aic_bay.DLRP[i, ] = loo::waic(loo::extract_log_lik(fit.reg.DLRP,
                                                     parameter_name = 'lp__'))$waic
  
  
  print(met_bay[i,])
  print(met_bay.DL[i,])
  print(met_bay.DLP[i,])
  print(met_bay.DLRP[i,])
  cat('iteração:', i, '\n')
  #print(Sys.time())
  rm(list = c('pca.dt','exo.downS', 'exo.smote.15',
    'fit.reg','fit.reg.DL', 'fit.reg.DLP', 'fit.reg.DLRP'))
  #save.image("Resultado repitacoes.RData")
  if(i%%10 == 0){
    save.image(glue::glue("App/Resultado repitacoes v{i} umap new 40p.RData"))
  }
  i = i + 1
}


rm(list = c('exo.test', 'exo.train'))
save.image("Resultado repitacoes umap new asso.RData")

#require('rstanarm')

#md1 = rstanarm::stan_glm(class ~ .,
#                   family = binomial(),
#                   data = pca.train,
#                   chains = 1,
#cores = 7)
#summary(md1) 
#md1$coefficients

Sh = 1/2 exp^u (-2 + u) (-1 + sgn(u)) + 1/2 e^(-u) (-2 + 4 e^u - u) (1 + sgn(u))

Su = -1/16 e^u (4 - 3 u + u^2) (-1 + sgn(u)) + 1/16 e^(-u) (-4 + 8 e^u - 3 u - u^2) (1 + sgn(u))
