
library(glmnet)            #### GLMNET
library(RandPro)           #### Random Projection
library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
library(nnet)              #### Neural Networks
library(class)             #### One Nearest Neighbour

n <- 20
m <- 20

# d.seq <- c(50,100,250,500,1000)
d.seq <- c(50,1000)

time.matrix <- matrix(0, nrow = length(d.seq), ncol = 13)

for (k in 1:length(d.seq)) {
   
   print(k)
   
   d <- d.seq[k]
   
   X <- matrix(rnorm(n*d, 1, 1),
               nrow = n,
               ncol = d,
               byrow = TRUE)
   
   Y <- matrix(rnorm((m+1)*d, 1, sqrt(2)),
               nrow = m+1,
               ncol = d,
               byrow = TRUE)
   
   test.sample <- Z <- Y[m+1 , ]   ## Test Observation from Y
   test.label <- 2
   
   Y <- Y[1:m,] # Training Sample
   
   train.sample <- rbind(X,Y)
   train.label <- c(rep(1,n), rep(2,m))
   
   print("COMPLETED : Data Generation")
   
   ################################ GLMNET
   
   system.time({
      mdl <- cv.glmnet(
      x = train.sample,
      y = as.factor(train.label),
      family = 'binomial',
      type.measure = 'class'
   )

   lam.opt <- mdl$lambda[which.min(mdl$cvm)]

   pred.lbl <- predict(
      object = mdl,
      newx = test.sample %>% t(),
      type = 'class',
      s = lam.opt
   )

   e_glm <- mean(pred.lbl != test.label)}) -> time.GLMNET

   print("DONE : GLMNET")
   
   ################################ Random Projection NN
   
   system.time({
      mdl2 <- RandPro::classify(
      train_data = as.matrix(train.sample),
      test_data = t(test.sample),
      train_label = as.factor(train.label),
      test_label = factor(test.label, level = c(1,2)),
      eps = 0.1
   )
   
   e_rnd = 1 - as.numeric(mdl2$overall[1])}) -> time.NN.Rand
   
   print("DONE : Random Projection")
   
   ################################ SVM Linear
   
   system.time({
      fit1 <- svm(x = train.sample, y = as.factor(train.label),
               kernel = "linear", gamma = 1/d)
      p_1 <- as.numeric(predict(fit1, test.sample %>% t()))
   
   e_SVM_lin <- mean(p_1 != test.label)}) -> time.SVM.Lin
   
   print("DONE : SVM Linear")
   
   ################################ SVM RBF
   
   system.time({
      fit4 <- svm(x = train.sample, y = as.factor(train.label),
               kernel = "radial")
   
   p_1 <- as.numeric(predict(fit4, test.sample %>% t()))
   
   e_SVM_rbf <- mean(p_1 != test.label)}) -> time.SVM.RBF
   
   print("DONE : SVM Radial Basis")
   
   ################################ Neural Networks
   
   ##### Logistic Activation
   
   Q <- data.frame(rbind(train.sample, test.sample))
   targets <- class.ind(c(train.label, test.label))
   
   system.time({mdl.nnet.logistic.1 <- nnet(Q[1:40,], targets[1:40,], size = 1,
                               decay = 5e-4, maxit = 100, MaxNWts = 10100,
                               linout = FALSE)
   p_log_1 <- predict(mdl.nnet.logistic.1, Q[-c(1:40),])
   e_nnet_log_1 <- mean(apply(p_log_1, 1, which.max) != test.label)}) -> 
      time.NNet.log.1
   
   print("DONE : NNet Logistic 1")
   
   system.time({mdl.nnet.logistic.3 <- nnet(Q[1:40,], targets[1:40,], size = 3,
                               decay = 5e-4, maxit = 100, MaxNWts = 10100,
                               linout = FALSE)
   p_log_3 <- predict(mdl.nnet.logistic.3, Q[-c(1:40),])
   e_nnet_log_3 <- mean(apply(p_log_3, 1, which.max) != test.label)}) ->
      time.NNet.log.3
   
   print("DONE : NNet Logistic 3")
   
   system.time({mdl.nnet.logistic.5 <- nnet(Q[1:40,], targets[1:40,], size = 5,
                               decay = 5e-4, maxit = 100, MaxNWts = 10100,
                               linout = FALSE)
   p_log_5 <- predict(mdl.nnet.logistic.5, Q[-c(1:40),])
   e_nnet_log_5 <- mean(apply(p_log_5, 1, which.max) != test.label)}) ->
      time.NNet.log.5
   
   print("DONE : NNet Logistic 5")
   
   system.time({mdl.nnet.logistic.10 <- nnet(Q[1:40,], targets[1:40,], size = 10,
                                decay = 5e-4, maxit = 100, MaxNWts = 10100,
                                linout = FALSE)
   p_log_10 <- predict(mdl.nnet.logistic.10, Q[-c(1:40),])
   e_nnet_log_10 <- mean(apply(p_log_10, 1, which.max) != test.label)}) ->
      time.NNet.log.10
   
   print("DONE : NNet Logistic 10")
   
   
   # ##### ReLU Activation
   
   system.time({mdl.nnet.ReLU.1 <- nnet(Q[1:40,], targets[1:40,], size = 1,
                           decay = 5e-4, maxit = 100, MaxNWts = 10100,
                           linout = TRUE)
   p_ReLU_1 <- predict(mdl.nnet.ReLU.1, Q[-c(1:40),])
   e_nnet_ReLU_1 <- mean(apply(p_ReLU_1, 1, which.max) != test.label)}) ->
      time.NNet.ReLU.1
   
   print("DONE : NNet ReLU 1")
   
   system.time({mdl.nnet.ReLU.3 <- nnet(Q[1:40,], targets[1:40,], size = 3,
                           decay = 5e-4, maxit = 100, MaxNWts = 10100,
                           linout = TRUE)
   p_ReLU_3 <- predict(mdl.nnet.ReLU.3, Q[-c(1:40),])
   e_nnet_ReLU_3 <- mean(apply(p_ReLU_3, 1, which.max) != test.label)}) ->
      time.NNet.ReLU.3
   
   print("DONE : NNet ReLU 3")
   
   system.time({mdl.nnet.ReLU.5 <- nnet(Q[1:40,], targets[1:40,], size = 5,
                           decay = 5e-4, maxit = 100, MaxNWts = 10100,
                           linout = TRUE)
   p_ReLU_5 <- predict(mdl.nnet.ReLU.5, Q[-c(1:40),])
   e_nnet_ReLU_5 <- mean(apply(p_ReLU_5, 1, which.max) != test.label)}) -> 
      time.NNet.ReLU.5
   
   print("DONE : NNet ReLU 5")
   
   system.time({mdl.nnet.ReLU.10 <- nnet(Q[1:40,], targets[1:40,], size = 10,
                            decay = 5e-4, maxit = 100, MaxNWts = 10100,
                            linout = TRUE)
   p_ReLU_10 <- predict(mdl.nnet.ReLU.10, Q[-c(1:40),])
   e_nnet_ReLU_10 <- mean(apply(p_ReLU_10, 1, which.max) != test.label)}) ->
      time.NNet.ReLU.10
   
   print("DONE : NNet ReLU 10")
   
   ################################ One Nearest Neighbour
   
   system.time({mdl.onn <- class::knn1(train = train.sample,
                          test = test.sample,
                          cl = as.factor(train.label))
   
   e_onn <- mean(mdl.onn != test.label)}) -> time.ONN
   
   print("DONE : ONN")
   
   ################################ Bayes Risk
   
   system.time({mdl.bayes.X <- apply(test.sample %>% t(), 1, function(val){
      return(sum(dnorm(val, 1, 1, log = T)))
   })
   
   mdl.bayes.Y <- apply(test.sample %>% t(), 1, function(val){
      return(sum(dnorm(val, 1, sqrt(2), log = T)))
   })
   
   G <- as.numeric(mdl.bayes.X > mdl.bayes.Y)
   H <- replace(G, G == 0, 2)
   
   bayes_risk <- mean(H != test.label)}) -> time.BYS
   
   print("DONE : Bayes Risk")
   
   ################################
   
   time.matrix[k, ] <- c(time.GLMNET[3],
                         time.ONN[3],
                         time.NN.Rand[3],
                         time.NNet.log.1[3], time.NNet.log.3[3],
                         time.NNet.log.5[3], time.NNet.log.10[3],
                         time.NNet.ReLU.1[3], time.NNet.ReLU.3[3],
                         time.NNet.ReLU.5[3], time.NNet.ReLU.10[3],
                         time.SVM.Lin[3],
                         time.SVM.RBF[3])
   
}

time.matrix <- time.matrix %>% as.data.frame()
colnames(time.matrix) <- c('GLMNET',
                           'ONN',
                           'NNRAND',
                           'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                           'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                           'SVMLIN',
                           'SVMRBF')

rownames(time.matrix) <- as.character(d.seq)


# end.time <- proc.time()[3]- start.time
# print(end.time)

# colnames(result) <- c('BYS',
#                       'GLMNET',
#                       'RF1','RF2','RF3','RF4',
#                       'NNRAND',
#                       'SVMLIN',
#                       'SVMRBF',
#                       'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
#                       'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
#                       'ONN'
# )
# 
# 
# res.list.C01.vs.C02 <- list("d=5" = res.list[[1]],
#                             "d=10" = res.list[[2]],
#                             "d=25" = res.list[[3]],
#                             "d=50" = res.list[[4]],
#                             "d=100" = res.list[[5]],
#                             "d=250" = res.list[[6]],
#                             "d=500" = res.list[[7]],
#                             "d=1000" = res.list[[8]])

# writexl::write_xlsx(x = res.list.C01.vs.C02,
#                     path = "E:\\Jyotishka\\Code\\Pop-C01-vs-C02-with-nn.xlsx")

