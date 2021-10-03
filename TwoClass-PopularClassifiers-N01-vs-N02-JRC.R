
library(glmnet)            #### GLMNET
library(RandPro)           #### Random Projection
library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
library(nnet)              #### Neural Networks
library(class)             #### One Nearest Neighbour

library(doParallel)

no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

iterations <- 100

n <- 20
m <- 20
ns <- 100
ms <- 100

d.seq <- c(5,10,25,50,100,250,500,1000)
res.list <- list()

for (k in 1:length(d.seq)) {
   print(k)
   start.time <- proc.time()[3]
   
   d <- d.seq[k]
   
   result <- foreach(u = 1:iterations, 
                     .combine = rbind, 
                     .packages = c('glmnet','RandPro','e1071','randomForest',
                                   'class','nnet')) %dopar% {
                                      
           set.seed(u)
           
           X <- matrix(rnorm((n + ns) * d, 0, 1),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)

           Y <- matrix(rnorm((m + ms) * d, 0, sqrt(2)),
                       nrow = m + ms,
                       ncol = d,
                       byrow = TRUE)
           
           test.sample <- Z <- rbind(X[(n + 1):(n + ns),], 
                                     Y[(m + 1):(m + ms),])   ## Test Observations
           test.label <- c(rep(1,ns), rep(2,ms))
           
           X <- X[1:n,] # Training Samples Class 1
           Y <- Y[1:m,] # Training Samples Class 2
           
           train.sample <- rbind(X,Y)
           train.label <- c(rep(1,n), rep(2,m))
           
           
           
           ################################ GLMNET
           
           mdl <- cv.glmnet(
              x = train.sample,
              y = as.factor(train.label),
              family = 'binomial',
              # family = ifelse(J==2, 'binomial', 'multinomial'),
              type.measure = 'class'
           )
           
           lam.opt <- mdl$lambda[which.min(mdl$cvm)]
           
           pred.lbl <- predict(
              object = mdl,
              newx = as.matrix(test.sample),
              type = 'class',
              s = lam.opt
           )
           
           e_glm <- mean(pred.lbl != test.label)
           
           
           ################################ Random Projection NN
           
           mdl2 <- RandPro::classify(
              train_data = as.matrix(train.sample),
              test_data = as.matrix(test.sample),
              train_label = as.factor(train.label),
              test_label = as.factor(test.label),
              eps = 0.1
           )
           
           e_rnd = 1 - as.numeric(mdl2$overall[1])
           
           
           ################################ SVM Linear
           
           fit1 <- svm(x = train.sample, y = as.factor(train.label), 
                       kernel = "linear", gamma = 1/d)
           p_1 <- as.numeric(predict(fit1, test.sample))
           
           e_SVM_lin <- mean(p_1 != test.label)
           
           
           ################################ SVM RBF
           
           multi <- 10
           h <- (1 : (2*multi)) * (1 / (multi*d))
           e_SVM_rbf <- rep(0,length(h))
           
           for(j in 1:(2*multi)){
              fit4 <- svm(x = train.sample, y = as.factor(train.label), 
                          kernel = "radial",
                          gamma = h[j])
              
              e_SVM_rbf[j] <- mean(fit4$fitted != train.label)
           }
           
           h0 = h[which.min(e_SVM_rbf)]
           
           fit4 <- svm(x = train.sample, y = as.factor(train.label), 
                       kernel = "radial", gamma = h0)
           
           p_1 <- as.numeric(predict(fit4, test.sample))
           
           e_SVM_rbf <- mean(p_1 != test.label)
           
           
           
           ################################ Random Forest
           
           fit1 <- randomForest(train.sample, as.factor(train.label),
                                ntree = 5000, mtry = d^0.1)
           fit2 <- randomForest(train.sample, as.factor(train.label),
                                ntree = 5000, mtry = d^0.25)
           fit3 <- randomForest(train.sample, as.factor(train.label),
                                ntree = 5000, mtry = d^0.5)
           fit4 <- randomForest(train.sample, as.factor(train.label),
                                ntree = 5000, mtry = d^0.75)
           
           p_1 <- as.numeric(predict(object = fit1, newdata = test.sample, 
                                     type = 'class'))
           p_2 <- as.numeric(predict(object = fit2, newdata = test.sample, 
                                     type = 'class'))
           p_3 <- as.numeric(predict(object = fit3, newdata = test.sample, 
                                     type = 'class'))
           p_4 <- as.numeric(predict(object = fit4, newdata = test.sample, 
                                     type = 'class'))
           
           e_RF_1 <- mean(p_1 != test.label)
           e_RF_2 <- mean(p_2 != test.label)
           e_RF_3 <- mean(p_3 != test.label)
           e_RF_4 <- mean(p_4 != test.label)
           
           
           ################################ Neural Network
           
           ##### Logistic Activation
           
           mdl.nnet.logistic.1 <- nnet(x = train.sample,
                                       y = class.ind(c(rep(1,n), rep(2,n))),
                                       size = 1,
                                       linout = TRUE,
                                       MaxNWts = 15000)
           mdl.nnet.logistic.3 <- nnet(x = train.sample,
                                       y = class.ind(c(rep(1,n), rep(2,n))),
                                       size = 3,
                                       linout = TRUE,
                                       MaxNWts = 15000)
           mdl.nnet.logistic.5 <- nnet(x = train.sample,
                                       y = class.ind(c(rep(1,n), rep(2,n))),
                                       size = 5,
                                       linout = TRUE,
                                       MaxNWts = 15000)
           mdl.nnet.logistic.10 <- nnet(x = train.sample,
                                        y = class.ind(c(rep(1,n), rep(2,n))),
                                        size = 10,
                                        linout = TRUE,
                                        MaxNWts = 15000)
           
           p_log_1 <- as.numeric(predict(object = mdl.nnet.logistic.1,
                                         newdata = test.sample
                                         #type = 'class'
           ))
           p_log_3 <- as.numeric(predict(object = mdl.nnet.logistic.3,
                                         newdata = test.sample
                                         #type = 'class'
           ))
           p_log_5 <- as.numeric(predict(object = mdl.nnet.logistic.5,
                                         newdata = test.sample
                                         #type = 'class'
           ))
           p_log_10 <- as.numeric(predict(object = mdl.nnet.logistic.10,
                                          newdata = test.sample
                                          #type = 'class'
           ))
           
           e_nnet_log_1 <- mean(p_log_1 != test.label)
           e_nnet_log_3 <- mean(p_log_3 != test.label)
           e_nnet_log_5 <- mean(p_log_5 != test.label)
           e_nnet_log_10 <- mean(p_log_10 != test.label)
           
           ##### ReLU Activation
           
           mdl.nnet.ReLU.1 <- nnet(x = train.sample,
                                   y = class.ind(c(rep(1,n), rep(2,n))),
                                   size = 1,
                                   linout = FALSE,
                                   MaxNWts = 15000)
           mdl.nnet.ReLU.3 <- nnet(x = train.sample,
                                   y = class.ind(c(rep(1,n), rep(2,n))),
                                   size = 3,
                                   linout = FALSE,
                                   MaxNWts = 15000)
           mdl.nnet.ReLU.5 <- nnet(x = train.sample,
                                   y = class.ind(c(rep(1,n), rep(2,n))),
                                   size = 5,
                                   linout = FALSE,
                                   MaxNWts = 15000)
           mdl.nnet.ReLU.10 <- nnet(x = train.sample,
                                    y = class.ind(c(rep(1,n), rep(2,n))),
                                    size = 10,
                                    linout = FALSE,
                                    MaxNWts = 15000)
           
           p_ReLU_1 <- as.numeric(predict(object = mdl.nnet.ReLU.1,
                                          newdata = test.sample
                                          #type = 'class'
           ))
           p_ReLU_3 <- as.numeric(predict(object = mdl.nnet.ReLU.3,
                                          newdata = test.sample
                                          #type = 'class'
           ))
           p_ReLU_5 <- as.numeric(predict(object = mdl.nnet.ReLU.5,
                                          newdata = test.sample
                                          #type = 'class'
           ))
           p_ReLU_10 <- as.numeric(predict(object = mdl.nnet.ReLU.10,
                                           newdata = test.sample
                                           #type = 'class'
           ))
           
           e_nnet_ReLU_1 <- mean(p_ReLU_1 != test.label)
           e_nnet_ReLU_3 <- mean(p_ReLU_3 != test.label)
           e_nnet_ReLU_5 <- mean(p_ReLU_5 != test.label)
           e_nnet_ReLU_10 <- mean(p_ReLU_10 != test.label)
           
           
           ################################ One Nearest Neighbour
           
           mdl.onn <- class::knn1(train = train.sample,
                                  test = test.sample, 
                                  cl = as.factor(train.label))
           
           e_onn <- mean(mdl.onn != test.label)
           
           
           ################################ Bayes Risk
           
           bayes_risk <- 0
           
           return(c(bayes_risk,
                    e_glm,
                    e_RF_1, e_RF_2, e_RF_3, e_RF_4,
                    e_rnd,
                    e_SVM_lin, 
                    e_SVM_rbf,
                    e_nnet_log_1, e_nnet_log_3, e_nnet_log_5, e_nnet_log_10,
                    e_nnet_ReLU_1, e_nnet_ReLU_3, e_nnet_ReLU_5, e_nnet_ReLU_10,
                    e_onn
                  ))
           
        }
   
   end.time <- proc.time()[3]- start.time
   print(end.time)
   
   result <- as.data.frame(result)
   colnames(result) <- c('BYS',
                         'GLMNET',
                         'RF1','RF2','RF3','RF4',
                         'NNRAND',
                         'SVMLIN',
                         'SVMRBF',
                         'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                         'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                         'ONN')
   rownames(result) <- 1:iterations
   
   res.list[[k]] <- result
}

res.list.2 <- list("d=5" = res.list[[1]],
                 "d=10" = res.list[[2]],
                 "d=25" = res.list[[3]],
                 "d=50" = res.list[[4]],
                 "d=100" = res.list[[5]],
                 "d=250" = res.list[[6]],
                 "d=500" = res.list[[7]],
                 "d=1000" = res.list[[8]])

writexl::write_xlsx(res.list.2, "C:\\Users\\JYOTISHKA\\Desktop\\N01-vs-N02.xlsx")

stopCluster(cl)
gc()

source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/TwoClass-PopularClassifiers-N01-vs-N11-JRC.R')















