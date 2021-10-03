rm(list = ls())
options(warn = 1)
start.time <- proc.time()
ITER = 100
library(glmnet)
library(RandPro)
library(e1071)
library(randomForest)
library(doParallel)
library(foreach)

O = NULL
no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)


#--- data reading begins ------------------------------------------

p = 'D:/RealData/UCRArchive_2018'
nms = list.files(p)
# nms = nms[c(10,13,21,26:28,39,44:51,53:55,64,72,77:78,84,86,90,92,97,99:102,106,112,119)]
nms = nms[c(10,26:28,36,44,45,50,51,53:55,77,78,86,92,97,99:102,119)]
len = length(nms)


for(Mi in 22:len){ # rememeber to run for Mi=1
   colon.train = read.table(paste(p, nms[Mi], paste(nms[Mi],'TRAIN.tsv',sep='_'),sep = '/'), sep = '\t')
   colon.test = read.table(paste(p, nms[Mi], paste(nms[Mi],'TEST.tsv',sep='_'),sep = '/'), sep = '\t')
   
   X = rbind(colon.train, colon.test)
   X = na.omit(X)
   d = ncol(X)
   
   rm(colon.test); rm(colon.train)
   
   pops.withlbl <- split(X,f = X[,1])
   classes = unlist(lapply(pops.withlbl, function(df) df[1,1]))
   
   nj <- sapply(pops.withlbl, nrow)
   n.all <- sum(nj)
   prior_j <- nj / n.all
   d <- ncol(X) - 1
   J <- length(nj)
   nj_train <- round(nj * 0.5)
   ntrain = sum(nj_train)
   rm(X)
   # -- data reading ends here --------------------------------------
   
   #------------dissimilarity computation---------------------------
   out2 <- NULL
   
   
   out2 = foreach(u = 1:ITER, .combine = rbind,.packages = c('glmnet','RandPro','e1071','randomForest')) %dopar% {
      train.index = test.index = train.sample = test.sample = list(NULL)
      for (cnt in 1:J)
         # partitioning into training and test set
      {
         train.index[[cnt]] <- sample(1:nj[cnt], size = nj_train[cnt])
         test.index[[cnt]] <- setdiff(1:nj[cnt], train.index[[cnt]])
         train.sample[[cnt]] <- pops.withlbl[[cnt]][train.index[[cnt]], ]
         test.sample[[cnt]] <- pops.withlbl[[cnt]][test.index[[cnt]], ]
      }
      
      njtrain <- sapply(train.sample, nrow) #training sizes
      
      njtest <- sapply(test.sample, nrow)
      nj <- njtrain + njtest
      
      train.sample = do.call('rbind', train.sample)
      train.lbl = train.sample[,1]
      train.sample = train.sample[,-1]
      
      ground.label <- unlist(lapply(test.sample,function(df) df[,1]))
      test.sample = do.call('rbind', test.sample)
      test.sample = test.sample[,-1]
      
      #---------------------- GLMNET -----------------------
      mdl = cv.glmnet(
         x = as.matrix(train.sample),
         y = as.factor(train.lbl),
         family = ifelse(J==2, 'binomial', 'multinomial'),
         type.measure = 'class'
      )
      lam.opt = mdl$lambda[which.min(mdl$cvm)]
      pred.lbl = predict(
         object = mdl,
         newx = as.matrix(test.sample),
         type = 'class',
         s = lam.opt
      )
      eglm = mean(pred.lbl != ground.label)
      
      #--------------------- NN-Rand --------------------------
      mdl2 = classify(
         train_data = as.matrix(train.sample),
         test_data = as.matrix(test.sample),
         train_label = as.factor(train.lbl),
         test_label = as.factor(ground.label),
         eps = 0.1
      )
      ernd = 1 - as.numeric(mdl2$overall[1])
      
      #--------------------SVMLIN --------------------------
      fit1 <- svm(train.sample, as.factor(train.lbl), kernel="linear",gamma=1/d)
      p_1=as.numeric(predict(fit1,test.sample))
      e_SVM_lin <- mean(classes[p_1]!= ground.label)
      
      #--------------------SVMRBF --------------------------
      multi <- 10
      h=(1:(2*multi))*(1/(multi*d))
      e_SVM_rbf <- rep(0,length(h))
      
      for(j in 1:(2*multi))
      {
         fit4 <- svm(train.sample, as.factor(train.lbl), kernel="radial",gamma=h[j])
         # p_4=predict(fit4,test.sample)
         # e_SVM_rbf[j] <- mean(classes[p_4] != ground.label)
         e_SVM_rbf[j] <- mean(fit4$fitted != ground.label)
         # e_SVM_rbf[j]=prior_1*(length(which(p_1[1:n_1_test] > 0))/n_1_test)+prior_2*(length(which(p_1[(1+n_1_test):(n_1_test+n_2_test)] <= 0))/n_2_test)
      }
      h0 = h[which.min(e_SVM_rbf)]
      
      fit4 <- svm(train.sample, as.factor(train.lbl), kernel="radial",gamma=h0)
      e_SVM_rbf <- mean(fit4$fitted != ground.label)
      
      #--------------------- random forest ------------------------------
      fit1 = randomForest(train.sample, as.factor(train.lbl),ntree = 5000,mtry = d^0.1)
      fit2 = randomForest(train.sample, as.factor(train.lbl),ntree = 5000,mtry = d^0.25)
      fit3 = randomForest(train.sample, as.factor(train.lbl),ntree = 5000,mtry = d^0.5)
      fit4 = randomForest(train.sample, as.factor(train.lbl),ntree = 5000,mtry = d^0.75)
      
      p_1=as.numeric(predict(object = fit1,newdata = test.sample,type = 'class'))
      p_2=as.numeric(predict(object = fit2,newdata = test.sample,type = 'class'))
      p_3=as.numeric(predict(object = fit3,newdata = test.sample,type = 'class'))
      p_4=as.numeric(predict(object = fit4,newdata = test.sample,type = 'class'))
      e_RF1 <- mean(classes[p_1]!= ground.label)
      e_RF2 <- mean(classes[p_2]!= ground.label)
      e_RF3 <- mean(classes[p_3]!= ground.label)
      e_RF4 <- mean(classes[p_4]!= ground.label)
      #---------------------------------------------------------------
      c(eglm,
        ernd,
        e_SVM_lin,
        e_SVM_rbf,
        e_RF1,
        e_RF2,
        e_RF3,
        e_RF4
      )
   }
   
   colnames(out2) = c(
      'GLMNET',
      'NNRAND',
      'SVMLIN',
      'SVMRBF',
      'RF1',
      'RF2',
      'RF3',
      'RF4'
   )
   
   ERR = colMeans(out2)
   SE = apply(out2, 2, sd) / sqrt(ITER)
   
   exec.time <- proc.time() - start.time
   print(exec.time)
   
   print(ERR)
   
   write.csv(out2, paste(nms[Mi],'_popularclassifiers.csv',sep=''),row.names=T)
   
   O[[Mi]] = c(nms[Mi],ERR)
   
   print(nms[Mi])
}


stopCluster(cl)
gc()

ALL = do.call('rbind',O)
write.csv(ALL,'UCR_100ITER_popularclassifiers.csv', row.names = F)
