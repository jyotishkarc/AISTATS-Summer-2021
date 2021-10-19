
library(glmnet)            #### GLMNET
library(RandPro)           #### Random Projection
library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
library(nnet)              #### Neural Networks
library(class)             #### One Nearest Neighbour

n <- 20
m <- 20

d.seq <- c(50,100,250,500,1000)

# d.seq <- c(50,1000)

time.matrix <- matrix(0, nrow = length(d.seq), ncol = 16)

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
   
   no.of.classes <- 2
   data.training.list.unlab <- list(X,Y)
   
   ################################ Delta0 sin (DELTA 0)
   
   system.time({
      Tjj <- lapply(data.training.list.unlab, function(df){
         
         tsin <- dissim.sin(train.set = as.matrix(df), 
                            no.cores = no.cores)
         # tsin.comp <- dissim.sin.comp(train.set = as.matrix(df), 
         #                              no.cores = no.cores)
         
         # return(c(sum(tsin), sum(tsin.comp))/(nrow(df) * (nrow(df) - 1)))
         return(sum(tsin)/(nrow(df)^2))
      })
      
      # print(Tjj)
      
      T.sin <- # T.sin.comp <-
         matrix(0, no.of.classes, no.of.classes)
      
      for (i in 1 : no.of.classes) {
         for (j in 1:i) {
            mat1 <- data.training.list.unlab[[i]]
            mat2 <- data.training.list.unlab[[j]]
            # print(dim(mat1))
            # print(dim(mat2))
            y1 <- dissim.sin(rbind(mat1,mat2), no.cores = no.cores)
            y1 <- as.matrix(y1)
            y1 <- y1[((nrow(mat1) + 1):(nrow(mat1) + nrow(mat2))), (1 : nrow(mat1))]
            T.sin[j,i] <- T.sin[i,j] <- sum(y1)/(nrow(mat1) * nrow(mat2))
            
            # y2 <- dissim.sin.comp(rbind(mat1,mat2), no.cores = no.cores)
            # y2 <- as.matrix(y2)
            # y2 <- y2[((nrow(mat1)+1):(nrow(mat1)+nrow(mat2))), 1:nrow(mat1)]
            # T.sin.comp[i,j] <- T.sin.comp[j,i] <- sum(y2)/(nrow(mat1) * nrow(mat2))
         }
         
         T.sin[i,i] <- # T.sin.comp[i,i] <- 
            0
      }
      
      # print(T.sin)
      # print(T.sin.comp)
      
      Tjj <- Tjj %>% do.call('rbind', .) %>% as.data.frame()
      #colnames(Tjj) <- c('sin','sin.comp')
      # print(Tjj)
      
      lbl.ensmbl <- t(apply(t(test.sample) , 1, function(Z){
         Z <- as.numeric(Z)
         
         TjZ.tmp <- lapply(data.training.list.unlab, function(df){
            
            return(colMeans(as.matrix(apply(df,1,function(vec){
               vec <- as.numeric(vec)
               
               u1 <- 1 + (t(vec) %*% Z)
               u2 <- 1 + (t(vec) %*% vec)
               u3 <- 1 + (t(Z) %*% Z)
               
               r1 <- asin(u1/sqrt(u2 * u3)) #sine
               return(r1)
               
               # v1 <- 1 + (vec * Z)
               # v2 <- 1 + (vec * vec)
               # v3 <- 1 + (Z * Z)
               # 
               # r2 <- mean(asin(v1/sqrt(v2*v3))) #sine component-wise
               # 
               # return(c(r1,r2))
               
            }))))
         })
         
         TjZ <- do.call('rbind', TjZ.tmp)
         
         print("Hello")
         
         LjZ <- Tjj/2 - TjZ[,1]
         # LjZ <- Tjj$sin/2 - TjZ[,1]
         
         # LjZ <- cbind((Tjj$sin/2 - TjZ[,1]), (Tjj$sin.comp/2 - TjZ[,2]))
         # print(LjZ)
         
         indicator_Z.sin <- ind.Z.sin <- 
            # indicator_Z.sin.comp <- ind.Z.sin.comp <- 
               matrix(0, no.of.classes, no.of.classes)
         
         Tjj <- Tjj[,1]
         
         for (i in 1 : no.of.classes) {
            for (j in 1:i) {
               indicator_Z.sin[i,j] <-
                  (Tjj[i] + Tjj[j] - 2 * T.sin[i,j]) * 
                  (LjZ[j,1] - LjZ[i,1]) - (Tjj[i] - Tjj[j]) * 
                  (LjZ[i,1] + LjZ[j,1] + T.sin[i,j])
               
               if(indicator_Z.sin[i,j] > 0){
                  ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- i}
               if(indicator_Z.sin[i,j] <= 0){
                  ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- j}
               
               # indicator_Z.sin.comp[i,j] <-
               #    (Tjj$sin.comp[i] + Tjj$sin.comp[j] - 2 * T.sin.comp[i,j]) * 
               #    (LjZ[j,2] - LjZ[i,2]) - (Tjj$sin.comp[i] - Tjj$sin.comp[j]) *
               #    (LjZ[i,2] + LjZ[j,2] + T.sin.comp[i,j])
               # 
               # if(indicator_Z.sin.comp[i,j] > 0){
               #    ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- i}
               # if(indicator_Z.sin.comp[i,j] <= 0){
               #    ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- j}
            }
         }
         
         # rm(LjZ, TjZ.tmp, T.sin, T.sin.comp, 
         #    indicator_Z.sin, indicator_Z.sin.comp)
         
         ind.Z.sin <- as.numeric(ind.Z.sin)
         # ind.Z.sin.comp <- as.numeric(ind.Z.sin.comp)
         
         # print(ind.Z.sin)
         # print(ind.Z.sin.comp)
         
         return(which.min(Tjj/2 - TjZ[,1]))
                  # which.min(Tjj$sin.comp/2 - TjZ[,2]), 
                  # mode.data(ind.Z.sin),
                  # mode.data(ind.Z.sin.comp)))
   }))
   
      out1 <- apply(lbl.ensmbl, 2, function(vec) {return(mean(vec != test.label))})
   }) -> time.delta0.sin
   
   print("DONE : Delta_0 sin")
   
   
   
   ################################ Delta0 sin.comp (DELTA 1)
   
   system.time({
      Tjj <- lapply(data.training.list.unlab, function(df){
         
         # tsin <- dissim.sin(train.set = as.matrix(df), 
         #                    no.cores = no.cores)
         tsin.comp <- dissim.sin.comp(train.set = as.matrix(df),
                                      no.cores = no.cores)
         
         return(sum(tsin.comp)/(nrow(df)^2))
      })
      
      # print(Tjj)
      
      # T.sin <- 
      T.sin.comp <- matrix(0, no.of.classes, no.of.classes)
      
      for (i in 1 : no.of.classes) {
         for (j in 1:i) {
            mat1 <- data.training.list.unlab[[i]]
            mat2 <- data.training.list.unlab[[j]]
            
            # y1 <- dissim.sin(rbind(mat1,mat2), no.cores = no.cores)
            # y1 <- as.matrix(y1)
            # y1 <- y1[((nrow(mat1) + 1):(nrow(mat1) + nrow(mat2))), (1 : nrow(mat1))]
            # T.sin[j,i] <- T.sin[i,j] <- sum(y1)/(nrow(mat1) * nrow(mat2))
            
            y2 <- dissim.sin.comp(rbind(mat1,mat2), no.cores = no.cores) %>% as.matrix()
            # y2 <- as.matrix(y2)
            y2 <- y2[((nrow(mat1)+1):(nrow(mat1)+nrow(mat2))), 1:nrow(mat1)]
            T.sin.comp[i,j] <- T.sin.comp[j,i] <- sum(y2)/(nrow(mat1) * nrow(mat2))
         }
         
         # T.sin[i,i] <- 
         T.sin.comp[i,i] <- 0
      }
      
      # print(T.sin)
      # print(T.sin.comp)
      
      Tjj <- Tjj %>% do.call('rbind', .) %>% as.data.frame()
      #colnames(Tjj) <- c('sin','sin.comp')
      # print(Tjj)
      
      lbl.ensmbl <- t(apply(t(test.sample) , 1, function(Z){
         Z <- as.numeric(Z)
         
         TjZ.tmp <- lapply(data.training.list.unlab, function(df){
            
            return(colMeans(as.matrix(apply(df,1,function(vec){
               vec <- as.numeric(vec)
               
               # u1 <- 1 + (t(vec) %*% Z)
               # u2 <- 1 + (t(vec) %*% vec)
               # u3 <- 1 + (t(Z) %*% Z)
               # 
               # r1 <- asin(u1/sqrt(u2 * u3)) #sine
               # return(r1)
               
               v1 <- 1 + (vec * Z)
               v2 <- 1 + (vec * vec)
               v3 <- 1 + (Z * Z)

               r2 <- mean(asin(v1/sqrt(v2*v3))) #sine component-wise
               return(r2)

               # return(c(r1,r2))
               
            }))))
         })
         
         TjZ <- do.call('rbind', TjZ.tmp)
         
         print("Hello")
         
         LjZ <- Tjj/2 - TjZ[,1]
         # LjZ <- Tjj$sin/2 - TjZ[,1]
         
         # LjZ <- cbind((Tjj$sin/2 - TjZ[,1]), (Tjj$sin.comp/2 - TjZ[,2]))
         # print(LjZ)
         
         # indicator_Z.sin <- ind.Z.sin <- 
         indicator_Z.sin.comp <- ind.Z.sin.comp <-
            matrix(0, no.of.classes, no.of.classes)
         
         Tjj <- Tjj[,1]
         
         for (i in 1 : no.of.classes) {
            for (j in 1:i) {
               # indicator_Z.sin[i,j] <-
               #    (Tjj[i] + Tjj[j] - 2 * T.sin[i,j]) * 
               #    (LjZ[j,1] - LjZ[i,1]) - (Tjj[i] - Tjj[j]) * 
               #    (LjZ[i,1] + LjZ[j,1] + T.sin[i,j])
               # 
               # if(indicator_Z.sin[i,j] > 0){
               #    ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- i}
               # if(indicator_Z.sin[i,j] <= 0){
               #    ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- j}
               
               indicator_Z.sin.comp[i,j] <-
                  (Tjj[i] + Tjj[j] - 2 * T.sin.comp[i,j]) *
                  (LjZ[j,1] - LjZ[i,1]) - (Tjj[i] - Tjj[j]) *
                  (LjZ[i,1] + LjZ[j,1] + T.sin.comp[i,j])

               if(indicator_Z.sin.comp[i,j] > 0){
                  ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- i}
               if(indicator_Z.sin.comp[i,j] <= 0){
                  ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- j}
            }
         }
         
         # rm(LjZ, TjZ.tmp, T.sin, T.sin.comp, 
         #    indicator_Z.sin, indicator_Z.sin.comp)
         
         # ind.Z.sin <- as.numeric(ind.Z.sin)
         ind.Z.sin.comp <- as.numeric(ind.Z.sin.comp)
         
         # print(ind.Z.sin)
         # print(ind.Z.sin.comp)
         
         return(# which.min(Tjj/2 - TjZ[,1]))
         which.min(Tjj/2 - TjZ[,1]))
         # mode.data(ind.Z.sin),
         # mode.data(ind.Z.sin.comp)))
      }))
      
      out2 <- apply(lbl.ensmbl, 2, function(vec) {return(mean(vec != test.label))})
      }) -> time.delta0.sin.comp
   
   print("DONE : Delta_0 sin.comp")
   
   
   ################################ Delta_2 sin.comp (DELTA 2)
   
   system.time({
      Tjj <- lapply(data.training.list.unlab, function(df){
         
         # tsin <- dissim.sin(train.set = as.matrix(df), 
         #                    no.cores = no.cores)
         tsin.comp <- dissim.sin.comp(train.set = as.matrix(df),
                                      no.cores = no.cores)
         
         return(sum(tsin.comp)/(nrow(df)^2))
      })
      
      # print(Tjj)
      
      # T.sin <- 
      T.sin.comp <- matrix(0, no.of.classes, no.of.classes)
      
      for (i in 1 : no.of.classes) {
         for (j in 1:i) {
            mat1 <- data.training.list.unlab[[i]]
            mat2 <- data.training.list.unlab[[j]]
            
            # y1 <- dissim.sin(rbind(mat1,mat2), no.cores = no.cores)
            # y1 <- as.matrix(y1)
            # y1 <- y1[((nrow(mat1) + 1):(nrow(mat1) + nrow(mat2))), (1 : nrow(mat1))]
            # T.sin[j,i] <- T.sin[i,j] <- sum(y1)/(nrow(mat1) * nrow(mat2))
            
            y2 <- dissim.sin.comp(rbind(mat1,mat2), no.cores = no.cores) %>% as.matrix()
            # y2 <- as.matrix(y2)
            y2 <- y2[((nrow(mat1)+1):(nrow(mat1)+nrow(mat2))), 1:nrow(mat1)]
            T.sin.comp[i,j] <- T.sin.comp[j,i] <- sum(y2)/(nrow(mat1) * nrow(mat2))
         }
         
         # T.sin[i,i] <- 
         T.sin.comp[i,i] <- 0
      }
      
      # print(T.sin)
      # print(T.sin.comp)
      
      Tjj <- Tjj %>% do.call('rbind', .) %>% as.data.frame()
      #colnames(Tjj) <- c('sin','sin.comp')
      # print(Tjj)
      
      lbl.ensmbl <- t(apply(t(test.sample) , 1, function(Z){
         Z <- as.numeric(Z)
         
         TjZ.tmp <- lapply(data.training.list.unlab, function(df){
            
            return(colMeans(as.matrix(apply(df,1,function(vec){
               vec <- as.numeric(vec)
               
               # u1 <- 1 + (t(vec) %*% Z)
               # u2 <- 1 + (t(vec) %*% vec)
               # u3 <- 1 + (t(Z) %*% Z)
               # 
               # r1 <- asin(u1/sqrt(u2 * u3)) #sine
               # return(r1)
               
               v1 <- 1 + (vec * Z)
               v2 <- 1 + (vec * vec)
               v3 <- 1 + (Z * Z)
               
               r2 <- mean(asin(v1/sqrt(v2*v3))) #sine component-wise
               return(r2)
               
               # return(c(r1,r2))
               
            }))))
         })
         
         TjZ <- do.call('rbind', TjZ.tmp)
         
         print("Hello")
         
         LjZ <- Tjj/2 - TjZ[,1]
         # LjZ <- Tjj$sin/2 - TjZ[,1]
         
         # LjZ <- cbind((Tjj$sin/2 - TjZ[,1]), (Tjj$sin.comp/2 - TjZ[,2]))
         # print(LjZ)
         
         # indicator_Z.sin <- ind.Z.sin <- 
         indicator_Z.sin.comp <- ind.Z.sin.comp <-
            matrix(0, no.of.classes, no.of.classes)
         
         Tjj <- Tjj[,1]
         
         for (i in 1 : no.of.classes) {
            for (j in 1:i) {
               # indicator_Z.sin[i,j] <-
               #    (Tjj[i] + Tjj[j] - 2 * T.sin[i,j]) * 
               #    (LjZ[j,1] - LjZ[i,1]) - (Tjj[i] - Tjj[j]) * 
               #    (LjZ[i,1] + LjZ[j,1] + T.sin[i,j])
               # 
               # if(indicator_Z.sin[i,j] > 0){
               #    ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- i}
               # if(indicator_Z.sin[i,j] <= 0){
               #    ind.Z.sin[i,j] <- ind.Z.sin[j,i] <- j}
               
               indicator_Z.sin.comp[i,j] <-
                  (Tjj[i] + Tjj[j] - 2 * T.sin.comp[i,j]) *
                  (LjZ[j,1] - LjZ[i,1]) - (Tjj[i] - Tjj[j]) *
                  (LjZ[i,1] + LjZ[j,1] + T.sin.comp[i,j])
               
               if(indicator_Z.sin.comp[i,j] > 0){
                  ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- i}
               if(indicator_Z.sin.comp[i,j] <= 0){
                  ind.Z.sin.comp[i,j] <- ind.Z.sin.comp[j,i] <- j}
            }
         }
         
         # rm(LjZ, TjZ.tmp, T.sin, T.sin.comp, 
         #    indicator_Z.sin, indicator_Z.sin.comp)
         
         # ind.Z.sin <- as.numeric(ind.Z.sin)
         ind.Z.sin.comp <- as.numeric(ind.Z.sin.comp)
         
         # print(ind.Z.sin)
         # print(ind.Z.sin.comp)
         
         ux <- unique(ind.Z.sin.comp)
         return(ux[which.max(tabulate(match(ind.Z.sin.comp, ux)))])
         
         # return(which.min(Tjj/2 - TjZ[,1]))
         #    which.min(Tjj/2 - TjZ[,1]))
         # mode.data(ind.Z.sin),
         # mode.data(ind.Z.sin.comp))
      }))
      
      out3 <- apply(lbl.ensmbl, 2, function(vec) {return(mean(vec != test.label))})
      }) -> time.delta2.sin.comp
   
   print("DONE : Delta_2 sin.comp")
   
   
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
   
   time.matrix[k, ] <- c(time.delta0.sin[3], 
                         time.delta0.sin.comp[3], 
                         time.delta2.sin.comp[3],
                         time.GLMNET[3],
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

rownames(time.matrix) <- as.character(d.seq)
colnames(time.matrix) <- c('Delta-0','Delta-1','Delta-2',
                           'GLMNET',
                           'ONN',
                           'NNRAND',
                           'NN-lg-1','NN-lg-3','NN-lg-5','NN-lg-10',
                           'NN-R-1','NN-R-3','NN-R-5','NN-R-10',
                           'SVMLIN',
                           'SVMRBF')

# library(writexl)

write_xlsx(cbind("Dimensions" = as.character(d.seq), time.matrix),
      "C:\\Users\\JYOTISHKA\\Desktop\\Time-Matrix-All-Classifiers-Single-Observation.xlsx")
