


for (k in 1:length(miss)) {
   
   data.renamed.labels <- labels.rename(data.miss.train[[k]], data.miss.test[[k]])
   
   result <- foreach(u = 1:100, 
                     .combine = rbind,
                     .packages = c('class','nnet')) %dopar% 
   {
   
      data.cleaned <- data.partition.multi(data.renamed.labels$TRAIN, 
                                           data.renamed.labels$TEST)
      
      train.sample <- data.cleaned$TRAIN
      test.sample <- data.cleaned$TEST
      
      train.label <- train.sample[,1]
      test.label <- test.sample[,1]
      
      train.sample <- train.sample[,-1]
      test.sample <- test.sample[,-1]
      
      N <- nrow(train.sample)
      
      ################################ Neural Networks
      
      Q <- data.frame(rbind(train.sample, test.sample))
      targets <- class.ind(c(train.label, test.label))
      
      ##### ReLU Activation
   
      mdl.nnet.ReLU.1 <- nnet(Q[1:N,], targets[1:N,], size = 1,
                              decay = 5e-4, maxit = 100, MaxNWts = 500000,
                              linout = TRUE)
   
      mdl.nnet.ReLU.3 <- nnet(Q[1:N,], targets[1:N,], size = 3,
                              decay = 5e-4, maxit = 100, MaxNWts = 500000,
                              linout = TRUE)
   
      mdl.nnet.ReLU.5 <- nnet(Q[1:N,], targets[1:N,], size = 5,
                              decay = 5e-4, maxit = 100, MaxNWts = 500000,
                              linout = TRUE)
   
      mdl.nnet.ReLU.10 <- nnet(Q[1:N,], targets[1:N,], size = 10,
                               decay = 5e-4, maxit = 100, MaxNWts = 500000,
                               linout = TRUE)
   
      p_ReLU_1 <- predict(mdl.nnet.ReLU.1, Q[-c(1:N),])
      p_ReLU_3 <- predict(mdl.nnet.ReLU.3, Q[-c(1:N),])
      p_ReLU_5 <- predict(mdl.nnet.ReLU.5, Q[-c(1:N),])
      p_ReLU_10 <- predict(mdl.nnet.ReLU.10, Q[-c(1:N),])
   
      e_nnet_ReLU_1 <- mean(apply(p_ReLU_1, 1, which.max) != test.label)
      e_nnet_ReLU_3 <- mean(apply(p_ReLU_3, 1, which.max) != test.label)
      e_nnet_ReLU_5 <- mean(apply(p_ReLU_5, 1, which.max) != test.label)
      e_nnet_ReLU_10 <- mean(apply(p_ReLU_10, 1, which.max) != test.label)
      
      
      ################################ One Nearest Neighbour
      
      mdl.onn <- class::knn1(train = train.sample,
                             test = test.sample,
                             cl = as.factor(train.label))
      
      e_onn <- mean(mdl.onn != test.label)
      
      
      return(c(e_nnet_ReLU_1, e_nnet_ReLU_3, e_nnet_ReLU_5, e_nnet_ReLU_10,
               e_onn))
   
   }
   
   path.current <- paste0("E:\\Jyotishka\\Code\\",miss[k],"_NN-ONN.xlsx")
   
   writexl::write_xlsx(result, path = path.current)
   
}
