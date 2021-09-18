
data.partition.multi <- function(X.train, X.test){
   
   X.train <- as.matrix(X.train)
   X.test <- as.matrix(X.test)
   
   X = rbind(X.train, X.test)
   
   classes = unique(X[,1])
   # print(classes)
   
   pops.withlbl <- S.list <- train.sample <- test.sample <- list()
   
   for (i in 1:length(classes)) {
      pops.withlbl[[i]] <- X[X[,1] == classes[i],]
   }
   
   nj <- sapply(pops.withlbl, nrow)
   
   for (i in 1:length(classes)) {
      S.list[[i]] <- sample(which(X[,1] == classes[i]), round(nj[i]/2))
      
      train.sample[[i]] <- X[S.list[[i]],]
      test.sample[[i]] <- X[setdiff(which(X[,1] == classes[i]), S.list[[i]]),]
   }
   
   # print(c(nrow(test.1), nrow(test.2)))
   
   train.sample <- train.sample %>% do.call('rbind', .) %>% as.matrix()
   test.sample <- test.sample %>% do.call('rbind', .) %>% as.matrix()
   
   return(list("TRAIN" = train.sample, "TEST" = test.sample))
}
