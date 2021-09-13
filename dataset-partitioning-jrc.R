
data.partition <- function(X.train, X.test){
   
   X.train <- as.matrix(X.train)
   X.test <- as.matrix(X.test)
   
   X = rbind(X.train, X.test)
   
   classes = unique(X[,1])
   # print(classes)
   
   pops.withlbl <- list(X[X[,1] == classes[1],], X[X[,1] == classes[2],])
   nj <- sapply(pops.withlbl, nrow)
   # print(nj)
   
   S.1 <- sample(which(X[,1] == classes[1]), round(nj[1]/2))
   S.2 <- sample(which(X[,1] == classes[2]), round(nj[2]/2))
   
   train.1 <- X[S.1,]
   train.2 <- X[S.2,]
   
   # print(c(nrow(train.1), nrow(train.2)))
   
   test.1 <- X[setdiff(which(X[,1] == classes[1]), S.1),]
   test.2 <- X[setdiff(which(X[,1] == classes[2]), S.2),]
   
   # print(c(nrow(test.1), nrow(test.2)))
   
   train.sample <- rbind(train.1, train.2)
   test.sample <- rbind(test.1, test.2)
   
   return(list(as.matrix(train.sample), as.matrix(test.sample)))
   
}
