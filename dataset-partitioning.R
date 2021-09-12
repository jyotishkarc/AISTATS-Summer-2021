
X.train #read the training set 
X.test #read the test set
X = rbind(X.train, X.test)
d = ncol(X)
classes = unique(X[,1])
pops.withlbl <- list(X[X[,1] == classes[1],],X[X[,1] == classes[2],])
nj <- sapply(pops.withlbl,nrow)
n <- sum(nj)
prior_j <- nj/n
d <- ncol(X) - 1
J <- length(nj)
nj_train <- round(nj*0.5)
ntrain=sum(nj_train)
rm(X)
REP = 100
for (iter in 1:REP)
{
   train.index = test.index = train.sample = test.sample = list(NULL)
   for (cnt in 1:J)
   {
      train.index[[cnt]] <- sample(1:nj[cnt], size = nj_train[cnt])
      test.index[[cnt]] <- setdiff(1:nj[cnt], train.index[[cnt]])
      train.sample[[cnt]] <- pops.withlbl[[cnt]][train.index[[cnt]],]
      test.sample[[cnt]] <- pops.withlbl[[cnt]][test.index[[cnt]],]
   }
   rm(train.index)
   rm(test.index)
   njtrain <- sapply(train.sample, nrow)
   njtest <- sapply(test.sample, nrow)
   nj <- njtrain + njtest
   
   train.sample <- do.call('rbind', train.sample)
   test.sample <- do.call('rbind', test.sample)
   
   train.lbl <- train.sample[, 1]
   trainingset <- train.sample[,-1]
   test.lbl <- test.sample[, 1]
   testset <- test.sample[,-1]
   
   rm(train.sample); rm(test.sample)
   
   ##############
   # classification begins
   
} #for loop ends
#---------------------------------------------------------------
