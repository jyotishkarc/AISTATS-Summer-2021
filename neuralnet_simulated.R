rm(list=ls())
# setwd('~/Documents/Simulated/EX3/blocksize10/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX4/blocksize10/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX3/blocksize10/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX6/blocksize10/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX5/blocksize10/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX1/')
setwd('C:/Users/Sarbojit/Documents/Simulated/EX2/')
# setwd('C:/Users/Sarbojit/Documents/Simulated/EX3/autocorrstruc/')
library(neuralnet)
library(foreach)
library(doParallel)
library(nnet)
################################################################################
################################# Data input ###################################
################################################################################

set.seed(1)
iteration=100
dimen <- c(50,100,250,500,1000)
summary.res <- NULL #c(colMeans(output),apply(output,2,sd))
glmnet.dimen <- function(k)
{
  mydf <- read.table(paste('dataset',dimen[k],sep=''),header=F,sep=' ')
  # mydf <- cbind(mydf[,ncol(mydf)],mydf[,-ncol(mydf)])
  # mydf <- read.csv(paste('dataset',dimen[k],'.csv',sep=''),header=T)
  
  # J = length(unique(mydf[,1]))
  J = length(unique(mydf[,dimen[k]+1]))
  d <- ncol(mydf)-1
  z <- mydf[-(1:(2+(2*dimen[k]))),]
  
  n_1 = n_2= 50
  n_train=n_1+n_2
  
  prior_1=n_1/n_train
  prior_2=n_2/n_train
  
  n_1_test = n_2_test = 250 
  
  n_test=n_1_test+n_2_test
  n=n_train+n_test
  e_nnet=array(0,dim=c(iteration))
  
  
  
  e_nnet <-  foreach(iter = 1:iteration,.combine = rbind) %dopar% 
    {
      # x_train_1 <- z[(n*(iter-1))+1:n_1,-1]
      # x_train_2 <- z[(n*(iter-1))+n_1+n_1_test+(1:n_2),-1]
      # 
      # x_test_1 <- z[(n*(iter-1))+n_1+(1:n_1_test),-1]
      # x_test_2 <- z[(n*(iter-1))+n_1+n_1_test+n_2+(1:n_2_test),-1]
      # 
      # y=as.factor(z[c((n*(iter-1))+1:n_1,(n*(iter-1))+n_1+n_1_test+(1:n_2)),1])
      # 
      # x_test = as.matrix(rbind(x_test_1,x_test_2))
      # y_test = z[c((n*(iter-1))+n_1+(1:n_1_test),(n*(iter-1))+n_1+n_1_test+n_2+(1:n_2_test)),1]
      
      x_train_1 <- z[(n*(iter-1))+1:n_1,-(dimen[k]+1)]
      x_train_2 <- z[(n*(iter-1))+n_1+n_1_test+(1:n_2),-(dimen[k]+1)]

      x_test_1 <- z[(n*(iter-1))+n_1+(1:n_1_test),-(dimen[k]+1)]
      x_test_2 <- z[(n*(iter-1))+n_1+n_1_test+n_2+(1:n_2_test),-(dimen[k]+1)]

      y=as.factor(z[c((n*(iter-1))+1:n_1,(n*(iter-1))+n_1+n_1_test+(1:n_2)),dimen[k]+1])
      
      x_test = as.matrix(rbind(x_test_1,x_test_2))
      y_test = z[c((n*(iter-1))+n_1+(1:n_1_test),(n*(iter-1))+n_1+n_1_test+n_2+(1:n_2_test)),dimen[k]+1]
      
      x_train=cbind(class.ind(y),rbind(x_train_1,x_train_2))
      
    
    ##################################################################
    ##########################  neuralnet  ###########################
    ##################################################################
    colnames(x_train)[1:J] <- c('l1','l2')
    nms <- colnames(x_train)
    # nms[1:J] <- gsub(pattern = '-',replacement = 'LL',x = nms[1:J])
    # nms[1:J] <- sub(pattern = '+',replacement = 'LL',x = nms[1:J])
    f <- as.formula(paste(paste(nms[1:J],collapse = '+')," ~", paste(nms[!nms %in% nms[1:J]], collapse = " + ")))
    
    nn1=neuralnet(f,data = x_train,hidden=1,linear.output = FALSE,act.fct = 'tanh')
    nn3=neuralnet(f,data = x_train,hidden=3,linear.output = FALSE,act.fct = 'tanh')
    nn5=neuralnet(f,data = x_train,hidden=5,linear.output = FALSE,act.fct = 'tanh')
    nn10=neuralnet(f,data = x_train,hidden=10,linear.output = FALSE,act.fct = 'tanh')
    
    Predict1=compute(nn1,x_test)
    Predict3=compute(nn3,x_test)
    Predict5=compute(nn5,x_test)
    Predict10=compute(nn10,x_test)
    
    prob1 <- Predict1$net.result; prob3 <- Predict3$net.result; prob5 <- Predict5$net.result; prob10 <- Predict10$net.result
    
    pred1 <- max.col(m = prob1); pred3 <- max.col(m = prob3); pred5 <- max.col(m = prob5); pred10 <- max.col(m = prob10);

    apply(cbind(pred1,pred3,pred5,pred10),2,function(vec){mean(vec != y_test)})
  }
  return(c(colMeans(e_nnet),apply(e_nnet,2,sd)))
  # return(summary.res)
}

cl <- makeCluster(detectCores()- 1)
registerDoParallel(cl)
clusterExport(cl,ls())
clusterEvalQ(cl, library(neuralnet))
clusterEvalQ(cl, library(nnet))
tmp <- NULL
for(i in 1:length(dimen))
{
  tmp[[i]] <- glmnet.dimen(i)
}

summary.res  <- cbind(dimen,do.call('rbind',tmp ))
colnames(summary.res) <- c('dim','1hiddenlayers','3hiddenlayers','5hiddenlayers','10hiddenlayers',rep('sd',4))
# write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/example3b_100ITER/Blocksize10/tanh_NEURALNET.csv',row.names=FALSE)
# write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/example_MVcauchy_MVcauchy/blocksize10/tanh_NEURALNET.csv',row.names=FALSE)
# write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/ClaytonCopula_MPN/blocksize10/tanh_NEURALNET.csv',row.names=FALSE)
# write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/example1_200ITER/tanh_NEURALNET.csv',row.names=FALSE)
write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/example2_200ITER/tanh_NEURALNET.csv',row.names=FALSE)
# write.csv(summary.res,'C:/Users/Sarbojit/Dropbox/NNclassification on HDLSS data/DataAnalysis_NN/Simulated/example3a_100ITER/tanh_NEURALNET.csv',row.names=FALSE)

stopCluster(cl)
gc()
