
library(magrittr)
library(doParallel)
library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)
# library(foreach)

start.time <- proc.time()

ITER = 50

source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dataset-partitioning-jrc.R')
source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dissim-sin-cpp.R')
source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dissim-sin-comp-cpp.R')

no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

training.data.original <- Wine.train
test.data.original <- Wine.test

training.data.cleaned <- training.data.original %>% na.omit() %>% as.matrix()
test.data.cleaned <- test.data.original %>% na.omit() %>% as.matrix()

data.renamed.labels <- labels.rename(training.data.cleaned, test.data.cleaned)

training.data <- data.renamed.labels$TRAIN
test.data <- data.renamed.labels$TEST

no.of.classes <- training.data[,1] %>% unique() %>% length()

out1 <- list()

Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
}

clusterExport(cl, ls())

for(u in 1:ITER){
   
   print(u)
   
   partitioned.data <- data.partition.multi(training.data, test.data)
   data.training <- as.matrix(partitioned.data[[1]])
   data.test <- as.matrix(partitioned.data[[2]])
   
   data.training.list <- list()
   
   for (k in 1 : no.of.classes) {
      data.training.list[[k]] <- data.training[which(data.training[,1] == k), ]
   }
   
   ground.label.test <- data.test[,1]     ## Test Observations
   
   data.training.list.unlab <- lapply(data.training.list, 
                                      function(df){return(df[,-1])})
   
   # print("Hello 1")
   
   Tjj = lapply(data.training.list.unlab, function(df){
      
      tsin = dissim.sin(train.set = as.matrix(df), no.cores = no.cores)
      tsin.comp = dissim.sin.comp(train.set = as.matrix(df), no.cores = no.cores)
      
      return(c(sum(tsin), sum(tsin.comp))/(nrow(df) * (nrow(df) - 1)))
   })
   
   T.sin <- T.sin.comp <- matrix(0, no.of.classes, no.of.classes)
   
   for (i in 1 : no.of.classes) {
      for (j in 1:i) {
         mat1 <- data.training.list.unlab[[i]]
         mat2 <- data.training.list.unlab[[j]]
         # print(dim(mat1))
         # print(dim(mat2))
         y <- as.matrix(dissim.sin(rbind(mat1,mat2), no.cores = no.cores))
         y <- y[((nrow(mat1)+1):(nrow(mat1)+nrow(mat2))), 1:nrow(mat1)]
         T.sin[j,i] <- T.sin[i,j] <- sum(y)/(nrow(mat1) * nrow(mat2))
         
         y <- as.matrix(dissim.sin.comp(rbind(mat1,mat2), no.cores = no.cores))
         y <- y[((nrow(mat1)+1):(nrow(mat1)+nrow(mat2))), 1:nrow(mat1)]
         
         T.sin.comp[i,j] <- T.sin.comp[j,i] <- sum(y)/(nrow(mat1) * nrow(mat2))
      }
      
      T.sin[i,i] <- T.sin.comp[i,i] <- 0
   }
   
   Tjj <- Tjj %>% do.call('rbind', .) %>% as.data.frame()
   colnames(Tjj) <- c('sin','sin.comp')
   
   clusterExport(cl, c('Tjj', 'data.training.list.unlab',
                       'no.of.classes','T.sin','T.sin.comp'))
   
   lbl.ensmbl = t(parApply(cl, data.test[,-1] , 1, function(Z){
      Z <- as.numeric(Z)
      
      TjZ.tmp <- lapply(data.training.list.unlab, function(df){
         
         return(colMeans(apply(df,1,function(vec){
            vec <- as.numeric(vec)
            
            u1 <- 1 + (t(vec) %*% Z)
            u2 <- 1 + (t(vec) %*% vec)
            u3 <- 1 + (t(Z) %*% Z)
            
            r1 = asin(u1/sqrt(u2*u3)) #sine
            
            
            v1 = 1 + (vec * Z)
            v2 = 1 + (vec * vec)
            v3 = 1 + (Z * Z)
            
            r2 = mean(asin(v1/sqrt(v2*v3))) #sine component-wise
            
            return(c(r1,r2))
         })))
      })
      
      TjZ <- do.call('rbind', TjZ.tmp)
      
      LjZ <- cbind(Tjj$sin/2 - TjZ[,1] , Tjj$sin.comp/2 - TjZ[,2])
      
      indicator_Z.sin <- indicator_Z.sin.comp <- ind.Z.sin <- ind.Z.sin.comp <-
         matrix(0, no.of.classes, no.of.classes)
      
      for (i in 1 : no.of.classes) {
         for (j in 1:i) {
            indicator_Z.sin[i,j] <-
               (Tjj$sin[i] + Tjj$sin[j] - 2 * T.sin[i,j]) * 
               (LjZ[j,1] - LjZ[i,1]) - (Tjj$sin[i] - Tjj$sin[j]) * 
               (LjZ[i,1] + LjZ[j,1] + T.sin[i,j])
            
            if(indicator_Z.sin[i,j] > 0){ind.Z.sin[i,j] <- i}
            if(indicator_Z.sin[i,j] < 0){ind.Z.sin[i,j] <- j}
            else ind.Z.sin[i,j] <- 0
            
            indicator_Z.sin.comp[i,j] <- indicator_Z.sin.comp[j,i] <-
               (Tjj$sin.comp[i] + Tjj$sin.comp[j] - 2 * T.sin.comp[i,j]) * 
               (LjZ[j,1] - LjZ[i,1]) - (Tjj$sin.comp[i] - Tjj$sin.comp[j]) *
               (LjZ[i,1] + LjZ[j,1] + T.sin.comp[i,j])
            
            if(indicator_Z.sin.comp[i,j] > 0){ind.Z.sin.comp[i,j] <- i}
            if(indicator_Z.sin.comp[i,j] < 0){ind.Z.sin.comp[i,j] <- j}
            else ind.Z.sin.comp[i,j] <- 0
         }
      }
      
      
      return(c(which.min(Tjj$sin/2 - TjZ[,1]), 
               which.min(Tjj$sin.comp/2 - TjZ[,2]), 
               Mode(as.numeric(indicator_Z.sin)),
               Mode(as.numeric(indicator_Z.sin.comp))))
   }))
   
   out1[[u]] <- apply(lbl.ensmbl, 2, function(vec){
      mean(vec != ground.label.test)})
}

out2 <- out1 %>% do.call('rbind', .)

colnames(out2) = c('delta0.sin', 'delta0.sin.comp',
                   'delta2.sin', 'delta2.sin.comp')

ERR = colMeans(out2)
SE = apply(out2, 2, sd) / sqrt(ITER)

exec.time <- proc.time() - start.time
print(exec.time)

stopCluster(cl)
gc()
