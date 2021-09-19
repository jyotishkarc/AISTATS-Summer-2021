
library(magrittr)
library(doParallel)
library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)
# library(foreach)

start.time <- proc.time()

ITER = 100

source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dataset-partitioning-jrc.R')
source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dissim-sin-cpp.R')
source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dissim-sin-comp-cpp.R')

no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

training.data.original <- Wine.train %>% na.omit() %>% as.matrix()
test.data.original <- Wine.test %>% na.omit() %>% as.matrix()

data.renamed.labels <- labels.rename(training.data.original, test.data.original)

training.data <- data.renamed.labels$TRAIN
test.data <- data.renamed.labels$TEST

no.of.classes <- training.data[,1] %>% unique() %>% length()

out1 <- list()

for(u in 1:ITER){
   
   print(u)
   
   partitioned.data <- data.partition.multi(training.data, test.data)
   data.training <- as.matrix(partitioned.data[[1]])
   data.test <- as.matrix(partitioned.data[[2]])
   
   data.training.list <- list()
   
   for (k in 1 : no.of.classes) {
      data.training.list[[k]] <- data.training[which(data.training[,1] == k), ]
   }
   
   
   ground.label.test <- data.test[,-1]     ## Test Observations
   
   
   data.training.list.unlab <- lapply(data.training.list, function(df) return(df[,-1]))
   
   print("Hello 1")
   
   Tjj = lapply(data.training.list.unlab, function(df){
      
      tsin = dissim.sin(train.set = as.matrix(df), no.cores = no.cores)
      tsin.comp = dissim.sin.comp(train.set = as.matrix(df), no.cores = no.cores)
      
      return(c(sum(tsin), sum(tsin.comp))/(nrow(df) * (nrow(df) - 1)))
   })
   
   print("Hello 2")
   
   Tjj <- do.call('rbind', Tjj)
   Tjj <- as.data.frame(Tjj)
   
   colnames(Tjj) <- c('sin','sin.comp')
   
   clusterExport(cl, c('Tjj', 'data.training.list.unlab'))
   
   print("Hello 3")
   
   lbl.ensmbl = t(parApply(cl, data.test[,-1] , 1, function(Z){
      
      Z <- as.numeric(Z)
      
      TjZ.tmp = lapply(data.training.list.unlab, function(df){
         
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
      
      TjZ = do.call('rbind', TjZ.tmp)
      
      return(c(which.min(Tjj$sin/2 - TjZ[,1]), 
               which.min(Tjj$sin.comp/2 - TjZ[,2])))
   }))
   
   out1[[u]] <- apply(lbl.ensmbl, 2, function(vec){
                        mean(vec != ground.label.test)})
}
   
out2 <- out1 %>% do.call('rbind', .)

colnames(out2) = c('delta0.sin', 'delta0.sin.comp')

ERR = colMeans(out2)
SE = apply(out2, 2, sd) / sqrt(ITER)

exec.time <- proc.time() - start.time
print(exec.time)

stopCluster(cl)
gc()
