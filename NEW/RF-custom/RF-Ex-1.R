
# library(glmnet)            #### GLMNET
# library(RandPro)           #### Random Projection
# library(e1071)             #### SVM (Linear & RBF Kernel)
library(randomForest)      #### Random Forest
# library(nnet)              #### Neural Networks
# library(class)             #### One Nearest Neighbour

library(writexl)
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

res.df <- matrix(NA, nrow = iterations, ncol = length(d.seq))

for (k in 1:length(d.seq)) {
   
   print(k)
   start.time <- proc.time()[3]
   
   d <- d.seq[k]
   
   result <- foreach(u = 1:iterations,
                     .combine = rbind,
                     .packages = c('randomForest')) %dopar% 
      {
         
         # set.seed(u)
         
         X <- matrix(rnorm((n + ns) * d, 1, sqrt(1)),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         # set.seed(u)
         
         Y <- matrix(rnorm((m + ms) * d, 1, sqrt(2)),
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
         
         ################################ Random Forest
         
         h <- tuneRF(train.sample, as.factor(train.label), ntreeTry = 5000)
         
         fit <- randomForest(train.sample, as.factor(train.label),
                              ntree = 5000, mtry = h[which.min(h[,2]),1])
         
         p_1 <- as.numeric(predict(object = fit, newdata = test.sample,
                                   type = 'class'))
         
         e_RF <- mean(p_1 != test.label)
         
         ################################
         
         return(e_RF)
      }
   
   res.df[,k] <- result
   
   end.time <- proc.time()[3]- start.time
   print(end.time)
}


res.df <- rbind(res.df,
                rep(NA, length(d.seq)),
                apply(res.df, 2, mean), 
                apply(res.df, 2, sciplot::se))


res.df <- res.df %>% as.data.frame()
colnames(res.df) <- as.character(d.seq)

writexl::write_xlsx(x = res.df,
                    path = "C:\\Users\\JYOTISHKA\\Desktop\\Ex-1.xlsx")

# writexl::write_xlsx(x = res.list.N11.vs.N12,
#                     path = "E:\\Jyotishka\\Code\\Pop-N11-vs-N12-with-nn.xlsx")

stopCluster(cl)
gc()
