
library(e1071)             #### SVM (Linear & RBF Kernel)

library(rio)
library(sciplot)
library(writexl)
library(doParallel)
library(EnvStats)

no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

iterations <- 100

n <- 20
m <- 20
ns <- 100
ms <- 100

d.seq <- c(5,10,25,50,100,250,500,1000)

df.Par11.vs.Par21 <- rio::import_list("~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/TwoClass-PopularClassifiers-Simulated-JRC/Results/Pop-Par11-vs-Par21-with-nn.xlsx")


for (k in 1:length(d.seq)) {
   
   print(k)
   start.time <- proc.time()[3]
   res <- c()
   
   d <- d.seq[k]
   
   result <- foreach(u = 1:iterations,
                     .combine = cbind,
                     .packages = c('e1071','EnvStats')) %dopar%
      {
         
         set.seed(u)
         
         X <- matrix(rpareto((n + ns) * d, location =  1, shape = 1),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         
         Y <- matrix(rpareto((m + ms) * d, location = 2, shape = 1),
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
         
         ################################ SVM RBF
         
         fit4 <- svm(x = train.sample, y = as.factor(train.label),
                     kernel = "radial")
         
         p_1 <- as.numeric(predict(fit4, test.sample))
         
         e_SVM_rbf <- mean(p_1 != test.label)
         
         ########################################
         
         return(e_SVM_rbf)
      }
   
   result <- as.numeric(result)
   
   df.Par11.vs.Par21[[k]]$SVMRBF <- c(result, NA, mean(result), sciplot::se(result))
   
   end.time <- proc.time()[3]- start.time
   print(end.time)
}


writexl::write_xlsx(x = df.Par11.vs.Par21,
                    path = "C:\\Users\\JYOTISHKA\\Desktop\\Pop-Par11-vs-Par21-updated.xlsx")

# writexl::write_xlsx(x = df.C01.vs.C02,
#                     path = "E:\\Jyotishka\\Code\\Pop-C01-vs-C02-with-nn.xlsx")

stopCluster(cl)
gc()
