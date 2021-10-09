
library(e1071)             #### SVM (Linear & RBF Kernel)

library(rio)
library(sciplot)
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

df.N11.vs.N12 <- rio::import_list("~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/TwoClass-PopularClassifiers-Simulated-JRC/Results/Pop-N11-vs-N12-with-nn.xlsx")


for (k in 1:length(d.seq)) {
   
   print(k)
   start.time <- proc.time()[3]
   res <- c()
   
   d <- d.seq[k]
   
   result <- foreach(u = 1:iterations,
                     .combine = cbind,
                     .packages = c('e1071')) %dopar%
      {
         
         set.seed(u)
         
         X <- matrix(rnorm((n + ns) * d, 1, sqrt(1)),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         
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
         
         ################################ SVM RBF
         
         fit4 <- svm(x = train.sample, y = as.factor(train.label),
                     kernel = "radial")
         
         p_1 <- as.numeric(predict(fit4, test.sample))
         
         e_SVM_rbf <- mean(p_1 != test.label)
         
         ########################################
         
         return(e_SVM_rbf)
      }
   
   result <- as.numeric(result)
   
   df.N11.vs.N12[[k]]$SVMRBF <- c(result, NA, mean(result), sciplot::se(result))
   
   end.time <- proc.time()[3]- start.time
   print(end.time)
}


writexl::write_xlsx(x = df.N11.vs.N12,
                    path = "C:\\Users\\JYOTISHKA\\Desktop\\Pop-N11-vs-N12-updated.xlsx")

# writexl::write_xlsx(x = df.C01.vs.C02,
#                     path = "E:\\Jyotishka\\Code\\Pop-C01-vs-C02-with-nn.xlsx")

stopCluster(cl)
gc()
