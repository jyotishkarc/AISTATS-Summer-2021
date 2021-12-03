
library(magrittr)
# library(e1071)             #### SVM (Linear & RBF Kernel)
library(kernlab)
library(writexl)
library(doParallel)

no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

mark <- 2

SVM.custom <- function(c, kerntype, normed, mark){
   
   iterations <- 100
   
   n <- 20
   m <- 20
   ns <- 100
   ms <- 100
   
   # d.seq <- c(5,10)
   d.seq <- c(5,10,25,50,100,250,500,1000)
   
   res.df <- matrix(NA, nrow = iterations, ncol = length(d.seq))
   
   for (k in 1:length(d.seq)) {
      
      print(k)
      start.time <- proc.time()[3]
      
      d <- d.seq[k]
      
      result <- foreach(u = 1:iterations,
                        .combine = rbind,
                        .packages = c('kernlab','magrittr')) %dopar% 
         {
            
            # set.seed(u)
            
            X <- matrix(rnorm((n + ns) * d, 0, sqrt(3)),
                        nrow = n + ns,
                        ncol = d,
                        byrow = TRUE)
            
            Y <- matrix(rt((m + ms) * d, df = 3),
                        nrow = m + ms,
                        ncol = d,
                        byrow = TRUE)
            
            test.sample <- rbind(X[(n + 1):(n + ns),],
                                 Y[(m + 1):(m + ms),])   ## Test Observations
            test.label <- as.factor(c(rep(1,ns), rep(2,ms)))
            
            X <- X[1:n,] # Training Samples Class 1
            Y <- Y[1:m,] # Training Samples Class 2
            
            train.sample <- rbind(X,Y)
            train.label <- as.factor(c(rep(1,n), rep(2,m)))
            
            ################################ SVM Exponential Kernel
            
            ########## Kernel Selection
            
            if(kerntype == "poly"){
               
               kern <- function(h,c) {
                  a <- 1
                  b <- 0
                  return(a * (h-c)^2 + b)
               }
            }
            
            if(kerntype == "exp"){
               kern <- function(h,c) {
                  a <- 1
                  return(exp(- a * (h-c)^2))
               }
            }
            
            all.data <- rbind(train.sample, test.sample)
            RA <- nrow(all.data)
            
            ########## Normed / Not normed
            
            if (normed == TRUE) {
               all.normed <- t(apply(all.data, 1, function(X){
                  return(X / norm(as.matrix(X), type = "F"))
               }))
            }
            else all.normed <- all.data
            
            
            K <- matrix(0, nrow = RA, ncol = RA)
            
            for (i in 1 : RA){
               for (j in i : RA){
                  K[i,j] <- K[j,i] <- kern(((all.normed[i,] - all.normed[j,]) 
                                            %*% (all.normed[i,] - all.normed[j,])), c)
               }
            }
            
            train.K <- as.kernelMatrix(K[1:(n+m), 1:(n+m)])
            
            SVM.exp <- kernlab::ksvm(train.K, train.label, type = "C-svc", 
                                     kernel = 'matrix', C = 1)
            
            test.K <- as.kernelMatrix(K[(n+m+1):RA, 1:(n+m)])
            # [,SVindex(SVM.exp), drop = F])
            
            p_1 <- as.numeric(predict(SVM.exp, test.K))
            e_SVM_exp <- mean(p_1 != test.label)
            
            ################################
            
            return(e_SVM_exp)
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
   
   # path <- paste0("C:\\Users\\JYOTISHKA\\Desktop\\SVM-Ex-",as.character(mark),"-",
   #                as.character(kerntype),"-",as.character(normed),"-",
   #                as.character(c),".xlsx")
   
   if(kerntype == "poly"){
      kt <- "Polynomial Kernels"
   }
   else kt <- "Exponential Kernels"
   
   if(normed == TRUE){
      normed.or.not <- "Normed"
   }
   else normed.or.not <- "Not Normed"
   
   path <- paste0("E:\\Jyotishka\\Code\\HDLSS-Summer-2021-main\\SVM-custom\\Results\\Example ",
                  as.character(mark),"\\",kt,"\\",normed.or.not,"\\SVM-Ex-",
                  as.character(mark),"-",as.character(kerntype),"-",
                  as.character(normed),"-",as.character(c),".xlsx")
   
   writexl::write_xlsx(x = res.df, path = path)
}


stopCluster(cl)
gc()

