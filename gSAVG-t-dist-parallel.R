#### Author : JYOTISHKA RAY CHOUDHURY
#### Date : 22.07.2021

# rm(list = ls())

library(doParallel)
no.cores = round(detectCores()*0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1) return(0)
   
   else return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
}

error.prop <- c()



classify.parallel <- function(Z, X, Y, A_XX, A_YY, A_XY, L_XY, S_XY){
   # print("Classification starting")
   R1 <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   A_XZ <- matrix(rep(0, n*R1), R1, n)
   A_YZ <- matrix(rep(0, m*R1), R1, m)
   
   A_XZ.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      # clusterExport(cl, c('Z','X','Q','i','j'))
      # clusterExport(cl, c('i','j'))
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Z[i,],X[j,],Q[val,])
      })))
      
      # return(sum(parSapply(cl,1:(n+m),function(val){
      #    rho(Z[i,],X[j,],Q[val,])
      # })))
   }
   
   indx.mat = cbind(rep(1:R1, each = n),rep(1:n, times = R1))
   clusterExport(cl, c('R1','n','m'), envir = environment())
   A_XZ = rowMeans(matrix(parApply(cl,indx.mat,1,A_XZ.rho.fun), R1, n, 
                          byrow = TRUE)) / (n+m-1)
   
   
   # clusterExport(cl, c('Y','Q','i','j'))
   
   A_YZ.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      # clusterExport(cl, c('Z','Y','Q','i','j'))
      # clusterExport(cl, c('i','j'))
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Z[i,],Y[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:R1, each = m),rep(1:m, times = R1))
   # clusterExport(cl, c('R','m'))
   A_YZ = rowMeans(matrix(parApply(cl,indx.mat,1,A_YZ.rho.fun), R1, m, 
                          byrow = TRUE)) / (n+m-1)
   
   
   L_XZ <- A_XZ - rep(A_XX, R1)/2
   L_YZ <- A_YZ - rep(A_YY, R1)/2
   
   S_QZ <- A_XZ + A_YZ - rep((A_XY + (A_XX + A_YY)/2), R1)
   
   T_Z <- L_XY * (L_YZ - L_XZ)/2 + S_XY * S_QZ/2
   # print(T_Z)
   
   prac.label <- rep(0, R1)
   prac.label[which(T_Z > 0)] <- 1
   prac.label[which(T_Z <= 0)] <- 2
   
   return(list(prac.label, T_Z))
}


clusterExport(cl, ls())

library(metRology)
t1 <- proc.time()

for(u in 1:50){
   n <- 20
   m <- 20
   d <- 10
   
   X <- matrix(rt(n*d, 2), nrow = n, ncol = d, byrow = TRUE)
   Y <- matrix(rt.scaled(m*d, 2, 3, 1), nrow = m, ncol = d, byrow = TRUE)
   Q <- rbind(X,Y)
   
   print(u)
   
   ##### A_XY
   A_XY <- matrix(rep(0, n*m), n, m)
   
   # clusterExport(cl, c('X','Y','Q','i','j'))
   
   A_XY.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(X[i,],Y[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:n, each = m),rep(1:m, times = n))
   clusterExport(cl, c('X','Y','Q','n','m'))
   a <- matrix(parApply(cl,indx.mat,1,A_XY.rho.fun), n, m, byrow = TRUE)/((n+m-2)*n*m)
   A_XY = sum(a)
   
   
   ##### A_XX
   A_XX <- matrix(rep(0, n^2), n, n)
   
   # clusterExport(cl, c('X','Q','i','j'))
   
   A_XX.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(X[i,],X[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:n, each = n),rep(1:n, times = n))
   A_XX = sum(parApply(cl,indx.mat,1,A_XX.rho.fun))/((n+m-2)*n*(n-1))
   
   
   ##### A_YY
   A_YY <- matrix(rep(0, m^2), m, m)
   
   # clusterExport(cl, c('Y','Q','i','j'))
   
   A_YY.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Y[i,],Y[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:m, each = m),rep(1:m, times = m))
   A_YY = sum(parApply(cl,indx.mat,1,A_YY.rho.fun))/((n+m-2)*m*(m-1))
   
   
   ##### L
   L_XY <- 2 * A_XY - A_XX - A_YY
   S_XY <- A_XX - A_YY
   
   
   ########## Test Observations
   ns <- 160
   ms <- 160
   
   Z_F <- matrix(rt(ns*d, 2), nrow = ns, ncol = d, byrow = TRUE)
   Z_G <- matrix(rt.scaled(ms*d, 2, 3, 1), nrow = ms, ncol = d, byrow = TRUE)
   Z <- rbind(Z_F, Z_G)
   
   ground.label <- c(rep(1,ns), rep(2,ms))
   
   clusterExport(cl, c('Z'))
   
   result <- classify.parallel(Z, X, Y, A_XX, A_YY, A_XY, L_XY, S_XY)
   prac.label <- result[[1]]
   T_Z <- result[[2]]
   # print(T_Z)
   
   # print(length(which(ground.label != prac.label)))
   error.prop[u] <- length(which(ground.label != prac.label)) / (ns + ms)
   print(error.prop[u])
   
   print((proc.time() - t1)/u) #avgtime required per iteration
}



stopCluster(cl)
gc()
