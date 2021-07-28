#### Author : JYOTISHKA RAY CHOUDHURY
#### Date : 22.07.2021

library(doParallel)
no.cores = round(detectCores()*0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

rho <- function(a,b,c){
   if (a != c && b != c) {
      return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
   }
   
   return(0)
}

error.prop <- c()

clusterExport(cl, ls())

for(u in 1:50){
   n <- 20
   m <- 20
   d <- 1000
   
   X <- matrix(rcauchy(n*d), nrow = n, ncol = d, byrow = TRUE)
   Y <- matrix(rnorm(m*d, 1, 1), nrow = m, ncol = d, byrow = TRUE)
   Q <- rbind(X,Y)
   
   print("OK")
   
   ##### A_XY
   A_XY <- matrix(rep(0, n*m), n, m)
   
   for (i in 1:n) {
      for (j in 1:m) {
         for (k in 1:(n+m)) {
            A_XY[i,j] <- A_XY[i,j] + rho(X[i,], Y[j,], Q[k,])
         }
      }
   }

   
   clusterExport(cl, c('X','Y','Q','i','j'))
   
   rho.fun = function(vec){
      i = vec[1];
      j = vec[2];
      
      sum(sapply(1:(n+m),function(val){
         rho(X[i,],Y[j,],Q[val,])
      }))/((n+m-1)*n*m) 
   }
   
   indx.mat = cbind(rep(1:n, each = m),rep(1:m, times = n))
   clusterExport(cl, c('n','m'))
   system.time({A_XY = parApply(cl,indx.mat,1,rho.fun)})
   
   
   A_XY <- sum(A_XY)/((n+m-1)*n*m)
   
   ##### A_XX
   A_XX <- matrix(rep(0, n^2), n, n)
   
   for(i in 1:n){
      for (j in 1:i) {
         for (k in 1:(n+m)) {
            A_XX[i,j] <- A_XX[i,j] + rho(X[i,], X[j,], Q[k,])
         }
      }
   }
   
   A_XX <- 2 * sum(A_XX)/((n+m-1)*(n-1)*n)
   
   ##### A_YY
   A_YY <- matrix(rep(0, m^2), m, m)
   
   for(i in 1:m){
      for (j in 1:i) {
         for (k in 1:(n+m)) {
            A_YY[i,j] <- A_YY[i,j] + rho(Y[i,], Y[j,], Q[k,])
         }
      }
   }
   
   A_YY <- 2 * sum(A_YY)/((n+m-1)*(m-1)*m)
   
   ##### L
   L_XY <- 2 * A_XY - A_XX - A_YY
   S_XY <- A_XX - A_YY
   
   ns <- 90
   ms <- 90
   
   Z_F <- matrix(rcauchy(ns*d), nrow = ns, ncol = d, byrow = TRUE)
   Z_G <- matrix(rnorm(ms*d, 1, 1), nrow = ms, ncol = d, byrow = TRUE)
   Z <- rbind(Z_F, Z_G)
   
   ground.label <- c(rep(1,ns), rep(2,ms))
   
   result <- classify(Z, X, Y, A_XX, A_YY, A_XY, L_XY, S_XY)
   prac.label <- result[[1]]
   T_Z <- result[[2]]
   
   # print(length(which(ground.label != prac.label)))
   error.prop[u] <- length(which(ground.label != prac.label)) / (ns + ms)
   print(error.prop[u])
}


classify <- function(Z, X, Y, A_XX, A_YY, A_XY, L_XY, S_XY){
   print("Classification starting")
   R <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   A_XZ <- matrix(rep(0, n*R), R, n)
   A_YZ <- matrix(rep(0, m*R), R, m)
   
   for(i in 1:R){
      for (j in 1:n) {
         for (k in 1:(n+m)) {
            A_XZ[i,j] <- A_XZ[i,j] + rho(Z[i,], X[j,], Q[k,])
         }
      }
      
      for (j in 1:m) {
         for (k in 1:(n+m)) {
            A_YZ[i,j] <- A_YZ[i,j] + rho(Z[i,], Y[j,], Q[k,])
         }
      }
   }
   
   A_XZ <- rowMeans(A_XZ) / (n+m-1)
   A_YZ <- rowMeans(A_YZ) / (n+m-1)
   
   L_XZ <- A_XZ - A_XX/2
   L_YZ <- A_YZ - A_YY/2
   
   S_QZ <- A_XZ + A_YZ - (A_XY + (A_XX + A_YY)/2)
   
   T_Z <- L_XY * (L_YZ - L_XZ)/2 + S_XY * S_QZ/2
   
   prac.label <- rep(0, R)
   prac.label[which(T_Z > 0)] <- 1
   prac.label[which(T_Z <= 0)] <- 2
   
   return(list(prac.label, T_Z))
}



stopCluster(cl)
gc()