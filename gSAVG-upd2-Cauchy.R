#### Author : JYOTISHKA RAY CHOUDHURY
#### Date : 22.07.2021

start.time <- proc.time()

library(doParallel)
no.cores = round(detectCores()*0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

rho <- function(a,b,c){
   if (prod(a == c)== 1 || prod(b == c) == 1) return(0)
   
   else return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
}

error.prop <- c()



classify.parallel <- function(Z, X, Y, T_FF, T_GG, T_FG, W, S_FG){
   # print("Classification starting")
   R1 <- nrow(Z)
   Q <- rbind(X,Y)
   n <- nrow(X)
   m <- nrow(Y)
   T_FZ <- matrix(rep(0, n*R1), R1, n)
   T_GZ <- matrix(rep(0, m*R1), R1, m)
   
   T_FZ.rho.fun <- function(vec){
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
   T_FZ = rowMeans(matrix(parApply(cl,indx.mat,1,T_FZ.rho.fun), R1, n, 
                          byrow = TRUE)) / (n+m-1)
   
   
   # clusterExport(cl, c('Y','Q','i','j'))
   
   T_GZ.rho.fun <- function(vec){
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
   T_GZ = rowMeans(matrix(parApply(cl,indx.mat,1,T_GZ.rho.fun), R1, m, 
                          byrow = TRUE)) / (n+m-1)
   
   
   L_FZ <- T_FZ - rep(T_FF, R1)/2
   L_GZ <- T_GZ - rep(T_GG, R1)/2
   
   S_Z <- L_FZ + L_GZ - T_FG
   
   W0_FG <- W[[1]]
   # W1_FG <- W[[2]]
   # W2_FG <- W[[3]]
   
   prac.label.0 <- prac.label.1 <- prac.label.2 <- rep(0, R1)
   
   #### CLASSIFIER 0
   delta0_Z <- L_GZ - L_FZ
   
   prac.label.0[which(delta0_Z > 0)] <- 1
   prac.label.0[which(delta0_Z <= 0)] <- 2
   
   #### CLASSIFIER 1
   delta1_Z <- W0_FG * sign(delta0_Z) / 2 + S_FG * sign(S_Z) / 2
   
   prac.label.1[which(delta1_Z > 0)] <- 1
   prac.label.1[which(delta1_Z <= 0)] <- 2
   
   #### CLASSIFIER 1
   delta2_Z <- W0_FG * delta0_Z + S_FG * S_Z
   
   prac.label.2[which(delta2_Z > 0)] <- 1
   prac.label.2[which(delta2_Z <= 0)] <- 2
   
   prac.label <- list(prac.label.0, prac.label.1, prac.label.2)
   
   return(prac.label)
}


clusterExport(cl, ls())

# t1 <- proc.time()
error.prop.0 <- error.prop.1 <- error.prop.2 <- c()

for(u in 1:50){
   n <- 20
   m <- 20
   ns <- 100
   ms <- 100
   
   d <- 200
   
   X <- matrix(rcauchy((n+ns)*d, 0, 3), nrow = n+ns, ncol = d, byrow = TRUE)
   Y <- matrix(rcauchy((m+ms)*d, 0, 4), nrow = m+ms, ncol = d, byrow = TRUE)
   
   Z <- rbind(X[(n+1):(n+ns),], Y[(m+1):(m+ms),])     ## Test Observations
   
   X <- X[1:n,]
   Y <- Y[1:m,]
   Q <- rbind(X,Y)
   
   if (u %% 6 == 0) {print(u)}
   
   ##### A_XY
   T_FG <- matrix(rep(0, n*m), n, m)
   
   # clusterExport(cl, c('X','Y','Q','i','j'))
   
   T_FG.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(X[i,],Y[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:n, each = m),rep(1:m, times = n))
   clusterExport(cl, c('X','Y','Q','n','m'))
   a <- matrix(parApply(cl,indx.mat,1,T_FG.rho.fun), n, m, byrow = T)/((n+m-2)*n*m)
   T_FG = sum(a)
   
   
   ##### A_XX
   T_FF <- matrix(rep(0, n^2), n, n)
   
   # clusterExport(cl, c('X','Q','i','j'))
   
   T_FF.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(X[i,],X[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:n, each = n),rep(1:n, times = n))
   T_FF = sum(parApply(cl,indx.mat,1,T_FF.rho.fun))/((n+m-2)*n*(n-1))
   
   
   ##### A_YY
   T_GG <- matrix(rep(0, m^2), m, m)
   
   # clusterExport(cl, c('Y','Q','i','j'))
   
   T_GG.rho.fun <- function(vec){
      i = vec[1];
      j = vec[2];
      
      return(sum(sapply(1:(n+m),function(val){
         rho(Y[i,],Y[j,],Q[val,])
      })))
   }
   
   indx.mat = cbind(rep(1:m, each = m),rep(1:m, times = m))
   T_GG = sum(parApply(cl,indx.mat,1,T_GG.rho.fun))/((n+m-2)*m*(m-1))
   
   
   ##### L
   W0_FG <- 2 * T_FG - T_FF - T_GG
   W1_FG <- W0_FG / 2 + abs(T_FF - T_GG) / 2
   W2_FG <- W0_FG^2 / 2 + (T_FF - T_GG)^2 / 2
   
   W <- list(W0_FG, W1_FG, W2_FG)
   S_FG <- T_FF - T_GG
   
   ########## Test Observations
   
   
   ground.label <- c(rep(1,ns), rep(2,ms))
   
   clusterExport(cl, c('Z'))
   
   prac.label <- classify.parallel(Z, X, Y, T_FF, T_GG, T_FG, W, S_FG)
   
   error.prop.0[u] <- length(which(ground.label != prac.label[[1]])) / (ns + ms)
   error.prop.1[u] <- length(which(ground.label != prac.label[[2]])) / (ns + ms)
   error.prop.2[u] <- length(which(ground.label != prac.label[[3]])) / (ns + ms)
   
   # print((proc.time() - t1)/u) #avgtime required per iteration
}

error.prop.mean <- list(mean(error.prop.0), mean(error.prop.1), mean(error.prop.2))
error.prop.sd <- list(sd(error.prop.0), sd(error.prop.1), sd(error.prop.2))

exec.time <- proc.time() - start.time

print("Cauchy")
print(exec.time)
print(error.prop.mean)

stopCluster(cl)
gc()
