#### Author : JYOTISHKA RAY CHOUDHURY

# rm(list = ls())
start.time <- proc.time()

# library(doParallel)
no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

d <- 5

iterations <- 100

#1.
rho.sin <- function(a, b) {
   o1 = 1 + t(a) %*% b
   o2 = 1 + t(a) %*% a
   o3 = 1 + t(b) %*% b
   return(asin(o1 / sqrt(o2 * o3)))
}

#2.
rho.sin.comp <- function(a, b) {
   o1 = a * b
   o2 = a * a
   o3 = b * b
   tmp = sapply(1:length(a), function(val) {
      asin((1 + o1[val]) / sqrt((1 + o2[val]) * (1 + o3[val])))
   })
   return(mean(tmp))
}

#3.
rho.cos <- function(a, b, c1) {
   if (prod(a == c1) == 1 || prod(b == c1) == 1)
      return(0)
   
   else
      return(acos(sum((a - c1) * (b - c1)) / sqrt(sum((a - c1) ^ 2)
                                                  * sum(( b - c1 ) ^ 2))) / pi)
}

#4.
rho.cos.comp <- function(a, b, c1) {
   return(length(which(sign((a - c1) * (b - c1)) == -1)) / length(a))
}


clusterExport(cl, ls())


e0.sin <- e0.sin.comp <- e0.cos <- e0.cos.comp <- 
   e1.sin <- e1.sin.comp <- e1.cos <- e1.cos.comp <- 
   e2.sin <- e2.sin.comp <- e2.cos <- e2.cos.comp <- c()

for (u in 1 : iterations) {
   n <- 20
   m <- 20
   ns <- 100
   ms <- 100
   
   X <- matrix(rnorm((n + ns) * d, 0, sqrt(1)),
             nrow = n + ns,
             ncol = d,
             byrow = TRUE)
   
   Y <- matrix(rnorm((m + ms) * d, 1, sqrt(1)),
               nrow = m + ms,
               ncol = d,
               byrow = TRUE)
   
   
   # Y <- matrix(rt((m + ms) * d, df = 3),
   #          nrow = m + ms,
   #          ncol = d,
   #          byrow = TRUE)
   
   Z <- rbind(X[(n + 1):(n + ns),], Y[(m + 1):(m + ms),])     ## Test Observations
   
   X <- X[1:n,]
   Y <- Y[1:m,]
   Q <- rbind(X, Y)
   
   if (u %% 9 == 0) {
      print(u)
   }
   
   ##### T_FG
   
   T_FG.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      tmp1 = sum(sapply(1:(n + m), function(val) {
         rho.cos(X[i, ], Y[j, ], Q[val, ])
      }))
      
      tmp2 = sum(sapply(1:(n + m), function(val) {
         rho.cos.comp(X[i, ], Y[j, ], Q[val, ])
      }))
      
      return(c(rho.sin(X[i, ], Y[j, ]), rho.sin.comp(X[i, ], Y[j, ]), tmp1, tmp2))
   }
   
   
   indx.mat = cbind(rep(1:n, each = m), rep(1:m, times = n))
   clusterExport(cl, c('X', 'Y', 'n', 'm' , 'Q'))
   tmp = t(parApply(cl, indx.mat, 1, T_FG.rho.fun))
   
   T_FG.sin = sum(tmp[,1]) / (n * m)
   T_FG.sin.comp = sum(tmp[,2]) / (n * m)
   T_FG.cos = sum(tmp[,3]) / (n * m * (n + m - 2))
   T_FG.cos.comp = sum(tmp[,4]) / (n * m * (n + m - 2))
   
   
   ##### T_FF
   
   T_FF.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      tmp1 = sum(sapply(1:(n + m), function(val) {
         rho.cos(X[i, ], X[j, ], Q[val, ])
      }))
      
      tmp2 = sum(sapply(1:(n + m), function(val) {
         rho.cos.comp(X[i, ], X[j, ], Q[val, ])
      }))
      
      return(c(rho.sin(X[i, ], X[j, ]), rho.sin.comp(X[i, ], X[j, ]), tmp1, tmp2))
   }
   
   indx.mat = cbind(rep(1:n, each = n), rep(1:n, times = n))
   tmp = t(parApply(cl, indx.mat, 1, T_FF.rho.fun))
   
   T_FF.sin = sum(tmp[, 1]) / (n * n)
   T_FF.sin.comp = sum(tmp[, 2]) / (n * n)
   T_FF.cos = sum(tmp[, 3]) / (n * n * (n + m - 2))
   T_FF.cos.comp = sum(tmp[, 4]) / (n * n * (n + m - 2))
   
   
   ##### T_GG
   
   T_GG.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      tmp1 = sum(sapply(1:(n + m), function(val) {
         rho.cos(Y[i, ], Y[j, ], Q[val, ])
      }))
      
      tmp2 = sum(sapply(1:(n + m), function(val) {
         rho.cos.comp(Y[i, ], Y[j, ], Q[val, ])
      }))
      
      return(c(rho.sin(Y[i, ], Y[j, ]), rho.sin.comp(Y[i, ], Y[j, ]), tmp1, tmp2))
   }
   
   indx.mat = cbind(rep(1:m, each = m), rep(1:m, times = m))
   tmp = t(parApply(cl, indx.mat, 1, T_GG.rho.fun))
   
   T_GG.sin = sum(tmp[, 1]) / (m * m)
   T_GG.sin.comp = sum(tmp[, 2]) / (m * m)
   T_GG.cos = sum(tmp[, 3]) / (m * m * (n + m - 2))
   T_GG.cos.comp = sum(tmp[, 4]) / (m * m * (n + m - 2))
   
   
   ##### 1.
   W0_FG.sin <- T_FF.sin - 2 * T_FG.sin + T_GG.sin
   W1_FG.sin <- W0_FG.sin / 2 + abs(T_FF.sin - T_GG.sin) / 2
   W2_FG.sin <- (W0_FG.sin ^ 2 + (T_FF.sin - T_GG.sin) ^ 2 )/ 2
   
   S_FG.sin <- T_FF.sin - T_GG.sin
   
   
   ##### 2.
   W0_FG.sin.comp <-
      T_FF.sin.comp - 2 * T_FG.sin.comp + T_GG.sin.comp
   W1_FG.sin.comp <-
      W0_FG.sin.comp / 2 + abs(T_FF.sin.comp - T_GG.sin.comp) / 2
   W2_FG.sin.comp <-
      (W0_FG.sin.comp ^ 2 + (T_FF.sin.comp - T_GG.sin.comp) ^ 2) / 2
   
   S_FG.sin.comp <- T_FF.sin.comp - T_GG.sin.comp
   
   ##### 3
   W0_FG.cos <- 2 * T_FG.cos - (T_FF.cos  + T_GG.cos)
   W1_FG.cos <- W0_FG.cos / 2 + abs(T_FF.cos - T_GG.cos) / 2
   W2_FG.cos <- (W0_FG.cos ^ 2  + (T_FF.cos - T_GG.cos) ^ 2 )/ 2
   
   S_FG.cos <- T_FF.cos - T_GG.cos
   
   
   ##### 4.
   W0_FG.cos.comp <-
      2 * T_FG.cos.comp - (T_FF.cos.comp + T_GG.cos.comp)
   W1_FG.cos.comp <-
      W0_FG.cos.comp / 2 + abs(T_FF.cos.comp - T_GG.cos.comp) / 2
   W2_FG.cos.comp <-
      (W0_FG.cos.comp ^ 2  + (T_FF.cos.comp - T_GG.cos.comp) ^ 2 )/ 2
   
   S_FG.cos.comp <- T_FF.cos.comp - T_GG.cos.comp
   
   ########## Test Observations
   
   
   ground.label <- c(rep(1, ns), rep(2, ms))
   
   clusterExport(cl, c('Z'))
   
   R1 <- nrow(Z)
   n <- nrow(X)
   m <- nrow(Y)
   
   T_FZ.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      tmp1 = sum(sapply(1:(n + m), function(val) {
         rho.cos(Z[i, ], X[j, ], Q[val, ])
      }))
      
      tmp2 = sum(sapply(1:(n + m), function(val) {
         rho.cos.comp(Z[i, ], X[j, ], Q[val, ])
      }))
      
      return(c(rho.sin(Z[i, ], X[j, ]), rho.sin.comp(Z[i, ], X[j, ]), tmp1, tmp2))
   }
   
   indx.mat = cbind(rep(1:R1, each = n), rep(1:n, times = R1))
   clusterExport(cl, c('R1'), envir = environment())
   
   tmp = t(parApply(cl, indx.mat, 1, T_FZ.rho.fun))
   
   T_FZ.sin = rowMeans(matrix(tmp[, 1], R1, n, byrow = TRUE))
   T_FZ.sin.comp = rowMeans(matrix(tmp[, 2], R1, n, byrow = TRUE))
   T_FZ.cos = rowMeans(matrix(tmp[, 3], R1, n, byrow = TRUE))/(n+m-1)
   T_FZ.cos.comp = rowMeans(matrix(tmp[, 4], R1, n, byrow = TRUE))/(n+m-1)
   
   T_GZ.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      tmp1 = sum(sapply(1:(n + m), function(val) {
         rho.cos(Z[i, ], Y[j, ], Q[val, ])
      }))
      
      tmp2 = sum(sapply(1:(n + m), function(val) {
         rho.cos.comp(Z[i, ], Y[j, ], Q[val, ])
      }))
      
      return(c(rho.sin(Z[i, ], Y[j, ]), rho.sin.comp(Z[i, ], Y[j, ]), tmp1, tmp2))
   }
   
   indx.mat = cbind(rep(1:R1, each = m), rep(1:m, times = R1))
   
   tmp = t(parApply(cl, indx.mat, 1, T_GZ.rho.fun))
   
   T_GZ.sin = rowMeans(matrix(tmp[, 1], R1, n, byrow = TRUE))
   T_GZ.sin.comp = rowMeans(matrix(tmp[, 2], R1, n, byrow = TRUE))
   T_GZ.cos = rowMeans(matrix(tmp[, 3], R1, n, byrow = TRUE))/(n+m-1)
   T_GZ.cos.comp = rowMeans(matrix(tmp[, 4], R1, n, byrow = TRUE))/(n+m-1)
   
   #1.
   L_FZ.sin <- rep(T_FF.sin, R1) / 2 - T_FZ.sin
   L_GZ.sin <- rep(T_GG.sin, R1) / 2 - T_GZ.sin
   S_Z.sin <- -rep(T_FG.sin, R1) - (L_FZ.sin + L_GZ.sin)
   
   #2.
   L_FZ.sin.comp <- rep(T_FF.sin.comp, R1) / 2 - T_FZ.sin.comp
   L_GZ.sin.comp <- rep(T_GG.sin.comp, R1) / 2 - T_GZ.sin.comp
   S_Z.sin.comp <- -rep(T_FG.sin.comp, R1) - (L_FZ.sin.comp + L_GZ.sin.comp)
   
   #3.
   L_FZ.cos <- T_FZ.cos - rep(T_FF.cos, R1) / 2
   L_GZ.cos <- T_GZ.cos - rep(T_GG.cos, R1) / 2
   S_Z.cos <- L_FZ.cos + L_GZ.cos - rep(T_FG.cos,R1)
   
   #4.
   L_FZ.cos.comp <- T_FZ.cos.comp - rep(T_FF.cos.comp, R1) / 2
   L_GZ.cos.comp <- T_GZ.cos.comp - rep(T_GG.cos.comp, R1) / 2
   S_Z.cos.comp <- L_FZ.cos.comp + L_GZ.cos.comp - rep(T_FG.cos.comp, R1)
   
   
   el.0.sin <- el.0.sin.comp <- el.0.cos <- el.0.cos.comp <- 
      el.1.sin <- el.1.sin.comp <- el.1.cos <- el.1.cos.comp <- 
      el.2.sin <- el.2.sin.comp <- el.2.cos <- el.2.cos.comp <- 0
   
   
   #### CLASSIFIER 0
   delta0_Z.sin <- 2 * (L_GZ.sin - L_FZ.sin)
   delta0_Z.sin.comp <- 2 * (L_GZ.sin.comp - L_FZ.sin.comp)
   delta0_Z.cos <- 2 * (L_GZ.cos - L_FZ.cos)
   delta0_Z.cos.comp <- 2 * (L_GZ.cos.comp - L_FZ.cos.comp)
   
   
   el.0.sin[which(delta0_Z.sin > 0)] <- 1
   el.0.sin[which(delta0_Z.sin <= 0)] <- 2
   
   el.0.sin.comp[which(delta0_Z.sin.comp > 0)] <- 1
   el.0.sin.comp[which(delta0_Z.sin.comp <= 0)] <- 2
   
   el.0.cos[which(delta0_Z.cos > 0)] <- 1
   el.0.cos[which(delta0_Z.cos <= 0)] <- 2
   
   el.0.cos.comp[which(delta0_Z.cos.comp > 0)] <- 1
   el.0.cos.comp[which(delta0_Z.cos.comp <= 0)] <- 2
   
   
   #### CLASSIFIER 1
   delta1_Z.sin = (W0_FG.sin * sign(delta0_Z.sin) * 0.5) + 
      (S_FG.sin * sign(S_Z.sin) * 0.5)
   
   delta1_Z.sin.comp = (W0_FG.sin.comp * sign(delta0_Z.sin.comp) * 0.5) + 
      (S_FG.sin.comp * sign(S_Z.sin.comp) * 0.5)
   
   delta1_Z.cos = (W0_FG.cos * sign(delta0_Z.cos) * 0.5) + 
      (S_FG.cos * sign(S_Z.cos) * 0.5)
   
   delta1_Z.cos.comp = (W0_FG.cos.comp * sign(delta0_Z.cos.comp) * 0.5) + 
      (S_FG.cos.comp * sign(S_Z.cos.comp) * 0.5)
   
   
   el.1.sin[which(delta1_Z.sin > 0)] <- 1
   el.1.sin[which(delta1_Z.sin <= 0)] <- 2
   
   el.1.sin.comp[which(delta1_Z.sin.comp > 0)] <- 1
   el.1.sin.comp[which(delta1_Z.sin.comp <= 0)] <- 2
   
   el.1.cos[which(delta1_Z.cos > 0)] <- 1
   el.1.cos[which(delta1_Z.cos <= 0)] <- 2
   
   el.1.cos.comp[which(delta1_Z.cos.comp > 0)] <- 1
   el.1.cos.comp[which(delta1_Z.cos.comp <= 0)] <- 2
   
   
   #### CLASSIFIER 2
   delta2_Z.sin <-
      (W0_FG.sin * delta0_Z.sin * 0.5) + (S_FG.sin * S_Z.sin)
   
   delta2_Z.sin.comp <-
      (W0_FG.sin.comp * delta0_Z.sin.comp * 0.5) + (S_FG.sin.comp * S_Z.sin.comp)
   
   delta2_Z.cos <-
      (W0_FG.cos * delta0_Z.cos * 0.5) + (S_FG.cos * S_Z.cos)
   
   delta2_Z.cos.comp <-
      (W0_FG.cos.comp * delta0_Z.cos.comp * 0.5) + (S_FG.cos.comp * S_Z.cos.comp)
   
   
   el.2.sin[which(delta2_Z.sin > 0)] <- 1
   el.2.sin[which(delta2_Z.sin <= 0)] <- 2
   
   el.2.sin.comp[which(delta2_Z.sin.comp > 0)] <- 1
   el.2.sin.comp[which(delta2_Z.sin.comp <= 0)] <- 2
   
   el.2.cos[which(delta2_Z.cos > 0)] <- 1
   el.2.cos[which(delta2_Z.cos <= 0)] <- 2
   
   el.2.cos.comp[which(delta2_Z.cos.comp > 0)] <- 1
   el.2.cos.comp[which(delta2_Z.cos.comp <= 0)] <- 2
   
   
   e0.sin[u] <- sum(ground.label != el.0.sin) / (ns + ms)
   e0.sin.comp[u] <- sum(ground.label != el.0.sin.comp) / (ns + ms)
   e0.cos[u] <- sum(ground.label != el.0.cos) / (ns + ms)
   e0.cos.comp[u] <- sum(ground.label != el.0.cos.comp) / (ns + ms)
   
   e1.sin[u] <- sum(ground.label != el.1.sin) / (ns + ms)
   e1.sin.comp[u] <- sum(ground.label != el.1.sin.comp) / (ns + ms)
   e1.cos[u] <- sum(ground.label != el.1.cos) / (ns + ms)
   e1.cos.comp[u] <- sum(ground.label != el.1.cos.comp) / (ns + ms)
   
   e2.sin[u] <- sum(ground.label != el.2.sin) / (ns + ms)
   e2.sin.comp[u] <- sum(ground.label != el.2.sin.comp) / (ns + ms)
   e2.cos[u] <- sum(ground.label != el.2.cos) / (ns + ms)
   e2.cos.comp[u] <- sum(ground.label != el.2.cos.comp) / (ns + ms)
   
   # print(u)
}

all.info.matrix <- matrix(c(e0.sin, e0.sin.comp, e0.cos, e0.cos.comp,
                            e1.sin, e1.sin.comp, e1.cos, e1.cos.comp,
                            e2.sin, e2.sin.comp, e2.cos, e2.cos.comp),
                          nrow = 12, ncol = iterations,
                          byrow = TRUE)

e0.mean <- c(mean(e0.sin),
             mean(e0.sin.comp),
             mean(e0.cos),
             mean(e0.cos.comp))

e1.mean <- c(mean(e1.sin),
             mean(e1.sin.comp),
             mean(e1.cos),
             mean(e1.cos.comp))

e2.mean <- c(mean(e2.sin),
             mean(e2.sin.comp),
             mean(e2.cos),
             mean(e2.cos.comp))


e0.sd <- c(sd(e0.sin), sd(e0.sin.comp), sd(e0.cos), sd(e0.cos.comp))
e1.sd <- c(sd(e1.sin), sd(e1.sin.comp), sd(e1.cos), sd(e1.cos.comp))
e2.sd <- c(sd(e2.sin), sd(e2.sin.comp), sd(e2.cos), sd(e2.cos.comp))

all.error.means <- list("Mean of Error Proportions for CLASSIFIER #0" = e0.mean,
                        "Mean of Error Proportions for CLASSIFIER #1" = e1.mean,
                        "Mean of Error Proportions for CLASSIFIER #2" = e2.mean)

all.error.sd <- list("SD of Error Proportions for CLASSIFIER #0" = e0.sd,
                     "SD of Error Proportions for CLASSIFIER #1" = e1.sd,
                     "SD of Error Proportions for CLASSIFIER #2" = e2.sd)


print(all.error.means)


exec.time <- proc.time() - start.time
print(exec.time)

stopCluster(cl)
gc()


