
# rm(list = ls())
start.time <- proc.time()

library(magrittr)
library(doParallel)
no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)

source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dataset-partitioning-jrc.R')

d <- ncol(X) - 1

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
      out <- (1 + o1[val]) / sqrt((1 + o2[val]) * (1 + o3[val]))
      
      if (abs(out) > 1) {
         return(asin(sign(out)))
      }
      else return(asin(out))
   })
   
   return(mean(tmp))
}


data.partition <- function(X.train, X.test){
   
   X.train <- as.matrix(X.train)
   X.test <- as.matrix(X.test)
   
   X = rbind(X.train, X.test)
   
   classes = unique(X[,1])
   
   pops.withlbl <- list(X[X[,1] == classes[1],], X[X[,1] == classes[2],])
   nj <- sapply(pops.withlbl, nrow)
   
   S.1 <- sample(which(X[,1] == classes[1]), round(nj[1]/2))
   S.2 <- sample(which(X[,1] == classes[2]), round(nj[2]/2))
   
   train.1 <- X[S.1,]
   train.2 <- X[S.2,]
   
   test.1 <- X[setdiff(which(X[,1] == classes[1]), S.1),]
   test.2 <- X[setdiff(which(X[,1] == classes[2]), S.2),]
   
   train.sample <- rbind(train.1, train.2)
   test.sample <- rbind(test.1, test.2)
   
   return(list(as.matrix(train.sample), as.matrix(test.sample)))
   
}


clusterExport(cl, ls())


e0.sin <- e0.sin.comp <- e1.sin <- e1.sin.comp <- e2.sin <- e2.sin.comp <- c()

for (u in 1 : iterations) {
   
   print(u)
   
   data.train <- Wine.train %>% na.omit %>% as.matrix
   data.test <- Wine.test %>% na.omit %>% as.matrix
   
   partitioned.data <- data.partition(data.train, data.test)
   data.train <- as.matrix(partitioned.data[[1]])
   data.test <- as.matrix(partitioned.data[[2]])
   
   data.train.c1 <- data.train[which(data.train[,1] == 1), ]
   data.train.c2 <- data.train[which(data.train[,1] == 2), ]
   
   
   Z <- data.test[,-1]     ## Test Observations
   
   X <- data.train.c1[,-1]
   Y <- data.train.c2[,-1]
   Q <- rbind(X, Y)
   
   n <- nrow(X)
   m <- nrow(Y)
   
   ##### T_FG
   
   T_FG.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      return(c(rho.sin(X[i, ], Y[j, ]), rho.sin.comp(X[i, ], Y[j, ])))
   }
   
   indx.mat = cbind(rep(1:n, each = m), rep(1:m, times = n))
   clusterExport(cl, c('X', 'Y', 'n', 'm' , 'Q'))
   tmp = t(parApply(cl, indx.mat, 1, T_FG.rho.fun))
   
   T_FG.sin = sum(tmp[,1]) / (n * m)
   T_FG.sin.comp = sum(tmp[,2]) / (n * m)
   
   
   ##### T_FF
   
   T_FF.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      return(c(rho.sin(X[i, ], X[j, ]), rho.sin.comp(X[i, ], X[j, ])))
   }
   
   indx.mat = cbind(rep(1:n, each = n), rep(1:n, times = n))
   tmp = t(parApply(cl, indx.mat, 1, T_FF.rho.fun))
   
   T_FF.sin = sum(tmp[, 1]) / (n * n)
   T_FF.sin.comp = sum(tmp[, 2]) / (n * n)
   
   
   ##### T_GG
   
   T_GG.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      return(c(rho.sin(Y[i, ], Y[j, ]), rho.sin.comp(Y[i, ], Y[j, ])))
   }
   
   indx.mat = cbind(rep(1:m, each = m), rep(1:m, times = m))
   tmp = t(parApply(cl, indx.mat, 1, T_GG.rho.fun))
   
   T_GG.sin = sum(tmp[, 1]) / (m * m)
   T_GG.sin.comp = sum(tmp[, 2]) / (m * m)
   
   
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
   
   
   ########## Test Observations
   
   ground.label <- data.test[,1]
   
   clusterExport(cl, c('Z'))
   
   R1 <- nrow(Z)
   n <- nrow(X)
   m <- nrow(Y)
   
   T_FZ.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      return(c(rho.sin(Z[i, ], X[j, ]), rho.sin.comp(Z[i, ], X[j, ])))
   }
   
   indx.mat = cbind(rep(1:R1, each = n), rep(1:n, times = R1))
   clusterExport(cl, c('R1'), envir = environment())
   
   tmp = t(parApply(cl, indx.mat, 1, T_FZ.rho.fun))
   
   T_FZ.sin = rowMeans(matrix(tmp[, 1], R1, n, byrow = TRUE))
   T_FZ.sin.comp = rowMeans(matrix(tmp[, 2], R1, n, byrow = TRUE))

   T_GZ.rho.fun <- function(vec) {
      i = vec[1]
      j = vec[2]
      
      return(c(rho.sin(Z[i, ], Y[j, ]), rho.sin.comp(Z[i, ], Y[j, ])))
   }
   
   indx.mat = cbind(rep(1:R1, each = m), rep(1:m, times = R1))
   
   tmp = t(parApply(cl, indx.mat, 1, T_GZ.rho.fun))
   
   T_GZ.sin = rowMeans(matrix(tmp[, 1], R1, n, byrow = TRUE))
   T_GZ.sin.comp = rowMeans(matrix(tmp[, 2], R1, n, byrow = TRUE))
   
   
   #1.
   L_FZ.sin <- rep(T_FF.sin, R1) / 2 - T_FZ.sin
   L_GZ.sin <- rep(T_GG.sin, R1) / 2 - T_GZ.sin
   S_Z.sin <- -rep(T_FG.sin, R1) - (L_FZ.sin + L_GZ.sin)
   
   #2.
   L_FZ.sin.comp <- rep(T_FF.sin.comp, R1) / 2 - T_FZ.sin.comp
   L_GZ.sin.comp <- rep(T_GG.sin.comp, R1) / 2 - T_GZ.sin.comp
   S_Z.sin.comp <- -rep(T_FG.sin.comp, R1) - (L_FZ.sin.comp + L_GZ.sin.comp)
   
   
   el.0.sin <- el.0.sin.comp <- 
      el.1.sin <- el.1.sin.comp <- 
         el.2.sin <- el.2.sin.comp <- 0
   
   
   #### CLASSIFIER 0
   delta0_Z.sin <- 2 * (L_GZ.sin - L_FZ.sin)
   delta0_Z.sin.comp <- 2 * (L_GZ.sin.comp - L_FZ.sin.comp)
   
   el.0.sin[which(delta0_Z.sin > 0)] <- 1
   el.0.sin[which(delta0_Z.sin <= 0)] <- 2
   
   el.0.sin.comp[which(delta0_Z.sin.comp > 0)] <- 1
   el.0.sin.comp[which(delta0_Z.sin.comp <= 0)] <- 2
   
   
   #### CLASSIFIER 1
   delta1_Z.sin = (W0_FG.sin * sign(delta0_Z.sin) * 0.5) + 
      (S_FG.sin * sign(S_Z.sin) * 0.5)
   
   delta1_Z.sin.comp = (W0_FG.sin.comp * sign(delta0_Z.sin.comp) * 0.5) + 
      (S_FG.sin.comp * sign(S_Z.sin.comp) * 0.5)
   
   
   el.1.sin[which(delta1_Z.sin > 0)] <- 1
   el.1.sin[which(delta1_Z.sin <= 0)] <- 2
   
   el.1.sin.comp[which(delta1_Z.sin.comp > 0)] <- 1
   el.1.sin.comp[which(delta1_Z.sin.comp <= 0)] <- 2
   
   
   #### CLASSIFIER 2
   delta2_Z.sin <-
      (W0_FG.sin * delta0_Z.sin * 0.5) + (S_FG.sin * S_Z.sin)
   
   delta2_Z.sin.comp <-
      (W0_FG.sin.comp * delta0_Z.sin.comp * 0.5) + (S_FG.sin.comp * S_Z.sin.comp)
   
   
   el.2.sin[which(delta2_Z.sin > 0)] <- 1
   el.2.sin[which(delta2_Z.sin <= 0)] <- 2
   
   el.2.sin.comp[which(delta2_Z.sin.comp > 0)] <- 1
   el.2.sin.comp[which(delta2_Z.sin.comp <= 0)] <- 2
   
   
   e0.sin[u] <- sum(ground.label != el.0.sin) / (ns + ms)
   e0.sin.comp[u] <- sum(ground.label != el.0.sin.comp) / (ns + ms)
   
   e1.sin[u] <- sum(ground.label != el.1.sin) / (ns + ms)
   e1.sin.comp[u] <- sum(ground.label != el.1.sin.comp) / (ns + ms)
   
   e2.sin[u] <- sum(ground.label != el.2.sin) / (ns + ms)
   e2.sin.comp[u] <- sum(ground.label != el.2.sin.comp) / (ns + ms)
   
   # print(u)
}

all.info.matrix <- matrix(c(e0.sin, e0.sin.comp,
                            e1.sin, e1.sin.comp,
                            e2.sin, e2.sin.comp),
                          nrow = 6, ncol = iterations,
                          byrow = TRUE)

e0.mean <- c(mean(e0.sin),
             mean(e0.sin.comp))

e1.mean <- c(mean(e1.sin),
             mean(e1.sin.comp))

e2.mean <- c(mean(e2.sin),
             mean(e2.sin.comp))

e0.sd <- c(sd(e0.sin), sd(e0.sin.comp))
e1.sd <- c(sd(e1.sin), sd(e1.sin.comp))
e2.sd <- c(sd(e2.sin), sd(e2.sin.comp))


all.error.means <- list("Mean of Error Proportions for CLASSIFIER #0" = e0.mean,
                        "Mean of Error Proportions for CLASSIFIER #1" = e1.mean,
                        "Mean of Error Proportions for CLASSIFIER #2" = e2.mean)

all.error.sd <- list("SD of Error Proportions for CLASSIFIER #0" = e0.sd,
                     "SD of Error Proportions for CLASSIFIER #1" = e1.sd,
                     "SD of Error Proportions for CLASSIFIER #2" = e2.sd)

cat("\nDimension =", d, "\n\n")

print(all.error.means)
print(all.error.sd)

exec.time <- proc.time() - start.time
print(exec.time)

stopCluster(cl)
gc()


