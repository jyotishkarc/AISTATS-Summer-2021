rho <- function(a,b,c){
   if (a != c && b != c) {
      return(acos(sum((a-c)*(b-c)) / sqrt(sum((a-c)^2) * sum((b-c)^2))) / pi)
   }
   
   return(0)
}


n <- 20
m <- 20
d <- 50

X <- matrix(rcauchy(n*d), nrow = n, ncol = d, byrow = TRUE)
Y <- matrix(rnorm(n*d), nrow = m, ncol = d, byrow = TRUE)
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

A_XY <- sum(A_XY)/((n+m-1)*n*m)
Sys.time()

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
Sys.time()

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
Sys.time()

##### L
L_XY <- 2 * A_XY - A_XX - A_YY
S_XY <- A_XX - A_YY

ns <- 60
ms <- 60

Z_F <- matrix(rcauchy(ns*d), nrow = ns, ncol = d, byrow = TRUE)
Z_G <- matrix(rnorm(ns*d), nrow = ms, ncol = d, byrow = TRUE)
Z <- rbind(Z_F, Z_G)

ground.label <- c(rep(1,ns), rep(2,ms))
prac.label <- classify(Z, X, Y, A_XX, A_YY, A_XY, L_XY, S_XY)

Sys.time()
print(length(which(ground.label != prac.label)))



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
   
   Sys.time()
   
   A_XZ <- rowMeans(A_XZ)
   A_YZ <- rowMeans(A_YZ)
   
   L_XZ <- A_XZ - rep(A_XX/2, R)
   L_YZ <- A_YZ - rep(A_YY/2, R)
   
   S_QZ <- A_XZ + A_YZ - rep((A_XY + (A_XX + A_YY)/2), R)
   
   T_Z <- L_XY * (L_YZ - L_XZ)/2 + S_XY * S_QZ/2
   
   prac.label <- c()
   prac.label[which(T_Z > 0)] <- 1
   prac.label[which(T_Z <= 0)] <- 2
   
   return(prac.label)
}



