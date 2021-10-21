library(magrittr)
library(sciplot)

iterations <- 100
d <- 1
n <- m <- 20
ns <- ms <- 0

res <- list()

rho.sin <- function(a, b) {
   o1 = 1 + a * b
   o2 = 1 + a * a
   o3 = 1 + b * b
   return(mean(asin(o1 / sqrt(o2 * o3)) / (2*pi)))
}


for (ex in 1:5){
   
   print(paste("EXAMPLE",ex, sep = " "))
   
   result <- matrix(0, nrow = 100, ncol = 3)
   
   for (u in 1 : iterations) {
      
      if (ex == 1){
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
      }
      
      if (ex == 2) {
         X <- matrix(rnorm((n + ns) * d, 0, sqrt(3)),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         
         Y <- matrix(rt((m + ms) * d, df = 3),
                     nrow = m + ms,
                     ncol = d,
                     byrow = TRUE)
      }
      
      if (ex == 3) {
         set.seed(u)
         X <- matrix(rcauchy((n + ns) * d, 0, 1),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         Y <- matrix(rcauchy((m + ms) * d, 1, 1),
                     nrow = m + ms,
                     ncol = d,
                     byrow = TRUE)
      }
      
      if (ex == 4) {
         
         set.seed(u)
         X <- matrix(rcauchy((n + ns) * d, 0, 1),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         Y <- matrix(rcauchy((m + ms) * d, 0, 2),
                     nrow = m + ms,
                     ncol = d,
                     byrow = TRUE)
      }
      
      if (ex == 5) {
         library(EnvStats)
         
         set.seed(u)
         X <- matrix(rpareto((n + ns) * d, location =  1, shape = 1),
                     nrow = n + ns,
                     ncol = d,
                     byrow = TRUE)
         
         set.seed(u)
         Y <- matrix(rpareto((m + ms) * d, location = 1.25, shape = 1),
                     nrow = m + ms,
                     ncol = d,
                     byrow = TRUE)
      }
      
      
      ########## T_FG
      
      T_FG.rho.fun <- function(vec) {
         i = vec[1]
         j = vec[2]
         
         return(rho.sin(X[i, ], Y[j, ]))
      }
      
      indx.mat = cbind(rep(1:n, each = m), rep(1:m, times = n))
      tmp = t(apply(indx.mat, 1, T_FG.rho.fun))
      tmp = matrix(tmp, nrow = n, byrow = T)   
      T_FG.sin = sum(tmp) / (n * m)
      
      
      ########## T_FF
      
      T_FF.rho.fun <- function(vec) {
         i = vec[1]
         j = vec[2]
         
         return(rho.sin(X[i, ], X[j, ]))
      }
      
      indx.mat = cbind(rep(1:n, each = n), rep(1:n, times = n))
      tmp = t(apply(indx.mat, 1, T_FF.rho.fun))
      tmp = matrix(tmp, nrow = n, byrow = T)
      diag(tmp) = 0
      T_FF.sin = sum(tmp) / ( n * (n-1))
      
      
      ########## T_GG
      
      T_GG.rho.fun <- function(vec) {
         i = vec[1]
         j = vec[2]
         
         return(rho.sin(Y[i, ], Y[j, ]))
      }
      
      indx.mat = cbind(rep(1:m, each = m), rep(1:m, times = m))
      tmp = t(apply(indx.mat, 1, T_GG.rho.fun))
      tmp = matrix(tmp, nrow = m, byrow = T)
      diag(tmp) = 0
      T_GG.sin = sum(tmp) / ( m * (m-1))
      
      
      result[u,] <- c(T_FF.sin, T_FG.sin, T_GG.sin)
   }
   
   res[[ex]] <- rbind(result, rep(NA, 3),
                      apply(result, 2, mean),
                      apply(result, 2, sciplot::se)) %>% as.data.frame()
   
   colnames(res[[ex]]) <- c("T_11","T_12","T_22")
}

res <- list("Example 1" = res[[1]],
            "Example 2" = res[[2]],
            "Example 3" = res[[3]],
            "Example 4" = res[[4]],
            "Example 5" = res[[5]])

writexl::write_xlsx(x = res,
                    path = "C:\\Users\\JYOTISHKA\\Desktop\\T11-T12-T22.xlsx")



