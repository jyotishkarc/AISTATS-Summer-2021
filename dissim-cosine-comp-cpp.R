# compile user-defined function and return pointer (RcppArmadillo is used as dependency)

library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)

dissim.cos.comp <- function(M, no.cores){
  d = ncol(M);
  nr = nrow(M);
  M = as.matrix(M)
  
  cstring.cos.comp = paste(
    "double pairEDist(const arma::mat &A, const arma::mat &B) {

            int k;
            double o1=0;
            double o2=0;
            int d = " ,
    d,
    ";

            double tmp1 = 0;
            double tmp2 = 0;
            double tmp3 = 0;

            for(k =0; k <d; k++){

                    o1 = A[k] * B[k];
                    if(o1>= 0){
                    o2 = o2 + acos(1);
                    }
                    else{
                    o2 = o2 + acos(-1);
                    }
            }
            return(o2/d);
  }",
  sep = ''
  )
  
  disFuncPtr.cos.comp <- cppXPtr(cstring.cos.comp,
                                 depends = c("RcppArmadillo"))
  
  tmp = apply(M,1,function(vec){
  MAT = M - matrix(rep(vec, times = nr), nrow = nr, byrow = TRUE)
  
  rho.cos.comp = parDist(MAT, 
                    method = "custom", 
                    func = disFuncPtr.cos.comp,
                    threads = no.cores)
  
  rho.cos.comp = as.matrix(rho.cos.comp)
  rho.cos.comp[is.na(rho.cos.comp)]=0
  
  return(list(rho.cos.comp))
  })
  
  tmp1 = lapply(tmp, function(obj) obj[[1]])
  
  return(Reduce('+', tmp1)/(nr-2))
}

