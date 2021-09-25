# compile user-defined function and return pointer (RcppArmadillo is used as dependency)

library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)

dissim.sin.comp <- function(train.set, no.cores){
   d = ncol(train.set);
   
   cstring = paste("double pairEDist(const arma::mat &A, const arma::mat &B) {
            
            int k;
            double o1=0;
            double o2=0;
            double o3=0;
            double o4 = 0;
            double o5 = 0;
            double o6 = 0;
            
            int d = " , d,  ";
            
            double tmp1 = 0;
            double tmp2 = 0;
            double tmp3 = 0;
            
            for(k =0; k <d; k++){
                    
                    o1 = A[k] * B[k] + 1;
                    o2 = A[k] * A[k] + 1;
                    o3 = B[k] * B[k] + 1;
                
                    o4 = pow(o2 * o3, 0.5);
                    if(o1/o4 >=1){
                    o5 = 1;
                    }
                    else{
                    if(o1/o4 <= -1){
                    o5 = -1;
                    }
                    else{
                    o5 = o1/o4;
                    }
                    }
            
                    o6 = o6 + asin(o5); 
            }
            return(o6/d);
  }", sep='')
   
   pairEnergyFuncPtr <- cppXPtr(cstring, 
                                depends = c("RcppArmadillo"))
   
   rho.sin.comp <- parDist(train.set, 
                          method = "custom", 
                          func = pairEnergyFuncPtr,
                          upper = TRUE,
                          diag = TRUE,
                          threads = no.cores)
   
   return(rho.sin.comp)
}