# compile user-defined function and return pointer (RcppArmadillo is used as dependency)

library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)

dissim.sin <- function(train.set, no.cores){
   d = ncol(train.set);
   
   cstring1 = paste("double pairEDist(const arma::mat &A, const arma::mat &B) {
            
            int k;
            double o1=0;
            double o2=0;
            double o3=0;
            double o4 = 0;
            double o5 = 0;
            
            int d = " , d,  ";
            
            double tmp1 = 0;
            double tmp2 = 0;
            double tmp3 = 0;
            
            for(k =0; k <d; k++){
                    tmp1 = A[k] * B[k];
                    tmp2 = A[k] * A[k];
                    tmp3 = B[k] * B[k];
                    
                    o1 = o1 +  tmp1;
                    o2 = o2 +  tmp2;
                    o3 = o3 +  tmp3;
            }
                o1 = o1 + 1;
                o2 = o2 + 1;
                o3 = o3 + 1;
                
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
            
            return asin(o5); 
            }", sep='')
   
   FuncPtr <- cppXPtr(cstring1, 
                      depends = c("RcppArmadillo"))
   
   rho.sin = parDist(train.set, 
                     method = "custom", 
                     func = FuncPtr,
                     upper = TRUE,
                     diag = TRUE,
                     threads = no.cores)
   
   return(rho.sin)
}
