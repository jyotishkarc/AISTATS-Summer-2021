# compile user-defined function and return pointer (RcppArmadillo is used as dependency)
dissim.cos <- function(M, no.cores){
  d = ncol(M);
  nr = nrow(M);
  
  
  
  
  tmp = apply(M,1,function(vec){
    MAT = M - matrix(rep(vec,times = nr),nrow = nr, byrow = T)
    rho.cos = parDist(MAT, method = "geodesic",threads = no.cores)
    rho.cos = as.matrix(rho.cos)
    rho.cos[is.na(rho.cos)]=0
    list(rho.cos)
  })
  
  tmp1 = lapply(tmp, function(obj) obj[[1]])
  
  Reduce('+',tmp1)/(nr-2)
}
