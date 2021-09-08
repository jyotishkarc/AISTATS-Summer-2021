rm(list = ls())
options(warn = 1)
start.time <- proc.time()
ITER = 15
library(doParallel)
library(RcppArmadillo)
library(RcppXPtrUtils)
library(parallelDist)
library(foreach)
source('dissim_cosine.R')
source('dissim_cosine_comp.R')
source('dissim_sine.R')
source('dissim_sine_comp.R')

O = NULL
no.cores = round(detectCores() * 0.75)
cl = makeCluster(spec = no.cores, type = 'PSOCK')
registerDoParallel(cl)


#--- data reading begins ------------------------------------------
# p = 'D:/RealData/UCRArchive/TSVformat/'
p = 'D:/RealData/UCRArchive/'
# p = 'D:/Sarbojit/RealData/UCRArchive/BIG/'
nms = list.files(p)
M=9


colon.train = read.table(paste(p, nms[41], sep = ''), sep = ',')
colon.test = read.table(paste(p, nms[40], sep = ''), sep = ',')
X = rbind(colon.train, colon.test)
d = ncol(X)
classes = unique(X[, 1])
muX = colMeans(X)
sdX = apply(X, 2, sd)
for (i in 2:ncol(X)) {
  X[, i] = (X[, i] - muX[i]) / sdX[i]
}

pops.withlbl <-
  list(X[X[, 1] == classes[1], ], X[X[, 1] == classes[2], ])

nj <- sapply(pops.withlbl, nrow)
n.all <- sum(nj)
prior_j <- nj / n.all
d <- ncol(X) - 1
J <- length(nj)
nj_train <- round(nj * 0.5)
ntrain = sum(nj_train)
rm(X)
# -- data reading ends here --------------------------------------

#---- c++ functions ----------------------------------------------
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

system.time({disFuncPtr.cos.comp <-
  cppXPtr(cstring.cos.comp,
          depends = c("RcppArmadillo"))})


#------------dissimilarity computation---------------------------
whole.set = as.matrix(rbind(pops.withlbl[[1]], pops.withlbl[[2]]))

system.time({TFG.sin.mat = as.matrix(dissim.sin(train.set = whole.set[,-1], no.cores = no.cores))})
system.time({TFG.sincomp.mat = as.matrix(dissim.sin.comp(train.set = whole.set[,-1], no.cores = no.cores))})

out1 <- NULL
out2 <- NULL


for(u in 1:ITER){
system.time({train.index = test.index = train.sample = test.sample = list(NULL)
  for (cnt in 1:J)
    # partitioning into training and test set
  {
    train.index[[cnt]] <- sample(1:nj[cnt], size = nj_train[cnt])
    test.index[[cnt]] <- setdiff(1:nj[cnt], train.index[[cnt]])
    train.sample[[cnt]] <- pops.withlbl[[cnt]][train.index[[cnt]], ]
    test.sample[[cnt]] <- pops.withlbl[[cnt]][test.index[[cnt]], ]
  }
  
  njtrain <- sapply(train.sample, nrow) #training sizes
  n = njtrain[1]
  m = njtrain[2]
  
  njtest <- sapply(test.sample, nrow)
  nj <- njtrain + njtest
  
  ns = njtest[1]
  ms = njtest[2]
  
  ground.label <- c(test.sample[[1]][,1],test.sample[[2]][,1])
  
  t1 = c(train.index[[1]], nj[1] + train.index[[2]])
  t2 = c(test.index[[1]], nj[1] + test.index[[2]])
  
  #---------------------- projection ensemble classifier -----------------------
  S.Train.sin = TFG.sin.mat[t1, t1] #(i,j)th element corresponds to rho-index between i-th and j-th training observations
  TFF.sin = sum(S.Train.sin[1:n, 1:n]) / (n * (n - 1))
  TFG.sin = sum(S.Train.sin[1:n, n + (1:m)]) / (n * m)
  TGG.sin = sum(S.Train.sin[n + (1:m), n + (1:m)]) / (m * (m - 1))
  
  en.sin = TFF.sin + TGG.sin - 2 * TFG.sin
  SFG.sin = TFF.sin - TGG.sin
  
  S.Test.sin = TFG.sin.mat[t1, t2]#(i,j)th element corresponds to rho-index between i-th training and j-th test observations
  TFZ.sin = colMeans(S.Test.sin[1:n, ])
  TGZ.sin = colMeans(S.Test.sin[n + (1:m), ])
  
  LFZ.sin = TFF.sin / 2 - TFZ.sin
  LGZ.sin = TGG.sin / 2 - TGZ.sin
  SZ.sin  = -TFG.sin - (LFZ.sin + LGZ.sin)
  
  lbl0.sin = lbl1.sin = lbl2.sin = rep(2, sum(njtest))
  lbl0.sin[LGZ.sin - LFZ.sin > 0] = 1 # classifier 0
  e0.sin = mean(classes[lbl0.sin] != ground.label)
  
  lbl1.sin[en.sin * sign(LGZ.sin - LFZ.sin) / 2 + SFG.sin * sign(SZ.sin) /
             2 > 0] = 1 # classifier 1
  e1.sin = mean(classes[lbl1.sin] != ground.label)
  
  lbl2.sin[en.sin * (LGZ.sin - LFZ.sin) / 2 + SFG.sin * SZ.sin > 0] = 1 #classifier 2
  e2.sin = mean(classes[lbl2.sin] != ground.label)
  
  #-------------------- projection ensemble classifier: component-wise----------
  S.Train.sin.comp = TFG.sincomp.mat[t1, t1]#(i,j)th element corresponds to rho-index between i-th and j-th training observations
  TFF.sin.comp = sum(S.Train.sin.comp[1:n, 1:n]) / (n * (n - 1))
  TFG.sin.comp = sum(S.Train.sin.comp[1:n, n + (1:m)]) / (n * m)
  TGG.sin.comp = sum(S.Train.sin.comp[n + (1:m), n + (1:m)]) / (m * (m -
                                                                       1))
  
  en.sin.comp = TFF.sin.comp + TGG.sin.comp - 2 * TFG.sin.comp
  SFG.Sin.Comp = TFF.sin.comp - TGG.sin.comp
  
  S.Test.sin.comp = TFG.sincomp.mat[t1, t2]#(i,j)th element corresponds to rho-index between i-th training and j-th test observations
  TFZ.sin.comp = colMeans(S.Test.sin.comp[1:n, ])
  TGZ.sin.comp = colMeans(S.Test.sin.comp[n + (1:m), ])
  
  LFZ.sin.comp = TFF.sin.comp / 2 - TFZ.sin.comp
  LGZ.sin.comp = TGG.sin.comp / 2 - TGZ.sin.comp
  SZ.sin.comp = -TFG.sin.comp - (LFZ.sin.comp + LGZ.sin.comp)
  
  lbl0.sin.comp = lbl1.sin.comp = lbl2.sin.comp = rep(2, sum(njtest))
  lbl0.sin.comp[LGZ.sin.comp - LFZ.sin.comp > 0] = 1
  e0.sin.comp = mean(classes[lbl0.sin.comp] != ground.label)
  
  lbl1.sin.comp[en.sin.comp * sign(LGZ.sin.comp - LFZ.sin.comp) / 2 + SFG.Sin.Comp * sign(SZ.sin.comp) /
                  2 > 0] = 1 # classifier 1
  e1.sin.comp = mean(classes[lbl1.sin.comp] != ground.label)
  
  lbl2.sin.comp[en.sin.comp * (LGZ.sin.comp - LFZ.sin.comp) / 2 + SFG.Sin.Comp * SZ.sin.comp >
                  0] = 1 #classifier 2
  e2.sin.comp = mean(classes[lbl2.sin.comp] != ground.label)
  #-----------------------------------------------------------------------------
  
  
  X = train.sample[[1]][, -1]
  Y = train.sample[[2]][, -1]
  
  Q <- do.call('rbind', train.sample)
  Z <- do.call('rbind', test.sample)
  
  lbl <- Q[, 1]
  Q <- Q[, -1]
  
  test.sample.lbl <- Z[, 1]
  Z <- Z[, -1]
  R1 = nrow(Z)
  
  if (u %% 5 == 0) {
    print(u)
  }
  
  #------------------------ projection average classifier ----------------------
  system.time({S.Train.cos = dissim.cos(M = as.matrix(Q), no.cores = no.cores)}) #(i,j)th element corresponds to rho-index between i-th and j-th training observations
  TFF.cos = sum(S.Train.cos[1:n, 1:n]) / (n * (n - 1))
  TFG.cos = sum(S.Train.cos[1:n, n + (1:m)]) / (n * m)
  TGG.cos = sum(S.Train.cos[n + (1:m), n + (1:m)]) / (m * (m - 1))
  
  en.cos = -(TFF.cos + TGG.cos) + 2 * TFG.cos
  SFG.cos = TFF.cos - TGG.cos
  
  
  system.time({S.Test.cos = dissim.cos.TEST(
    M = Q,
    MTEST = Z,
    n = n,
    m = m,
    no.cores = no.cores
  )}) # returns a matrix with TFZ values in first column and TGZ values in the second
  TFZ.cos = as.numeric(S.Test.cos[, 1])
  TGZ.cos = as.numeric(S.Test.cos[, 2])
  
  LFZ.cos = TFZ.cos - TFF.cos / 2
  LGZ.cos = TGZ.cos - TGG.cos / 2
  SZ.cos  = -TFG.cos + (LFZ.cos + LGZ.cos)
  
  lbl0.cos = lbl1.cos = lbl2.cos = rep(2, sum(njtest))
  lbl0.cos[LGZ.cos - LFZ.cos > 0] = 1 # classifier 0
  e0.cos = mean(classes[lbl0.cos] != ground.label)
  
  lbl1.cos[en.cos * sign(LGZ.cos - LFZ.cos) / 2 + SFG.cos * sign(SZ.cos) /
             2 > 0] = 1 # classifier 1
  e1.cos = mean(classes[lbl1.cos] != ground.label)
  
  lbl2.cos[en.cos * (LGZ.cos - LFZ.cos) / 2 + SFG.cos * SZ.cos > 0] = 1 #classifier 2
  e2.cos = mean(classes[lbl2.cos] != ground.label)
  
  #----------------- projection average classifier component-wise ---------------
  system.time({S.Train.cos.comp = dissim.cos.comp(M = Q,
                                     no.cores = no.cores,
                                     disFuncPtr.cos.comp = disFuncPtr.cos.comp)}) #(i,j)th element corresponds to rho-index between i-th and j-th training observations
  TFF.cos.comp = sum(S.Train.cos.comp[1:n, 1:n]) / (n * (n - 1))
  TFG.cos.comp = sum(S.Train.cos.comp[1:n, n + (1:m)]) / (n * m)
  TGG.cos.comp = sum(S.Train.cos.comp[n + (1:m), n + (1:m)]) / (m * (m -
                                                                       1))
  
  en.cos.comp = -(TFF.cos.comp + TGG.cos.comp) + 2 * TFG.cos.comp
  SFG.cos.comp = TFF.cos.comp - TGG.cos.comp
  
  
  system.time({S.Test.cos.comp = dissim.cos.comp.TEST(
    M = Q,
    MTEST = Z,
    n = n,
    m = m,
    no.cores = no.cores
  )}) # returns a matrix with TFZ values in first column and TGZ values in the second
  TFZ.cos.comp = as.numeric(S.Test.cos.comp[, 1])
  TGZ.cos.comp = as.numeric(S.Test.cos.comp[, 2])
  
  LFZ.cos.comp = TFZ.cos.comp - TFF.cos.comp / 2
  LGZ.cos.comp = TGZ.cos.comp - TGG.cos.comp / 2
  SZ.cos.comp  = -TFG.cos.comp + (LFZ.cos.comp + LGZ.cos.comp)
  
  lbl0.cos.comp = lbl1.cos.comp = lbl2.cos.comp = rep(2, sum(njtest))
  lbl0.cos.comp[LGZ.cos.comp - LFZ.cos.comp > 0] = 1 # classifier 0
  e0.cos.comp = mean(classes[lbl0.cos.comp] != ground.label)
  
  lbl1.cos.comp[en.cos.comp * sign(LGZ.cos.comp - LFZ.cos.comp) / 2 + SFG.cos.comp * sign(SZ.cos.comp) /
                  2 > 0] = 1 # classifier 1
  e1.cos.comp = mean(classes[lbl1.cos.comp] != ground.label)
  
  lbl2.cos.comp[en.cos.comp * (LGZ.cos.comp - LFZ.cos.comp) / 2 + SFG.cos.comp * SZ.cos.comp >
                  0] = 1 #classifier 2
  e2.cos.comp = mean(classes[lbl2.cos.comp] != ground.label)
  out1[[u]] = c(
    e0.cos,
    e0.cos.comp,
    e0.sin,
    e0.sin.comp,
    e1.cos,
    e1.cos.comp,
    e1.sin,
    e1.sin.comp,
    e2.cos,
    e2.cos.comp,
    e2.sin,
    e2.sin.comp
  )
})
  print(u)
}

out2 = do.call('rbind',out1)
colnames(out2) = c(
  'delta0.cos',
  'delta0.cos.comp',
  'delta0.sin',
  'delta0.sin.comp',
  'delta1.cos',
  'delta1.cos.comp',
  'delta1.sin',
  'delta1.sin.comp',
  'delta2.cos',
  'delta2.cos.comp',
  'delta2.sin',
  'delta2.sin.comp'
)

ERR = colMeans(out2)
SE = apply(out2, 2, sd) / sqrt(ITER)

exec.time <- proc.time() - start.time
print(exec.time)

ERR[1:4]
ERR[5:8]
ERR[9:12]
# write.csv(ERR, paste(nms[90],'_projavg.csv',sep=''),row.names=T)
O[[M]] = c(nms[41],ERR)

stopCluster(cl)
gc()

ALL = do.call('rbind',O)
write.csv(ALL,'UCRsmall_15ITER.csv', row.names = F)
