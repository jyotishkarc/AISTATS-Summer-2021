
library(dplyr)
library(stringr)
library(readxl)

# path <- "E:/Jyotishka/Data/UCR-Results/UCR/"
path <- "C:/Users/JYOTISHKA/Dropbox/Robust_Energy_based_Classifier/Gof_Project/Results/Real/UCR/"

files <- list.files(path = path)
files.df <- as.data.frame(files, "filenames" = files)

# all.UCR <- read_excel("E:/Jyotishka/UCR-datasets.xlsx", col_names = FALSE)
all.UCR <- read_excel("~/UCR-datasets.xlsx", col_names = FALSE)

colnames(all.UCR) <- c('datasets','length')
N <- nrow(all.UCR)

M.UCR.1 <- read.csv("D:/My Documents/Real Data Analysis_ Three Databases - UCR.csv")
# M.orig <- read.csv("E:/Jyotishka/Real Data Analysis_ Three Databases - UCR.csv")


# df.table <- as.data.frame(matrix(NA, nrow = N, ncol = 15))
df.table <- M.UCR.1


colnames(df.table) <- H <- c("Dataset", "Length", 
                             "delta0.sin","delta0.sin.comp",
                             "delta2.sin", "delta2.sin.comp", 
                             "GLMNET", "RF", "RP","SVMlin", "SVMRBF",
                             "Nnet", "1NN", "SAVG")

# mV.which <- which(str_detect(files, "majorityVoting"))
# proj.which <- which(str_detect(files, "projavg"))

Z <- list()

for (k in 1:N) {
   print(k)
   
   Z[[k]] <- files.df %>% filter(str_detect(files, all.UCR$datasets[k]))
   
   if (is.na(as.numeric(M.UCR.1[k,3])) == FALSE && 
       is.na(as.numeric(M.UCR.1[k,7])) == TRUE) {
   
      popular <- Z[[k]] %>% filter(str_detect(files, "popularclassifiers")) 
      
      if (prod(dim(popular)) != 0) {
         temp.popular <- read.csv(paste(path, as.character(popular), sep = ""))
         temp.popular <- temp.popular[,-1]
         popular.mean <- colMeans(temp.popular)
         
         # RF.pos <- which.min(popular.mean[5:8]) + 4
         popular.mean <- c(popular.mean[1:4], min(popular.mean[5:8]))
         
         #popular.se <- apply(temp.popular, 2, function(val) sciplot::se(val))
         #popular.se <- c(popular.se[1:4], popular.se[RF.pos])
         
         popular.mean <- c(popular.mean[1], popular.mean[5], popular.mean[2],
                           popular.mean[3], M.UCR.1$SVMRBF[k])
         # popular.se <- c(popular.se[1], popular.se[5], popular.se[2],
         #                 popular.se[3], popular.se[4], NA, NA)
      }
      else {
         popular.mean <- c(rep(NA,4), M.UCR.1$SVMRBF[k])
      }
      
      df.table[k, c(7:11)] <- popular.mean
   }
}


for(j in 2:14){
   df.table[,j] <- round(as.numeric(df.table[,j]), 5)
}

View(df.table)


pref.mat <- matrix(0, N, 10)
B <- H[c(3:4,6:7,9:14)]

for (i in 1:N) {
   if(is.na(sum(df.table[i,c(3:4,6:7,9:14)])) == FALSE){
      R <- df.table[i,c(3:4,6:7,9:14)] %>%
         as.numeric() %>%
         rank(ties.method = "first")

      v <- c()
      for(j in 1:10){
         v[j] <- B[which(R == j)]
      }

      pref.mat[i,] <- v
   }
   else pref.mat[i,] <- rep(NA,10)
}

pref.mat <- cbind(all.UCR$datasets, pref.mat)
pref.mat.no.NA <- pref.mat %>% na.omit() %>% as.data.frame()

View(pref.mat.no.NA)

