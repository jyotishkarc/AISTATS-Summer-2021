
library(dplyr)
library(stringr)
library(readxl)

path <- "D:/My Documents/UCR/"
files <- list.files(path = path)
files.df <- as.data.frame(files, "filenames" = files)
# filenames <- as.character(files)

#all.UCR <- UCR_datasets <- read_excel("~/UCR-datasets.xlsx", col_names = FALSE)

colnames(all.UCR) <- c('datasets','length')
N <- nrow(all.UCR)

# all.UCR$datasets[52] <- "Gunpoint1"
# all.UCR$datasets[52] <- "Gunpoint1"

df.table <- as.data.frame(matrix(NA, nrow = 2*N, ncol = 14))
colnames(df.table) <- c("Dataset", "Length", 
                        "delta0.sin","delta0.sin.comp",
                        "delta2.sin", "delta2.sin.comp", 
                        "GLMNET", "RF", "RP",
                        "SVMlin", "SVMRBF", "Nnet", "1NN", "SAVG")

for (k in 1:N) {
   print(k)
   
   Z[[k]] <- files.df %>% filter(str_detect(filenames, all.UCR$datasets[k]))
   
   if (prod(dim(Z[[k]])) != 0) {
      mV <- Z[[k]] %>% filter(str_detect(files, "majorityVoting"))
      SAVG <- Z[[k]] %>% filter(str_detect(files, "SAVG"))
      # popular <- Z[[k]] %>% filter(str_detect(files, "popular"))
      # SVMRBF <- Z[[k]] %>% filter(str_detect(files, "SVMRBF"))
      
      if (prod(dim(mV)) != 0) {
         temp.mV <- read.csv(paste(path, as.character(mV), sep = ""))
         delta.mean <- colMeans(temp.mV)
         delta.se <- apply(temp.mV, 2, function(val) sciplot::se(val))
      }
      else {delta.mean <- delta.se <- rep(NA, 4)}
      
      popular.mean <- popular.se <- rep(NA, 7)
      
      if (prod(dim(SAVG)) != 0) {
         temp.SAVG <- read.csv(paste(path, as.character(SAVG), sep = ""))
         savg.mean <- colMeans(temp.SAVG)
         savg.se <- apply(temp.SAVG, 2, function(val) sciplot::se(val))
      }
      else {savg.value <- savg.se <- NA}
      
      df.table[(2*k - 1), ] <- c(all.UCR$datasets[k], all.UCR$length[k],
                                 delta.mean, popular.mean, savg.mean)
      df.table[(2*k), ] <- c(all.UCR$datasets[k], all.UCR$length[k],
                             delta.se, popular.se, savg.se)
   }
   else {df.table[(2*k - 1), ] <- c(all.UCR$datasets[k], all.UCR$length[k],
                                    rep(NA, 12))
         df.table[(2*k), ] <- c(all.UCR$datasets[k], all.UCR$length[k],
                                rep(NA, 12))
   }

}








