
miss <- c()

for (k in 1:N) {
   if (is.na(sum(df.table[k, c(3,7)])) == FALSE && 
       is.na(sum(df.table[k,c(12:13)])) == TRUE) {
      miss <- c(miss, df.table[k,1])
   }
}


path.ucr <- "D:/My Documents/Datasets/Datasets/Classification Datasets/UCRArchive_2018/"

data.miss.train <- data.miss.test <- list()

for(k in 1:length(miss)){
   
   data.miss.train[[k]] <- paste0(path.ucr,"/",miss[k],"/",miss[k],"_",
                                  "TRAIN.tsv") %>% read.csv(header = F, sep = "\t")
   data.miss.test[[k]] <- paste0(path.ucr,"/",miss[k],"/",miss[k],"_",
                                 "TEST.tsv") %>% read.csv(header = F, sep = "\t")
}

source('~/R/R Codes/Classification of HDLSS Data (Summer, 2021)/HDLSS-Summer-2021/dataset-partitioning-JRC.R')


















