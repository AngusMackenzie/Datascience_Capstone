#### N-gram model creation for Capstone Project

#### Set up ####
## Clear Environment Data
rm(list=ls())
## Turn-off scientific notation
options(scipen=999)  

library(tidyverse);library(lubridate);library(tidytext)


#### Directories

myScriptz <- dirname(rstudioapi::getSourceEditorContext()$path)
    myDir <- sub("RScripts", "", myScriptz)
        myDat <- paste0(myDir,"Data/")
        myApp <- paste0(myDir,"App/")
        

#### Helpful functions
'%nin%' <- Negate('%in%')


#### Data Process ####
        
## Import cleaned text
lngram <- readRDS(paste0(myDat, "English_Ngram.rds"))


#### Make Tables
## n = 3
dfn3 <- lngram$n3 %>% group_by(ngram) %>% summarise(ncnt = sum(ncnt)) %>% filter(ncnt > 1) %>%
  ungroup() %>% arrange(desc(ncnt)) %>% mutate(nprp = cumsum(ncnt)/sum(ncnt)) %>% filter(nprp < .65)

dfsplit3 <- data.frame(str_split_fixed(dfn3$ngram, " ", n = 3))
    names(dfsplit3) <- c("word1", "word2", "predict")
    dfsplit3$ncnt <- dfn3$ncnt
    
## n = 2
dfn2 <- lngram$n2 %>% group_by(ngram) %>% summarise(ncnt = sum(ncnt)) %>% filter(ncnt > 1) %>%
  ungroup() %>% arrange(desc(ncnt)) %>% mutate(nprp = cumsum(ncnt)/sum(ncnt)) %>% filter(nprp < .80) 
    
dfsplit2 <- data.frame(str_split_fixed(dfn2$ngram, " ", n = 2))
    names(dfsplit2) <- c("word1", "predict")
    dfsplit2$ncnt <- dfn2$ncnt
    
## n = 1
dfn1 <- lngram$n1


#### Info DF

dfinfo <- data.frame(ngram = 1:3, nmin_rep = c(min(dfn1$ncnt), min(dfsplit2$ncnt), min(dfsplit3$ncnt)),
                     nsize = c(object.size(dfn1), object.size(dfsplit2), object.size(dfsplit3)) )

    
#### Model Function ####

x <- "today"
x <- "some of"
x <- "po"

next_word <- function(x, n = 10) {
  ## Clean input
  vclean <- tolower(str_squish(gsub("[^[:alnum:][:space:]']|[0-9]+|_", " ", x)))
  vclean <- unlist(str_split(vclean, " "))
  
  ntxt_len <- length(vclean);dfpredict <- data.frame()
  
  if (ntxt_len >= 2) {
    ## Get all combinations
    dfpredict <- dfsplit3 %>% filter(word1 == vclean[ntxt_len-1] & word2 == vclean[ntxt_len])
    if (nrow(dfpredict) > 0) {
      ## Find count in n = 2
      n2_count <- dfsplit2 %>% filter(word1 == vclean[ntxt_len-1] & predict == vclean[ntxt_len]) %>% pull(ncnt)
      ## Insert for probability
      dfpredict <- dfpredict %>% mutate(nprb = (ncnt-0.5)/n2_count)
      ## Other predicted words
      vother_word <- dfn1$ngram[dfn1$ngram %nin% dfpredict$predict]
      ## n = 2 alpha
      n1_count <- dfn1$ncnt[dfn1$ngram == vclean[ntxt_len]]
      n2_alpha <- dfsplit2 %>% filter(predict == vclean[ntxt_len]) %>% 
        summarise(nalpha = 1 - sum(ncnt-0.5)/n1_count) %>% pull(nalpha)
      ## Back off probability
      dfback_off <- dfsplit2 %>% filter(word1 == vclean[ntxt_len] & predict %in% vother_word) %>%
        mutate(nprb = ncnt/n1_count)
      ## Missing BO prob
      dfmiss_bo <- data.frame(predict = vother_word[vother_word %nin% dfback_off$predict]) %>%
        mutate(word1 = vclean[ntxt_len]) %>% select(word1, predict) %>% left_join(dfn1, by = c('predict' = 'ngram')) %>%
        mutate(nprb = n2_alpha*ncnt/sum(ncnt))
      ## n = 3 alpha
      n3_alpha <- 1 - sum((dfpredict$ncnt - 0.5)/n2_count)
      ## Find highest BO prob
      dfall_bo <- dfback_off %>% bind_rows(dfmiss_bo) %>% rename(word2 = word1) %>% 
        mutate(nprb = n3_alpha*nprb/sum(nprb), word1 = vclean[ntxt_len-1]) 
      
      #### Return DF
      dfpredict <- bind_rows(dfpredict, dfall_bo) %>% arrange(desc(nprb))
    }
  }
  
  if (ntxt_len == 1 | nrow(dfpredict) == 0) {
    ## Get combinations
    dfpredict <- dfsplit2 %>% filter(word1 == vclean[ntxt_len])
    if (nrow(dfpredict) > 0) {
      ## n = 1 alpha
      n1_count <- dfn1$ncnt[dfn1$ngram == vclean[ntxt_len]]
      n1_alpha <- 1 - sum(dfpredict$ncnt-0.5)/n1_count
      ## Missing BO prob
      vother_word <- dfn1$ngram[dfn1$ngram %nin% dfpredict$predict]
      dfmiss_bo <- dfn1 %>% filter(ngram %in% vother_word) %>% rename(predict = ngram) %>%
        mutate(word1 = vclean[ntxt_len], nprb = n1_alpha*ncnt/sum(ncnt))
      
      #### Return with probability
      dfpredict <- dfpredict %>% mutate(nprb = n1_alpha*(ncnt-0.5)/n1_count) %>% 
        bind_rows(dfmiss_bo) %>% arrange(desc(nprb))
    }
  }
  
  if (nrow(dfpredict) == 0) {
    dfpredict <- data.frame(predict = "Prediction failed", nprb = 1)
  }
  
  return(dfpredict %>% top_n(n, nprb))
}


#### Test ####

vphrase <- c("gloop bloop", "this is great, it is going to", "thankfully to have", "hopefully we find", "find")

for (p in vphrase) {
  itime_s <- Sys.time()
  cat(paste(p, "\n"))
  print(next_word(p, 5))
  cat(paste("\n", difftime(Sys.time(), itime_s), " sec\n\n"))
}


#### Save for App

saveRDS(list(dfn1 = dfn1, dfn2 = dfsplit2, dfn3 = dfsplit3), paste0(myApp, "app_dfs.rds"))
   
write.csv(dfinfo, paste0(myDat, "App_data_info.csv"), row.names = FALSE)      
