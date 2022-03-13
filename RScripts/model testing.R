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
lapp_data <- readRDS(paste0(myApp, "app_dfs.rds"))
dftest_data <- readRDS(paste0(myDat, "English_clean250k.rds"))

## Get ngram frames
dfsplit3 <- lapp_data$dfn3; dfsplit2 <- lapp_data$dfn2; dfn1 <- lapp_data$dfn1

## Get test sentences

dfsample <- dftest_data[sample(1:nrow(dftest_data), 40), ]
dfsample <- dfsample %>% mutate(nwrd = sapply(text, function(x) sum(str_count(x, '\\w+')))) %>%
  filter(nwrd > 10)

dfsample <- data.frame(str_split_fixed(dfsample$text, " ", n = 10)) %>%
  mutate(phrase = paste(X5, X6, X7, X8), result = X9) %>% select(phrase, result)

dfsamp <- dfsample[1:10,]
    
#### Model Function ####

next_word <- function(x) {
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
  
  return(dfpredict)
}


#### Test ####

dftest_result <- data.frame()

for (p in 1:nrow(dfsamp)) {
  iphrase <- dfsamp$phrase[p]
  iresult <- dfsamp$result[p]
  
  itime_s <- Sys.time()
  
  dfltest <-  next_word(iphrase)
  
  nltime <-  difftime(Sys.time(), itime_s)
  
  if (dfltest$predict[1] == "Prediction failed") {
    dflresult <- data.frame(isucc = FALSE, iphrase = iphrase, iresult = iresult, npos = NA, 
                            nprob = NA, ntime = nltime)
  } else {
    dflresult <- data.frame(isucc = TRUE, iphrase = iphrase, iresult = iresult, npos = which(dfltest$predict == iresult), 
                            nprob = dfltest$nprb[which(dfltest$predict == iresult)], ntime = nltime)
  }
  
  dftest_result <- rbind(dftest_result, dflresult)

}


#### Save for App
   
write.csv(dftest_result, paste0(myDat, "Model_test_result.csv"), row.names = FALSE)      
