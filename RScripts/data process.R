#### Script to clean text data and process into n-gram for word prediction

#### Set up ####
## Clear Environment Data
rm(list=ls())
## Turn-off scientific notation
options(scipen=999)  

library(tidyverse);library(lubridate);library(lexicon);library(tidytext)


#### Directories

myScriptz <- dirname(rstudioapi::getSourceEditorContext()$path)
    myDir <- sub("RScripts", "", myScriptz)
        myDat <- paste0(myDir,"Data/")


#### Data Import ####
        
## Data previously downloaded and decompressed into Data folder
## Download link: "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        
## DF with names
dffiles <- data.frame(place = list.files(myDat, pattern = "\\.txt", recursive = TRUE)) %>% 
  mutate(name = gsub("\\.", "_", gsub(".*\\/|\\.txt", "", place))) %>%
  filter(grepl("en_US", name))

#### Import Loop
dftext <- data.frame()

for (f in 1:nrow(dffiles)) {
  Dz <- read_lines(paste0(myDat, dffiles$place[f]))
  Dz <- data.frame(text = Dz, type = gsub("en_US_", "", dffiles$name[f])) 
  
  dftext <- rbind(dftext, Dz)
  rm(Dz)
}

#### Data clean ####
## Swear Words
data(profanity_alvarez)
vprofanity <- unique(c(profanity_alvarez)); vprofanity <- vprofanity[!grepl("[[:punct:]]", vprofanity)]
 

#### Get Sample
## Words per type
dfwpl <- dftext %>% mutate(text = str_squish(gsub("_", " ", text))) %>%
  group_by(type) %>% summarise(ntot = sum(str_count(text, '\\w+'))) %>%
  ungroup() %>% mutate(nprp = (sum(ntot)-ntot)/sum(ntot))

vweight <- dfwpl$nprp

## Sample 300K
dftext <- dftext %>% mutate(wt = case_when(type == 'blogs' ~ vweight[1], 
                                           type == 'news' ~ vweight[2], 
                                           type == 'twitter' ~ vweight[3])) %>%
  slice_sample(n = 300000, weight_by = wt)

## Clean text
dfclean <- dftext %>% mutate(text = tolower(gsub("[0-9]+|_", " ", text)),
                             text = str_squish(gsub("[^[:alnum:][:space:]']", "", text)),
                             iprof_check = grepl(paste(vprofanity, collapse = "|"), text) ) %>%
  filter(!iprof_check) %>% select(-iprof_check)


#### N-Gram ####
## n = 1
dfn1 <- dfclean %>% unnest_tokens(ngram, text) %>% 
  group_by(ngram) %>% summarise(ncnt = n()) %>% ungroup () %>% arrange(desc(ncnt)) %>% 
  mutate(ntot = cumsum(ncnt)/sum(ncnt)) %>% filter(ntot <= 0.95) %>% select(-ntot)

## n = 2
dfn2 <- dfclean %>% unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  group_by(ngram) %>% summarise(ncnt = n()) %>% ungroup() %>% filter(ncnt > 1) %>% arrange(desc(ncnt)) 

dfsplit <- data.frame(str_split_fixed(dfn2$ngram, " ", n = 2)) %>%
  mutate(check = (X1 %in% dfn1$ngram * X2 %in% dfn1$ngram) > 0)

dfn2 <- dfn2[dfsplit$check,]

## n = 3
dfn3 <- dfclean %>% unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  group_by(ngram) %>% summarise(ncnt = n()) %>% ungroup() %>% filter(ncnt > 1)  %>% arrange(desc(ncnt)) 

dfsplit <- data.frame(str_split_fixed(dfn3$ngram, " ", n = 3)) %>%
  mutate(check = (X1 %in% dfn1$ngram * X2 %in% dfn1$ngram * X3 %in% dfn1$ngram) > 0)

dfn3 <- dfn3[dfsplit$check,]


#### Save data

saveRDS(dfclean %>% select(-wt), paste0(myDat, "English_clean250k.rds"))

saveRDS(list(n1 = dfn1, n2 = dfn2, n3 = dfn3), paste0(myDat, "English_Ngram.rds"))








