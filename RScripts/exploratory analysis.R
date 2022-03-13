#### Exploratory Data Analysis for Capstone

#### Set up ####
## Clear Environment Data
rm(list=ls())
## Turn-off scientific notation
options(scipen=999)  

library(tidyverse);library(lubridate);library(tidytext)
library(colorspace);library(ggrepel)


#### Directories

myScriptz <- dirname(rstudioapi::getSourceEditorContext()$path)
    myDir <- sub("RScripts", "", myScriptz)
        myDat <- paste0(myDir,"Data/")
        
#### Colours
        
col_bg <- "#222d32";col_fg <- "#3C8DBC";col_ct <- '#024788'
col_ml <- '#E66100';col_fc <- '#5D3A9B';col_ln <- "#748696"

## Text type
vtxt <- c("blogs", "news", "twitter")

vcol_typ <- c('#5A8F5A', '#877C97', '#B36E6D')
vcol_typd <- darken(vcol_typ, 0.5); vcol_typl <- lighten(vcol_typ, 0.5, method = 'relative')
    names(vcol_typ) <- vtxt; names(vcol_typd) <- vtxt; names(vcol_typl) <- vtxt

#### Functions

spaceK <- function(x) {
  case_when(x >= 10^9 ~ paste0(format(round(x/10^9, 1), big.mark = " ", scientific = FALSE, trim = TRUE), "B"),
            x < 10^9 & x >= 10^6 ~ paste0(format(round(x/10^6, 1), big.mark = " ", scientific = FALSE, trim = TRUE), "M"),
            x < 10^6 & x >= 10^3 ~ paste0(format(round(x/10^3,1), big.mark = " ", scientific = FALSE, trim = TRUE), "K"),
            TRUE ~ as.character(round(x,1)))
}
            
#### Data Import

dftext <- readRDS(paste0(myDat, "English_text.rds"))

#### Word Frequency ####

dffreq <- dftext %>% group_by(type) %>% unnest_tokens(word, text) %>% 
  group_by(type, word) %>% summarise(ncnt = n()) %>% 
  group_by(type) %>% arrange(desc(ncnt)) %>% mutate(ntot = cumsum(ncnt)/sum(ncnt))

## Reduced
df50 <- dffreq[dffreq$ntot <= 0.5,]
df90 <- dffreq[dffreq$ntot <= 0.9,]


#### In how many sentences
## Parallel Setup

library(foreach)
library(doSNOW)

ncores <- parallel::detectCores()
cl <- makeSOCKcluster(ncores - 2)
niter <- nrow(df50)
registerDoSNOW(cl)

## Progress Bar
pb <- txtProgressBar(min=1, max=niter, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

## Loop
tind <- Sys.time()

ltimes <- foreach(i = 1:niter, .options.snow=opts) %dopar% {
  sum(grepl(paste0("\\b", df50$word[i], "\\b"), 
            dftext$text[dftext$type == df50$type[i]], ignore.case = TRUE), 
      na.rm = TRUE)
}

stopCluster(cl)
print(difftime(Sys.time(),tind)) 

## Graph
df50$ntms <- unlist(ltimes)

df50 %>% ggplot(aes(x = ncnt, y = ntms, color = type, fill = type)) +
  geom_point() + geom_smooth(formula = "y ~ x", method = "loess") + 
  scale_color_manual(values = vcol_typ) + scale_fill_manual(values = vcol_typl) + 
  scale_x_continuous(labels = spaceK) + scale_y_continuous(labels = spaceK) +
  labs(x = "Times word is repeated", y = "Lines containing word") +
  theme_bw() + theme(
    legend.title = element_blank(), legend.position = c(0.05, 0.9), 
    legend.background = element_rect(fill = "white"))


#### n-gram ####

#### n = 2

dfbigram <- dftext %>% group_by(type) %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  group_by(type, bigram) %>% summarise(ncnt = n()) 

dfsplit <- data.frame(str_split_fixed(dfbigram$bigram, " ", n = 2))
    names(dfsplit) <- c("word1", "word2")

dfbigram <- dfbigram %>% bind_cols(dfsplit) %>%
  filter(word1 %in% df90$word | word2 %in% df90$word)

rm(dfsplit)

#### n = 3

dftrigram <- dftext %>% group_by(type) %>% unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  group_by(type, trigram) %>% summarise(ncnt = n()) 

dfsplit <- data.frame(str_split_fixed(dftrigram$trigram, " ", n = 3))
    names(dfsplit) <- c("word1", "word2", "word3")

dftrigram <- dftrigram %>% bind_cols(dfsplit) %>%
  filter(word1 %in% df90$word | word2 %in% df90$word | word3 %in% df90$word)

rm(dfsplit)


#### Save Data ####

saveRDS(list(text_sample = dftext, word_freq = dffreq, word_50 = df50, bigram = dfbigram, trigam = dftrigram),
        paste0(myDat, "Processed_English.rds"))


