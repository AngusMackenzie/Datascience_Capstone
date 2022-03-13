##### Shiny App to predict the next word in phrase

#### Set up ####
## Clear Environment Data
rm(list=ls())
## turns off scientific notation
options(scipen=999) 

library(tidyverse);library(shiny);library(shinyWidgets);library(shinythemes)
library(gridtext);library(grid);library(treemapify)


## Directories
myApp <- getwd()

## Functions
'%nin%' <- Negate('%in%')


#### Colors
## Standard
col_bg <- "#222d32";col_fg <- "#3C8DBC";col_ct <- '#024788'
col_ml <- '#E66100';col_fc <- '#5D3A9B';col_ln <- "#748696"

## Words
vcol_wrd <- c('#5A8F5A', '#877C97', '#B4895F', '#B36E6D', '#A21214', '#884110', 
              '#284D7D', '#AB015A', '#B65A00', '#137055', '#B6B66D', '#53507F',	
              '#487615', '#A47A01', '#765414', '#7693A2', '#7F9F62', '#B4884F')


## Load Data
ldata <- readRDS(paste0(myApp, "/app_dfs.rds"))
dfsplit3 <- ldata$dfn3; dfsplit2 <- ldata$dfn2; dfn1 <- ldata$dfn1


## Information
vtitle <- paste0("<span style = 'color:white;font-size:22pt'><b>", "Word Predictor", "</b></span><br>")
vinfo <- paste0("<span style = 'color:white;font-size:15pt'>", 
                "Select the maximum predicted words you want to see. The probability of these are shown in graph. ", "</span>")
vlink <- paste0("<span style = 'color:black;font-size:10pt'><i>", "Probability found using the Katz backoff model. Click here for the </span>")


#### UI ####

ui <- fluidPage(theme = shinytheme("flatly"),
                title = "Word Predict",
                  div(HTML(vtitle), img(src = 'nlp_icon.png', height = "60px"),
                      style="display: flex; justify-content: space-between; width: 100%; height: 64px; background-color:#748696;
                      padding-top: 2px; padding-right: 20px; padding-bottom: 2px; padding-left: 20px;align-items: center; margin: auto; 
                      border: 2px solid; border-radius: 5px; border-color:white;"),
                  sidebarPanel(
                    div(style="display: flex; align-items: center; justify-content: space-between;padding: 20px",
                        div(HTML(vinfo),
                        style="flex-grow: 3; display: inline-block; width: 45%; height: 100%; background-color:#222d32;
                        padding: 10px; text-align: left; margin: auto; letter-spacing: 0.98pt;
                        border: 2px solid; border-radius: 2px; border-color:white;"),
                        knobInput(inputId = "find_value", label = "Select number of words:",
                                  value = 4, min = 2, max = 16, displayPrevious = TRUE, step = 1,
                                  lineCap = "round", fgColor = col_fg, inputColor = col_ml
                        )),
                    HTML(vlink), tags$a(href = "https://github.com/AngusMackenzie/Datascience_Capstone", "Git repository"),
                    width = 4),
                  mainPanel(
                    div(style="flex-grow: 3; display: inline-block; height: 60px;",
                        textInputAddon(inputId = "text_input", label = NULL, placeholder = "Input text here.", 
                                       addon = icon("keyboard"), width = '100%')),
                    plotOutput(outputId = "treemap", height = 600),
                    width = 8)
)

#### Server ####
server <- function(session,input, output) {
  
  dfout <- reactive({
    
    if (input$text_input != "") {
      
      ## Clean input
      vclean <- tolower(str_squish(gsub("[^[:alnum:][:space:]']|[0-9]+|_", " ", input$text_input)))
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
      
      ## Filter top amount selected
      dfpredict <- dfpredict %>% top_n(input$find_value, nprb)
      
      if (nrow(dfpredict) > input$find_value) { dfpredict <- dfpredict[1:input$find_value, ] }
      
      return(dfpredict)
    }
    
  })
  
  output$treemap <- renderPlot({
    
    if (is.null(dfout())) {
      
      grid.newpage()
      grid.draw(richtext_grob("<span style = 'color:white;font-size:28pt'><b>Input text for prediction</b></span>", x = 0.5, y = 1, 
                              hjust = 0.5, vjust = 1, valign = 0.5, margin = unit(c(6, 10, 4, 10), "pt"), padding = unit(c(6, 10, 4, 10), "pt"),
                              r = unit(2, "pt"), gp = gpar(), box_gp = gpar(col = "white", fill = col_ln)))
      
    } else {
      if(dfout()[1,1] == "Prediction failed") {
        vftext <- paste0("<span style = 'color:white;font-size:18pt'><b>Prediction failed</b><br><br>",
                         "Check for misspelt words and remove profanities, however the system is limited.<br>",
                         "For more information follow the link on the left to the pitch presentation.")
        grid.newpage()
        grid.draw(richtext_grob(vftext, x = 0.5, y = 1, hjust = 0.5, vjust = 1, halign = 0, valign = 0.5, 
                                margin = unit(c(6, 10, 4, 10), "pt"), padding = unit(c(6, 10, 4, 10), "pt"),
                                r = unit(2, "pt"), gp = gpar(), box_gp = gpar(col = "white", fill = col_ln)))
        
      } else {
        ## Data
        dfgtree <- dfout() %>% 
          mutate(lab_t = paste0("'", predict, "'\n", round(nprb*100,1), "%"))
        
        ## Colors
        vcol_g <- vcol_wrd[1:nrow(dfgtree)] ; names(vcol_g) <- dfgtree$predict
        
        ## Tree Map
        dfgtree %>%
          ggplot(aes(area = nprb, fill = predict, subgroup = lab_t)) + 
          geom_treemap(color = NA, start = 'topleft', show.legend = FALSE) + 
          geom_treemap_subgroup_text(fontface = "bold", colour = "white", place = "centre", 
                                     grow = TRUE, padding.x = unit(2.5, "mm"), start = 'topleft') +
          geom_treemap_subgroup_border(color = "grey95", size = 1.5, start = "topleft") + 
          scale_fill_manual(values = vcol_g) + guides(fill = "none") + theme(
            panel.background= element_rect(fill = 'transparent', color = "grey60"),
            plot.background = element_rect(fill = 'transparent', color = "grey60"),
            plot.margin = margin(2,2,2,2) )
      }
    }
  })

}

#### App
shinyApp(ui = ui, server = server)

