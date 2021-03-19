
library(shiny)
library(shinyWidgets)
library(qdap)
library(quanteda)
library(data.table)
library(proftools)
library(stringr)
library(mgsub)
library(tm)
library(rmarkdown)


#Is it better to use the gt discount that is more representative of the half dataset or the sample dataset?
#need to do something about repeat grams 
    
ui <- fluidPage(
    
    setBackgroundColor(
        color = c("white","green"),
        gradient = "linear",
        direction = "bottom",
        shinydashboard = FALSE
    ),

    titlePanel(strong("TxT.lt: A Next Word Prediction App.")),
    tags$hr(),
    
    sidebarLayout(
            sidebarPanel(
               
                    textInput(inputId = "human.text", label = h4(strong("Input text to see next word predictions:"))),
                    tags$hr(),
                    sliderInput("k.slider", label = h5(strong("How many word predictions would you like to see?")), min = 1, max = 25, value = 5),
                    br(),
                    h5(strong("How would you like your predictions?")),
                    checkboxInput("stop", label = "Stopwords Filtered", value = FALSE),
                    checkboxInput("profanity", label = "Profanity filtered", value = FALSE),
                    #checkboxInput("both", label = "Both Stopwords and Profanity Filtered", value = FALSE) 
                    
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Prediction App",
                        h4(strong("Most Likely Next Word")),
                        verbatimTextOutput("value"),
                        tags$hr(),
                        
                        h4(strong("Table of Word Predictions")),
                        div(dataTableOutput("pred.table"), style='font-size:150%'),
                    ),
                    tabPanel("Documentation",
                        
                        includeMarkdown("Prediction.App.Documentation.Rmd")
                       
                            )
                   
                )  
            ),
         ),
    
    
    tags$hr(),
    h5(strong("The processed text (what the algorithm sees):")),
    verbatimTextOutput("value2")
    
    
)


server <- function(input, output) {
   
        process.fun <- function(text.input, stop.bad=0){

                text.input <- str_replace_all(text.input, "´", "'")
                text.input <- str_replace_all(text.input, "-", "'")

                text.input <- replace_contraction(text.input, contraction = qdapDictionaries::contractions)
                
                text.input <- tolower(text.input)
                text.input <- str_replace_all(text.input, "\\S*@+\\S*", " ")
                text.input <- str_replace_all(text.input, "\\S*#+\\S*", " ")
                text.input <- str_replace_all(text.input, "[^[a-zA-Z][:space:]]", "")
                
                text.input <- tokens(text.input)
                digit.left <- c("pm", "nd", "rd", "th")
                text.input <- tokens_select(text.input, digit.left, selection = "remove")
                abbrev <- c("aka", "apt", "appt", "approx", "asap", "afk", "bnb", "bff", "byob", "btw", "cm", "diy", "dnd", "dr", "eta", "eg", "faq", "idk", "kg", "misc", "ms", "mrs", "mr", "na", "lb", "sr", "ttyl", "tgif", "vs", "vip")
                halfword <- c("also known as","apartment","appointment","approximately","as soon as possible", "away from keyboard","bed and breakfast","best friend forever","bring your own bottle","by the way","centimeter","do it yourself","do not disturb","doctor","estimated time of arrival","for example","frequently asked questions","i do not know", "kilogram","miscellaneous","miss", "misses","mister", "not applicable", "pound","senior","talk to you later","thank god its friday","versus","very important person")
                text.input <- tokens_replace(text.input, abbrev, halfword)
                
                remove.word <- read.table("badwords.txt", nrows=-1, sep="\n")
                remove.word <- as.vector(remove.word[1:447,])
                remove.word <- tolower(remove.word)
                
                if(stop.bad == 1 ){
                        text.input <- tokens_select(text.input, pattern = stopwords("en"), selection = "remove")
                }
                if(stop.bad == 2){
                        text.input <- tokens_select(text.input, remove.word, selection = "remove")
                }
                if(stop.bad == 3){
                        text.input <- tokens_select(text.input, pattern = stopwords("en"), selection = "remove")
                        text.input <- tokens_select(text.input, remove.word, selection = "remove")
                }
                text.input <- tokens_split(text.input)
                text.input <- gsub(" ", " ", text.input)
                return(text.input)
        }
        
        process.which <- function(text.input){
            
            
                if(input$stop & input$profanity) {
                    text.input <- process.fun(text.input, stop.bad=3)
                } else if(input$stop){
                    text.input <- process.fun(text.input, stop.bad=1)
                } else if (input$profanity){
                    text.input <- process.fun(text.input, stop.bad=2)
                } else {
                    text.input <- process.fun(text.input)
                }
            
                return(text.input)
        }

        uni.back.fun <- function(prev.words,  k){
                
                if(input$stop) {
                        uni.words <- fread("uni.words.60.pkn.1.csv")
                } else if(input$profanity){
                        uni.words <- fread("uni.words.60.pkn.2.csv")
                } else if (input$profanity & input$stop){
                        uni.words <- fread("uni.words.60.pkn.3.csv")
                } else {
                        uni.words <- fread("uni.words.60.pkn.csv")
                }
                
                uni <- uni.words[!(last %in% prev.words)]
                new.k <- min(k, nrow(uni))
                pred.words <- uni$last[1:new.k] 
                pred.probs <- uni$p.kn[1:new.k] 
                
                preds <- data.table(return.words = pred.words, return.probs = pred.probs )
                
                remove(uni.words)
                
                return(preds)
        }
 
        bi.back.fun <- function(bi, prev.words,  k){
                
                if(input$stop) {
                        bi.words <- fread("bi.words.60.pkn.1.csv")
                } else if(input$profanity){
                        bi.words <- fread("bi.words.60.pkn.2.csv")
                } else if (input$profanity & input$stop){
                        bi.words <- fread("bi.words.60.pkn.3.csv")
                } else {
                        bi.words <- fread("bi.words.60.pkn.csv")
                }
                
                word3 <- bi[1]
                
                bi.exist <- any(!(is.na( bi.words[first==word3][!(last %in% prev.words)])))
                
                if (bi.exist){
                        
                        bi <- bi.words[first==word3][!(last %in% prev.words)]
                        new.k <- min(k, nrow(bi))
                        pred.words <- bi$last[1:new.k]
                        pred.probs <- bi$p.kn[1:new.k]
                        
                        # If backing off
                        uni.back <- uni.back.fun(c(prev.words, pred.words), k)
                        pred.words <- c(pred.words, uni.back$return.words)
                        pred.probs <- c(pred.probs, uni.back$return.probs)
                        
                } else {
                        
                        uni.back <- uni.back.fun(prev.words, k)
                        pred.words <- uni.back$return.words
                        pred.probs <- uni.back$return.probs
                        
                }
                
                preds = data.table(return.words = pred.words, return.probs = pred.probs)
                
                remove(bi.words)
                
                return(preds) 
        }
        
        tri.back.fun <- function(tri, prev.words, k) {
                
                if(input$stop) {
                        tri.words <- fread("tri.words.60.pkn.1.csv")
                } else if(input$profanity){
                        tri.words <- fread("tri.words.60.pkn.2.csv")
                } else if (input$profanity & input$stop){
                        tri.words <- fread("tri.words.60.pkn.3.csv")
                } else {
                        tri.words <- fread("tri.words.60.pkn.csv")
                }
                
                
                word2 <- tri[1]
                word3 <- tri[2]
                
                tri.exist <- any(!(is.na( tri.words[first==word2 & second==word3][!(last %in% prev.words)])))
                
                if (tri.exist){
                        
                        tri <- tri.words[first==word2 & second==word3][!(last %in% prev.words)]
                        new.k <- min(k, nrow(tri))
                        pred.words <- tri$last[1:new.k]
                        pred.probs <- tri$p.kn[1:new.k]
                        
                        
                        # If backing off
                        bi.back <- bi.back.fun(bi=word3, c(prev.words, pred.words), k)
                        pred.words <- c(pred.words, bi.back$return.words)
                        pred.probs <- c(pred.probs, bi.back$return.probs)
                        
                } else {
                        
                        bi.back <- bi.back.fun(bi=word3, prev.words, k)
                        pred.words <- bi.back$return.words
                        pred.probs <- bi.back$return.probs
                        
                }
                
                preds = data.table(return.words = pred.words, return.probs = pred.probs)
                
                remove(tri.words)
                
                return(preds) 
        }
        
        quadr.back.fun <- function(quadr, prev.words, k) {
                
                if(input$stop) {
                        quadr.words <- fread("quadri.words.60.pkn.1.csv")
                } else if(input$profanity){
                        quadr.words <- fread("quadri.words.60.pkn.2.csv")
                } else if (input$profanity & input$stop){
                        quadr.words <- fread("quadri.words.60.pkn.3.csv")
                } else {
                        quadr.words <- fread("quadri.words.60.pkn.csv")
                }
                
                word1 <- quadr[1]
                word2 <- quadr[2]
                word3 <- quadr[3]
                
                quadr.exist <- any(!(is.na( quadr.words[first==word1 & second==word2 & third==word3][!(last %in% prev.words)])))
                
                if (quadr.exist){
                        
                        quadr <- quadr.words[first==word1 & second==word2 & third==word3]#[!(last %in% prev.words)]
                        new.k <- min(k, nrow(quadr))
                        pred.words <- quadr$last[1:new.k]
                        pred.probs <- quadr$p.kn[1:new.k]
                        
                        # If backing off
                        tri.back <- tri.back.fun(tri=c(word2,word3), c(prev.words, pred.words), k)
                        pred.words <- c(pred.words, tri.back$return.words)
                        pred.probs <- c(pred.probs, tri.back$return.probs)
                        
                } else {
                        
                        tri.back <- tri.back.fun(tri=c(word2,word3), prev.words=0, k)
                        pred.words <- tri.back$return.words
                        pred.probs <- tri.back$return.probs
                        
                }
                
                preds <- data.table(return.words = pred.words, return.probs = pred.probs)
                
                remove(quadr.words)
                
                return(preds) 
                
        }
        
        predict.next <- function(input.text, k){
            
            
            n <- length(input.text)
            
            prev.words <- character(0)
            
            if (n >= 3){
                
                quadr <- input.text[(n-2): n]
                pred.words <- quadr.back.fun(quadr, prev.words, k)
                
            }
            else if (n == 2){
                
                tri <- input.text
                pred.words <- tri.back.fun(tri, prev.words, k)
                
            }
            else if (n == 1){
                
                bi <- input.text
                pred.words <- bi.back.fun(bi, prev.words, k)
                
            } else {
                
                pred.words <- uni.back.fun(prev.words, k)
            }
            
            
            return(pred.words)
        }
        
        
        processed <- reactive({
            process.which(input$human.text)
        })
        
        next.pred <- reactive({
            predict.next(processed(), input$k.slider)
        })
        
        
        output$value <- renderText({
            word.out <- next.pred()$return.words
            word.out[1]
        })
        
        output$pred.table <- renderDataTable(
            #
            #pred.table <- next.pred(),
            data.table(Words=next.pred()$return.words[1:input$k.slider], Probabilities =  next.pred()$return.probs[1:input$k.slider]),
            options = list(searching = FALSE, paging=FALSE, info=FALSE)
            )
        
        output$value2 <- renderText({
                processed()
        })
        

}


#options(shiny.reactlog=TRUE) 

shinyApp(ui, server)
