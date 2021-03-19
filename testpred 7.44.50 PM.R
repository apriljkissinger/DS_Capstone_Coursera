# library(caret)
# library(data.table)
# library(quanteda)
# library(stringr)
# library(qdap)



# #############################################
# # #Splitting the corpus into training and validation sets
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(valid.data)), p=.10, list=FALSE)
# test.data.10 <- valid.data[inTest]
# unused.test.data <- valid.data[-inTest]
# 
# save(test.data.10, file = "test.data.10.Rda")

# remove(inTest)
# remove(unused.test.data)
# ###########################################


# #Text Processing function
# 
# text.proc.fun <- function(text.object, stop.bad=0){
# 
#           text.object <- str_replace_all(text.object, "-", " ")
# 
#           text.object <- replace_contraction(text.object, contraction = qdapDictionaries::contractions)
# 
#           text.object <- tolower(text.object)
#           text.object <- str_replace_all(text.object, "\\S*@+\\S*", " ")
#           text.object <- str_replace_all(text.object, "\\S*#+\\S*", " ")
#           text.object <- str_replace_all(text.object, "[^[a-zA-Z][:space:]]", "")
# 
#           text.object <- tokens(text.object)
#           digit.left <- c("pm", "nd", "rd", "th")
#           text.object <- tokens_select(text.object, digit.left, selection = "remove")
#           abbrev <- c("aka", "appt", "approx", "apt", "asap", "ave", "afk", "bnb", "bff", "byob", "btw", "cm", "diy", "dnd", "dr", "eta", "eg", "faq", "idk", "kg", "misc", "ms", "mrs", "mr", "na", "lb", "sr", "ttyl", "tgif", "vs", "vip")
#           fullword <- c("also known as","apartment","appointment","approximately","as soon as possible","avenue","away from keyboard","bed and breakfast","best friend forever","bring your own bottle","by the way","centimeter","do it yourself","do not disturb","doctor","estimated time of arrival","for example","frequently asked questions","i do not know", "kilogram","miscellaneous","miss", "misses","mister", "not applicable", "pound","senior","talk to you later","thank god its friday","versus","very important person")
#           text.object <- tokens_replace(text.object, abbrev, fullword)
# 
#           remove.word <- read.table("badwords.txt", nrows=-1, sep="\n")
#           remove.word <- as.vector(remove.word[1:447,])
#           remove.word <- tolower(remove.word)
# 
#           if(stop.bad == 1){
#             text.object <- tokens_select(text.object, pattern = stopwords("en"), selection = "remove")
#           }
#           if(stop.bad == 2){
#             text.object <- tokens_select(text.object, remove.word, selection = "remove")
#           }
#           if(stop.bad == 3){
#             text.object <- tokens_select(text.object, pattern = stopwords("en"), selection = "remove")
#             text.object <- tokens_select(text.object, remove.word, selection = "remove")
#           }
#           text.object <- tokens_split(text.object)
#           return(text.object)
# 
# }
# 
# valid.Toke <- text.proc.fun(test.data.10)

# #Gets the last 4 words of each text
# 
# #Creating an empty list for the loop to gather the last four words of each text
# valid.last.4 <- vector(mode = "list")
# #
# # #gathering the last 4 words of each text. We will be predicting the last word of each list element(text).
# # #need to chaned i below to the number of rows after completion
# for (i in 1:length(valid.Toke)){
#         # Only takes in strings that are greater than 3 words long & printing out the last 4 words. This will be wha t is
#         #predicted on.
#         if (length(valid.Toke[[i]])>3){
# 
#                 valid.last.4[i][1] <- rev(valid.Toke[[i]])[4]
#                 valid.last.4[[i]][2] <- rev(valid.Toke[[i]])[3]
#                 valid.last.4[[i]][3] <- rev(valid.Toke[[i]])[2]
#                 valid.last.4[[i]][4] <- rev(valid.Toke[[i]])[1]
# 
#         }
# }
# 
# #Making a data.table of the last four words of each text
# valid.DT <- data.table(lapply(valid.last.4, unlist))
# valid.DT <- valid.DT[, c("first", "second", "third", "temp") := tstrsplit(as.character(V1), ",", fixed=TRUE)]
# valid.DT[, c("fourth") := tstrsplit(temp, ")", fixed=TRUE)]
# valid.DT[, first := gsub("c\\(", "", first)]
# valid.DT[,c("V1","temp"):=NULL]
# valid.DT[, first := gsub("\\\"", "", first)]
# valid.DT[, second := gsub(" \\\"", "", second)]
# valid.DT[, second := gsub("\\\"", "", second)]
# valid.DT[, third := gsub(" \\\"", "", third)]
# valid.DT[, third := gsub("\\\"", "", third)]
# valid.DT[, fourth := gsub(" \\\"", "", fourth)]
# valid.DT[, fourth := gsub("\\\"", "", fourth)]



# #Back off functions

# uni.back.fun <- function(prev.words,  k){
# 
#         uni.words <- fread("uni.words.60.pkn.csv")
# 
#         uni <- uni.words[!(last %in% prev.words)]
#         new.k <- min(k, nrow(uni))
#         pred.words <- uni$last[1:new.k]
#         pred.probs <- uni$p.kn[1:new.k]
# 
#         preds <- data.table(return.words = pred.words, return.probs = pred.probs )
# 
#         remove(uni.words)
# 
#         return(preds)
# }
# 
# bi.back.fun <- function(word3, prev.words,  k){
# 
#         bi.words <- fread("bi.words.60.pkn.csv")
# 
#         bi.exist <- any(!(is.na( bi.words[first==word3][!(last %in% prev.words)])))
# 
#         if (bi.exist){
# 
#                 bi <- bi.words[first==word3][!(last %in% prev.words)]
#                 new.k <- min(k, nrow(bi))
#                 pred.words <- bi$last[1:new.k]
#                 pred.probs <- bi$p.kn[1:new.k]
# 
#                 # If backing off
#                 uni.back <- uni.back.fun(c(prev.words, pred.words), k)
#                 pred.words <- c(pred.words, uni.back$return.words)
#                 pred.probs <- c(pred.probs, uni.back$return.probs)
# 
#         } else {
# 
#                 uni.back <- uni.back.fun(prev.words, k)
#                 pred.words <- uni.back$return.words
#                 pred.probs <- uni.back$return.probs
# 
#         }
# 
#         preds = data.table(return.words = pred.words, return.probs = pred.probs)
# 
#         remove(bi.words)
# 
#         return(preds)
# }
# 
# tri.back.fun <- function(word2, word3, prev.words, k) {
# 
#         tri.words <- fread("tri.words.60.pkn.csv")
# 
#         tri.exist <- any(!(is.na( tri.words[first==word2 & second==word3][!(last %in% prev.words)])))
# 
#         if (tri.exist){
# 
#                 tri <- tri.words[first==word2 & second==word3][!(last %in% prev.words)]
#                 new.k <- min(k, nrow(tri))
#                 pred.words <- tri$last[1:new.k]
#                 pred.probs <- tri$p.kn[1:new.k]
# 
# 
#                 # If backing off
#                 bi.back <- bi.back.fun(word3, c(prev.words, pred.words), k)
#                 pred.words <- c(pred.words, bi.back$return.words)
#                 pred.probs <- c(pred.probs, bi.back$return.probs)
# 
#         } else {
# 
#                 bi.back <- bi.back.fun(word3, prev.words, k)
#                 pred.words <- bi.back$return.words
#                 pred.probs <- bi.back$return.probs
# 
#         }
# 
#         preds = data.table(return.words = pred.words, return.probs = pred.probs)
# 
#         remove(tri.words)
# 
#         return(preds)
# }
# 
# quadr.back.fun <- function(word1, word2, word3, prev.words, k) {
# 
#         quadr.words <- fread("quadri.words.60.pkn.csv")
# 
#         quadr.exist <- any(!(is.na( quadr.words[first==word1 & second==word2 & third==word3])))
# 
#         if (quadr.exist){
# 
#                 quadr <- quadr.words[first==word1 & second==word2 & third==word3]
#                 new.k <- min(k, nrow(quadr))
#                 pred.words <- quadr$last[1:new.k]
#                 pred.probs <- quadr$p.kn[1:new.k]
# 
#                 # If backing off
#                 tri.back <- tri.back.fun(word2,word3, c(prev.words, pred.words), k)
#                 pred.words <- c(pred.words, tri.back$return.words)
#                 pred.probs <- c(pred.probs, tri.back$return.probs)
# 
#         } else {
# 
#                 tri.back <- tri.back.fun(word2,word3, prev.words=0, k)
#                 pred.words <- tri.back$return.words
#                 pred.probs <- tri.back$return.probs
# 
#         }
# 
#         preds <- data.table(return.words = pred.words, return.probs = pred.probs)
# 
#         remove(quadr.words)
# 
#         return(preds)
# 
# }


#Runs words 1, 2, and 3 of the last four words of each text through the KN back off probability functions to build 
#a table of next wrd predictions 
#Searches each list for word 4 from the testing data and if found prints TRUE, if not found prints FALSE. 

# preds <- vector(mode="list")
# time.60.3 <- system.time({
# for (i in 1:length(valid.DT[[1]])){
# #for (i in 1:20){
# 
#         preds[i] <- quadr.back.fun(valid.DT[[1]][i], valid.DT[[2]][i], valid.DT[[3]][i], prev.words=0, k=25)
#         preds[i] <- valid.DT[[4]][i] %in% unlist(preds[i])
# }
# })

# for (i in 1:length(preds)){
#         true.pred <- length(which(preds =="TRUE"))
# }

#How many of the textx received an accurate prediction?

#valid.length <- nrow(valid.DT)

summary.preds <- data.frame(Model=c("Unfiltered", "Stop.Words.Filtered", "Profanity.Filtered", "Both.Filtered"), Sys.Time=c(time.60.3[2],time.60.3.1[2],time.60.3.2[2],time.60.3.3[2]), Percent.Correct=c((true.pred/valid.length)*100,(true.pred.1/valid.length)*100,(true.pred.2/valid.length)*100,(true.pred.3/valid.length)*100))
save(summary.preds, file="summary.preds")