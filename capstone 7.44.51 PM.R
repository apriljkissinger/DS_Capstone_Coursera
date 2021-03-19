
# library(tidyverse)
# library(stringr)
# library(quanteda)
# library(qdap)
# library(R.utils)
# library(caret)
# library(data.table)
# library(ggplot2)
# library(ggwordcloud)
# library(gridExtra)
# library(MODIS)

# # #Opening the file connections to read in files and then closing them
# contw <- file("en_US.twitter.txt", "r")
# twitter <- readLines(contw)
# close.connection(contw)
# remove(contw)
# conbl <- file("en_US.blogs.txt")
# blogs <- readLines(conbl)
# close.connection(conbl)
# remove(conbl)
# connw <- file("en_US.news.txt", "r")
# newsa <- readLines(connw)
# close.connection(connw)
# remove(connw)

# corpus <- c(twitter, blogs, newsa)

# # Sampling
# #Splitting the corpus into training and testing sets
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(twitter)), p=.995, list=FALSE)
# corpus.split.tw <- twitter[inTest]
# valid.data.tw <- twitter[-inTest]
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(blogs)), p=.995, list=FALSE)
# corpus.split.bl <- blogs[inTest]
# valid.data.bl <- blogs[-inTest]
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(newsa)), p=.995, list=FALSE)
# corpus.split.nw <- newsa[inTest]
# valid.data.nw <- newsa[-inTest]
# valid.data <- c(valid.data.tw,valid.data.bl,valid.data.nw)
# corpus.split <- c(corpus.split.tw,corpus.split.bl,corpus.split.nw)
# 
# #Splitting into training set
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(corpus.split)), p=.60, list=FALSE)
# train.data <- corpus.split[inTest]
# data.split <- corpus.split[-inTest]

# #Splitting into quick set
# set.seed(11152020)
# inTest <- createDataPartition(seq_len(NROW(data.split)), p=.10, list=FALSE)
# quick.data <- data.split[inTest]
# unused.data <- data.split[-inTest]
#
#
remove(newsa)
remove(blogs)
remove(twitter)
remove(corpus.split)
remove(inTest)
remove(corpus.split.bl)
remove(corpus.split.nw)
remove(corpus.split.tw)
remove(data.split)
#remove(valid.data)
remove(valid.data.bl)
remove(valid.data.nw)
remove(valid.data.tw)

#########################
#
# Exploratory analysis

# save(twitter.proc, file="twitter.proc.rda")
# save(blogs.proc, file="blogs.proc.rda")
# save(newsa.proc, file="newsa.proc.rda")
#
# twwords.proc <- word_count(twitter.proc, byrow = FALSE)
# twlen.proc <-length(twitter.proc)
# twsize.proc <- fileSize("twitter.proc.rda", units="MB")
# 
# blwords.proc <- word_count(blogs.proc, byrow = FALSE)
# bllen.proc <-length(blogs.proc)
# blsize.proc <- fileSize("blogs.proc.rda", units="MB")

# nwwords.proc <- word_count(newsa.proc, byrow = FALSE)
# nwlen.proc <- length(newsa.proc)
# nwsize.proc <- fileSize("newsa.proc.rda", units="MB")

# twunique <- ncol(twitter.dfm)
# blunique <- ncol(blogs.dfm)
# nwunique <- ncol(newsa.dfm)

# summary.proc <- data.frame(Source=c("Twitter", "Blogs", "News"), File.Length=c(twlen.proc,bllen.proc,nwlen.proc), Word.Count=c(twwords.proc,blwords.proc,nwwords.proc), Avg.Words.Per.Line=c(twwords.proc/twlen.proc, blwords.proc/bllen.proc, nwwords.proc/nwlen.proc), File.Size.MB=c(twsize.proc,blsize.proc,nwsize.proc), Unique.Word.Count=c(twunique,blunique,nwunique))
# save(summary.proc, file="Summary.proc.rda")

#########################

# #Feature selection: PreProcessing function
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

# text.Toke.60 <- text.proc.fun(train.data)

#corpus.proc <- text.proc.fun(corpus)

# twitter.proc <- text.proc.fun(twitter)
# blogs.proc <- text.proc.fun(blogs)
# newsa.proc <- text.proc.fun(newsa)

########################

# #Function to generate dfms out of the tokens object and then trim to only those grams which appear at least x times.
# 
# token.DFM.fun <- function(tokens.object, n.gram){
#         token.n <- tokens_ngrams(tokens.object, n=n.gram)
#         token.n <- dfm(token.n)
#         token.n <- dfm_trim(token.n, min_termfreq = 3)
#         return(token.n)
# }

# twitter.dfm <- token.DFM.fun(twitter.proc,1)
# blogs.dfm <- token.DFM.fun(blogs.proc,1)
# newsa.dfm <- token.DFM.fun(newsa.proc,1)

# token1DFM2.3 <- token.DFM.fun(text.Toke.60, 1)
# token2DFM2.3 <- token.DFM.fun(text.Toke.60, 2)
# token3DFM2.3 <- token.DFM.fun(text.Toke.60, 3)
# token4DFM2.3 <- token.DFM.fun(text.Toke.60, 4)

# save(text.Toke.60, file="text.Toke.60.rda")
# save(text.Toke.60.1, file="text.Toke.60.1.rda")

# corpus.dfm.0 <- token.DFM.fun(corpus.proc, 1) 
# corpus.word.0 <- ncol(corpus.dfm.0)
# corpus.word.3 <- ncol(corpus.dfm.3)
# corpus.unique <- data.frame( . = c("Min.Term.Freq.0", "Min.Term.Freq.3"), Unique.Word.Count = c(corpus.word.0, corpus.word.3))
# save(corpus.unique, file = "corpus.unique.rda")

# unique.0 <- ncol(token1DFM2.3)
# unique.1 <- ncol(token1DFM2.3.1)
# unique.2 <- ncol(token1DFM2.3.2)
# unique.3 <- ncol(token1DFM2.3.3)
# 
# options.unique <- data.frame( .=c("Number of Unique Words"), Unfiltered=unique.0, No.Stop=unique.1, No.Profane=unique.2, No.Both=unique.3)
# save(options.unique, file="options.unique.rda")
# 
# #(1)
# #Summing word counts across n-grams
# uni.sums <- colSums(token1DFM2.3)
# bi.sums <- colSums(token2DFM2.3)
# tri.sums <- colSums(token3DFM2.3)
# quadri.sums <- colSums(token4DFM2.3)
# 
# remove(token1DFM2.3)
# remove(token2DFM2.3)
# remove(token3DFM2.3)
# remove(token4DFM2.3)
# 
# #(2)
# #Creating 3 data.tables with each unique n-gram as a row and separating each word into individual columns.
# uni.words.60.3 <- data.table(last = names(uni.sums), count = uni.sums)
# bi.words.60.3 <- data.table(first = sapply(strsplit(names(bi.sums), "_", fixed=TRUE), '[[', 1), last = sapply(strsplit(names(bi.sums), "_", fixed=TRUE), '[[', 2), count=bi.sums)
# tri.words.60.3 <- data.table(first = sapply(strsplit(names(tri.sums), "_", fixed=TRUE), '[[', 1), second = sapply(strsplit(names(tri.sums), "_", fixed=TRUE), '[[', 2),last = sapply(strsplit(names(tri.sums), "_", fixed=TRUE), '[[', 3), count=tri.sums)
# quadri.words.60.3 <- data.table(first = sapply(strsplit(names(quadri.sums), "_", fixed=TRUE), '[[', 1), second = sapply(strsplit(names(quadri.sums), "_", fixed=TRUE), '[[', 2),third = sapply(strsplit(names(quadri.sums), "_", fixed=TRUE), '[[', 3),last = sapply(strsplit(names(quadri.sums), "_", fixed=TRUE), '[[', 4), count=quadri.sums)
# 
# remove(uni.sums)
# remove(bi.sums)
# remove(tri.sums)
# remove(quadri.sums)

#################
# #Performing Good Turing Frequency Estimation to get the discount factor, d, that will later be used in the Kneser Ney Smoothing algorithm
# #Good Turing Frequency Estimate is performed using th following formula:
# #c* = (c+1)(N(c+1)/N(c))
#
# #Good Turing discount function
# 
# gt.disc.fun <- function (n.words){
#         N.c <- n.words[, .(N = .N), by=count]
#         setkey(n.words, count)
#         setkey(N.c, count)
#         n.words <- merge(n.words, N.c, all.x=TRUE)
#         n.word <- n.words[, .(count.plus=count+1)]
#         n.words <- cbind(n.words, n.word)
#         N.c.2 <- rename(N.c, "count.plus"="count")
#         setkey(n.words, count.plus)
#         setkey(N.c.2, count.plus)
#         n.words <- merge(n.words, N.c.2, all.x=TRUE)
#         n.words <- rename(n.words, "N.count"="N.x", "N.count.plus"="N.y")
#         new.count <- n.words[, .(count.gt=(count+1)*(N.count.plus/N.count))]
#         n.words <- cbind(n.words,new.count)
#         n.words.nona <- na.omit(n.words)
#         gt.disct <- mean(n.words.nona$count-n.words.nona$count.gt)
#         return(gt.disct)
# }
# gt.discount.bi <- gt.disc.fun(bi.words.60.3)
# gt.discount.tri <- gt.disc.fun(tri.words.60.3)
# gt.discount.quadri <- gt.disc.fun(quadri.words.60.3)
# 
# gt.data <- data.frame(N.Gram.Type=c("Bi-Grams", "Tri-Grams", "Quadri-grams"), GT.Discount=c(gt.discount.bi, gt.discount.tri, gt.discount.quadri))
# save(gt.data, file="gt.data.Rda")


###########################

# #Kneser-Kney Smoothing
# 
# #Unigrams
# 
# p.kn.uni.fun <- function(last){
# 
#         #Cont Count: Distinct # of 'last' in bi.words
#         cont.count.uni <- bi.words[, .N, by=last]
#         setnames(cont.count.uni, "N", "cont.count")
#         setkey(uni.words, last)
#         setkey(cont.count.uni, last)
#         #adding the cont count
#         uni.words<- cont.count.uni[uni.words, nomatch=0]
#         #Bigram count
#         count.bi <- nrow(bi.words)
#         uni.words <- uni.words[, p.kn := cont.count/count.bi]
#         uni.words.kn <- data.table(last=uni.words$last, p.kn=uni.words$p.kn)
#         return(uni.words.kn)
# 
#  }
# 
# #Bigrams
# 
# p.kn.bi.fun <- function(first, last){
# 
# #Abs discounted cont count = # of unique trigrams ending with the bigram)/ The cont count of the unigram ending in any unigram
# bi.words <- bi.words[,last.two:= paste(first, last, sep = ".")]
# tri.words <- tri.words[,last.two:= paste(second, last, sep = ".")]
# cont.count.bi <- tri.words[, .N, by=last.two]
# setnames(cont.count.bi, "N", "cont.count")
# setkey(bi.words, last.two)
# setkey(cont.count.bi, last.two)
# bi.words <- cont.count.bi[bi.words, nomatch=0]
# #Trigrams whose second word is second
# unique.second.tri <- tri.words[, .N, by=second]
# setnames(unique.second.tri, "second", "first")
# setnames(unique.second.tri, "N", "norm.cont")
# setkey(bi.words, first)
# setkey(unique.second.tri, first)
# bi.words <- unique.second.tri[bi.words, nomatch=0]
# #Abs disc count
# bi.words <- bi.words[, disc.cont:=max(0,(cont.count-gt.discount.bi))/norm.cont, by=1:nrow(bi.words)]
# 
# #Lambda
# #Interpolation Weight
# bi.words <- bi.words[,uni.pref:=.N, by=first ]
# bi.words <- bi.words[, lambda := (gt.discount.bi/norm.cont)*(uni.pref)]
# 
# #p.kn.uni.fun backoff probabilities
# p.kn.uni <- p.kn.uni.fun(last)
# setkey(p.kn.uni, last)
# setkey(bi.words, last)
# bi.words <- p.kn.uni[bi.words, nomatch=0]
# 
# #P.kn
# bi.words <- bi.words[, p.kn:= disc.cont+(lambda*p.kn)]
# 
# bi.words.kn <- data.table(first=bi.words$first, last=bi.words$last, p.kn=bi.words$p.kn)
#         return(bi.words.kn)
# 
# }
# 
# #Trigrams
# 
# p.kn.tri.fun <- function(first, second, last){
# 
#         #Abs discounted cont count
#         #(# of unique quadriagrams ending with the trigram)/(The cont count of the trigram ending in any unigram)
#         tri.words <- tri.words[,last.three:= paste(first, second, last, sep = ".")]
#         quadri.words <- quadri.words[,last.three:= paste(second,third, last, sep = ".")]
#         cont.count.tri <- quadri.words[, .N, by=last.three]
#         setnames(cont.count.tri, "N", "cont.count")
#         setkey(tri.words, last.three)
#         setkey(cont.count.tri, last.three)
#         tri.words <- cont.count.tri[tri.words, nomatch=0]
#         #quadri-grams having the sequence (_,b,c,_)
#         quadri.words <- quadri.words[, middle.two:= paste(second, third, sep = ".")]
#         tri.words <- tri.words[, middle.two:= paste(first, second, sep=".")]
#         unique.bimid.quadri <- quadri.words[, .N, by=middle.two]
#         setnames(unique.bimid.quadri, "N", "norm.cont")
#         setkey(tri.words, middle.two)
#         setkey(unique.bimid.quadri, middle.two)
#         tri.words <- unique.bimid.quadri[tri.words, nomatch=0]
#         #Abs disc count
#         tri.words <- tri.words[, disc.cont:=max(0,(cont.count-gt.discount.tri))/norm.cont, by=1:nrow(tri.words)]
# 
#         #Lambda
#         tri.words <- tri.words[, bi.pref:=.N, by=middle.two]
#         tri.words <- tri.words[, lambda := (gt.discount.tri/norm.cont)*(bi.pref)]
# 
#         #p.kn.bi.fun backoff probabilities
#         p.kn.bi <- p.kn.bi.fun(second, last)
#         setnames(p.kn.bi, c("first", "last"), c("second", "last"))
#         setkey(p.kn.bi, second, last)
#         setkey(tri.words, second, last)
#         tri.words <- p.kn.bi[tri.words, nomatch=0]
# 
#         #P.kn
#         tri.words <- tri.words[, p.kn:= disc.cont+(lambda*p.kn)]
# 
#         tri.words.kn <- data.table(first=tri.words$first, second=tri.words$second, last=tri.words$last, p.kn=tri.words$p.kn)
#         return(tri.words.kn)
# 
# }
# 
# p.kn.quadri.fun <- function(first, second, third, last){
# 
#         #Abs.disc (count of the quadriagram)/(count of distinct trigrams beginning the quadriagram )
#         #counts of distinct trigrams put into the quadri-gram data.table
#         quadri.words[, tri.pref := paste(first, second, third, sep = ".")]
#         tri.words[, tri.pref := paste(first, second, last, sep = ".")]
#         quadri.tri.pref <- data.table(tri.pref = tri.words$tri.pref, dist.tri = tri.words$count)
#         setkey(quadri.tri.pref, tri.pref)
#         setkey(quadri.words, tri.pref)
#         quadri.words <- quadri.tri.pref[quadri.words, nomatch=0]
#         #Abs Disc
#         quadri.words <- quadri.words[, disc.count:= (count-gt.discount.quadri)/(dist.tri)]
# 
#         #Lambda
#         quadri.words <- quadri.words[,pref.count := .N, tri.pref]
#         quadri.words <- quadri.words[, lambda:=(gt.discount.quadri/dist.tri)*(pref.count)]
# 
#         #p.kn.tri.fun backoff probabilities
#         p.kn.tri <- p.kn.tri.fun(second, third, last)
#         setnames(p.kn.tri, c("first", "second", "last"),  c("second", "third", "last"))
#         setkey(p.kn.tri, second, third, last)
#         setkey(quadri.words, second, third, last)
#         quadri.words <- p.kn.tri[quadri.words, nomatch=0]
# 
#         #P.kn
#         quadri.words <- quadri.words[, p.kn:= disc.count+(lambda*p.kn)]
# 
#         quadri.words.kn <- data.table(first=quadri.words$first, second=quadri.words$second, third=quadri.words$third,  last=quadri.words$last, p.kn=quadri.words$p.kn)
#         return(quadri.words.kn)
# 
# }

# 
# 
# #######
# uni.words.3 <- uni.words.60.3
# bi.words <- bi.words.60.3
# tri.words <- tri.words.60.3
# quadri.words <- quadri.words.60.3

# save(uni.words, file="uni.words.rda")
# save(bi.words, file="bi.words.rda")
# save(tri.words, file="tri.words.rda")
# save(quadri.words, file="quadri.words.rda")

# 
# remove(uni.words.60.3)
# remove(bi.words.60.3)
# remove(tri.words.60.3)
# remove(quadri.words.60.3)
# 
# uni.words.final <- p.kn.uni.fun(uni.words$last)
# bi.words.final <- p.kn.bi.fun(bi.words$first, bi.words$last)
# tri.words.final <- p.kn.tri.fun(tri.words$first, tri.words$second, tri.words$last)
# quadri.words.final <- p.kn.quadri.fun(quadri.words$first, quadri.words$second, quadri.words$third, quadri.words$last)
# 
# #data Preparation
# #preparing the data files for use with the app.
# 
# uni.words.60.pkn.3 <- setorder(uni.words.final,-p.kn)
# bi.words.60.pkn.3 <- setorder(bi.words.final, -p.kn)
# tri.words.60.pkn.3 <- setorder(tri.words.final, -p.kn)
# quadri.words.60.pkn.3 <- setorder(quadri.words.final, -p.kn)
# 
# write.csv(uni.words.60.pkn.3, 'uni.words.60.pkn.3.csv')
# write.csv(bi.words.60.pkn.3, 'bi.words.60.pkn.3.csv')
# write.csv(tri.words.60.pkn.3, 'tri.words.60.pkn.3.csv')
# write.csv(quadri.words.60.pkn.3, 'quadri.words.60.pkn.3.csv')

############
# #Graphs for exploratory analysis

# tw.proc.94 <- as.character(twitter.proc[94])
# save(tw.proc.94, file="tw.proc.94.rda")
# bl.8 <- as.character(blogs[8])
# save(bl.8, file="bl.8.rda")
# bl.proc.8 <- as.character(blogs.proc[8])
# save(bl.proc.8, file="bl.proc.8.rda")
# nw.25262 <- as.character(newsa[25262])
# save(nw.25262, file="nw.25262.rda")
# nw.proc.25262 <- as.character(newsa.proc[25262])
# save(nw.proc.25262, file="nw.proc.25262.rda")

# #Uni-grams
# top.words.uni <- uni.words[order(-count)]
# top.words.uni <- top.words.uni[1:50]
# uni.cloud <- ggplot(top.words.uni, aes(label= last, size=count)) +
# labs( title="Top 50 Uni-grams") + geom_text_wordcloud_area(eccentricity = 1) + scale_size_area(max_size = 14) 
# top.words.uni.nostop <- uni.words.3[order(-count)]
# top.words.uni.nostop <- top.words.uni.nostop[1:50]
# uni.cloud.nostop <- ggplot(top.words.uni.nostop, aes(label= last, size=count)) +
# labs( title="Top 50 Uni-grams: Stopwords Removed") + scale_size_area(max_size = 7) + geom_text_wordcloud_area(eccentricity = 1)
# uni.hist <- ggplot(top.words.uni[1:15], aes(y=reorder(last, count), x=count))+geom_bar(stat="identity") +
# labs( x="", y="Uni-gram", title="Top 15 Uni-grams")
# 
# #
# jpeg("clouds.jpg")
# clouds <- grid.arrange(uni.cloud, uni.cloud.nostop)
# dev.off()

# #Bi-grams
# top.words.bi <- bi.words[,Bi.gram := paste(first, last, sep = "_")]
# top.words.bi <- bi.words[order(-count)]
# top.words.bi <- top.words.bi[1:15]
# bi.hist <- ggplot(top.words.bi, aes(y=reorder(Bi.gram, count), x=count))+geom_bar(stat="identity") +
# labs( x="", y="Bi-gram", title="Top 15 Bi-grams")
# save(bi.hist, file="bi.hist.Rda")
# 
# #Tri-grams
# top.words.tri <- tri.words[,Tri.gram := paste(first, second, last, sep = "_")]
# top.words.tri <- tri.words[order(-count)]
# top.words.tri <- top.words.tri[1:15]
# tri.hist <- ggplot(top.words.tri, aes(y=reorder(Tri.gram, count), x=count))+geom_bar(stat="identity") +
# labs( x="", y="Tri-gram", title="Top 15 Tri-grams")
# save(tri.hist, file="tri.hist.Rda")
# 
# #Quadri-grams
# top.words.quadri <- quadri.words[,Quadri.gram := paste(first, second, third,last, sep = "_")]
# top.words.quadri <- quadri.words[order(-count)]
# top.words.quadri <- top.words.quadri[1:15]
# quadri.hist <- ggplot(top.words.quadri, aes(y=reorder(Quadri.gram, count), x=count))+geom_bar(stat="identity") +
# labs( x="", y="Quadri-gram", title="Top 15 Quadri-grams")
# save(quadri.hist, file="quadri.hist.Rda")
# 
# jpeg("hists.jpg")
# hists <- grid.arrange(uni.hist, bi.hist, tri.hist, quadri.hist, ncol=2)
# dev.off()

