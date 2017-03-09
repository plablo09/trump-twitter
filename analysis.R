#!/usr/bin/env Rscript

# Read command line args
args = commandArgs(trailingOnly=TRUE)

# if no command line args, use test database
if(length(args)==0){
    data_file <- "data/tuits_sample.csv"
} else{
    data_file <- args[1]
}

# Set repo
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})

# Install/load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis,
               ggplot2, parallel, rlist, ldatuning, magrittr, reshape,
               ggstance)
# Install quanteda from github
if (!require("quanteda")) install_github("kbenoit/quanteda")
# Install language recognition library from archive
if (!require("cldr",character.only = TRUE)){
    url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
    pkgFile<-"cldr_1.1.0.tar.gz"
    download.file(url = url, destfile = pkgFile)
    install.packages(pkgs=pkgFile, type="source", repos=NULL)
    unlink(pkgFile)    
}
library(cldr)

# Read functions
############################################
funcs <- c("ftime","parseDetectLanguage", "topicmodels2LDAvis", "filterByLength",
           "saveWordclouds", "getOptimalK")
for(f in funcs){
    if(!exists(f, mode="function")) source("functions.R")
}

# Make cluster and register functions
############################################
no_cores <- detectCores() -2
cl <- makeCluster(no_cores)
cl_exports <- c("ftime", "parseDetectLanguage", "detectLanguage")
clusterExport(cl, cl_exports)

# Read data, format dates and detect language
############################################
print("Reading data")
data_file <- "data/tuits_sample.csv"
tuits <- read.csv(data_file, stringsAsFactors = FALSE)
print("Parsing dates")
tuits$Fecha_tweet <- parLapply(cl,tuits$Fecha_tweet, ftime)
tuits$Fecha_tweet <- as.POSIXct(strptime(tuits$Fecha_tweet,
                                         "%Y-%m-%d %H.%M.%OS"),
                                format="%Y-%m-%d %H.%M.%OS")
names(tuits)[names(tuits) == 'Fecha_tweet'] <- 'tstamp'
tuits$fecha <- as.Date(tuits$tstamp)
print("Classifying languages")
tuits$lang <- parLapply(cl,tuits$Texto,parseDetectLanguage)
tuits$lang <- as.character(tuits$lang)

# Add column identifying date slice
fechas = c("2014-04-03","2015-06-16","2015-06-25","2015-06-30",
           "2015-07-06","2015-07-23","2015-12-07" ,"2016-05-03",
           "2016-07-18","2016-07-25", "2016-08-26","2016-08-31",
           "2016-09-01","2016-10-03","2016-10-07","2016-10-08",
           "2016-10-10","2016-10-15","2016-10-19","2016-11-09")
cont <- 1
tuits$slice <- NA
for(f in fechas){
    if(cont < length(fechas)){
        d <- as.Date(f)
        index <- tuits$fecha >= d & tuits$fecha <= as.Date(fechas[[cont + 1]])
        if(nrow(tuits[index,]) > 0){
            tuits[index,]["slice"] <- f
        }        
    }
    cont <- cont +1
}


# Create corpus, merge some phrases and subset
# corpus into spanish and english
############################################
corpus.all <- corpus(tuits, text_field = "Texto")

corpus.english <- corpus_subset(corpus.all, lang == "ENGLISH")
corpus.spanish <- corpus_subset(corpus.all, lang == "SPANISH")


# Create DTMs
############################################
myStopWords <- c("trump", "donald", "realdonaldtrump", "amp", "https", "http")

mergeWords <- dictionary(list(pena_nieto = c("peña nieto", "pena nieto"),
                              hillary_clinton = c("hillary clinton")))
dtm.english <- dfm(phrasetotoken(corpus.english, mergeWords),
                   remove = c(stopwords("english"), myStopWords),
                   groups = "slice", removeURL = TRUE, removeTwitter = TRUE,
                   removeSymbols = TRUE,
                   removePunct = TRUE,
                   removeNumbers = TRUE,
                   thesaurus = lapply(mergeWords, function(x) gsub("\\s", "_", x)))

dtm.spanish <- dfm(corpus.spanish, remove = c(stopwords("spanish"), myStopWords),
                   groups = "slice", removeURL = TRUE, removeTwitter = TRUE,
                   removeSymbols = TRUE,
                   removePunct = TRUE,
                   removeNumbers = TRUE,
                   thesaurus = lapply(mergeWords, function(x) gsub("\\s", "_", x)))

# Filter words smaller than 4 chars
###########################################
dtm.english <- filterByLength(dtm.english, 3)
dtm.spanish <- filterByLength(dtm.spanish, 3)

# Wordclouds
###########################################
set.seed(100)
saveWordclouds(dtm.english, 150, "english", "img/")
saveWordclouds(dtm.spanish, 150, "spanish", "img/")

# Find optimal K for LDA topicmodel
##########################################
control <- list(burnin = 500, iter = 1000, keep = 100, seed = 2500)
result.english <- FindTopicsNumber(
  dtm.english,
  topics = seq(from = 2, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = control,
  mc.cores = no_cores,
  verbose = TRUE
)
result.spanish <- FindTopicsNumber(
  dtm.spanish,
  topics = seq(from = 2, to = 50, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = control,
  mc.cores = no_cores,
  verbose = TRUE
)
save(result.spanish, file =  "data/ldatuning_result_spanish.RData")
save(result, file =  "data/ldatuning_result_english.RData")
## Get the optimal K value for each dtm
###########################################
K.english <- getOptimalK(result.english)
K.spanish <- getOptimalK(result.spanish)

## Save metrics plots for both languages
pdf("img/optimal-K-english.pdf")
FindTopicsNumber_plot(result.english)
dev.off()
pdf("img/optimal-K-spanish.pdf")
FindTopicsNumber_plot(result.spanish)
dev.off()

# Fit models with found optimal Ks and save them
##########################################
lda.english <- LDA(dtm.english, K.english, method = "Gibbs", control = control)
lda.spanish <- LDA(dtm.spanish, K.spanish, method = "Gibbs", control = control)
save(lda.english, file = "data/lda.english.RData")
save(lda.spanish, file = "data/lda.spanish.RData")

# Plot topic proportions per time slice
##########################################
t_levels <- c("2014-04-03", "2015-07-06", "2015-07-23", "2015-12-07", "2016-05-03",
                 "2016-07-18", "2016-07-25", "2016-08-26", "2016-08-31", "2016-09-01",
                 "2016-10-03", "2016-10-07", "2016-10-08", "2016-10-10", "2016-10-15",
              "2016-10-19")

topics.english <- posterior(lda.english, dtm.english)$topics
sp <- plotProportions(topics.english, lda.english, t_levels, "english", "img")
pdf("img/english_topic_proportions.pdf")
sp
dev.off()
topics.spanish <- posterior(lda.spanish, dtm.spanish)$topics
sp <- plotProportions(topics.spanish, lda.spanish, t_levels, "spanish", "img")
pdf("img/spanish_topic_proportions.pdf")
sp
dev.off()

jsonVis <- topicmodels2LDAvis(optimaLDA)

## Serve visualization with:
## LDAvis::serVis(jsonVis)
