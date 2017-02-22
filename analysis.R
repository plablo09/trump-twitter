# Set repo
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})

# Install/load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis,
               ggplot2, quanteda, parallel, rlist)
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
funcs <- c("ftime","parseDetectLanguage")
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


# Create corpus and subset them into spanish and english corpus
############################################
corpus.all <- corpus(tuits, text_field = "Texto")
corpus.english <- corpus_subset(corpus.all, lang == "ENGLISH")


# Create DTMs
############################################
myStopWords <- c("trump", "donald", "realdonaldtrump", "amp", "rt", "https", "t.co")
dtm.english <- dfm(corpus.english, remove = c(stopwords("english"), myStopWords),
                   groups = "slice", removeSymbols = TRUE, removeTwitter = TRUE,
                   removeNumbers = TRUE)

# Filter words smaller than 4 chars
###########################################
features <- featnames(dtm.english)
feat_rem <- sapply(features, function(i) nchar(i) <= 3)
features <- features[!feat_rem]
dtm.english <- dfm_select(dtm.english, features = features, selection = "keep",
                          valuetype = "fixed")

                                        # Wordclouds
###########################################
set.seed(100)
cont <- 1
for(name in docnames(dtm.english)){
    f.name <- paste("english", "wordcloud", name, sep = "_")
    f.name <- paste(f.name,"png", sep = ".")
    f.name <- paste("img", f.name, sep = "/")
    png(f.name, width=1280,height=800)
    textplot_wordcloud(dtm.english[cont,], max.words = 150, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
    dev.off()
    cont <- cont + 1
}


