# Set repo
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})

# Install/load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis,
               ggplot2, quanteda, parallel)
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

# Create corpus
############################################
corpus.all <- corpus(tuits, text_field = "Texto")
