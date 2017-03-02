#' Format date.
#' 
#' @param s A string with format 2016-10-11T11:58:09.000Z.
#' @return The formatted string 2016-10-11 11.58.09.000Z 
#' @examples
#' ftime(2016-10-11T11:58:09.000Z)
ftime <- function(s){
  s = gsub(":", ".", s)
  s = gsub("T", " ", s)
}

#' Detect a string language.
#' 
#' @param s A string.
#' @return The language deteced by cldr or NULL if language couldn't be detected
#' @examples
#' parseDetectLanguage("This one is in English")
parseDetectLanguage <- function(s) {
    df = detectLanguage(s)
    result = df["detectedLanguage"][[1]]
    return(result)
}


#' Transform Model Output for Use with the LDAvis Package
#'
#' Convert a \pkg{topicmodels} output into the JSON form required by the \pkg{LDAvis} package.
#'
#' @param model A \code{\link[]{topicmodel}} object.
#' @param \ldots Currently ignored.
#' @seealso \code{\link[LDAvis]{createJSON}}
#' @export
#' @examples
#' \dontrun{
#' data("AssociatedPress", package = "topicmodels")
#' model <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 3)
#' LDAvis::serVis(topicmodels2LDAvis(model))
#' }
topicmodels2LDAvis <- function(x, ...){
    post <- topicmodels::posterior(x)
    if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
    mat <- x@wordassignments
    LDAvis::createJSON(
        phi = post[["terms"]], 
        theta = post[["topics"]],
        vocab = colnames(post[["terms"]]),
        doc.length = slam::row_sums(mat, na.rm = TRUE),
        term.frequency = slam::col_sums(mat, na.rm = TRUE)
    )
}

#' Filter words smaller than specified length.
#' 
#' @param dtm a \pkg{quanteda} document feature matrix.
#' @param minSize integer The minimmum size of words to be kept in the dtm
#' @return The \pkg{quanteda} document feature matrix with the words removed
#' @examples
#' new.dtm <- filterByLength(dtm, 4)
filterByLength <- function(dtm, minSize){
    features <- featnames(dtm)
    feat_rem <- sapply(features, function(i) nchar(i) <= minSize)
    features <- features[!feat_rem]
    dtm <- dfm_select(dtm, features = features, selection = "keep",
                      valuetype = "fixed")
    return(dtm)

}

#' Save pdfs with wordclouds for each document in the dtm.
#' 
#' @param dtm a \pkg{quanteda} document feature matrix.
#' @param maxWords Max words in the wordclouds
#' @param language string The language of the documents (used only for naming)
#' @param basePath string The base path to save the wordclouds
#' @examples
#' saveWordClouds(dtm, 150, "english", "img/")
saveWordclouds <- function(dtm, maxWords, language, basePath){
    cont <- 1
    for(name in docnames(dtm)){
        f.name <- paste(language, "wordcloud", name, sep = "_")
        f.name <- paste(f.name,"pdf", sep = ".")
        f.name <- paste(basePath, f.name, sep = "/")
        pdf(f.name)
        textplot_wordcloud(dtm[cont,], max.words = maxWords, random.order = FALSE,
                           rot.per = .25, 
                           colors = RColorBrewer::brewer.pal(8,"Dark2"))
        dev.off()
        cont <- cont + 1
    }
    
}


#' Get the optimal K from the metrics provided by \pkg{ldatuning}.
#' 
#' @param result A \pkg{ldatuning} result object.
#' @return The optimal K 
#' @examples
#' K <- getOptimalK(result)
getOptimalK <- function(result){
    result$rankArun <- ave(result$Arun2010,
                           FUN=function(x) rank(x,ties.method="min"))
    minArun <- subset(result, rankArun==1)$topics

    result$rankCao <- ave(result$CaoJuan2009,
                          FUN=function(x) rank(x,ties.method="min"))
    minCao <- subset(result, rankCao==1)$topics

    result$rankGriffiths <- ave(-result$Griffiths2004,
                                FUN=function(x) rank(x,ties.method="min"))
    maxGriffiths <- subset(result, rankGriffiths==1)$topics

    result$rankDeveaud <- ave(-result$Deveaud2014,
                              FUN=function(x) rank(x,ties.method="min"))
    maxDeveaud <- subset(result, rankDeveaud==1)$topics

    candidates <- c(minArun, minCao, maxGriffiths, maxDeveaud)

    # We are going to use the median as the representative value  
    # though this must be revised
    return(median(candidates))

}
