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

#' Return a json representation of the LDA moodel as needed by the LDAVis package.
#' Taken from: https://gist.github.com/christophergandrud/00e7451c16439421b24a#file-topicmodels_json_ldavis-r
#' @param fitted The result of running topicmodels LDA.
#' @param corpus The tm Corpus object
#' @param doc_term The tm Document Term Matrix
#' @return The json object needed by LDAVis
#' @examples
#' jsonObj <- topicmodels_json_ldavis(ldaOut, corpus, dtm)

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)

    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)

    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                            vocab = vocab,
                            doc.length = doc_length,
                            term.frequency = freq_matrix$Freq)

    return(json_lda)
}
