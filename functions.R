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
