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

#' Detect a string languga.
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
