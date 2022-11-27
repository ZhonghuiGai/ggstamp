#' trim black before and after a variable
#'
#' @param x the charactor vector
#'
#' @return a trimed vector
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' x <- c("test ", " trim ")
#' trim(x)
trim <- function(x){
  x <- gsub("^[ ]+", "", x)
  x <- gsub("[ ]+$", "", x)
  return(x)
}
