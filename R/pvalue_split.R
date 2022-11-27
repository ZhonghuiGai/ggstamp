#' Format p values to charactor
#'
#' @param data a p value vector
#'
#' @return a data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' pvalue_split(c("2.27037499009e-22", "7.18282161205e-03"))
pvalue_split <- function(data){
  res <- sapply(as.character(data), pvalue_split_) |> as.data.frame()
  colnames(res) <- NULL
  return(res)
}

pvalue_split_ <- function(x){
  a <- strsplit(x, split = "") [[1]]
  l <- length(a)
  if (l > 8) {
    m <- paste0(a[1], a[2], a[3], a[4], a[l-3], a[l-2], a[l-1], a[l])
  }
  if (l <= 8) {
    m <- x
  }
  return(m)
}
