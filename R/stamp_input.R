#' Readin the data.txt file, and trim it.
#'
#' @param path the path for the data form STAMP
#'
#' @return a list containing 2 data.frame, 1 for fig1, 1 for fig2-3.
#' @export
#'
#' @author ZHonghui Gai
#' @examples
#' stamp_input("stamp.txt")
stamp_input <- function(path = "stamp.txt"){
  require(ggplot2)
  st <- read.delim(file = path, row.names = 1, sep = "\t")
  st <- st[, c(1, 3, 6, 8, 9)]

  names <- colnames(st)
  names <- gsub("..mean.rel..freq.....", "", colnames(st))
  names <- gsub("..corrected.", "", names)
  names <- gsub("X95.0..", "", names)
  colnames(st) <- names

  st <- st[st$p.values < 0.05, ]
  st <- st[order(st$p.values, decreasing = FALSE), ]
  rownames(st) <- gsub("\\[.*\\]","", rownames(st))
  rownames(st) <- (rownames(st))

  st$CI <- (st$lower.CI + st$upper.CI)/2
  st <- st[order(st$p.values), ]
  st <- data.frame(markers = rownames(st), st)
  rownames(st) <- NULL
  st$markers <- trim(st$markers)
  st$markers <- factor(st$markers, levels = rev(st$markers))
  st$group <- ifelse(st[, 2] > st[, 3], colnames(st)[2], colnames(st)[3])
  st$group <- as.factor(st$group)
  fig1.data <- st[, 1:3]
  fig1.data <- reshape2::melt(fig1.data, id = "markers")
  out <- list(fig1 = fig1.data, fig23 = st)
  return(out)
}
