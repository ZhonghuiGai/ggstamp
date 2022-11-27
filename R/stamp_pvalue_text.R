#' Draw p value text
#'
#' @param data list from stamp_input function
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' stamp_pvalue_text(stamp_input())
stamp_pvalue_text <- function(data) {
  data <- data$fig23
  data$p.label <- pvalue_split(data$p.values)[, 1]
  data$p.label <- ifelse(data$p.values < 1e-15, "<1e-15", data$p.label)
  data$p.label <- ifelse(grepl("e", data$p.label), data$p.label, round(data$p.values, 4))
  p <- ggplot(data, aes(x = markers, p.label)) +
    geom_text(aes(y = 0, x = markers), label = data$p.label, family = "serif",
              hjust = 0, fontface = "bold", inherit.aes = FALSE, size = 3) +
    geom_text(aes(x = length(p.label)/2 + 0.5, y = 0.15),label = "",
              srt = 90, fontface = "bold",size = 5) + labs(title = "p values") +
    coord_flip() + xlab(NULL) + ylab(NULL) + theme_void() +
    theme(plot.title = element_text(face = "bold", size = 10,
                                    hjust = 0, family = "serif",
                                    margin = margin(0,0,2,0)),
          plot.margin = unit(c(1, 0, 1, 0), "mm"))
  return(p)
}


