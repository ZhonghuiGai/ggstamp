#' Draw pvalue point
#'
#' @param data the output of stamp_input function
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' stamp_pvalue_point(stamp_input("stamp.txt"))
stamp_pvalue_point <- function(data){
  data <- data$fig23
  p <- ggplot(data, aes(x = markers, y= -log10(p.values))) +
    scale_x_discrete(limits = levels(data$markers)) +
    coord_flip() + theme_stamp() +
    xlab(NULL) + ylab("-log10(p)") + labs(title = "p = 0.05")
  for (i in 1:(nlevels(data$markers)-1)){
    p <- p + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
  }
  p <- p + geom_point(aes(x = markers, y = -log10(p.values)), shape = 19, size = 0.8,
                        color = ifelse(data$p.value < 0.05, "#ff445d", "black")) +
    geom_hline(aes(yintercept = -log10(0.05)), linetype = 'dashed', color = 'black') +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 10, face = "bold",
                                    colour = "black", hjust = 0),
          plot.margin = unit(c(1, 2, 1, 0), "mm"),
          axis.title.x = element_text(size = 10))
  return(p)
}
