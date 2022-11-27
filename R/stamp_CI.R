#' Draw the CI plot of STAMP
#'
#' @param data the output of stamp_input()
#' @param group color information
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' stamp_CI(stamp_input())
stamp_CI <- function(data, group = NULL){
  data <- data$fig23
  p <- ggplot(data, aes(x = markers, y = CI)) +
    scale_x_discrete(limits = levels(data$markers)) +
    coord_flip() +  xlab(NULL) +
    ylab("Diffrence in mean proportion (%)") +
    labs(title="95% confidence intervals") +
    theme_stamp()
  for (i in 1:(nlevels(data$markers)-1)) {
    p <- p + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
  }
  p <- p + geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI),
                  position = position_dodge(0.8), width = 0.4, size = 0.5) +
    geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'gray50') +
    theme(axis.text.y = element_blank(),
          legend.position = "none",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 10, face = "bold", colour = "black", hjust = 0.5),
          plot.margin = unit(c(1, 0, 1, 0), "mm"),
          axis.title.x = element_text(size = 10, hjust = 1)) +
    geom_point(aes(fill = group), shape = 23)
  if (!is.null(group)) {
    p <- p + scale_fill_manual(values = group)
  }else{
    p <- p + ggsci::scale_fill_aaas()
  }
  return(p)
}
