#' Draw the bar plot
#'
#' @param data the list from stamp_input function
#' @param legend.position legend.position
#' @param group color information
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' group <- c("#E69F00", "#56B4E9")
#' stamp_bar(stamp_input(data = stamp_input(), group = group))
stamp_bar <- function(data,
                      legend.position = c(-0.8, 1.025),
                      group = NULL){
  data <- data$fig1
  p <- ggplot(data, aes(markers, value, fill = variable)) +
    scale_x_discrete(limits = levels(data$markers)) +
    coord_flip() + xlab(NULL) + ylab("Mean proportion (%)") +
    theme_stamp()
  for (i in 1:(nlevels(data$markers)-1)) {
    p <- p + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
  }
  p <- p + geom_bar(stat = "identity", position = "dodge",
                    width = 1, colour = "white") +
    scale_y_continuous(expand = c(0, 0)) +
    theme(axis.text.y = element_text(margin = margin(0,0,0,0,'mm')),
          plot.margin = unit(c(1, 0, 1, 0), "mm"),
          axis.title.x = element_text(size = 10, hjust = 0),
          legend.position = legend.position,
          axis.line.y = element_line(colour = "gray50"),
          legend.key.height =  unit(0.05, "cm"),
          legend.key.width =  unit(0.35, "cm"))
  if (!is.null(group)) {
    p <- p + scale_fill_manual(values = group)
  }else{
    p <- p + ggsci::scale_fill_aaas()
  }
  return(p)
}
