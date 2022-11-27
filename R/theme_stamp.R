#' ggplot theme for publication ready Plots, adapted from ggtheme::theme_pub
#'
#' @param base_size the default value is 14
#' @param base_family the default value is "serif"
#' @param border `logical`. Should a border be drawn around the plot?
#' @param angle `integer`. Angle of x_axis text in degrees.
#' One of: `-30, 45, 90, 270`.
#'
#' @return ggplot theme
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gglm::gglm() + ggtheme::theme_stamp()
#' gglm(group = "Species", label.y = 0.99) + ggtheme::theme_stamp() +
#' ggprism::scale_color_prism(palette = "floral")
theme_stamp <- function(base_size = 13,
                      base_family = "serif",
                      border = FALSE,
                      angle = 0) {
  library(ggplot2)
  # Draw border or not
  if(!is.logical(border)) {
    stop("border must be either: TRUE or FALSE")
  } else {
    if(border){
      panel.border <- element_rect(fill = NA)
      axis.line <- element_blank()
    }
    else if (!border) {
      panel.border <- element_blank()
      axis.line <- element_line()
    }
  }
  # Ensure x axis text is at a sensible angle
  angle <- angle[1]
  if(!angle %in% c(-30, 0, 45, 90, 270))
    stop(sprintf("'axis_text_angle' must be one of [%s]",
                 paste(c(-30, 0, 45, 90, 270), collapse=", ")),
         ".\nFor other angles, use the guide_axis() function in ggplot2 instead",
         call. = FALSE)

  ggthemes::theme_foundation(base_size = base_size, base_family=base_family) +
    theme(plot.title =       element_text(face = "bold", size = rel(1.2), hjust = 0.5,
                                          margin = margin(0,0,2,0)),
          text =             element_text(face = "bold"),
          axis.line =        axis.line,
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border =       panel.border,
          plot.background =  element_rect(fill = "transparent", colour = NA),
          axis.title =       element_text(face = "bold", size = rel(1)),
          axis.title.y =     element_text(angle = 90, vjust = 2),
          axis.title.x =     element_text(vjust = -0.1),
          axis.text =        element_text(face = "bold"),
          axis.text.x =      element_text(angle = angle,
                                          hjust = ifelse(angle %in% c(45, 90, 270), 1, 0.5),
                                          vjust = ifelse(angle %in% c(-30, 0, 90, 270), 0.5, 1)),
          axis.line.x =      element_line(colour = "black"),
          axis.line.y =      element_line(colour = "black"),
          axis.ticks.x =     element_line(),
          axis.ticks.y =     element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key =       element_rect(fill = "transparent", colour = NA),
          legend.position =  "bottom",
          legend.direction = "horizontal",
          legend.box =       "vetical",
          legend.margin =    margin(0, 0, 0, 0),
          legend.title =     element_blank(),
          legend.text =      element_text(size = 10),
          legend.background =  element_rect(fill = "transparent", colour = NA),
          plot.margin =      unit(c(1, 1, 1, 1), "mm"),
          strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
          strip.text =       element_text(face = "bold"),
          plot.tag =         element_text(size = 25, family = "serif",
                                          hjust = 0, vjust = -1)
    )
}
