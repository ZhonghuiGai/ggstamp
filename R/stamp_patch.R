#' Patch figures
#'
#' @param p.bar the output of stamp_bar
#' @param p.CI the output of stamp_CI
#' @param p.values the output of stamp_Point
#'
#' @return a patched ggplots object
#' @export
#'
#' @examples
#' stamp_patch(stamp_bar(stamp_input()), stamp_CI(stamp_input()), stamp_pvalue_point(stamp_input()))
stamp_patch <- function(p.bar, p.CI, p.values){
  library(patchwork)
  p <- p.bar + p.CI + p.values + plot_layout(widths = c(4,6,2), tag_level = "new")
  return(p)
}

