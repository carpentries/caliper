# Function to Generate Plot
#' Generate Plots
#'
#' @param dat
#' @param x
#' @param y
#' @param fill
#'
#' @return plot
#' @export
#' @import ggplot2
#'
#' @examples
generate_plot <- function(dat, x, y, fill = NULL) {
  plot <- ggplot(dat, aes_string(x = x, y = y, fill = fill)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = after_stat(y)), stat = "identity", vjust = -0.3, position = position_dodge(width = 0.9)) +
    facet_grid(year ~ quarter) +
    theme_carpentries()
  return(plot)
}


#' Save and Export Plots
#'
#' @param dir
#' @param plot
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
save_plot <- function(dir, plot) {
  path <- paste0('programs/', dir, '/output/', plot, '.png')
  print(path)
  ggsave(path, plot = plot, device = 'png', dpi = 300, width = 4, height = 3)
}
