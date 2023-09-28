# Function to Generate Plot
#' Generate Plots
#'
#' @param dat data frame
#' @param x variable on x axis
#' @param y variable on y axis
#' @param fill variable to be used at fill
#'
#' @return plot
#' @export
#' @import ggplot2
#' @import paintbrush
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
#' @param dir a file directory to save the plot
#' @param plot the name of the plot
#'
#' @return a file
#' @import ggplot2
#' @export
#'
#' @examples
save_plot <- function(dir, plot) {
  path <- paste0('programs/', dir, '/output/', plot, '.png')
  print(path)
  ggsave(path, plot = plot, device = 'png', dpi = 300, width = 4, height = 3)
}
