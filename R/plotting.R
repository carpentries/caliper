# Generate Bar Plots with Faceting and Custom Carpentries Theme
#'
#' This function generates a bar plot using `ggplot2` and a custom Carpentries theme.
#' It allows for specifying variables for the x-axis, y-axis, and fill color. The plot is faceted by year and quarter.
#' Text labels indicating the y-values are also included above the bars.
#'
#'
#' @param dat Dataframe; The data frame containing the data to be plotted.
#' @param x Character; The name of the variable to be plotted on the x-axis.Character; The name of the variable to be plotted on the x-axis.
#' @param y Character; The name of the variable to be plotted on the y-axis.
#' @param fill Character; Optional. The name of the variable to be used for fill colors.
#'
#' @return A ggplot object representing the generated plot.
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_col geom_text facet_grid
#' @importFrom paintbrush theme_carpentries
#
#'
#' @examples
#' \dontrun{
#'   # Assume 'data_frame' is a data frame with appropriate columns
#'   example_plot <- generate_plot(dat = data_frame, x = "variable_x", y = "variable_y", fill = "variable_fill")
#'   print(example_plot)
#' }
#'
generate_plot <- function(dat, x, y, fill = NULL) {
  plot <- ggplot(dat, aes_string(x = x, y = y, fill = fill)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = after_stat(y)), stat = "identity", vjust = -0.3, position = position_dodge(width = 0.9)) +
    facet_grid(year ~ quarter) +
    theme_carpentries()
  return(plot)
}


#' Save and Export ggplot2 Plots to a Specified Directory
#'
#' This function saves a ggplot2 plot as a PNG file in a specified directory.
#' The plot will be saved under the 'programs/<dir>/output/' folder with the provided plot name.
#'
#' @param dir Character; The name of the subdirectory under 'programs' where the plot will be saved.
#' @param plot ggplot; The ggplot2 plot object that you want to save.
#'
#' @return Saves the plot as a PNG file and returns the full path where the file is saved.
#'
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assume 'some_plot' is a ggplot2 plot object
#'   saved_path <- save_plot(dir = "exampleDir", plot = some_plot)
#'   print(saved_path)
#' }
save_plot <- function(dir, plot) {
  path <- paste0('programs/', dir, '/output/', plot, '.png')
  print(path)
  ggsave(path, plot = plot, device = 'png', dpi = 300, width = 4, height = 3)
}
