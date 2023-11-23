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
    facet_grid(year ~ quarter)
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

#' Add Data Labels to a ggplot2 Plot
#'
#' This function adds text labels to a ggplot2 plot. It supports two types of labels: counts and percentages.
#' The labels can be customized in terms of their accuracy, color, vertical adjustment, and font face.
#'
#' @param label_type A character string specifying the type of label to add.
#'                   Acceptable values are "count" for count labels and "percent" for percentage labels.
#' @param accuracy An optional numeric value specifying the number of decimal places for percentage labels.
#'                 Only applicable if `label_type` is "percent". Default is 0.1.
#' @param color A character string specifying the color of the text labels. Default is "white".
#' @param vjust A numeric value for vertical adjustment of the text labels.
#'              Positive values move text up, and negative values move it down. Default is 1.6.
#' @param fontface A character string specifying the font face for text labels.
#'                 Common values are "plain", "bold", "italic", "bold.italic". Default is "bold".
#' @param nudge_x A numeric value specifying horizontal adjustment of the text labels.
#'                Positive values move text to the right, and negative values move it to the left. Default is 0.
#'
#' @importFrom ggplot2 geom_text
#' @importFrom scales label_percent
#'
#' @return A ggplot2 layer with the specified text labels added.
#' @export
#'
#' @examples


add_text_geom <- function(label_type = "count", accuracy = 0.1, color = "white", vjust = 1.6, fontface = "bold", nudge_x = 0) {
  if (label_type == "count") {
    plot + geom_text(aes(label = count), color = color, vjust = vjust, nudge_x = nudge_x, fontface = fontface)
  } else if (label_type == "percent") {
    geom_text(aes(label = scales::label_percent(accuracy = accuracy)(percent)), color = color, vjust = vjust, fontface = fontface)
  }
}

#' Format and Text Wrap X Axis Labels in ggplot2
#'
#' This function applies text wrapping to x-axis labels in a ggplot2 plot.
#' It allows for customizing the width of the wrap, making long labels more readable by breaking them into multiple lines.
#'
#' @param labels A vector of labels to be used for the x-axis.
#'               These labels should correspond to the categories or values present on the x-axis.
#' @param wrap_width An integer specifying the maximum width (in characters) before wrapping a label onto the next line.
#'                   The default value is 15.
#'
#' @return A ggplot2 scale layer, specifically a modified `scale_x_discrete`,
#'         with the labels formatted to wrap text based on the specified `wrap_width`.
#' @export
#'
#' @examples
#' # Example using the diamonds dataset from ggplot2
#' library(ggplot2)
#' data(diamonds)
#'
#' # Create artificial long labels for illustration
#' long_labels <- c("Fair Quality Cut", "Good Quality Cut", "Very Good Quality Cut",
#'                  "Premium Quality Cut", "Ideal Quality Cut")
#' names(long_labels) <- levels(diamonds$cut)
#'
#' basic_plot <- ggplot(diamonds, aes(x = cut, y = price)) + geom_bar(stat = "identity")
#' # Apply custom scale with wrapped labels
#' basic_plot + custom_scale_x_discrete(labels = long_labels, wrap_width = 10)
custom_scale_x_discrete <- function(labels, wrap_width = 15) {
  scale_x_discrete(
    drop = FALSE,
    labels = function(x) lapply(
      strwrap(labels, width = wrap_width, simplify = FALSE),
      paste,
      collapse = "\n"
    )
  )
}

#' Add Percent Sign to Data Label
#'
#' This function formats numeric values as percentages. It rounds the numbers to the specified decimal places and appends a percent sign.
#'
#' @param x Numeric vector; the values to be formatted as percentages.
#' @param decimal Integer; the number of decimal places to round the numeric values.
#'               Defaults to 1, indicating one decimal place.
#'
#' @return A character vector with the numeric values formatted as percentages.
#' @export
#'
#' @examples
#' # Example usage of percent_label
#' numeric_values <- c(0.123, 0.456, 0.789)
#' percent_labels <- percent_label(numeric_values)
#' print(percent_labels)
#' # Output: "12.3%", "45.6%", "78.9%"
percent_label <- function(x, decimal = 1) {
  paste0(round(x, decimal), "%")
}
