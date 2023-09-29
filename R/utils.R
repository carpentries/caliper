#' Retrieve Environment Variable Value
#'
#' This function fetches the value of a specified environment variable.
#' If the environment variable is not set, the function will stop execution and return an error message.
#'
#' @param var Character; The name of the environment variable to retrieve.
#'
#' @return The value of the specified environment variable as a character string.
#' Throws an error if the environment variable is not set.
#'
#' @examples
#' \dontrun{
#'   # Set an example environment variable
#'   Sys.setenv(EXAMPLE_VAR = "example_value")
#'
#'   # Fetch the value of the environment variable
#'   val <- get_env_var("EXAMPLE_VAR")
#'   print(val)  # Output should be "example_value"
#' }
get_env_var <- function(var) {
  result <- Sys.getenv(var)

  if (identical(result, "")) {
    stop(sQuote(var), " not set as environment variable.", call. = FALSE)
  }

  result
}
