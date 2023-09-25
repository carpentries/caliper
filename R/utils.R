get_env_var <- function(var) {
  result <- Sys.getenv(var)

  if (identical(result, "")) {
    stop(sQuote(var), " not set as environment variable.", call. = FALSE)
  }

  result
}
