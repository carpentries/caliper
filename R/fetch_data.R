#' Fetch Data from Redash
#'
#' ' This function fetches data from a Redash instance using a specific query ID and API key.
#' The API key is fetched from an environment variable named `REDASH_QUERY_<query_id>`.
#' The data is read into a data frame using `utils::read.csv` and returned.
#'
#'
#' @param query_id Numeric or Character; The query ID corresponding to the Redash query. This ID can be found in the Redash UI.Numeric or Character; The query ID corresponding to the Redash query. This ID can be found in the Redash UI.
#'
#' @return A data frame containing the results of the specified Redash query.
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   # Make sure to set environment variable for API key like so:
#'   # Sys.setenv(REDASH_QUERY_17 = "<your_api_key>")
#'   test <- fetch_redash(17)
#' }
#'
fetch_redash <- function(query_id) {
  api_key <- get_env_var(paste0("REDASH_QUERY_", query_id))
  url <-
    paste0(
      "https://redash.carpentries.org/api/queries/",
      query_id,
      "/results.csv?api_key=",
      api_key
    )
  data <- utils::read.csv(url, stringsAsFactors = FALSE)
  return(data)
}

#' Fetch Data from Airtable
#'
#'Fetches data from an Airtable base into a data frame. This function relies on the `airtabler` package.
#' You can find the required `id` and `db_name` parameters in your Airtable API documentation.
#'
#'
#' @param id Character; The base ID corresponding to your Airtable base. This can be found in the Airtable API documentation.
#' @param db_name Character; The name of the database (table) within the Airtable base to fetch data from.
#'
#' @return A data frame containing all records from the specified Airtable database (table).
#' @importFrom airtabler airtable

#' @export
#'
#' @examples
#' \dontrun{
#'   # Make sure to have `airtabler` package installed and set up
#'   # Example usage:
#'   data_fetched <- fetch_airtable(id = "your_base_id", db_name = "your_db_name")
#' }

fetch_airtable <- function(id, db_name) {
  data <- airtabler::airtable(id, db_name)$`db_name`$select_all()
  #
  return(data)

}
