#' Fetch Redash Data
#'
#' @param query_id Can be found in redash.
#'
#' @return the data frame from the redash query supplied
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#'
#' test <- fetch_redash(17)
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

#' Fetch Airtable Data
#'
#' @param id the base id. Can be found in Airtable API documentation
#' @param db_name the Database name. Can be found in Airtable API documentation.
#'
#' @return data frame
#' @export
#'
#' @examples

fetch_airtable <- function(id, db_name) {
  data <- airtabler::airtable(id, db_name)$`db_name`$select_all()
  #
  return(data)

}
