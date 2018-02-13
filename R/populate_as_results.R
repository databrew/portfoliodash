#' Populate the as_results relation 
#' 
#' Populate the as_results relation from "portfolio.as_results.csv" (emailed to developer)
#' @param csv The location of the "portfolio.as_results.csv" file, emailed by Soren
#' @param the_co An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return The local "portfolios.as_results" relation in the postgresql database will be updated
#' @import dplyr
#' @import RPostgreSQL
#' @import readr
#' @import dbplyr
#' @export
#' @examples
#' 2+2

populate_as_results <- function(csv = 'data/portfolio.as_results.csv',
                      the_co = NULL){
  # If not connection object, try to find one
  if(is.null(the_co)){
    message(paste0('No connection_object provided. Will try ',
                   'to find a credentials file.'))
    # Get credentials
    the_credentials <- credentials_extract()
    # Establish the connection
    the_co <- credentials_connect(the_credentials)
  }
  
  # Read the csv
  ar <- read_csv(csv)
  # Upload to results
  # results <- get_data(tab = 'as_results', connection_object = the_co)
  copy_to(the_co, 
          ar, 
          dbplyr::in_schema("portfolio", "as_results"),
          temporary = FALSE,
          overwrite = TRUE)
  
  }