#' Create an initial portfolio_indicators table in the psql database
#'
#' Create an initial portfolio_users data table in the psql database
#' @param portfolio_indicators A dataframe with the columns portfolio_id, indicator_id
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return A "portfolio_indicators" table added to PSQL database
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @export

create_portfolio_indicators_db <- function(portfolio_indicators = NULL,
                                           connection_object = NULL){
  
  # If not connection object, try to find one
  if(is.null(connection_object)){
    message(paste0('No connection_object provided. Will try ',
                   'to find a credentials file.'))
    # Get credentials
    the_credentials <- credentials_extract()
    # Establish the connection
    connection_object <- credentials_connect(the_credentials)
  }
  
  # If not table supplied, create one based on database
  if(is.null(portfolio_indicators)){
    as_portfolio <- get_data(query = 'SELECT * FROM portfolio.as_portfolio',
                             connection_object = connection_object)
    portfolio_indicators <- as_portfolio %>%
      dplyr::select(primary_business_line_code, project_id) %>%
      dplyr::rename(portfolio_name = primary_business_line_code) %>%
      mutate(portfolio_id = as.integer(factor(portfolio_name))) %>%
      dplyr::filter(!duplicated(portfolio_id)) %>%
      dplyr::select(portfolio_id)
    portfolio_indicators <-
      expand.grid(portfolio_id = portfolio_indicators$portfolio_id,
                  indicator_id = 1:3) # Not sure what indicators should actually be here
    
  }
  
  # Upload it to the database
  copy_to(connection_object, portfolio_indicators,
          dbplyr::in_schema("portfolio", "portfolio_indicators"),
          temporary = FALSE,
          overwrite = TRUE)
}
