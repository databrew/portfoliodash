#' Create an initial portfolio_projects table in the psql database
#'
#' Create an initial portfolio_users data table in the psql database
#' @param portfolio_projects A dataframe with the columns portfolio_id, project_id
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return A "portfolio_projects" table added to PSQL database
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @export

create_portfolio_projects_db <- function(portfolio_projects = NULL,
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
  if(is.null(portfolio_projects)){
    as_portfolio <- get_data(tab = 'as_portfolio',
                             connection_object = connection_object)
    portfolio_projects <- as_portfolio %>%
      dplyr::select(primary_business_line_code, project_id) %>%
      dplyr::rename(portfolio_name = primary_business_line_code) %>%
      mutate(portfolio_id = as.integer(factor(portfolio_name))) %>%
      group_by(portfolio_id, project_id) %>%
      tally %>%
      ungroup %>%
      dplyr::select(-n)
  }
  
  # Upload it to the database
  copy_to(connection_object, portfolio_projects, "portfolio_projects",
          temporary = FALSE,
          overwrite = TRUE)
}
