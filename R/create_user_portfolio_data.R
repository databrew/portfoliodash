#' Create an initial user_portfolio data table in the psql database
#'
#' Fetch a table or a specific query of a table from the database.
#' @param users A SQL query as a chracter string of length 1
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return A "user_portfolio" table added to PSQL database
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @export

create_user_portfolio_data <- function(users = NULL,
                                       connection_object = NULL){
  
  # If no users, use the one from the package
  if(is.null(users)){
    users <- portfoliodash::users
  }

  # If not connection object, try to find one
  if(is.null(connection_object)){
    message(paste0('No connection_object provided. Will try ',
                   'to find a credentials file.'))
    # Get credentials
    the_credentials <- credentials_extract()
    # Establish the connection
    connection_object <- credentials_connect(the_credentials)
  }
  
  # Get the as_portfolio table
  as_portfolio <- get_data(tab = 'as_portfolio',
                           connection_object = connection_object)
  # For now, just keeping 200 for speeds sake
  as_portfolio <- sample_n(as_portfolio, 200)
  
  # Create a table of users' portfolios
  user_portfolio <- expand.grid(username = users$username,
                                project_id = unique(as_portfolio$project_id)) %>%
    arrange(username)
  user_portfolio <- data.frame(user_portfolio)
  # Upload it to the database

  copy_to(connection_object, user_portfolio, "user_portfolio",
          temporary = FALSE,
          overwrite = TRUE)
}
