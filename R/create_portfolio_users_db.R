#' Create an initial portfolio_users data table in the psql database
#'
#' Create an initial portfolio_users data table in the psql database
#' @param portfolio_users A dataframe with the columns portfolio_id, user_id, is_creator, and is_admin
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return A "portfolio_users" table added to PSQL database
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @export

create_portfolio_users_db <- function(portfolio_users = NULL,
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

  # Use the portfolio projects table to subscribe users
  # to some portfolios
  if(is.null(portfolio_users)){
    pp <- get_data(query = 'SELECT * FROM portfolio.portfolio_projects',
                   connection_object = connection_object)
    pp <- pp %>% dplyr::select(portfolio_id) %>%
      dplyr::filter(!duplicated(portfolio_id))
    pu <- expand.grid(portfolio_id = pp$portfolio_id,
                      user_id = 1:3)
    # Randomly re-order rows
    pu <- pu[sample(1:nrow(pu), nrow(pu), replace = FALSE),]
    # Keep only 3 portfolios per user
    pu <- pu %>%
      mutate(dummy = 1) %>%
      group_by(user_id) %>%
      mutate(d = cumsum(dummy)) %>%
      ungroup %>%
      filter(d <= 3) %>%
      dplyr::select(-dummy, -d)
    # assign admin / creator
    pu <- pu %>%
      mutate(dummy = 1) %>%
      group_by(portfolio_id) %>%
      mutate(d = cumsum(dummy)) %>%
      mutate(is_creator = d ==1) %>%
      mutate(is_admin = is_creator) %>%
      ungroup %>%
      dplyr::select(-dummy, -d)
    portfolio_users <- 
      pu
  }
  
  # Upload it to the database
  copy_to(connection_object, portfolio_users, 
          dbplyr::in_schema("portfolio", "portfolio_users"),
          temporary = FALSE,
          overwrite = TRUE)
}
