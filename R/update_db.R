#' Update DB
#' 
#' Update the portfolio.user_portfolio table
#' @param u The username of the person whose portfolio is being modified
#' @param project_ids A vector of project_ids to be associated with that user
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return The local "portfolios.user_portfolio" relation in the postgresql database will be updated
#' @import dplyr
#' @import RPostgreSQL
#' @export
#' @examples
#' 2+2

update_db <- function(u,
                      project_ids,
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
  # Get the current user_portfolio table
  up <- get_data(tab = 'user_portfolio', connection_object = connection_object)
  # Remove all data associated with the username
  up <- up %>% filter(username != u)
  # Define new data
  new_data <- data_frame(username = as.character(u),
                         project_id = project_ids)
  # Add new data to up
  up <- bind_rows(up, new_data)
  # Replace the table in db
  copy_to(connection_object, 
          up, 
          "user_portfolio",
          temporary = FALSE,
          overwrite = TRUE)
  message('Modified the portfolio of user ', u)
}