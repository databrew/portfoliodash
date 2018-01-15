#' Create an initial users table
#'
#' Create a users table in the database
#' @param users A dataframewith the columns user_id, name, email, upi, and last_login
#' @param connection_object An open connection to adatabase (as created through \code{credentials_extract} and \code{credentials_connect} or \code{credentials_now}); if \code{NULL}, the function will try to create a \code{connection_object} by retrieving user information from the \code{credentials/credentials.yaml}
#' in or somewhere upwards of the working directory.
#' @return A "users" table added to PSQL database
#' @import DBI
#' @import dplyr
#' @import RPostgreSQL
#' @import babynames
#' @export

create_users_db <- function(users = NULL,
                            connection_object = NULL){
  
  # If no users, create one
  if(is.null(users)){
    babynames <- babynames::babynames
    users <- 
      data_frame(user_id = 1:26,
                 name = babynames$name[sample(1:nrow(babynames), 26, replace = FALSE)])
    users$email <- paste0(users$name, '@aol.com')
    users$upi <- sample(100000:999999, nrow(users), replace = FALSE)
    users$last_login <- as.POSIXct(Sys.time())
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
  
  copy_to(connection_object, users, "users",
          temporary = FALSE,
          overwrite = TRUE)
}
