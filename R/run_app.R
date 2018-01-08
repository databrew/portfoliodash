#' Run app
#' 
#' Run the Shiny portfolio web application
#' @return Web application served
#' @importFrom shiny runApp
#' @export

run_app <- function(){
  app_location <- paste0(system.file(package = 'portfoliodash'), '/shiny/app.R')
  shiny::runApp(app_location)
}
