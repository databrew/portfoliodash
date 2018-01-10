#' Filter portfolio
#' 
#' Filter an as_portfolio table with any number of filter conditions. Each condition is a separate argument, wrapped in quotations.
#' @return A filtered as_portfolio table
#' @import dplyr
#' @export
#' @examples
#' fp <- filter_portfolio(portfolio = as_portfolio)
#' fp <- filter_portfolio(portfolio = as_portfolio, 'ifc_investee_pct == 100', 'project_name != "SI Aureos GTAA"')
#' fp <- filter_portfolio(portfolio = as_portfolio, 'ifc_investee_pct == 100', 'project_name != "SI Aureos GTAA"')

filter_portfolio <- function(portfolio, ...){
  arguments <- c(as.list(environment()), 
                                      list(...))
  if(length(arguments) == 1){
    return(portfolio)
  }
  arguments <- arguments[2:length(arguments)]
  arguments <- unlist(arguments)
  for(i in 1:length(arguments)){
    portfolio <- portfolio %>%
      dplyr::filter_(arguments[i])
  }
  return(portfolio)
}
