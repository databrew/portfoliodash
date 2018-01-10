#' Prettify a table for HTML documents
#'
#' Create a data table object from a dataframe with optional aesthetic improvements. Userful for inclusion in .Rmd files being knitted to HTML
#' @param the_table The dataframe to be prettified
#' @param remove_underscores_columns Whether to remove underscores in column names and replace them with spaces
#' @param cap_columns Whether to capitalize the first letter of the names of columns
#' @param cap_characters Whether to capitalize the first letter of elements of character vectors
#' @param comma_numbers Whether to include a comma between every three digits of numeric columns
#' @param date_format The format for printing date columns
#' @param round_digits How many digits to round numberic columns to
#' @param remove_row_names Whether to remove row names
#' @param remove_line_breaks Whether to remove line breaks from the elements of columns
#' @param nrows The number of rows to show
#' @param download_options Whether to show options for downloading the table
#' @return A data table ready for inclusion in an .Rmd to be knitted to HTML
#' @importFrom Hmisc capitalize
#' @importFrom DT datatable
#' @importFrom scales comma
#' @export

prettify <- function(the_table,
                     remove_underscores_columns = TRUE,
                     cap_columns = TRUE,
                     cap_characters = TRUE,
                     comma_numbers = TRUE,
                     date_format = '%B %d, %Y',
                     round_digits = 2,
                     remove_row_names = TRUE,
                     remove_line_breaks = TRUE,
                     data_table = TRUE,
                     nrows = 5,
                     download_options = FALSE){
  
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x){unlist(class(x))[1]})
  
  if(cap_columns){
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  
  if(remove_underscores_columns){
    names(the_table) <- gsub('_', ' ', names(the_table))
  }
  
  for (j in 1:ncol(the_table)){
    the_column <- the_table[,j]
    the_class <- classes[j][1]
    if(the_class %in% c('character', 'factor')){
      if(cap_characters){
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if(remove_line_breaks){
        the_column <- gsub('\n', ' ', the_column)
      }
    } else if(the_class %in% c('POSIXct', 'Date')){
      the_column <- format(the_column, format = date_format)
    } else if(the_class %in% c('numeric', 'integer')){
      the_column <- round(the_column, digits = round_digits)
      if(comma_numbers){
        the_column <- scales::comma(the_column)
      }
    }
    the_table[,j] <- the_column
  }
  if(remove_row_names){
    row.names(the_table) <- NULL
  }
  if(data_table){
    if(download_options){
      the_table <- DT::datatable(the_table,
                                 options = list(pageLength = nrows,
                                                # buttons = c('copy',
                                                #             'csv',
                                                #             'excel',
                                                #             'pdf'),
                                                dom = 'Bfrtip',
                                                buttons = 
                                                  list('copy', 'print', list(
                                                    extend = 'collection',
                                                    buttons = 'csv',# c('csv', 'excel', 'pdf'),
                                                    text = 'Download'
                                                  ))),
                                 rownames = FALSE,
                                 extensions = 'Buttons')
    } else {
      the_table <- DT::datatable(the_table,
                                 options = list(pageLength = nrows,
                                                columnDefs = list(list(className = 'dt-right', 
                                                                       targets = 0:(ncol(the_table) - 1)))),
                                 rownames = FALSE)
    }
    
  }
  return(the_table)
}
