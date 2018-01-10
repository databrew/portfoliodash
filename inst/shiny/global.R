#############################################
#                                           #
#   KM Portfolio Dashboard                  #
#   Version 0.4.1 (Alpha)                   #
#   27/09/2017                              #
#   Christian Ruckteschler                  #
#                                           #
#############################################


# Give more memory to java environment (not required when loading data from local csv)
#options(java.parameters = "- Xmx2048m")
library(xlsx)
library(shiny)
library(shinydashboard)
library(timevis)
library(rCharts)
library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(Hmisc)

# Read in the as portfolio table directly from the database
quick_load <- TRUE
if('as_portfolio.RData' %in% dir() & quick_load){
  load('as_portfolio.RData')
} else {
  co <- src_postgres(dbname = 'portfolio')
  as_portfolio <- portfoliodash::get_data(query = NULL,
                                          tab = 'as_portfolio',
                                          dbname = 'portfolio',
                                          connection_object = co)
  # Keep only a few for now
  as_portfolio <- sample_n(as_portfolio, 200)
  save(as_portfolio, file = 'as_portfolio.RData')
}

if('user_portfolio.RData' %in% dir() & quick_load){
  load('user_portfolio.RData')
} else {
  co <- src_postgres(dbname = 'portfolio')
  user_portfolio <- portfoliodash::get_data(query = NULL,
                                          tab = 'user_portfolio',
                                          dbname = 'portfolio',
                                          connection_object = co)
  # Keep only a few for now
  save(user_portfolio, file = 'user_portfolio.RData')
}


# Define filter choices
filter_choices <- c('Is (among)',
                    'Is not (among)',
                    'Is greater than',
                    'Is greater than or equal to',
                    'Is less than',
                    'Is less than or equal to')
filter_choices_character <- c('Is (among)', 'Is not (among)')
'%!in%' <- function(x,y)!('%in%'(x,y))
operator_dictionary <- 
  data_frame(name = c('Is greater than',
                      'Is greater than or equal to',
                      'Is less than or equal to',
                        'Is less than',
                        'Is (among)',
                        'Is not (among)'),
             operator = c('>',
                          '>=',
                          '<=',
                          '<',
                          '%in%',
                          '%!in%'))
classify <- function(x){
  x <- class(x)
  x <- ifelse(x == 'integer', 'numeric', 
              ifelse(x == 'factor', 'character',
                     x))
  return(x)
}
filter_classes <- unlist(lapply(as_portfolio, classify))
make_filter <- function(variable, operator, selection){
  operator <- operator_dictionary$operator[operator_dictionary$name == operator]
  if(!is.numeric(selection[1])){
    selection <- paste0('c(', paste0(paste0("'", selection, "'"), collapse = ', '), ')')
  } else {
    selection <- paste0('c(', paste0(selection, collapse = ', '), ')')
  }
  
  paste0(variable, operator, selection, collapse = ' ')
}

var_choices <- names(as_portfolio)
var_choices <- var_choices[!grepl('_id', var_choices, fixed = TRUE)]
var_choices_labels <- Hmisc::capitalize(gsub('_', ' ', var_choices))
names(var_choices) <- var_choices_labels
# Set working directory
# Two options depending on how OneDrive files are stored on the local machine

dir1 <- paste0(dirname(path.expand("~")),"/WBG/Sinja Buri - FIG SSA MEL/Program Operations/Projects/Knowledge Product - Dashboards & Viz/Portfolio Dashboard/portfolio_dashboard")
dir2 <- paste0(dirname(path.expand("~")),"/WBG/Sinja Buri - Program Operations/Projects/Knowledge Product - Dashboards & Viz/Portfolio Dashboard/portfolio_dashboard")

if (file.exists(dir1)) {
  setwd(dir1)
} else if(file.exists(dir2)){
  setwd(dir2)
} else {
  message('No OneDrive directory for the portfolio dashboard exists. Using the contents of this directory.')
}

# LOAD DATA
dir_longevity_data <- "data/longevity_data.csv"
dir_portfolio_data <- "data/portfolio_funding_data.csv"
dir_portfolio_volume <- "data/portfolio_volume.csv"

longevity_data <- data.frame(read.csv(dir_longevity_data, blank.lines.skip = TRUE))
portfolio_data <- data.frame(read.csv(dir_portfolio_data, blank.lines.skip = TRUE))
portfolio_vol_data <- data.frame(read.csv(dir_portfolio_volume, blank.lines.skip = TRUE))

portfolio_vol_data[is.na(portfolio_vol_data)] <- 0
portfolio_vol_mat = as.matrix(portfolio_vol_data[1:3,2:5])
rownames(portfolio_vol_mat) <- portfolio_vol_data[,"Category"]
portfolio_vol_mat <- t(portfolio_vol_mat)
portfolio_vol_mat <- portfolio_vol_mat / 1000000 # convert to numbers in mio USD






#######################################################################################
## 1) Time and Money Charts (Longevity)
#######################################################################################


dir_add_details <- "data/fig_ssa_addtional_details.csv"
add_data <- data.frame(read.csv(dir_add_details, blank.lines.skip = TRUE)[c("project_id", "funding_source", "type", "mcf_extended")])
longevity_data <- merge(longevity_data, add_data, by = "project_id", all.x = TRUE)

longevity_data$dataset_date <- as.Date(longevity_data$dataset_date, format="%d/%m/%Y", tz = "GMT")
longevity_data$graph_start_date <- as.Date(longevity_data$graph_start_date, format="%d/%m/%Y", tz = "GMT")
longevity_data$project_end_date <- as.Date(longevity_data$project_end_date, format="%d/%m/%Y", tz = "GMT")

active <- longevity_data$active_duration > 0
closed <- longevity_data$closed_duration > 0
pipeline <- longevity_data$pipeline_duration > 0
longevity_data <- cbind(longevity_data, active, closed, pipeline)


# extract region and business line from the portfolio_set column
longevity_data$region <- substr(longevity_data$portfolio_set, 1, 3)
longevity_data$bline <- substr(longevity_data$portfolio_set, 5, nchar(as.character(longevity_data$portfolio_set)))

# Replace Missing Values for Added Variables
longevity_data$funding_source <- as.character(longevity_data$funding_source)
longevity_data$type <- as.character(longevity_data$type)
longevity_data[is.na(longevity_data$funding_source), "funding_source"] <- "Undefined"
longevity_data[is.na(longevity_data$type), "type"] <- "Undefined"
longevity_data[is.na(longevity_data$mcf_extended), "mcf_extended"] <- 0
longevity_data$mcf_extended <- as.integer(longevity_data$mcf_extended)


# Factors
dir_factors <- "data/factors.csv"
factors <- read.csv(dir_factors, blank.lines.skip = TRUE)
funding_src <- factors[factors$factor=="funding_source",2:ncol(factors)]
funding_src <- funding_src[funding_src != ""]
institution_type <- factors[factors$factor=="type",2:ncol(factors)]
institution_type <- institution_type[institution_type != ""]
custom_portfolio <- factors[factors$factor=="custom_portfolio",2:ncol(factors)]
custom_portfolio <- custom_portfolio[custom_portfolio != ""]


# Calcualte end of current fiscal year
today <- Sys.Date()
year <- format(today, "%Y")
comp <- as.Date(paste0(year, "-07-01"))
if(today > comp) {
  end_fiscal_year <- as.Date(paste0(as.integer(year)+1, "-07-01"))
} else {
  end_fiscal_year <- comp
}


timeline <- timevis()


# Add HTML Styling
#####################################################

# add table
longevity_data$html <- paste("<table width=310 style='font-size: 13px;'><tr><td>", longevity_data$project_name, "</td><td style='width:50px; text-align:right;'>", 
                             longevity_data$burn_rate ,"%</td><td style='width:45px; text-align:right;'>", 
                             sprintf("$M %3.2f", longevity_data$prorated_total_funds_managed_by_ifc/1000000) ,"</td></tr></table>", 
                             sep = "")


# coloring based on project status
longevity_data$color <- "blue"
longevity_data[longevity_data$pipeline_duration > 0, "color"] <- "lightblue"
longevity_data[longevity_data$closed_duration > 0, "color"] <- "orange"


#######################################################################################
## 2) Portfolio Funding Chart
#######################################################################################

# Get data matrix from data frame, adjust units to $M, and transpose
portfolio_mat <- t(data.matrix(portfolio_data[,3:5]))
portfolio_mat <- portfolio_mat / 1000000
rownames(portfolio_mat) <- c("Active Funds", "Closed Funds", "Pipeline Funds")

# Get dates from data frame to label the x-axis
quarters <- format(as.Date(portfolio_data[,1], format="%d/%m/%Y", tz = "GMT"), format = "%b-%y")

# Get number of projects for second x-axis
projects <- t(data.matrix(portfolio_data[,6]))
