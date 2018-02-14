library(shiny)
library(tidyr)
library(shinydashboard)
library(DT)
library(lubridate)
library(googleVis)
library(timevis)
library(rCharts)
library(dplyr)
library(tidyr)
library(Hmisc)
library(yaml)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(RPostgreSQL)
library(DBI)
library(scales)
library(httr)
library(readr)
library(babynames)
library(rlang)
library(scales)
library(pool) # devtools::install_github("rstudio/pool")

options(scipen = '999')

# Source package files (if not having installed "portfoliodash")
package_files <- dir('R')
for(i in 1:length(package_files)){
  source(paste0('R/', package_files[i]))
}

# Define whether being run in a development env (ie, "local" on Joe's computer)
# or on production
# We only do this so as to use cached data (rather than database) when testing locally
local <- grepl('joebrew', getwd())
quick_load <- TRUE

# Define function for loading in data
load_data <- function(local = FALSE,
                      quick_load = FALSE,
                      table = 'as_portfolio'){
  file_name <- paste0(table, '.RData')
  # Read in the as portfolio table directly from the database
  if((file_name %in% dir() & quick_load) | !local){
    load(file_name)
  } else {
    co <- src_postgres(dbname = 'portfolio')
    x <- get_data(query = paste0('SELECT * FROM portfolio.', table),
                             dbname = 'portfolio',
                             connection_object = co)
    save(x, file = file_name)
  }
  assign(table,
         x,
         envir = .GlobalEnv)
}

# Load the as_portfolio table
load_data(local = local,
          quick_load = quick_load,
          table = 'as_portfolio')
# Load the as_results table (Not doing yet, because not using yet)
# load_data(local = local,
#           quick_load = quick_load,
#           table = 'as_results')
# Load the portfolio_indicators table (Not doing yet, because not using yet)
# load_data(local = local,
#           quick_load = quick_load,
#           table = 'portfolio_indicators')
# Load the portfolio_projects table
load_data(local = local,
          quick_load = FALSE,
          table = 'portfolio_projects')
# Load the portfolio_users table
load_data(local = local,
          quick_load = FALSE,
          table = 'portfolio_users')
# Load the portfolios table
load_data(local = local,
          quick_load = FALSE,
          table = 'portfolios')
# Load the users table
load_data(local = local,
          quick_load = FALSE,
          table = 'users')

# Define filter choices
filter_choices <- c('Is (among)',
                    'Is not (among)',
                    'Is greater than',
                    'Is less than')
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
dir_longevity_data <- "flat_files/longevity_data.csv"
dir_portfolio_data <- "flat_files/portfolio_funding_data.csv"
dir_portfolio_volume <- "flat_files/portfolio_volume.csv"

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


dir_add_details <- "flat_files/fig_ssa_addtional_details.csv"
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
dir_factors <- "flat_files/factors.csv"
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


# Height for main page charts
main_page_plot_height_num <- 200
main_page_plot_height <- paste0(main_page_plot_height_num * 1.4, 'px')

# Make some plots
df <- expand.grid(date = as.Date(paste0('01-', quarters),
                                 format = '%d-%b-%y'),
                  key = letters[1:5],
                  fac = c('Registered users/accounts',
                          '# of agents',
                          'Some other metric',
                          'And another one'))
df$val <- jitter(as.numeric(df$date) - 15000, factor = 30)
cols <- colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(length(unique(df$key)))
g1 <- ggplot(data = df,
             aes(x = date,
                 y = val,
                 group = key,
                 color = key)) +
  geom_line() +
  facet_wrap(~fac) +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(name = '',
                     values = cols)
g2 <- ggplot(data = df,
             aes(x = date,
                 y = val,
                 group = key,
                 color = key)) +
  geom_smooth() +
  theme_fivethirtyeight() +
  scale_color_manual(name = '',
                     values = cols)
g3 <- ggplot(data = df %>% filter(key == 'a',
                                  date == '2017-07-01'),
             aes(x = fac,
                 y = val)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_fivethirtyeight()

# Create spending fish background layer
gg_spending_fish <- function(){
  require(splines)
  data <- data.frame(x = 1:100,
                     y = seq(-30, 30, length = 100))
  data$ymax <- c(seq(20, 5, length = 20), seq(5, 20, length = 60), seq(20, 1, length = 20))
  data$ymin <- -1 * data$ymax
  g <- ggplot(data = data,
         aes(x = x,
             ymax = ymax,
             ymin = ymin)) +
    geom_ribbon(alpha = 0.6) +
    theme_fivethirtyeight() +
    ylim(-50, 50)
  return(g)
}

# Establish a connection to use during app session
pool <- create_pool(options_list = credentials_extract())

# Create a dummy id for use in as_portfolio
as_portfolio <- as_portfolio %>%
  arrange(dataset_date) 
as_portfolio$id <- 1:nrow(as_portfolio)
