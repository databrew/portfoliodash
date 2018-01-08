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
dir_longevity_data <- "longevity_data.csv"
dir_portfolio_data <- "portfolio_funding_data.csv"
dir_portfolio_volume <- "portfolio_volume.csv"

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


dir_add_details <- "fig_ssa_addtional_details.csv"
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
dir_factors <- "factors.csv"
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
longevity_data$color <- "green"
longevity_data[longevity_data$pipeline_duration > 0, "color"] <- "lightblue"
longevity_data[longevity_data$closed_duration > 0, "color"] <- "red"









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




















######################################################
## Shiny
######################################################

source("ui.R", local = TRUE)



server <- function(input, output, session) {
  
  
#  output$fundingPlot <- renderPlot({
#    
#    par(mar=c(7,7,7,7), mgp = c(4, 1, 0))
#    plot <- barplot(data_mat, 
#                    main = "Portfolio Funding Chart", 
#                    xlab = "Quarter", 
#                    ylab = "Portfolio Volume ($M)",
#                    ylim = c(0, 1.1 * max(colSums(data_mat))),
#                    legend = rownames(data_mat),
#                    col = c("darkblue", "red", "lightblue"),
#                    names.arg = quarters,
#                    las = 2)
#    
#    text(x = plot, y = colSums(data_mat), label = projects, pos = 3)
#    plot
#    
#  })
  
  
  
  
  
  # DASHBOARD LANDING PAGE
  ##########################################
  
  output$infoBoxActive <- renderInfoBox({
    infoBox(
      "Active Projects", paste0(nrow(longevity_data[longevity_data$active == 1,])), 
      icon = icon("list"),
      color = "green"
    )
  })
  
  output$infoBoxClosed <- renderInfoBox({
    infoBox(
      "Closed Projects", paste0(nrow(longevity_data[longevity_data$closed == 1,])), 
      icon = icon("check"),
      color = "green"
    )
  })
  
  output$infoBoxAvgSize <- renderInfoBox({
    infoBox(
      "Avg Project Size", paste0("$", sprintf("%.1f", mean(longevity_data[,"prorated_total_funds_managed_by_ifc"]/1000000)), "M"), 
      icon = icon("bar-chart"),
      color = "green"
    )
  })
  
  output$infoBoxTotalSize <- renderInfoBox({
    infoBox(
      "Total Portfolio Size", paste0("$", sprintf("%.1f", sum(longevity_data[,"prorated_total_funds_managed_by_ifc"]/1000000)), "M"), 
      icon = icon("line-chart"),
      color = "green"
    )
  })
  
  output$infoBoxBurn <- renderInfoBox({
    infoBox(
      "Avg Burn Rate", sprintf("%.1f %%", mean(longevity_data[,"burn_rate"])), 
      icon = icon("percent"),
      color = "green"
    )
  })
  
  
  
  
  # PORTFOLIO FUNDING PANEL
  #####################################################################
  
  
  output$fundingPlot <- renderPlot({
    
    par(mar=c(7,7,7,7), mgp = c(4, 1, 0))
    plot <- barplot(portfolio_mat, 
                    main = "Portfolio Funding Chart", 
                    xlab = "Quarter", 
                    ylab = "Portfolio Volume ($M)",
                    ylim = c(0, 1.1 * max(colSums(portfolio_mat))),
                    legend = rownames(portfolio_mat),
                    col = c("darkblue", "red", "lightblue"),
                    names.arg = quarters,
                    las = 2)
    
    text(x = plot, y = colSums(portfolio_mat), label = projects, pos = 3)
    plot
    
  })
  
  
  
  
  
  
  
  

  # LONGEVITY PANEL
  ########################################################
  
  
  output$fundingPlot2 <- renderPlot({
    
    par(mar=c(5,7,0,0), mgp = c(4, 1, 0))
    plot <- barplot(portfolio_mat, 
                    ylab = "Portfolio Volume ($M)",
                    ylim = c(0, 1.1 * max(colSums(portfolio_mat))),
                    legend = rownames(portfolio_mat),
                    col = c("darkblue", "red", "lightblue"),
                    names.arg = quarters,
                    las = 2)
    
    text(x = plot, y = colSums(portfolio_mat), label = projects, pos = 3)
    plot
    
  }, width = 'auto')
  
  
  output$longevityPlot <- renderTimevis({
    
    
    # Filter data by project type
    if(length(input$selectProjType) == 3){
      data_subset <- longevity_data[longevity_data[,input$selectProjType[1]] | longevity_data[,input$selectProjType[2]] | longevity_data[,input$selectProjType[3]],]
    }
    else if(length(input$selectProjType) == 2){
      data_subset <- longevity_data[longevity_data[,input$selectProjType[1]] | longevity_data[,input$selectProjType[2]],]
    }
    else if(length(input$selectProjType) == 1)
    {
      data_subset <- longevity_data[longevity_data[,input$selectProjType[1]],]
    }
    else
    {
      data_subset <- longevity_data[FALSE,]
    }
    
    
    # Filter data by custom portfolio
    if(input$selectPortfolio == "MCF") {
      data_subset <- data_subset[data_subset$funding_source == "MCF",]
    }
    else if(input$selectPortfolio == "MCF Extended") {
      data_subset <- data_subset[data_subset$mcf_extended == 1,]
    }
    
    
    
    
    # sort data
    if(input$orderDir == "ascending")
    {
      data_subset <- data_subset[order(data_subset[,input$selectOrder]),]
      
    }
    else
    {
      data_subset <- data_subset[order(data_subset[,input$selectOrder], decreasing = TRUE),]
    }
    
    if(nrow(data_subset)==0)
    {
      timevis()
    }
    else
    {
      data_subset$ID <- seq.int(nrow(data_subset))
      
      timevis_data <- data.frame(
        id = data_subset$project_id,
        group = data_subset$project_id,
        title = data_subset$project_name,
        content = "",
        start = data_subset$graph_start_date,
        end = data_subset$project_end_date,
        style = paste0("background-color: ", data_subset$color, "; height: 12px"),
        type = 'range',
        stringsAsFactors = FALSE
      )
      
      group_data <- data.frame(
        id = data_subset$project_id,
        content = data_subset$html,
        title = data_subset$project_name,
        style = "font-size: 12px; height: 15px;",
        ID = data_subset$ID,
        stringsAsFactors = FALSE
      )
      
      #add end of fiscal year element
      efy <- data.frame(
              id = 0,
              group = NA,
              title = "End of Fiscal Year",
              content = "",
              start = end_fiscal_year,
              end = end_fiscal_year + 7,
              style = "background-color: red;",
              type = 'background',
              stringsAsFactors = FALSE
      )
      timevis_data <- rbind(timevis_data, efy)
      
      timevis_data[timevis_data$id == input$longevityPlot_selected, "style"] <- paste0("background-color: yellow; height: 12px")
      

      timeline <- timevis(timevis_data, groups = group_data, showZoom = FALSE, fit = FALSE,
                          options = list(zoomable = FALSE, 
                                         horizontalScroll = FALSE,
                                         timeAxis = list(scale = 'month', step = 6),
                                         orientation = 'both',
                                         groupOrder = "ID",
                                         showCurrentTime = FALSE,
                                         autoResize = TRUE,
                                         start = as.Date("2015-01-01"),
                                         end = as.Date("2020-01-01")
                          )
      ) %>% addCustomTime(longevity_data$dataset_date[1], "datadate")
      
      timeline
      
      
    } # end else
    
  })
  
  

  
  
  # Updating data in project details box
  ######################################
  
  output$projName <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "project_name"])
  })
  
  output$projID <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "project_id"])
  })
  
  output$projStatus <- renderText({
    if(!is.null(input$longevityPlot_selected))
    {
      if(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "active"] == TRUE){
        paste0("Active")
      }
      else if(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "pipeline"] == TRUE){
        paste0("Pipeline")
      }
      else{
        paste0("Closed")
      }
    }
  })
  
  
  output$projStart <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "graph_start_date"])
  })
  
  output$projEnd <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "project_end_date"])
  })
  
  output$projSize <- renderText({
    paste0("$", longevity_data[longevity_data$project_id == input$longevityPlot_selected, "prorated_total_funds_managed_by_ifc"]/1000000, "M")
  })
  
  output$projBurn <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevityPlot_selected, "burn_rate"], "%")
  })
  
  
  
  
  
  observeEvent(input$btn, {
    setWindow("longevityPlot", as.Date("2012-07-01"), as.Date("2020-07-01"))
  })
  
  
  
  
  
  # Actual Portfolio Volume & Expenditure Plots
  ####################################################
  
  output$actPortfolio1 <- renderPlot({
    
    plotData <- as.matrix(portfolio_vol_mat[,"Active Portfolio"])
    
    textPos <- apply(plotData, 2L, cumsum)
    textPos <- textPos - plotData / 2
    
    par(mar=c(0,1,0,1), mgp = c(0, 0, 0))
    plot <- barplot(plotData, 
          horiz = TRUE, axes = FALSE, xlim = c(0, 100), 
          col = c('#4d79ff', '#000099', '#00004d', '#000000'),
          border = NA)
    text(textPos, rep(plot, each = nrow(textPos)), labels = sprintf("%3.0f", plotData), col = '#ffffff')
    
  }, height = 30)
  
  output$actPortfolio2 <- renderPlot({
    
    plotData <- as.matrix(portfolio_vol_mat[,"Expenses To-Date"])
    
    textPos <- apply(plotData, 2L, cumsum)
    textPos <- textPos - plotData / 2
    
    par(mar=c(0,1,0,1), mgp = c(0, 0, 0))
    plot <- barplot(plotData, 
                    horiz = TRUE, axes = FALSE, xlim = c(0, 100), 
                    col = c('#ff3333', '#cc0000', '#ffffff', '#ffffff'),
                    border = NA)
    text(textPos, rep(plot, each = nrow(textPos)), labels = sprintf("%3.0f", plotData), col = '#ffffff')
    
  }, height = 30)
  
  output$actPortfolio3 <- renderPlot({
    
    plotData <- as.matrix(portfolio_vol_mat[,"Available Budget"])
    
    textPos <- apply(plotData, 2L, cumsum)
    textPos <- textPos - plotData / 2
    
    par(mar=c(3,1,0,1), mgp = c(2, 1, 0))
    plot<- barplot(as.matrix(portfolio_vol_mat[,"Available Budget"]), 
          horiz = TRUE, axes = TRUE, xlim = c(0, 100), xlab = "Million (US-$)", 
          col = c('#ffffff', '#ffffff', '#00802b', '#00e64d'),
          border = NA)
    
    text(textPos, rep(plot, each = nrow(textPos)), labels = sprintf("%3.0f", plotData), col = '#ffffff')
    
  }, height = 75)
  
  
  
  
}

shinyApp(ui, server)








