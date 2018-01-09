source('global.R')
######################################################
## Shiny
######################################################

ui <- tagList(
  dashboardPage(
    ######################################################
    ## Header
    ######################################################
    
    dashboardHeader(
      title = "FIG SSA MEL Dashboard"  
    ), 
    ######################################################
    ## Sidebar
    ######################################################
    dashboardSidebar(
      
      width = 250,
      
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Portfolio Funding", tabName = "funding", icon = icon("bar-chart")),
        menuItem("Portfolio Longevity", tabName = "longevity", icon = icon("calendar")),
        menuItem("Time and budget", tabName = "time_and_budget", icon = icon("clock-o")),
        menuItem("Flag view", tabName = "flag_view", icon = icon("flag")),
        menuItem("Project view", tabName = "project_view", icon = icon("cogs"))
      ),
      
      div(style="position: absolute; bottom: 0px; background-color: white; width: 100%;", 
          
          div(style="width: 203px; margin:0 auto; padding: 0;",
              
              tags$a(href='http://www.mastercardfdn.org/',
                     tags$img(src='mcf_logo.png', style="width: 100px; display: inline;")),
              
              tags$a(href='http://www.ifc.org/',
                     tags$img(src='ifc_logo.jpeg', style="width: 100px; display: inline;"))   
          ))
    ),
    
    ######################################################
    ## Body
    ######################################################
    dashboardBody(
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      tabItems(
        
        ######################################################
        ## Body - Landing Page
        ######################################################
        # Dashboard landing page
        tabItem(tabName = "dashboard",
                
                column(width = 6, 
                       
                       box(
                         title = "Welcome to the Portfolio Dashboard",
                         solidHeader = TRUE,
                         width = NULL, 
                         status = "warning",
                         
                         tags$p(style = "font-size: 13px;",
                                paste0(
                                  "This dashboard was developed as a portfolio management tool for the Partnership for ",
                                  "Financial Inclusion between mastercard foundation and IFC. It provides an overview ",
                                  "over all active, closed and pipeline projects. The underlying dataset currently covers all ",
                                  "IFC FIG advisory projects for Sub-Saharan Africa."
                                ),
                                tags$br()
                         ),
                         
                         tags$p(style = "font-size: 13px;",
                                paste0("The "),
                                tags$b("Portfolio Funding Chart "),
                                paste0("reports portfolio volume over time and into the future based on closed, current ",
                                       "and pipeline projects' reported start and end dates. ", 
                                       "The chart shows a picture of where the portfolio is today and simultaneously ",
                                       "allows comparison with portfolio volumes and count in the past; ",
                                       "and where the portfolio is headed into the future."),
                                tags$br()
                         ),
                         
                         tags$p(style = "font-size: 13px;",
                                paste0("The "),
                                tags$b("Portfolio Longevity Chart "),
                                paste0("lists each project within the specified portfolio (by business line or ",
                                       "graphically for overall region). Each project bar is entered according to ",
                                       "project start date and end date to provide an overall view of the portfolio ",
                                       "maturity and composition."),
                                tags$br()
                         ),
                         
                         tags$p(style = "font-size: 12px;",
                                
                                paste0("The dashboard is still under development. Version: 0.4 Alpha"),
                                tags$br(),
                                paste0("The underlying data was last updated on: ", longevity_data$dataset_date[1])
                                
                         )
                         
                       )
                       
                ), # end first column
                
                column(width = 6, 
                       
                       infoBoxOutput("infoBoxActive", width = NULL),
                       infoBoxOutput("infoBoxClosed", width = NULL),
                       infoBoxOutput("infoBoxBurn", width = NULL), 
                       infoBoxOutput("infoBoxAvgSize", width = NULL),
                       infoBoxOutput("infoBoxTotalSize", width = NULL) 
                       
                ) # end second column
                
        ),
        ######################################################
        ## Body - Funding Tab
        ######################################################
        
        # Funding Tab
        tabItem(tabName = "funding", 
                
                fluidRow(
                  
                  box(
                    title = "Funding Chart",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 9,
                    plotOutput("funding_plot1"),
                    
                    
                    tags$ul(
                      tags$li("Numbers on top of bars indicate active projects in that quarter."),
                      tags$li("Based on project start and end dates."),
                      tags$li("Trends are estimates since projects frequently stay active past listed end dates; 
                              and pipeline projects may change in volume and timeline.")
                      )
                    ),
                  
                  box(
                    title = "Controls",
                    solidHeader = TRUE,
                    status = "warning",
                    width = 3,
                    
                    selectInput("selectFundingRegion", "Region: ", c("Sub-Saharan Africa" = "SSA"), selected = "SSA")
                    
                  )  
                  )
                
        ),
        ######################################################
        ## Body - Longevity Tab
        ######################################################
        
        # Longevity Tab
        tabItem(tabName = "longevity",
                
                fluidRow(
                  
                  column( width = 9,
                          
                          box(
                            title = "Controls",
                            status = "warning",
                            solidHeader = TRUE,
                            width = NULL,
                            
                            div(style="display: inline-block;vertical-align:top; width: 175px; margin-right:10px;",
                                selectInput("selectRegion", "Region: ", c("Sub-Saharan Africa" = "SSA"), selected = "SSA")
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 175px; margin-right:10px;",
                                selectInput("selectBline", "Business Line: ", c("FIG" = "FIG"), selected = "FIG")
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 175px; margin-right:10px;",
                                selectInput("selectPortfolio", "Custom Portfolio: ", custom_portfolio, selected = custom_portfolio[2])
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 250px; margin-right:10px;",
                                checkboxGroupInput("selectProjType", "Display Projects With Status:", 
                                                   c("Active" = "active", "Closed" = "closed", "Pipeline" = "pipeline"),
                                                   inline = TRUE,
                                                   selected = c("active", "closed", "pipeline"))
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                
                                selectInput("selectOrder", "Order by:", c("Start Date" = "graph_start_date", "End Date" = "project_end_date", "Burn Rate" = "burn_rate",
                                                                          "Size ($M)" = "prorated_total_funds_managed_by_ifc"), selected = c("project_end_date"))
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                selectInput("orderDir", "Direction:", c("Ascending" = "ascending", "Descending" = "descending"), selected = c("ascending"))
                            )
                          ),
                          
                          
                          box(
                            title = "Funding Chart",
                            solidHeader = TRUE,
                            status = "primary",
                            width = NULL,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            # showOutput("funding_plot", "nvd3"),
                            tags$div(style="width: 600px; margin-left: 200px;",
                                     showOutput("funding_plot", "nvd3")
                            )
                            
                            # tags$div(style="width: 950px; margin-left: 200px;",
                            #          plotOutput("funding_plot2")
                            # )
                          ),
                          box(
                            title = "Time and Budget Plot",
                            solidHeader = TRUE,
                            status = "primary",
                            width = NULL,
                            
                            
                            timevisOutput("longevity_plot")
                          )
                  ),  
                  
                  column( width = 3,
                          
                          box(
                            title = "Legend",
                            status = "warning",
                            solidHeader = TRUE,
                            width = NULL,
                            
                            
                            tags$p(
                              tags$b("Dataset Date: "),
                              paste0(longevity_data$dataset_date[1]),
                              tags$br(),
                              tags$span(style = 'color: red;', "Red Line"), ": End of Fiscal Year",
                              tags$br(),
                              tags$span(style = 'color: blue;', "Blue Line"), ": Dataset Date (Default)",
                              
                              
                              tags$table( style = 'border-spacing: 5px; border-collapse: separate;',
                                          
                                          tags$tr(
                                            tags$td(style = 'width: 15px; background-color: green;', " "),
                                            tags$td(style = 'font-size:12px;', "Active Projects")
                                          ),
                                          tags$tr(
                                            tags$td(style = 'width: 15px; background-color: red;', " "),
                                            tags$td(style = 'font-size:12px;', "Closed Projects")
                                          ),
                                          tags$tr(
                                            tags$td(style = 'width: 15px; background-color: lightblue;', " "),
                                            tags$td(style = 'font-size:12px;', "Pipeline Projects")
                                          ),
                                          tags$tr(
                                            tags$td(style = 'width: 15px; background-color: yellow;', " "),
                                            tags$td(style = 'font-size:12px;', "Selected Project")
                                          )
                                          
                              ),
                              
                              tags$p("Click on a projcet in the timeline to see project details below.")
                              
                            ),
                            
                            actionButton("btn", "Adjust time")
                            
                          ),
                          
                          
                          box(
                            title = "Project Details",
                            status = "warning",
                            solidHeader = TRUE,
                            width = NULL,
                            
                            tags$table(
                              
                              tags$tr(
                                tags$td(tags$b("Name: ")),
                                tags$td(style="padding-left:20px;", textOutput("projName"))
                              ),
                              tags$tr(
                                tags$td(tags$b("Project ID: ")),
                                tags$td(style="padding-left:20px;", textOutput("projID"))
                              ),
                              tags$tr(
                                tags$td(tags$b("Status: ")),
                                tags$td(style="padding-left:20px;", textOutput("projStatus"))
                              ),
                              tags$tr(
                                tags$td(tags$b("Start Date: ")),
                                tags$td(style="padding-left:20px;", textOutput("projStart"))
                              ),
                              tags$tr(
                                tags$td(tags$b("End Date: ")),
                                tags$td(style="padding-left:20px;", textOutput("projEnd"))
                              ),
                              tags$tr(
                                tags$td(tags$b("Size: ")),
                                tags$td(style="padding-left:20px;", textOutput("projSize"))
                              ),
                              tags$tr(
                                tags$td(tags$b("Burn Rate: ")),
                                tags$td(style="padding-left:20px;", textOutput("projBurn"))
                              )
                              
                            )
                            
                          ), # End project details box
                          
                          box(
                            title = "Actual Portfolio Volume & Expenditure",
                            status = "warning",
                            solidHeader = TRUE,
                            width = NULL,
                            
                            tags$b("Active Portfolio"),
                            div(style='height: 35px;',
                                plotOutput("actPortfolio1")
                            ),
                            
                            tags$b("Expenses To-Date"),
                            div(style='height: 35px;',
                                plotOutput("actPortfolio2")
                            ),
                            
                            tags$b("Available Budget"),
                            div(style='height: 80px;',
                                plotOutput("actPortfolio3")
                            ),
                            tags$table( style = 'border-spacing: 5px; border-collapse: separate;',
                                        
                                        tags$tr(
                                          tags$td(style = 'width: 15px; background-color: #4d79ff;', " "),
                                          tags$td(style = 'font-size:12px;', "Active Portfolio Projects, Operationally Closed")
                                        ),
                                        tags$tr(
                                          tags$td(style = 'width: 15px; background-color: #00e64d;', " "),
                                          tags$td(style = 'font-size:12px;', "Expected Pipeline Volume")
                                        ) 
                            )       
                          )
                  )
                )
        ),
        tabItem(tabName = 'time_and_budget',
                fluidPage(fluidRow(h3('Under construction')))),
        tabItem(tabName = 'flag_view',
                fluidPage(fluidRow(h3('Under construction')))),
        tabItem(tabName = 'project_view',
                fluidPage(fluidRow(h3('Under construction'))))
    )
    )
  )
)




server <- function(input, output, session) {
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
  
  
  output$funding_plot <- renderChart({
    
    # Reshape portfolio_mat into a "long" dataset (better for plotting)
    portfolio_mat_data <- as.data.frame(portfolio_mat)
    names(portfolio_mat_data) <- as.Date(paste0(quarters, '-15'),
                                         format = '%b-%y-%d')
    portfolio_mat_data$key <- row.names(portfolio_mat_data)
    portfolio_mat_data <- 
      tidyr::gather(portfolio_mat_data, date, value, 1:(ncol(portfolio_mat_data)-1))
    portfolio_mat_data$date <- as.Date(portfolio_mat_data$date)
    
    # Filter data to only include the date range from the timevis chart
    tr <- longevity_plot_range()
    if(!is.null(tr)){
      tr <- as.Date(tr)
      portfolio_mat_data <-
        portfolio_mat_data %>%
        filter(between(date, tr[1], tr[2]))
    } else {
      tr <- as.Date(c('2015-01-15',
                      '2016-01-15'))
    }
    
    # "Expand" the data so that there are 0s at the dates of interest
    left <- expand.grid(date = seq(as.Date(paste0(format(as.Date(tr[1]), '%Y-%m'), '-15')),
                                   as.Date(paste0(format(as.Date(tr[2]), '%Y-%m'), '-15')),
                            by = 'month'),
                        key = unique(portfolio_mat_data$key))
    # Capture the date range of the portfolio_mat_data
    date_range <- range(portfolio_mat_data$date)
    portfolio_mat_data <-
      left_join(left, portfolio_mat_data,
                by = c('key', 'date')) %>%
      # If within date range and empty, remove; if outside, keep
      filter(date <= date_range[1] | date >= date_range[2] | !is.na(value)) %>%
      mutate(date = format(date, '%Y-%m'))
      # mutate(#value = ifelse(is.na(value), 0, value),
      #        date = as.character(date))
    
    n1 <- nPlot(value ~ date, group = "key", data = portfolio_mat_data, type = "multiBarChart", width = 500, dom = 'funding_plot')
    n1$chart(stacked = TRUE)
    # n1$xAxis(
    #   tickFormat =
    #     "#! function(d) {
    #     return d3.time.format('%b %Y')(new Date(d * 24 * 60 * 60 * 1000))
    # } !#"
    # )
    return(n1)
    # par(mar=c(7,7,7,7), mgp = c(4, 1, 0))
    # plot <- barplot(portfolio_mat,
    #                 main = "Portfolio Funding Chart",
    #                 xlab = "Quarter",
    #                 ylab = "Portfolio Volume ($M)",
    #                 ylim = c(0, 1.1 * max(colSums(portfolio_mat))),
    #                 legend = rownames(portfolio_mat),
    #                 col = c("darkblue", "red", "lightblue"),
    #                 names.arg = quarters,
    #                 las = 2)
    # 
    # text(x = plot, y = colSums(portfolio_mat), label = projects, pos = 3)
    # plot
  })
  
  
  output$funding_plot1 <- renderPlot({
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
  
  
  # output$funding_plot2 <- renderPlot({
  #   
  #   par(mar=c(5,7,0,0), mgp = c(4, 1, 0))
  #   plot <- barplot(portfolio_mat, 
  #                   ylab = "Portfolio Volume ($M)",
  #                   ylim = c(0, 1.1 * max(colSums(portfolio_mat))),
  #                   legend = rownames(portfolio_mat),
  #                   col = c("darkblue", "red", "lightblue"),
  #                   names.arg = quarters,
  #                   las = 2)
  #   
  #   text(x = plot, y = colSums(portfolio_mat), label = projects, pos = 3)
  #   plot
  #   
  # }, width = 'auto')
  
  
  # Capture the parameters of the longevity plot
  longevity_plot_range <- reactive({
    x <- input$longevity_plot_window
    if(is.null(x)){
      return(NULL)
    } else {
      x <- as.Date(as.POSIXct(x))
      as.character(x)
    }
  })

  # Render a longevity plot
  output$longevity_plot <- renderTimevis({
    
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
      
      timevis_data[timevis_data$id == input$longevity_plot_selected, "style"] <- paste0("background-color: yellow; height: 12px")
      

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
    paste0(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "project_name"])
  })
  
  output$projID <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "project_id"])
  })
  
  output$projStatus <- renderText({
    if(!is.null(input$longevity_plot_selected))
    {
      if(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "active"] == TRUE){
        paste0("Active")
      }
      else if(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "pipeline"] == TRUE){
        paste0("Pipeline")
      }
      else{
        paste0("Closed")
      }
    }
  })
  
  
  output$projStart <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "graph_start_date"])
  })
  
  output$projEnd <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "project_end_date"])
  })
  
  output$projSize <- renderText({
    paste0("$", longevity_data[longevity_data$project_id == input$longevity_plot_selected, "prorated_total_funds_managed_by_ifc"]/1000000, "M")
  })
  
  output$projBurn <- renderText({
    paste0(longevity_data[longevity_data$project_id == input$longevity_plot_selected, "burn_rate"], "%")
  })
  
  
  
  
  
  observeEvent(input$btn, {
    setWindow("longevity_plot", as.Date("2012-07-01"), as.Date("2020-07-01"))
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








