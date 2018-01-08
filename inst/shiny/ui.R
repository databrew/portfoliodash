
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
        menuItem("Portfolio Longevity", tabName = "longevity", icon = icon("calendar"))
      ),
      
      
      div(style="position: absolute; bottom: 0px; background-color: white; width: 100%;", 
          
        div(style="width: 203px; margin:0 auto; padding: 0;",
          
          tags$a(href='http://www.mastercardfdn.org/',
                 tags$img(src='mcf_logo.png', style="width: 100px; display: inline;")),
          
          tags$a(href='http://www.ifc.org/',
                 tags$img(src='ifc_logo.jpeg', style="width: 100px; display: inline;"))
          
        )
          
      )
      
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
                  
                  tags$p(style = "font-size: 16px;",
                    paste0(
                    "This dashboard was developed as a portfolio management tool for the Partnership for ",
                    "Financial Inclusion between mastercard foundation and IFC. It provides an overview ",
                    "over all active, closed and pipeline projects. The underlying dataset currently covers all ",
                    "IFC FIG advisory projects for Sub-Saharan Africa."
                    ),
                    tags$br()
                  ),
                  
                  tags$p(style = "font-size: 16px;",
                    paste0("The "),
                    tags$b("Portfolio Funding Chart "),
                    paste0("reports portfolio volume over time and into the future based on closed, current ",
                           "and pipeline projects' reported start and end dates. ", 
                           "The chart shows a picture of where the portfolio is today and simultaneously ",
                           "allows comparison with portfolio volumes and count in the past; ",
                           "and where the portfolio is headed into the future."),
                    tags$br()
                  ),
                  
                  tags$p(style = "font-size: 16px;",
                    paste0("The "),
                    tags$b("Portfolio Longevity Chart "),
                    paste0("lists each project within the specified portfolio (by business line or ",
                           "graphically for overall region). Each project bar is entered according to ",
                           "project start date and end date to provide an overall view of the portfolio ",
                           "maturity and composition."),
                    tags$br()
                  ),
                  
                  tags$p(style = "font-size: 14px;",
                    
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
                    
                    plotOutput("fundingPlot"),
                    
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
                            collapsed = TRUE,
                            
                            tags$div(style="width: 950px; margin-left: 200px;",
                                     plotOutput("fundingPlot2")
                            )
                          ),
                          
                          
                          box(
                            title = "Time and Budget Plot",
                            solidHeader = TRUE,
                            status = "primary",
                            width = NULL,
                            
                            
                            timevisOutput("longevityPlot")
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
        )
        
    )
    
    
    
    
    )
    
    
  ) # end dashboardPage
  
  
) # end tagList
