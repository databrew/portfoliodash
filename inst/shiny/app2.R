library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

source('global.R')

# Define acceptable password username combinations
users <- data.frame(username = letters[1:5],
                    password = 1:5)

# Height for main page charts
main_page_plot_height_num <- 250
main_page_plot_height <- paste0(main_page_plot_height_num * 1.4, 'px')

header <- dashboardHeader(title="Portfolio Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    h4(textOutput('submit_text'), align = 'center'),
    h3(textOutput('submit_text_below'), align = 'center'),
    menuItemOutput('main_menu'),
    uiOutput('username_ui'),
    uiOutput('password_ui'),
    h6(textOutput('incorrect_password_text')),
    uiOutput('submit_ui'),
    menuItemOutput('longevity_menu'),
    menuItemOutput('budget_menu'),
    menuItemOutput('flag_view_menu'),
    menuItemOutput('project_view_menu'),
    menuItemOutput('about_menu'),
    uiOutput('log_out_ui'),
    div(style="position: absolute; bottom: 0px; background-color: white; width: 100%;", 
        
        div(style="width: 203px; margin:0 auto; padding: 0;",
            
            tags$a(href='http://www.mastercardfdn.org/',
                   tags$img(src='mcf_logo.png', style="width: 100px; display: inline;")),
            
            tags$a(href='http://www.ifc.org/',
                   tags$img(src='ifc_logo.jpeg', style="width: 100px; display: inline;"))   
        ))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabItems(
    tabItem(tabName = 'welcome',
            h3('Landing')),
    tabItem(
      tabName="main",
      fluidPage(
      uiOutput('welcome_page'),
      uiOutput('main_page')
        )
    ),
    tabItem(
      tabName = 'longevity',
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 20px;",
                   'Here is some text here'
            ),
            title = 'Controls',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12
          )
        ),
        box(
          title = "Funding Chart",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          collapsible = TRUE,
          collapsed = FALSE,
          tags$div(style="width: 600px; margin-left: 200px;",
                   showOutput("funding_plot", "nvd3")
          )
        ),
        box(
          title = "Time and Budget Plot",
          solidHeader = TRUE,
          status = "primary",
          width = NULL,
          
          
          timevisOutput("longevity_plot")
        )
        
      )
    ),
    tabItem(
      tabName = 'budget',
      fluidPage(h3('Under construction'))
    ),
    tabItem(
      tabName = 'flag_view',
      fluidPage()
    ),
    tabItem(
      tabName = 'project_view',
      fluidPage()
    ),
    tabItem(
      tabName = 'about',
      fluidPage()
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  # Generate inputs for username/password
  output$username_ui <- renderUI({
    if(ok()){
      NULL
    } else {
      textInput('username', 'User name')
    }
  })
  output$password_ui <- renderUI({
    if(ok()){
      NULL
    } else {
      textInput('password', 'Password')
    }
  })
  output$submit_ui <- renderUI({
    okay <- ok()
    if(okay){
      NULL
    } else {
      actionButton('submit', 'Submit', icon = icon('id-badge'))
    }
  })
  output$log_out_ui <- renderUI({
    okay <- ok()
    if(okay){
      actionButton('log_out', 'Log out', icon = icon('id-badge'))
    } else {
      NULL
    }
  })
  
  output$submit_text <- renderText({
    okay <- ok()
    if(okay){
      paste0('Logged in as ')
    } else {
      NULL
    }
  })
  output$submit_text_below <-
    renderText({
      user()
    })
  
  output$main_menu <-
    renderMenu({
      # if(ok()){
      menuItem(
        text="Main",
        tabName="main",
        icon=icon("eye"))
      # }
    })
  
  output$longevity_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
      menuItem(
        text="Longevity",
        tabName="longevity",
        icon=icon("expand"))
      }
    })
  
  output$budget_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text="Budget",
          tabName="budget",
          icon=icon("money"))
      }
    })
  
  output$flag_view_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text="Flag view",
          tabName="flag_view",
          icon=icon("flag"))
      }
    })
  
  output$project_view_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text="Project view",
          tabName="project_view",
          icon=icon("group"))
      } 
    })
  
  output$about_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text = 'About',
          tabName = 'about',
          icon = icon("cog", lib = "glyphicon"))
      }
    })
  
  wrong_pw_submitted <- reactiveVal(value = FALSE)
  observeEvent(input$submit, {
    wrong_pw_submitted(!ok())
  })
  observeEvent(input$log_out, {
    wrong_pw_submitted(FALSE)
  })
  output$incorrect_password_text <- renderText({
    wps <- wrong_pw_submitted()
    okay <- ok()
    if(okay){
      NULL
    } else if(wps){
      'Wrong password'
    } else {
      NULL
    }
  })
  
  # Evlaute whether a correct username / password has been entered
  ok <- reactiveVal(value = FALSE)
  observeEvent(input$submit,{
    out <- FALSE
    un <- input$username
    pw <- input$password
    if(!is.null(un) & !is.null(pw)){
      the_password <- users %>%
        filter(username == un) %>%
        .$password
      if(length(the_password) == 1){
        if(the_password == pw){
          out <- TRUE
        }
      }
    }
    ok(out)
  }
  )
  observeEvent(input$log_out,{
    ok(FALSE)
    user(NULL)
  })
  
  # Create an observing user
  user <- reactiveVal(value = NULL)
  observeEvent({
    input$username
    input$submit
  },{
    if(ok()){
      u <- input$username
      user(u)
      message('Logged in as username: ', u)
    }
  })
  
  output$text1 <- renderText({
    if(ok()){
      'Congratulations, you logged in! Now you get to see a plot:'
    } else {
      'Log in on the left (using one of the user/pass combinations from below'
    }
  })
  output$table1 <- renderTable({
    if(!ok()){
      return(users)
    }
  })
  output$plot1 <- renderPlot({
    if(ok()){
      barplot(1:10)
    }
  })
  
  output$fap_plot <- renderPlot({
    barplot(1:10)
  }, height = main_page_plot_height_num)
  output$apv_plot <- renderPlot({
    barplot(10:1)
  }, height = main_page_plot_height_num)
  output$aps_plot <- renderPlot({
    plot(10:1, (10:1)^3)
  }, height = main_page_plot_height_num)
  
  output$main_page <- renderUI({
    okay <- ok()
    if(okay){
      fluidPage(
        fluidRow(
          shinydashboard::box(
            plotOutput('fap_plot'),
            title = 'Financially active portfolio',
            width = 4,
            solidHeader = TRUE,
            status = "primary",
            height = main_page_plot_height),
          shinydashboard::box(
            plotOutput('apv_plot'),
            title = 'Active portfolio volume',
            width = 4,
            solidHeader = TRUE,
            status = "primary",
            height = main_page_plot_height),
          shinydashboard::box(
            plotOutput('aps_plot'),
            title = 'Active portfolio sp',
            width = 4,
            solidHeader = TRUE,
            status = "primary",
            height = main_page_plot_height)),
        fluidRow(valueBox(value = 1, 
                          subtitle = 'Active projects', 
                          icon = NULL, 
                          color = "orange", 
                          width = 4,
                          href = NULL),
                 valueBox(value = 1, 
                          subtitle = 'Average burn rates', 
                          icon = NULL, 
                          color = "orange", 
                          width = 4,
                          href = NULL),
                 valueBox(value = 1, 
                          subtitle = 'Average project size', 
                          icon = NULL, 
                          color = "orange", 
                          width = 4,
                          href = NULL))
      )
    }
  })
  output$welcome_page <- renderUI({
    okay <- ok()
    welcome_width <- ifelse(okay, 8, 11)
    cl <- ifelse(okay, TRUE, FALSE)
    filter_width <- 12 - welcome_width
    welcome_text <- 
      paste0(
        "This dashboard was developed as a portfolio ",
        "management tool for the Partnership for ",
        "Financial Inclusion between mastercard foundation ",
        "and IFC. It provides an overview ",
        "over all active, closed and pipeline projects. ",
        "The underlying dataset currently covers all ",
        "IFC FIG advisory projects for Sub-Saharan Africa."
      )
    welcome_box <-
      shinydashboard::box(
        tags$p(style = "font-size: 20px;",
               welcome_text
        ),
        title = 'Welcome',
        status = 'warning',
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = cl,
        width = 12
      )
    # if(okay){
      fluidRow(
        column(welcome_width,
               welcome_box),
        column(filter_width,
               uiOutput('industry_filter'))
        
      )
    # }
  })
  output$industry_filter <- renderUI({
    okay <- ok()
    if(okay){
      selectInput('industry',
                  'Industry',
                  choices = letters)
    }
  })
  
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
    n1 <- nPlot(value ~ date, group = "key", data = portfolio_mat_data, type = "multiBarChart", width = 500, dom = 'funding_plot')
    n1$chart(stacked = TRUE)
    # n1$xAxis(
    #   tickFormat =
    #     "#! function(d) {
    #     return d3.time.format('%b %Y')(new Date(d * 24 * 60 * 60 * 1000))
    # } !#"
    # )
    return(n1)
  })
  
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
    data_subset <- longevity_data
    # Carry out lots of filtering here once inputs are created
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
      ID = seq.int(nrow(data_subset)),
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
})
  
  
}

shinyApp(ui, server)