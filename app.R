library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RColorBrewer)
source('global.R')

header <- dashboardHeader(title="Portfolio Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'tabs',
    h4(textOutput('submit_text'), align = 'center'),
    h3(textOutput('submit_text_below'), align = 'center'),
    menuItemOutput('main_menu'),
    uiOutput('username_ui'),
    uiOutput('password_ui'),
    h6(textOutput('incorrect_password_text')),
    uiOutput('submit_ui'),
    menuItemOutput('configure_menu'),
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
      tabName = 'configure',
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 28px",
                   'Pick up to 4 fields for filtering your portfolio'
            ),
            fluidRow(column(12, helpText(textOutput('portfolio_size_text')))),
            fluidRow(column(3,
                   selectInput('filter_1',
                               'Filter 1',
                               choices = c('', var_choices),
                               selected = '')),
            column(3,
                   selectInput('filter_2',
                               'Filter 2',
                               choices = c('', var_choices),
                               selected = '')),
            column(3,
                   selectInput('filter_3',
                               'Filter 3',
                               choices = c('', var_choices),
                               selected = '')),
            column(3,
                   selectInput('filter_4',
                               'Filter 4',
                               choices = c('', var_choices),
                               selected = ''))),
            fluidRow(
              column(3, uiOutput('filter_1_b')),
              column(3, uiOutput('filter_2_b')),
              column(3, uiOutput('filter_3_b')),
              column(3, uiOutput('filter_4_b'))
            ),
            fluidRow(
              column(3, uiOutput('filter_1_c')),
              column(3, uiOutput('filter_2_c')),
              column(3, uiOutput('filter_3_c')),
              column(3, uiOutput('filter_4_c'))
            ),
            fluidRow(
              column(5),
              column(2, uiOutput('filter_action')),
              column(5)
            ),
            fluidRow(
              column(1),
              column(10,
                     p('Note: filters are treated as "OR" statements (ie, projects are kept if they satisfy any of the above filter conditions). Filters are applied to your already previously filtered data. If you want to start from scratch (with all projects, click below.')),
              column(1)
            ),
            fluidRow(
              column(4, uiOutput('filter_restart')),
              column(4, uiOutput('filter_reboot')),
              column(4, uiOutput('filter_save'))),
            title = 'Controls',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12)
        ),
        fluidRow(
          shinydashboard::box(
            DT::dataTableOutput('old_portfolio_table'),
            title = 'Your old portfolio',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12)),
        fluidRow(
          shinydashboard::box(
            DT::dataTableOutput('new_portfolio_table'),
            title = 'Your new portfolio',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12))
      )
    
    ),
    tabItem(
      tabName = 'longevity',
      fluidPage(
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
        textOutput('tr_text'),
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
      fluidPage(
        fluidRow(
          box(
            title = "Spending Fish",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('spending_fish_plot')
          )
        ),
        fluidRow(
          box(
            title = "Portfolio spending rates by project duration",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('spending_rates_plot')
          )
        )
        
      )
    ),
    tabItem(
      tabName = 'flag_view',
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 20px;",
                   'Some controls will go here'
            ),
            title = 'Controls',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Some plot",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('flag_view_plot')
          )
        )
      )
    ),
    tabItem(
      tabName = 'project_view',
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 15px;",
                   'Some details will go here'
            ),
            title = 'Project name',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6
          ),
          shinydashboard::box(
            tags$p(style = "font-size: 15px;",
                   'More details will go here'
            ),
            title = 'Summary',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Indicators: results and targets",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            plotOutput('indicators_plot')
          )
        )
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(h4("The dashboard was developed as a part of activities under the ", 
                    a(href = 'http://www.ifc.org/wps/wcm/connect/region__ext_content/ifc_external_corporate_site/sub-saharan+africa/priorities/financial+inclusion/za_ifc_partnership_financial_inclusion',
                      target='_blank',
                      "Partnership for Financial Inclusion"),
                    " (a $37.4 million joint initiative of the ",
                    a(href = "http://www.ifc.org/wps/wcm/connect/corp_ext_content/ifc_external_corporate_site/home",
                      target='_blank',
                      'IFC'),
                    " and the ",
                    a(href = "http://www.mastercardfdn.org/",
                      target='_blank',
                      'MasterCard Foundation'),
                    " to expand microfinance and advance digital financial services in Sub-Saharan Africa) by the FIG Africa Digital Financial Services unit (the MEL team).")),
        br(),
        fluidRow(div(img(src='partnership logo.bmp', align = "center"), style="text-align: center;"),
                 br(),
                 div(a(actionButton(inputId = "email", label = "Contact", 
                                    icon = icon("envelope", lib = "font-awesome")),
                       href="mailto:sheitmann@ifc.org",
                       align = 'center')), 
                 style = 'text-align:center;'
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  # Create reactive variables which are selected for filtering
  filter_var_1 <- reactive({
    out <- NULL
      if(ok() & !is.null(input$filter_1)){
        out <- unlist(as_portfolio[,input$filter_1])
      }
    })
  filter_var_2 <- reactive({
    out <- NULL
    if(ok() & !is.null(input$filter_2)){
      out <- unlist(as_portfolio[,input$filter_2])
    }
  })
  filter_var_3 <- reactive({
    out <- NULL
    if(ok() & !is.null(input$filter_3)){
      out <- unlist(as_portfolio[,input$filter_3])
    }
  })
  filter_var_4 <- reactive({
    out <- NULL
    if(ok() & !is.null(input$filter_4)){
      out <- unlist(as_portfolio[,input$filter_4])
    }
  })
  
  # Restart the filter selection upon a re-start
  # filter_selection <- reactiveVal(value = '')
  # observeEvent(input$filter_restart_button, {
  #   filter_selection('')
  # })
  # 
  # Generate filters b, if filters a are selected
  output$filter_1_b <- renderUI({
    if(!ok()){
      NULL
    } else {
      # See if filter a is something other than ''
      if(!is.null(input$filter_1)){
        if(input$filter_1 != ''){
          if(classify(filter_var_1()) == 'character'){
            choices <- filter_choices_character} else { choices <- filter_choices
            }
          selectInput('filter_1_b_in',
                      'Operation',
                      choices = choices)
        }
      }
    }
  })
  output$filter_2_b <- renderUI({
    if(!ok()){
      NULL
    } else {
      # See if filter a is something other than ''
      if(!is.null(input$filter_2)){
        if(input$filter_2 != ''){
          if(classify(filter_var_2()) == 'character'){
            choices <- filter_choices_character} else { choices <- filter_choices
            }
          selectInput('filter_2_b_in',
                      'Operation',
                      choices = choices)
        }
      }
    }
  })
  output$filter_3_b <- renderUI({
    if(!ok()){
      NULL
    } else {
      # See if filter a is something other than ''
      if(!is.null(input$filter_3)){
        if(input$filter_3 != ''){
          if(classify(filter_var_3()) == 'character'){
            choices <- filter_choices_character} else { choices <- filter_choices
            }
          selectInput('filter_3_b_in',
                      'Operation',
                      choices = choices)
        }
      }
    }
  })
  output$filter_4_b <- renderUI({
    if(!ok()){
      NULL
    } else {
      # See if filter a is something other than ''
      if(!is.null(input$filter_4)){
        if(input$filter_4 != ''){
          if(classify(filter_var_4()) == 'character'){
            choices <- filter_choices_character} else { choices <- filter_choices
            }
          selectInput('filter_4_b_in',
                      'Operation',
                      choices = choices)
        }
      }
    }
  })
  output$filter_1_c <- renderUI({
    if(!ok()){
      NULL
    } else {
      if(!is.null(input$filter_1_b_in)){
         # Get the variable and class
        var <- filter_var_1()
        the_class <- classify(var)
        # If numeric, render a slider for adding a number
        if(the_class == 'numeric'){
          step <-  max(var, na.rm = TRUE) / 100
          step <- ifelse(step > 1, round(step), round(step, digits = 3))
          sliderInput('filter_1_c_in',
                      label = '',
                      min = min(var, na.rm = TRUE),
                      max = max(var, na.rm = TRUE),
                      value = mean(var, na.rm = TRUE),
                      step = step)
        } else if(the_class == 'Date'){
          dateInput('filter_1_c_in',
                    label = '',
                    value = Sys.Date())
        } else if(the_class == 'character'){
          selectInput('filter_1_c_in',
                    label = '',
                    choices = sort(unique(var)),
                    multiple = TRUE)
        }
      }
    }
  })
  output$filter_2_c <- renderUI({
    if(!ok()){
      NULL
    } else {
      if(!is.null(input$filter_2_b_in)){
        # Get the variable and class
        var <- filter_var_2()
        the_class <- classify(var)
        # If numeric, render a slider for adding a number
        if(the_class == 'numeric'){
          step <-  max(var, na.rm = TRUE) / 100
          step <- ifelse(step > 1, round(step), round(step, digits = 3))
          sliderInput('filter_2_c_in',
                      label = '',
                      min = min(var, na.rm = TRUE),
                      max = max(var, na.rm = TRUE),
                      value = mean(var, na.rm = TRUE),
                      step = step)
        } else if(the_class == 'Date'){
          dateInput('filter_2_c_in',
                    label = '',
                    value = Sys.Date())
        } else if(the_class == 'character'){
          selectInput('filter_2_c_in',
                      label = '',
                      choices = sort(unique(var)),
                      multiple = TRUE)
        }
      }
    }
  })
  output$filter_3_c <- renderUI({
    if(!ok()){
      NULL
    } else {
      if(!is.null(input$filter_3_b_in)){
        # Get the variable and class
        var <- filter_var_3()
        the_class <- classify(var)
        # If numeric, render a slider for adding a number
        if(the_class == 'numeric'){
          step <-  max(var, na.rm = TRUE) / 100
          step <- ifelse(step > 1, round(step), round(step, digits = 3))
          sliderInput('filter_3_c_in',
                      label = '',
                      min = min(var, na.rm = TRUE),
                      max = max(var, na.rm = TRUE),
                      value = mean(var, na.rm = TRUE),
                      step = step)
        } else if(the_class == 'Date'){
          dateInput('filter_3_c_in',
                    label = '',
                    value = Sys.Date())
        } else if(the_class == 'character'){
          selectInput('filter_3_c_in',
                      label = '',
                      choices = sort(unique(var)),
                      multiple = TRUE)
        }
      }
    }
  })
  output$filter_4_c <- renderUI({
    if(!ok()){
      NULL
    } else {
      if(!is.null(input$filter_4_b_in)){
        # Get the variable and class
        var <- filter_var_4()
        the_class <- classify(var)
        # If numeric, render a slider for adding a number
        if(the_class == 'numeric'){
          step <-  max(var, na.rm = TRUE) / 100
          step <- ifelse(step > 1, round(step), round(step, digits = 3))
          sliderInput('filter_4_c_in',
                      label = '',
                      min = min(var, na.rm = TRUE),
                      max = max(var, na.rm = TRUE),
                      value = mean(var, na.rm = TRUE),
                      step = step)
        } else if(the_class == 'Date'){
          dateInput('filter_4_c_in',
                    label = '',
                    value = Sys.Date())
        } else if(the_class == 'character'){
          selectInput('filter_4_c_in',
                      label = '',
                      choices = sort(unique(var)),
                      multiple = TRUE)
        }
      }
    }
  })
  # Action buttons for applying filters
  output$filter_action <- renderUI({
    if(!ok()){
      NULL
    } else {
      # See if any filter is something other than ''
      if(input$filter_1 != '' |
         input$filter_2 != '' |
         input$filter_3 != '' |
         input$filter_4 != ''){
        actionButton('filter_action_button',
                     'Apply filters',
                     icon = icon('binoculars'))
      
      }
    }
  })
  
  output$filter_restart <- renderUI({
    if(!ok()){
      NULL
    } else {
        actionButton('filter_restart_button',
                     'Undo changes since last save',
                     icon = icon('cubes'))
        
    }
  })
  
  output$filter_reboot <- renderUI({
    if(!ok()){
      NULL
    } else {
      actionButton('filter_reboot_button',
                   'Start new portfolio from scratch',
                   icon = icon('cubes'))
      
    }
  })
  
  output$filter_save <- renderUI({
    if(!ok()){
      NULL
    } else {
      actionButton('filter_save_button',
                   'Save your new portfolio',
                   icon = icon('binoculars'))
      
    }
  })
  
    
  # Reactive users portfolio, based on filters
  new_portfolio <- reactiveValues(data = as_portfolio)
  old_portfolio <- reactiveValues(data = as_portfolio)
  user_portfolio <- reactiveValues(data = user_portfolio_static)
  
  # Update the old_portfolio at log-in, log-out and tab change
  observeEvent({
    input$log_out
    input$submit; 
    input$tabs
  }, {
    message(paste0('Selected tab is: ', input$tabs))
    if(ok()){
      uu <- user()
      if(!is.null(uu)){
        these_projects  <-
          user_portfolio$data %>%
          filter(username == uu) %>%
          .$project_id
        x <- 
          as_portfolio %>%
          filter(project_id %in% these_projects)
        new_portfolio$data <- x
        old_portfolio$data <- x
      }
    }
  })
  
  observeEvent(input$filter_save_button,{
    # Write to the database
    uu <- user()
    message('Saving new portfolio for ', uu)
    pids <- new_portfolio$data
    old_portfolio$data <- new_portfolio$data
    pids <- pids$project_id
    if(local){
      update_db(u = uu,
                project_ids = pids)
    } else {
      warning('No access to a database. Not actually updating.')
    }
    # Update the user_portfolio table
    Sys.sleep(0.3)
    if(local){
      user_portfolio$data <-
        portfoliodash::get_data(query = NULL,
                                tab = 'user_portfolio',
                                dbname = 'portfolio',
                                connection_object = co)
    } else {
      message('This is not being run in a context with the database. Therefore, we cannot update user data.')
      user_portfolio$data <-
        user_portfolio_static
    }
  })
  
  # Clear the portfolio and start from scratch upon restart
  observeEvent(input$filter_restart_button, {
    new_portfolio$data <- 
      old_portfolio$data
  })
  
  observeEvent(input$filter_reboot_button, {
    new_portfolio$data <- 
      as_portfolio
  })

  # Create reactive filters
  filter_a <- reactive({
    make_filter(variable = input$filter_1,
                operator = input$filter_1_b_in,
                selection = input$filter_1_c_in)
  })
  filter_b <- reactive({
    make_filter(variable = input$filter_2,
                operator = input$filter_2_b_in,
                selection = input$filter_2_c_in)
  })
  filter_c <- reactive({
    make_filter(variable = input$filter_3,
                operator = input$filter_3_b_in,
                selection = input$filter_3_c_in)
  })
  filter_d <- reactive({
    make_filter(variable = input$filter_4,
                operator = input$filter_4_b_in,
                selection = input$filter_4_c_in)
  })
  
  output$portfolio_size_text <-
    renderText({
      paste0('Your current portfolio has ',
             nrow(old_portfolio$data),
             ' projects. If saved, your new portfolio would have ',
             nrow(new_portfolio$data),
              ' projects.')
    })
  
  # Create a vector of filter conditions
  filter_conditions <- reactive({
    f1 <- filter_a()
    f2 <- filter_b()
    f3 <- filter_c()
    f4 <- filter_d()
    fs <- c(f1, f2, f3, f4)
    fs <- fs[!fs %in% c('', "c('')")]
    return(fs)
  })
  
  # Apply filters when the apply filter button is clicked
  observeEvent(input$filter_action_button, {
    x <- new_portfolio$data
    # Get filter conditions
    fc <- filter_conditions()
    y <- do.call(filter_portfolio,
                 c(list(portfolio = x),
                   fc))
    new_portfolio$data <- y
  })
  
  output$new_portfolio_table <-
    DT::renderDataTable({
      if(ok()){
        x <- new_portfolio$data
        prettify(x, download_options = TRUE)
      }
    })
  output$old_portfolio_table <-
    DT::renderDataTable({
      if(ok()){
        x <- old_portfolio$data
        prettify(x, download_options = TRUE)
      }
    })
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
  output$configure_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text="Configure portfolio",
          tabName="configure",
          icon=icon("envelope-open"))
      }
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

  output$fap_plot <- renderPlot({
    g2
  }, height = main_page_plot_height_num)
  output$apv_plot <- renderPlot({
    g1
  }, height = main_page_plot_height_num)
  output$aps_plot <- renderPlot({
    g3
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
        fluidRow(
          valueBox(
            subtitle = "Active Projects", 
            value = paste0(nrow(longevity_data[longevity_data$active == 1,])), 
            icon = icon("list"),
            color = "orange"
          ),
          valueBox(
            subtitle = "Burn Rate", 
            value = sprintf("%.1f %%", mean(longevity_data[,"burn_rate"])), 
            icon = icon("percent"),
            color = "orange"
          ),
          valueBox(
            subtitle = "Avg Project Size", 
            value = paste0("$", sprintf("%.1f", mean(longevity_data[,"prorated_total_funds_managed_by_ifc"]/1000000)), "M"), 
            icon = icon("bar-chart"),
            color = "orange"
          )
        ),
        fluidRow(
          valueBox(
            subtitle = "Closed Projects", 
            value = paste0(nrow(longevity_data[longevity_data$closed == 1,])), 
            icon = icon("check"),
            color = "blue"
          ),
          valueBox(
            subtitle = "Total Portfolio Size", 
            value = paste0("$", sprintf("%.1f", sum(longevity_data[,"prorated_total_funds_managed_by_ifc"]/1000000)), "M"), 
            icon = icon("line-chart"),
            color = "blue"
          ),
          valueBox(
            subtitle = 'Some other indicator',
            value = 123,
            icon = icon('gears'),
            color = 'blue'
          )
        )
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
    fluidPage(
      fluidRow(
        column(welcome_width,
               welcome_box),
        column(filter_width,
               uiOutput('industry_filter'))
        
      ),
      if(!okay){
        fluidRow(
          column(2),
          shinydashboard::box(
                   tags$p(style = "font-size: 16px;",
                          paste0('During the development phase, you can log-in using the credentials below')
                   ),
                   DT::dataTableOutput('credentials_table'),
                   title = 'Credentials',
                   status = 'warning',
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   collapsed = FALSE,
                   width = 7
                 ),
          column(3))
      } else {
        fluidRow()
      }
    )
  })
  output$credentials_table <- DT::renderDataTable({
    prettify(users)
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
      tr <- as.Date(c('2014-01-15',
                      '2016-01-15'))
    }
    tr <- lubridate::round_date(tr, 'quarter')
    
    # "Expand" the data so that there are 0s at the dates of interest
    left <- expand.grid(date = seq(as.Date(paste0(format(as.Date(tr[1]), '%Y-%m'), '-15')),
                                   as.Date(paste0(format(as.Date(tr[2]), '%Y-%m'), '-15')),
                                   by = 'quarter'),
                        key = unique(portfolio_mat_data$key))
    # Capture the date range of the portfolio_mat_data
    date_range <- range(portfolio_mat_data$date)
    portfolio_mat_data <-
      left_join(left, portfolio_mat_data,
                by = c('key', 'date')) %>%
      # If within date range and empty, remove; if outside, keep
      filter(date <= date_range[1] | date >= date_range[2] | !is.na(value)) %>%
      mutate(date = format(date, '%Y-%m'))
    n1 <- nPlot(value ~ date, group = "key", data = portfolio_mat_data, type = "multiBarChart", width = 850, dom = 'funding_plot')
    n1$chart(#color = c('red', 'blue', 'green'),
             stacked = TRUE)
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
  
  output$tr_text <- renderText({longevity_plot_range()})
  
  output$spending_fish_plot <- renderPlot({
    g1
  })
  output$spending_rates_plot <- renderPlot({
    g2
  })
  output$flag_view_plot <- renderPlot({
    g3
  })
  output$indicators_plot <- renderPlot({
    g1
  })
  
}

shinyApp(ui, server)