
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
      uiOutput('configure_page')
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
  
  # Create reactive user and user_id values
  user <- reactiveVal(value = NULL)
  user_id <- reactiveVal(value = NULL)
  observeEvent({
    input$username
    input$submit
  },{
    if(ok()){
      u <- input$username
      user(u)
      uid <- users %>% filter(name == u) %>% .$user_id
      user_id(uid)
      message('Logged in as username: ', u, ' which is user_id: ', uid)
    }
  })
  
  # Based on user and user_id, create reactive portfolios
  as_portfolio_reactive <- reactiveValues(data = as_portfolio)
  portfolio_projects_reactive <- reactiveValues(data = portfolio_projects)
  portfolio_users_reactive <- reactiveValues(data = portfolio_users)
  portfolios_reactive <- reactiveValues(data = portfolios)
  users_reactive <- reactiveValues(data = users)
  
  # Update the above reactive objects on submissions
  observeEvent({
    input$username
    input$submit
  },{
    if(ok()){
      uu <- user_id()
      ur <- users_reactive$data
      
      # Users
      users_reactive$data <- 
        ur %>% filter(user_id == uu)

      # Portfolio users
      pp <- portfolio_users_reactive$data
      portfolio_users_reactive$data <- pp %>%
        filter(user_id == uu)

      # Portfolios
      p <- portfolios_reactive$data
      portfolios_reactive$data <- p %>%
        filter(portfolio_id %in% portfolio_users_reactive$data$portfolio_id)

      # Portfolio projects
      pp <- portfolio_projects_reactive$data
      portfolio_projects_reactive$data <- pp %>%
        filter(portfolio_id %in% portfolios_reactive$data$portfolio_id)

      # As portfolio
      ap <- as_portfolio_reactive$data
      as_portfolio_reactive$data <- 
        ap %>%
        filter(project_id %in% portfolio_projects_reactive$data$project_id)
    }
  })
  
    
  # Message about tabs
  observeEvent({
    input$log_out
    input$submit; 
    input$tabs
  }, {
    message(paste0('Selected tab is: ', input$tabs))
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
          icon=icon("wrench"))
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
      'Wrong username/password combination'
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
      if(un %in% users$name){ # We'll eventually add password conditions here
        out <- TRUE
      }
    }
    ok(out)
  }
  )
  observeEvent(input$log_out,{
    ok(FALSE)
    user(NULL)
  })
  
  output$text1 <- renderText({
    if(ok()){
      'Congratulations, you logged in! Now you get to see a plot:'
    } else {
      'Log in on the left (using one of the user/pass combinations from below'
    }
  })

  output$fap_plot <- renderGvis({
    dat <- as_portfolio %>%
      group_by(status = region_name) %>%
      summarise(percent = n())
    dat$fraction = dat$percent / sum(dat$percent)
    dat = dat[order(dat$fraction), ]
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    # dat$status <- paste0(dat$status, ': ', dat$percent, '%')
    cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(dat$status)))
    
    doughnut <- gvisPieChart(dat,
                          options=list(
                            # width=500,
                            height="100%",
                            legend='none',
                            colors = paste0('[', paste0("'", cols, "'", collapse = ',' ), ']', collapse = NULL),
                            pieSliceText='label',
                            pieHole=0.5),
                          chartid="doughnut")
    doughnut
  })
  
  output$apv_plot <- renderGvis({
    
    dat <- as_portfolio %>%
      group_by(status = region_name) %>%
      summarise(volume = sum(total_project_size, na.rm = TRUE))
    dat$fraction = dat$volume / sum(dat$volume)
    dat = dat[order(dat$fraction), ]
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(dat$status)))
    
    d <- gvisPieChart(dat,
                             options=list(
                               # width=500,
                               height="100%",
                               legend='none',
                               colors = paste0('[', paste0("'", cols, "'", collapse = ',' ), ']', collapse = NULL),
                               pieSliceText='label',
                               pieHole=0.5),
                             chartid="d")
    d
    
    
  })
  
  output$user_details <- DT::renderDataTable({
    if(ok()){
      x <- users_reactive$data
      names(x) <- toupper(names(x))
      names(x) <- gsub('_', ' ', names(x))
      DT::datatable(x,
                    options = list(dom = 't'),
                    rownames = FALSE)
    }
  })
  
  output$main_page <- renderUI({
    okay <- ok()
    if(okay){
      fluidPage(
        fluidRow(
          shinydashboard::box(
            DT::dataTableOutput('user_details'),
            title = 'User details',
            width = 12,
            solidHeader = TRUE,
            status = "primary")
          
        ),
        fluidRow(
          shinydashboard::box(
            # plotOutput('fap_plot'),
            htmlOutput('fap_plot'),
            title = 'Financially active portfolio',
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            height = main_page_plot_height),
          shinydashboard::box(
            htmlOutput('apv_plot'),
            title = 'Active portfolio volume',
            width = 6,
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
  
  output$configure_page <- renderUI({
    okay <- ok()
    if(okay){
      # Get current subscriptions
      current_subscriptions <- portfolios_reactive$data %>%
        .$portfolio_name
      current_subscriptions <- paste0(current_subscriptions, collapse = ', ')
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 20px;",
                   current_subscriptions
            ),
            title = 'Current portfolio subscriptions',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 3
          ),
          shinydashboard::box(
            fluidPage(
              fluidRow(
                column(6,
                       actionButton('action_subscribe', 
                                    'Subscribe to a portfolio',
                                    icon = icon('copy'))),
                column(6,
                       actionButton('action_remove', 
                                    'Remove a subscription',
                                    icon = icon('eraser')))
                
              ),
              br(),
              fluidRow(
                column(6,
                       actionButton('action_modify', 
                                    'Modify your subscriptions',
                                    icon = icon('scissors'))),
                column(6,
                       fluidRow(actionButton('action_create', 
                                             'Create a new portfolio',
                                             icon = icon('plus-square')))
                       
                )
              )
            ),
            title = 'Actions',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 9
          )
        ),
        uiOutput('editing')
      )
    } else {
      fluidPage()
    }
  })
  
  edit_request <- reactiveVal(value = FALSE)
  edit_type <- reactiveVal(value = '')
  observeEvent(input$action_subscribe, {
    edit_request(TRUE)
    edit_type('Subscribe')
  })
  observeEvent(input$action_create, {
    edit_request(TRUE)
    edit_type('Create')
  })
  observeEvent(input$action_modify, {
    edit_request(TRUE)
    edit_type('Modify')
  })
  observeEvent(input$action_remove, {
    edit_request(TRUE)
    edit_type('Remove')
  })
  output$edit_content <- renderUI({
    et <- edit_type()
    pr <- portfolios_reactive$data
    if(et == 'Subscribe'){
      choices <- portfolios %>%
        filter(!portfolio_id %in% pr$portfolio_id)
      choices_labels <- paste0(choices$portfolio_name, ' (ID:',
                                       choices$portfolio_id,
                                       ')')
      choices <- choices$portfolio_id
      names(choices) <- choices_labels
      fluidPage(
        fluidRow(
          selectInput('subscribe_new',
                      'Which portfolio(s) do you want to subscribe to?',
                      choices = choices,
                      multiple = TRUE)
        ),
        fluidRow(
          actionButton('subscribe_confirm',
                       'Subscribe',
                       icon = icon('check-square'))
        )
      )
    } else if(et == 'Create'){
      project_choices <- as_portfolio %>%
        group_by(project_id) %>%
        summarise(project_name = dplyr::first(project_name))
      project_choices_labels <- paste0(project_choices$project_name, ' (ID:',
                                       project_choices$project_id,
                                       ')')
      project_choices <- project_choices$project_id
      names(project_choices) <- project_choices_labels
      fluidPage(
        fluidRow(
          textInput('create_new',
                    'What do you want the name of your new portfolio to be?',
                    value = 'ABC')
        ),
        fluidRow(
          selectInput('create_vals',
                    'Which projects do you want to include in this portfolio?',
                    choices = project_choices,
                    multiple = TRUE)
        ),
        fluidRow(
          actionButton('create_confirm',
                       'Create',
                       icon = icon('check-square'))
        )
      )
    } else if(et == 'Modify'){
      choices <- portfolios %>%
        filter(!portfolio_id %in% pr$portfolio_id) %>%
        .$portfolio_name
      choices <- sort(choices)
      fluidPage(
        fluidRow(
          selectInput('modify_new',
                      'Which portfolio do you want to modify?',
                      choices = choices),
          helpText('You can only modify those portfolios to which you subscribe and are an administrator')
        ),
        fluidRow(
          actionButton('modify_confirm',
                       'Modify',
                       icon = icon('check-square'))
        )
      )
    } else if(et == 'Remove'){
      choices <- sort(pr$portfolio_name)
      fluidPage(
        fluidRow(
          selectInput('remove_new',
                      'Which portfolio(s) do you want to remove?',
                      choices = choices,
                      multiple = TRUE)
        ),
        fluidRow(
          actionButton('remove_confirm',
                       'Remove',
                       icon = icon('check-square'))
        )
      )
      
      
    } else {
      NULL
    }
  })
  
  # Observe edits to modify the database
  observeEvent(input$subscribe_confirm, {
    message('SUBSCRIPTION CONFIRMED, CHANGE EVERYTHING!!!')

    pu <- portfolio_users
    # Add new rows for the new subscription
    new_rows <- input$subscribe_new
    if(length(new_rows) > 0){
      new_rows <- data_frame(portfolio_id = as.numeric(new_rows),
                             user_id = user_id(),
                             is_creator = FALSE,
                             is_admin = FALSE)
      # Add the new rows to the old rows
      portfolio_users <- bind_rows(pu, new_rows)
      
      # Update the database
      copy_to(connection_object, 
              portfolio_users, 
              "portfolio_users",
              temporary = FALSE,
              overwrite = TRUE)
      
      # Update the session too
      portfolio_users_reactive$data <- portfolio_users %>%
        filter(user_id == user_id())
      portfolios_reactive$data <- portfolios %>%
        filter(portfolio_id %in% portfolio_users_reactive$data$portfolio_id)

    }
  })
  observeEvent(input$create_confirm, {
    message('CREATION CONFIRMED, CHANGE EVERYTHING!!!')
  })
  observeEvent(input$remove_confirm, {
    message('REMOVAL CONFIRMED, CHANGE EVERYTHING!!!')
  })
  observeEvent(input$modify_confirm, {
    message('MODIFICATION CONFIRMED, CHANGE EVERYTHING!!!')
  })
  
  output$editing <- renderUI({
    okay <- ok()
    editing <- edit_request()
    if(okay & editing){
      fluidRow(
        shinydashboard::box(
          uiOutput('edit_content'),
          title = edit_type(),
          status = 'primary',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12
        )
      )
    } else {
      NULL
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
               uiOutput('portfolios_text'))
        
      ),
      if(!okay){
        fluidRow(
          column(2),
          shinydashboard::box(
                   tags$p(style = "font-size: 16px;",
                          paste0('During the development phase, you can log-in using any of the names below as the username (the password can be anything).')
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
    x <- users %>% dplyr::select(name)
    DT::datatable(x,
                  options = list(dom = 't'),
                  rownames = FALSE,
                  colnames = '')
  })
  
  
  output$portfolios_text <- renderUI({
    okay <- ok()
    out <- NULL
    if(okay){
      # Get the portfolios available to the user
      p <- portfolios_reactive$data
      p <- p$portfolio_name
      n <- length(p)
      if(length(p) > 0){
        p <- paste0(p, collapse = ', ')
        out <- 
          valueBox(value = n,
                 subtitle = 'Subscribed portfolios',
                 icon = icon('tasks'),
                 color = 'blue',
                 width = 12)
      } 
    } 
    return(out)
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