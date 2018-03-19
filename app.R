# library(shinyURL) #https://github.com/aoles/shinyURL

source('global.R')

header <- dashboardHeader(title="Portfolio Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'tabs',
    h4(textOutput('submit_text'), align = 'center'),
    h3(textOutput('submit_text_below'), align = 'center'),
    menuItemOutput('main_menu'),
    uiOutput('username_ui'),
    # uiOutput('password_ui'),
    h6(textOutput('incorrect_password_text')),
    uiOutput('submit_ui'),
    menuItemOutput('configure_menu'),
    menuItemOutput('longevity_menu'),
    menuItemOutput('budget_menu'),
    menuItemOutput('results_menu'),
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
        )),
    verbatimTextOutput("queryText")
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
      tabName = 'results',
      fluidPage(
        h1('Under construction')
      )
    ),
    tabItem(
      tabName = 'flag_view',
      fluidPage(
        fluidRow(
          shinydashboard::box(
            tags$p(style = "font-size: 20px;",
                   'Some stuff will go here'
            ),
            title = 'Stuff',
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
                 div(a(actionButton(inputId = "email_button", label = "Contact", 
                                    icon = icon("envelope", lib = "font-awesome")),
                       href="mailto:sheitmann@ifc.org",
                       align = 'center')), 
                 style = 'text-align:center;'
        ),
        br(),
        fluidRow(
          shinydashboard::box(
            title = 'Soren Heitmann',
            fluidPage(
              fluidRow(
                div(a(img(src='about/Soren Heitmann.jpg', 
                          align = "center",
                          height = '80'),
                      href="mailto:sheitmann@ifc.org"), 
                    style="text-align: center;")
              ),
              fluidRow(h5('Project Lead'),
                       h5('Johannesburg, ', 
                          a(href = 'mailto:sheitmann@ifc.org',
                            'sheitmann@ifc.org'))),
              fluidRow(helpText("Soren has a background in database management, software engineering and web technology. He manages the applied research and integrated monitoring, evaluation and learning program for the IFC-MasterCard Foundation Partnership for Financial Inclusion. He works at the nexus of data-driven research and technology to help drive learning and innovation within IFC’s Digital Financial Services projects in Sub-Saharan Africa."))
            ),
            width = 4),
          shinydashboard::box(
            title = 'Oleksiy Anokhin',
            fluidPage(
              fluidRow(
                div(a(img(src='about/Oleksiy Anokhin.jpg', 
                          align = "center",
                          height = '80'),
                      href="mailto:oanokhin@ifc.org"), 
                    style="text-align: center;")
              ),
              fluidRow(h5('Project Specialist'),
                       h5('Washington, DC, ', 
                          a(href = 'mailto:oanokhin@ifc.org',
                            'oanokhin@ifc.org'))),
              fluidRow(helpText("Oleksiy focuses on data-driven visualization solutions for international development. He is passionate about using programmatic tools (such as interactive dashboards) for better planning and implementation of projects, as well as for effective communication of projects results to various stakeholders."))
            ),
            width = 4),
          shinydashboard::box(
            title = 'Joe Brew',
            fluidPage(
              fluidRow(
                div(a(img(src='about/Joe Brew.png', 
                          align = "center",
                          height = '80'),
                      href="mailto:jbrew1@worldbank.org"), 
                    style="text-align: center;")
              ),
              fluidRow(h5('Data Scientist'),
                       h5('Amsterdam, ', 
                          a(href = 'mailto:jbrew1@worldbank.org',
                            'jbrew1@worldbank.org'))),
              fluidRow(helpText("Joe is a data scientist for", a(href = 'http://databrew.cc/', 'DataBrew.'), "He has a background in epidemiology and development economics. He works in both industry as a consultant as well as academia. His research focuses on the economics of malaria elimination programs in Sub-Saharan Africa."))#,
              # fluidRow(shinyURL.ui())
            ),
            width = 4)
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output, session) {
  
  # Create reactive user and user_id values
  user <- reactiveVal(value = NULL)
  user_id <- reactiveVal(value = NULL)
  ok <- reactiveVal(value = FALSE)
  # Evlaute whether a correct username / password has been entered
  
  observeEvent(input$submit,{
    out <- FALSE
    un <- input$email
    if(!is.null(un)){
      if(un %in% users$email){ 
        out <- TRUE
        user(un)
        uid <- users %>% filter(email == un) %>% .$user_id
        user_id(uid)
        message('Logged in as : ', un, ' which is user_id: ', uid)
      }
    }
    ok(out)
  })
  
  observeEvent(input$log_out,{
    ok(FALSE)
    user(NULL)
  })
  
  
  # Based on user and user_id, create reactive datasets
  as_portfolio_this_user <- reactiveValues(data = as_portfolio)
  portfolio_projects_this_user <- reactiveValues(data = portfolio_projects)
  portfolio_users_this_user <- reactiveValues(data = portfolio_users)
  portfolios_this_user <- reactiveValues(data = portfolios)
  users_this_user <- reactiveValues(data = users)
  
  # Create reactive datasets which are global (not user specific)
  as_portfolio_all <- reactiveValues(data = as_portfolio)
  portfolio_projects_all <- reactiveValues(data = portfolio_projects)
  portfolio_users_all <- reactiveValues(data = portfolio_users)
  portfolios_all <- reactiveValues(data = portfolios)
  users_all <- reactiveValues(data = users)
  
  # Create reactive datasets for the visualizations, based on selection on main page
  as_portfolio_selected <- reactiveValues(data = as_portfolio)
  
  # Update the above reactive objects on submissions
  observeEvent({
    input$submit
  },{
    if(ok()){
      uu <- user_id()
      message('uu is ', uu)
      ur <- users_this_user$data
      
      # Users
      users_this_user$data <- 
        ur %>% filter(user_id == uu)
      
      # Portfolio users
      pp <- portfolio_users_this_user$data
      portfolio_users_this_user$data <- pp %>%
        filter(user_id == uu)
      
      # Portfolios
      p <- portfolios_this_user$data
      portfolios_this_user$data <- p %>%
        filter(portfolio_id %in% portfolio_users_this_user$data$portfolio_id)
      
      # Portfolio projects
      pp <- portfolio_projects_this_user$data
      portfolio_projects_this_user$data <- pp %>%
        filter(portfolio_id %in% portfolios_this_user$data$portfolio_id)
      
      # As portfolio
      ap <- as_portfolio_this_user$data
      as_portfolio_this_user$data <- 
        ap %>%
        filter(project_id %in% portfolio_projects_this_user$data$project_id)
    }
  })
  
  observeEvent({
    ok()
    input$selected_portfolio
    input$filter_portfolio
    input$filter_portfolio_region
  },{
    # As portfolio (just selected)
    spi <- as.numeric(input$selected_portfolio)
    ppu <- portfolio_projects_this_user$data
    ifp <- input$filter_portfolio
    ifpr <- input$filter_portfolio_region
    these_projects <- ppu %>%
      dplyr::filter(portfolio_id %in% spi)
    these_projects <- these_projects$project_id
    as_portfolio_selected$data <-
      as_portfolio_all$data %>%
      filter(project_id %in% these_projects) 
    if(!is.null(ifp)){
      as_portfolio_selected$data <- 
        as_portfolio_selected$data %>%
        filter(project_status %in% ifp)
    }
    if(!is.null(ifpr)){
      as_portfolio_selected$data <- 
        as_portfolio_selected$data %>%
        filter(region_name %in% ifpr)
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
      textInput('email', 'Email:', value = 'jbrew1@worldbank.org')
    }
  })
  # output$password_ui <- renderUI({
  #   if(ok()){
  #     NULL
  #   } else {
  #     textInput('password', 'Password')
  #   }
  # })
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
      ue <- user()
      if(!is.null(ue)){
        un <- users %>% filter(email == ue)
        un$name
      } else {
        NULL
      }
      
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
          text="Configuration",
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
  
  output$results_menu <-
    renderMenu({
      okay <- ok()
      if(okay){
        menuItem(
          text="Results",
          tabName="results",
          icon=icon("cubes"))
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
  
  
  
  output$text1 <- renderText({
    if(ok()){
      'Congratulations, you logged in! Now you get to see a plot:'
    } else {
      'Log in on the left (using one of the user/pass combinations from below'
    }
  })
  
  # Active portfolio volume
  output$apv_plot <- renderPlot({
    # Get this user's portfolio
    ap <- as_portfolio_selected$data
    ap <- ap %>%
      # Keep only the last observation for each project
      arrange(dataset_date) %>%
      group_by(project_id) %>%
      filter(id == dplyr::last(id)) %>%
      ungroup %>%
      filter(project_status == 'ACTIVE') %>%
      group_by(Stage = project_stage) %>%
      summarise(Amount = sum(total_funding, na.rm = TRUE)) %>%
      ungroup %>%
      # for now, hard-coding the filter of a few levels
      filter(!Stage %in% c('UNKNOWN', 'PRE-PIPELINE', 'OTHER'))
    
    if(nrow(ap) > 0){
      ap$fraction = ap$Amount / sum(ap$Amount)
      ap = ap[order(ap$fraction), ]
      ap$ymax = cumsum(ap$fraction)
      ap$ymin = c(0, head(ap$ymax, n=-1))
      ap$grp <- 'Active volume'
      
      ap2 <- as_portfolio_selected$data
      ap2 <- ap2 %>%
        # Keep only the last observation for each project
        arrange(dataset_date) %>%
        group_by(project_id) %>%
        filter(id == dplyr::last(id)) %>%
        ungroup %>%
        # filter(project_status == 'ACTIVE') %>%
        group_by(Stage = project_stage) %>%
        # I have no idea if total_fytd_expenditures is the right column here
        summarise(Amount = sum(total_fytd_expenditures, na.rm = TRUE)) %>%
        ungroup  %>%
        # for now, hard-coding the filter of a few levels
        filter(!Stage %in% c('UNKNOWN', 'PRE-PIPELINE', 'OTHER'))
      
      ap2$fraction = ap2$Amount / sum(ap2$Amount)
      ap2 = ap2[order(ap2$fraction), ]
      ap2$ymax = cumsum(ap2$fraction)
      ap2$ymin = c(0, head(ap2$ymax, n=-1))
      ap2$grp <- 'Amount spent'
      
      # Combine active volume and amount spent
      ap <- bind_rows(ap, ap2)
      
      cols <- colorRampPalette(brewer.pal(n = 9,
                                          name = 'Set3'))(length(unique(ap$Stage)))
      
      ggplot(data = ap, 
             aes(fill=Stage, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(colour="grey30", size = 0.5) +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme_fivethirtyeight() +
        theme(panel.grid=element_blank()) +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank()) +
        scale_fill_manual(name = '',
                          values = cols) +
        guides(fill=guide_legend(ncol=2)) +
        facet_wrap(~grp)
    } else {
      NULL
    }
  })
  
  
  output$user_details <- DT::renderDataTable({
    if(ok()){
      x <- users_this_user$data
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
      
      # Calculate number of active projects
      ap <- as_portfolio_selected$data
      project_statuses <- ap %>%
        arrange(dataset_date) %>%
        group_by(project_id) %>%
        summarise(status = dplyr::last(project_status)) %>%
        ungroup %>%
        group_by(status) %>% 
        tally
        
      active_projects <- project_statuses$n[project_statuses$status == 'ACTIVE']
      if(length(active_projects) == 0){active_projects <- 0}
      closed_projects <- project_statuses$n[project_statuses$status == 'CLOSED']
      if(length(closed_projects) == 0){closed_projects <- 0}
      
      # Calculate total portfolio size
      tps <- ap %>%
        arrange(dataset_date) %>%
        group_by(project_id) %>%
        filter(project_status == dplyr::last(project_status)) %>%
        ungroup %>%
        summarise(x = sum(total_project_size, na.rm = TRUE)) %>%
        .$x
      tps <- comma(round(tps / 1000000))
      tps <- paste0('$', tps, 'M')
      
      fluidPage(
        # fluidRow(
        #   shinydashboard::box(
        #     DT::dataTableOutput('user_details'),
        #     title = 'User details',
        #     width = 12,
        #     solidHeader = TRUE,
        #     status = "primary")
        # ),
        fluidRow(
          valueBox(
            subtitle = "Active Projects", 
            value = active_projects,
            # value = paste0(nrow(longevity_data[longevity_data$active == 1,])), 
            icon = icon("list"),
            color = "orange"
          ),
          valueBox(
            subtitle = "Burn Rate (placeholder)", 
            value = sprintf("%.1f %%", mean(longevity_data[,"burn_rate"])), 
            icon = icon("percent"),
            color = "orange"
          ),
          valueBox(
            subtitle = "Avg Project Size (placeholder)", 
            value = paste0("$", sprintf("%.1f", mean(longevity_data[,"prorated_total_funds_managed_by_ifc"]/1000000)), "M"), 
            icon = icon("bar-chart"),
            color = "orange"
          )
        ),
        fluidRow(
          valueBox(
            subtitle = "Closed Projects", 
            value = closed_projects, 
            icon = icon("check"),
            color = "blue"
          ),
          valueBox(
            subtitle = "Total Portfolio Size", 
            value = tps, 
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
      current_subscriptions <- portfolios_this_user$data %>%
        .$portfolio_name
      current_subscriptions <- paste0(current_subscriptions, collapse = ', ')
      fluidPage(
        fluidRow(
          shinydashboard::box(
            fluidPage(
              fluidRow(tags$p(style = "font-size: 20px;",
                              current_subscriptions
              )),
              fluidRow(actionButton('action_remove', 
                                    'Remove a subscription',
                                    icon = icon('remove')))
            ),
            title = 'Current portfolio subscriptions',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 4
          ),
          shinydashboard::box(
            fluidPage(
              fluidRow(
                column(6,
                       actionButton('action_subscribe', 
                                    'Subscribe to a portfolio',
                                    icon = icon('copy'))),
                column(6,
                       actionButton('action_delete', 
                                    'Delete a portfolio',
                                    icon = icon('eraser')))
              ),
              br(),
              fluidRow(
                column(6,
                       actionButton('action_modify', 
                                    'Modify a portfolio',
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
            width = 5
          ),
          shinydashboard::box(title = 'Your subscription details',
                              status = 'primary',
                              collapsbile = TRUE,
                              collapsed = TRUE,
                              solidHeader = TRUE,
                              width = 3,
                              DT::dataTableOutput('your_package'))
        ),
        uiOutput('editing'),
        uiOutput('edit_content2'),
        uiOutput('control_filters'),
        uiOutput('edit_content3')
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
  observeEvent(input$action_delete, {
    edit_request(TRUE)
    edit_type('Delete')
  })
  output$edit_content <- renderUI({
    et <- edit_type()
    if(et == 'Delete'){
      pr <- portfolios_all$data
      choices <- pr$portfolio_id
      names(choices) <- pr$portfolio_name
      fluidPage(
        fluidRow(
          selectInput('delete_new',
                      'Which portfolio do you want to delete?',
                      choices = choices)
        ),
        fluidRow(
          actionButton('delete_confirm',
                       'Delete',
                       icon = icon('trash'))
        )
      )
    } else if(et == 'Subscribe'){
      pr <- portfolios_this_user$data
      choices <- portfolios_all$data %>%
        filter(!portfolio_id %in% pr$portfolio_id)
      choices_labels <- paste0(choices$portfolio_name, ' (ID:',
                               choices$portfolio_id,
                               ')')
      choices <- choices$portfolio_id
      names(choices) <- choices_labels
      choices <- sort(choices)
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
      # Start with all projects
      filtered_projects <- as_portfolio_all$data %>%
        filter(!duplicated(project_id))
      
      # Filter down based on the filter controls
      if(!is.null(input$filter_region)){
        filtered_projects <- 
          filtered_projects %>%
          dplyr::filter(region_name %in% input$filter_region)
      }
      if(!is.null(input$filter_project_status)){
        filtered_projects <- 
          filtered_projects %>%
          dplyr::filter(project_status %in% input$filter_project_status)
      }
      if(!is.null(input$filter_business_line)){
        filtered_projects <- 
          filtered_projects %>%
          dplyr::filter(primary_business_line_name %in% input$filter_business_line)
      }
      if(!is.null(input$filter_direction)){
        if(input$filter_direction == 'descending'){
          filtered_projects <- filtered_projects %>% 
            dplyr::arrange(dplyr::desc(UQ(sym(input$filter_order))))
        } else {
          filtered_projects <- filtered_projects %>% 
            dplyr::arrange_(input$filter_order)
        }
        out <- filtered_projects %>%
          dplyr::select(project_id, project_name) %>%
          dplyr::filter(!duplicated(project_id))
        po <- out$project_id
        names(po) <- out$project_name
      }
      fluidPage(
        fluidRow(
          textInput('create_new',
                    'What do you want the name of your new portfolio to be?',
                    value = 'ABC')
        ),
        fluidRow(
          selectInput('create_vals',
                      'Which projects do you want to include in this portfolio?',
                      choices = po,
                      multiple = TRUE)
        ),
        fluidRow(
          actionButton('create_confirm',
                       'Create',
                       icon = icon('check-square'))
        )
      )
    } else if(et == 'Modify'){
      pr <- portfolios_all$data
      choices <- pr %>%
        .$portfolio_name
      choices <- sort(choices)
      fluidPage(
        fluidRow(
          selectInput('modify_new',
                      'Which portfolio do you want to modify?',
                      choices = choices,
                      selected = choices[1]),
          helpText('You can only modify those portfolios to which you subscribe and are an administrator')
        ),
        
        fluidRow(
          actionButton('modify_confirm',
                       'Modify',
                       icon = icon('check-square'))
        )
      )
    } else if(et == 'Remove'){
      pr <- portfolios_this_user$data
      choices_labels <- pr$portfolio_name
      choices <- pr$portfolio_id
      names(choices) <- choices_labels
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
  
  # Secondary area for edit content
  # only applicable to the "modification" option
  # because the add/remove rows menus need to be in a 
  # different fluidpage than the reactive element on which they're based
  output$edit_content2 <- renderUI({
    if(ok()){
      et <- edit_type()
      
      if(et == 'Modify'){
        pi <- projects_in()
        po <- projects_out()
        filtered_projects <- as_portfolio_all$data %>%
          filter(!duplicated(project_id)) %>%
          filter(project_id %in% po)
        
        # Filter po based on the filter controls
        if(!is.null(input$filter_region)){
          filtered_projects <- 
            filtered_projects %>%
            dplyr::filter(region_name %in% input$filter_region)
        }
        if(!is.null(input$filter_project_status)){
          filtered_projects <- 
            filtered_projects %>%
            dplyr::filter(project_status %in% input$filter_project_status)
        }
        if(!is.null(input$filter_business_line)){
          filtered_projects <- 
            filtered_projects %>%
            dplyr::filter(primary_business_line_name %in% input$filter_business_line)
        }
        if(!is.null(input$filter_direction)){
          if(input$filter_direction == 'descending'){
            filtered_projects <- filtered_projects %>% 
              dplyr::arrange(dplyr::desc(UQ(sym(input$filter_order))))
          } else {
            filtered_projects <- filtered_projects %>% 
              dplyr::arrange_(input$filter_order)
          }
          out <- filtered_projects %>%
            dplyr::select(project_id, project_name) %>%
            dplyr::filter(!duplicated(project_id))
          po <- out$project_id
          names(po) <- out$project_name
        }
        
        fluidPage(
          fluidRow(column(6,
                          selectInput('modify_add',
                                      'Add projects',
                                      choices = po,
                                      multiple = TRUE,
                                      selected = NULL)),
                   column(6,
                          selectInput('modify_remove',
                                      'Remove projects',
                                      choices = pi,
                                      multiple = TRUE,
                                      selected = NULL)))
        )
      }
    }
  })
  
  
  # Create reactive lists of projects associated with each portfolio
  projects_in <- reactiveVal(value = sort(unique(portfolio_projects$project_id)))
  projects_out <- reactiveVal(value = sort(unique(portfolio_projects$project_id)))
  observeEvent({
    input$action_modify;
    input$modify_new},{
      if(!is.null(input$modify_new)){
        et <- edit_type()
        if(et == 'Modify'){
          this_portfolio <- input$modify_new # this is NULL
          this_portfolio_id <- portfolios_all$data %>%
            filter(portfolio_name == this_portfolio) %>%
            .$portfolio_id
          these_projects_in <-
            portfolio_projects_all$data %>%
            filter(portfolio_id == this_portfolio_id) %>%
            left_join(as_portfolio %>%
                        dplyr::select(project_id,
                                      project_name),
                      by = 'project_id') %>%
            filter(!duplicated(project_id))
          these_projects_out <-
            portfolio_projects_all$data %>%
            filter(portfolio_id != this_portfolio_id) %>%
            left_join(as_portfolio %>%
                        dplyr::select(project_id,
                                      project_name),
                      by = 'project_id') %>%
            filter(!duplicated(project_id))
          these_projects_in_labels <- these_projects_in$project_name
          these_projects_in <- these_projects_in$project_id
          names(these_projects_in) <- these_projects_in_labels
          these_projects_out_labels <- these_projects_out$project_name
          these_projects_out <- these_projects_out$project_id
          names(these_projects_out) <- these_projects_out_labels
          these_projects_in <- these_projects_in[!is.na(these_projects_in) &
                                                   !is.na(these_projects_in_labels)]
          these_projects_out <- these_projects_out[!is.na(these_projects_out) &
                                                     !is.na(these_projects_out_labels)]
          if(!is.null(these_projects_in)){
            projects_in(these_projects_in)
          }
          if(!is.null(these_projects_out)){
            projects_out(these_projects_out)
          }
        }
        
      }
    })
  
  # Observe edits to modify the database
  observeEvent(input$subscribe_confirm, {
    message('SUBSCRIPTION CONFIRMED')
    
    pu <- portfolio_users_all$data
    # Add new rows for the new subscription
    new_rows <- input$subscribe_new
    if(length(new_rows) > 0){
      new_rows <- data_frame(portfolio_id = as.numeric(new_rows),
                             user_id = user_id(),
                             is_creator = FALSE,
                             is_admin = FALSE)
      # Add the new rows to the old rows
      portfolio_users_all$data <- bind_rows(pu, new_rows)
      
      # Update the database
      copy_to(pool, 
              portfolio_users_all$data, 
              "portfolio_users",
              temporary = FALSE,
              overwrite = TRUE)
      
      # Update the session too
      portfolio_users_this_user$data <- portfolio_users_all$data %>%
        filter(user_id == user_id())
      portfolios_this_user$data <- portfolios_all$data %>%
        filter(portfolio_id %in% portfolio_users_this_user$data$portfolio_id)
      portfolio_projects_this_user$data <-
        portfolio_projects_all$data %>%
        filter(portfolio_id %in% portfolios_this_user$data$portfolio_id)
      as_portfolio_this_user$data <- as_portfolio_all$data %>%
        filter(project_id %in% portfolio_projects_this_user$data$project_id)
    }
  })
  observeEvent(input$create_confirm, {
    message('CREATION CONFIRMED')
    
    new_portfolio_name <- input$create_new
    p <- portfolios_all$data
    new_portfolio_id <- max(p$portfolio_id,
                            na.rm = TRUE) + 1
    included_projects <- as.numeric(input$create_vals)
    
    # Create the new portfolio
    new_portfolio <- data_frame(portfolio_id = new_portfolio_id,
                                portfolio_name = new_portfolio_name)
    # ... and update the old stuff
    if(!new_portfolio$portfolio_id %in% p$portfolio_id){
      message('Creating new portfolios table')
      # session
      portfolios_all$data <- bind_rows(p, new_portfolio)
      p <- portfolios_all$data
      # db
      copy_to(pool, 
              portfolios_all$data, 
              "portfolios",
              temporary = FALSE,
              overwrite = TRUE)
    } else {
      'The portfolio id already exists, cant go'
    }
    
    # Create the new portfolio projects
    new_portfolio_projects <- 
      data_frame(portfolio_id = new_portfolio_id,
                 project_id = included_projects)
    # ... and update the old stuff
    pp <- portfolio_projects_all$data
    if(!new_portfolio_projects$portfolio_id[1] %in% pp$portfolio_id){
      message('Creating new portfolio_projects table')
      portfolio_projects_all$data <-
        bind_rows(portfolio_projects_all$data,
                  new_portfolio_projects)
      copy_to(pool, 
              portfolio_projects_all$data, 
              "portfolio_projects",
              temporary = FALSE,
              overwrite = TRUE)
    }
  })
  observeEvent(input$remove_confirm, {
    message('REMOVAL CONFIRMED')
    pu <- portfolio_users_all$data
    ui <- user_id()
    # Remove from pu those which need to be removed
    remove_rows <- as.numeric(input$remove_new)
    remove_rows <-
      pu$user_id == ui &
      pu$portfolio_id %in% remove_rows
    
    if(length(remove_rows) > 0){
      pu <- pu[!remove_rows,]
      portfolio_users_all$data <-pu
      
      # Update the database
      copy_to(pool,
              portfolio_users_all$data,
              "portfolio_users",
              temporary = FALSE,
              overwrite = TRUE)
      
      # Update the session too
      portfolio_users_this_user$data <- portfolio_users_all$data %>%
        filter(user_id == user_id())
      portfolios_this_user$data <- portfolios_all$data %>%
        filter(portfolio_id %in% portfolio_users_this_user$data$portfolio_id)
      portfolio_projects_this_user$data <-
        portfolio_projects_all$data %>%
        filter(portfolio_id %in% portfolios_this_user$data$portfolio_id)
      as_portfolio_this_user$data <- as_portfolio_all$data %>%
        filter(project_id %in% portfolio_projects_this_user$data$project_id)
    }
  })
  observeEvent(input$modify_confirm, {
    message('MODIFICATION CONFIRMED!!!')
    
    #modify_add / modify_remove
    this_portfolio_id <- 
      portfolios_all$data %>%
      filter(portfolio_name == input$modify_new) %>%
      .$portfolio_id
    # Get the portfolio_projects table for this portfolio id
    pp <- portfolio_projects_all$data
    pp <- pp %>% filter(portfolio_id == this_portfolio_id)
    
    # Add / remove rows if applicable
    add_these <- as.numeric(input$modify_add)
    remove_these <- as.numeric(input$modify_remove)
    
    if(!is.null(add_these)){
      if(length(add_these) > 0){
        pp <- pp %>%
          bind_rows(
            data_frame(portfolio_id = this_portfolio_id,
                       project_id = add_these)
          ) %>%
          dplyr::filter(!duplicated(project_id))
      }
    }
    if(!is.null(remove_these)){
      if(length(remove_these) > 0){
        pp <- pp %>%
          dplyr::filter(!project_id %in% remove_these)
      }
    }
    
    # pp is just for this portfolio_id
    # Update the entire portfolio_projects table
    new_pp <- portfolio_projects_all$data
    new_pp <- new_pp %>%
      filter(portfolio_id != this_portfolio_id) %>% 
      bind_rows(pp)
    # Update the database
    copy_to(pool,
            new_pp,
            "portfolio_projects",
            temporary = FALSE,
            overwrite = TRUE)
    
    # Update the session too
    portfolio_projects_all$data <- new_pp
    portfolio_users_this_user$data <- portfolio_users_all$data %>%
      filter(user_id == user_id())
    portfolios_this_user$data <- portfolios_all$data %>%
      filter(portfolio_id %in% portfolio_users_this_user$data$portfolio_id)
    portfolio_projects_this_user$data <-
      portfolio_projects_all$data %>%
      filter(portfolio_id %in% portfolios_this_user$data$portfolio_id)
    as_portfolio_this_user$data <- as_portfolio_all$data %>%
      filter(project_id %in% portfolio_projects_this_user$data$project_id)
  })
  
  observeEvent(input$delete_confirm, {
    p <- portfolios_all$data
    message('DELETION CONFIRMED!!!')
    portfolio_to_delete <- as.integer(input$delete_new)
    if(length(portfolio_to_delete == 1)){
      p <- p %>%
        filter(portfolio_id != portfolio_to_delete)
    }
    # Update session # for all
    portfolio_projects_all$data <- portfolio_projects_all$data %>%
      filter(portfolio_id != portfolio_to_delete)
    portfolio_users_all$data <- portfolio_users_all$data %>%
      filter(portfolio_id != portfolio_to_delete)
    portfolios_all$data <- portfolios_all$data %>%
      filter(portfolio_id != portfolio_to_delete)
    # Update session # for user
    portfolio_projects_this_user$data <- portfolio_projects_this_user$data %>%
      filter(portfolio_id != portfolio_to_delete)
    portfolio_users_this_user$data <- portfolio_users_this_user$data %>%
      filter(portfolio_id != portfolio_to_delete) 
    portfolios_this_user$data <- portfolios_this_user$data %>%
      filter(portfolio_id != portfolio_to_delete)
    as_portfolio_this_user$data <- as_portfolio_this_user$data %>%
      filter(project_id %in% portfolio_projects_this_user$data$project_id)
    # Update database
    copy_to(pool,
            portfolio_projects_all$data,
            "portfolio_projects",
            temporary = FALSE,
            overwrite = TRUE)
    copy_to(pool,
            portfolio_users_all$data ,
            "portfolio_users",
            temporary = FALSE,
            overwrite = TRUE)
    copy_to(pool,
            portfolios_all$data ,
            "portfolios",
            temporary = FALSE,
            overwrite = TRUE)
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
  
  output$available_portfolios <- 
    renderDataTable({
      x <- portfolio_projects_all$data
      x <- x %>% 
        group_by(portfolio_id) %>%
        summarise(`Number of projects` = length(unique(project_id)))
      x <- x %>% left_join(portfolios_all$data,
                           by = 'portfolio_id') %>%
        dplyr::select(portfolio_name, portfolio_id, `Number of projects`) %>%
        dplyr::rename(Name = portfolio_name,
                      ID = portfolio_id)
      DT::datatable(x,
                    rownames = FALSE,
                    options = list(pageLength = 5))
    })
  
  output$your_package <- 
    renderDataTable({
      x <- portfolio_projects_this_user$data
      x <- x %>%
        summarise(Portfolios = length(unique(portfolio_id)),
                  Projects = length(unique(project_id)))
      DT::datatable(x,
                    rownames = FALSE,
                    options = list(dom = 't'))
    })
  
  output$edit_content3 <- renderUI({ 
    if(ok()){
      fluidPage(
        fluidRow(
          shinydashboard::box(title = 'Available portfolios',
                              status = 'primary',
                              collapsible = TRUE,
                              collapsed = TRUE,
                              width = 12,
                              DT::dataTableOutput('available_portfolios'))),
        fluidRow(
          shinydashboard::box(title = 'All projects in portfolio(s)',
                              status = 'primary',
                              collapsible = TRUE,
                              collapsed = TRUE,
                              width = 12,
                              DT::dataTableOutput('details_table'))
        )
      )
    }
  })
  
  output$details_table <- DT::renderDataTable({
    right <- 
      portfolio_projects_all$data %>% 
      left_join(portfolios_all$data)
    left <- as_portfolio_all$data %>%
      dplyr::select(project_id,
                    project_name)  
    details <- left_join(left, right)
    details <- details %>%
      group_by(project_id) %>%
      summarise(project_name = dplyr::first(project_name),
                portfolios = paste0(sort(unique(portfolio_name)),
                                    collapse = ', ')) %>%
      mutate(portfolios = ifelse(nchar(portfolios) < 1,
                                 'None',
                                 portfolios))
    names(details) <- Hmisc::capitalize(gsub('_', ' ', names(details)))
    details
  })
  
  output$control_filters <- renderUI({
    
    # Note: as of now, all the below inputs are hard-coded
    # and they're not being used anywhere
    
    et <- edit_type()
    if(et %in% c('Create', 'Modify')){
      # Show the filters only in the case of creation or modification
      # Deletion and subscription doesn't require filters
      
      #Create a title which will show what you're filtering for
      controls_title <- ifelse(et == 'Create',
                               'Filter which projects will go in your newly created portfolio',
                               ifelse(et == 'Modify',
                                      'Filter which projects you want to consider adding to this portfolio', NA))
      
      # Create some text to explain further
      controls_text <- ifelse(et == 'Create',
                              'Use the below controls to narrow down potential projects to only those which match the region, business line, or status you\'re interested in.',
                              ifelse(et == 'Modify',
                                     'Use the below controls to narrow down the "Add projects" field above to only those which match the region, business line, or status you\'re interested in.', NA))
      
      fluidPage(
        fluidRow(
          box(
            title = controls_title,
            status = "warning",
            solidHeader = TRUE,
            width = NULL,
            fluidPage(
              fluidRow(p(controls_text)),
              fluidRow(
                column(4,
                       selectInput("filter_region", 
                                   "Region: ", 
                                   choices = sort(unique(as_portfolio$region_name)),
                                   selected = sort(unique(as_portfolio$region_name))[1],
                                   multiple = TRUE)),
                column(4,
                       selectInput("filter_business_line", 
                                   "Business Line: ", 
                                   choices = sort(unique(as_portfolio$primary_business_line_name)),
                                   selected = sort(unique(as_portfolio$primary_business_line_name))[1],
                                   multiple = TRUE)),
                column(4)
              ),
              fluidRow(
                column(4,
                       checkboxGroupInput("filter_project_status", "Display Projects With Status:", 
                                          sort(unique(as_portfolio$project_status)),
                                          inline = TRUE,
                                          selected = sort(unique(as_portfolio$project_status)))),
                column(4,
                       selectInput("filter_order", 
                                   "Order by:", 
                                   c("Start Date" = "implementation_start_date", 
                                     "End Date" = "implementation_end_date", 
                                     "Size ($M)" = "total_project_size"), 
                                   selected = c("implementation_end_date"))),
                column(4,
                       selectInput("filter_direction", 
                                   "Direction:", 
                                   c("Ascending" = "ascending", 
                                     "Descending" = "descending"), 
                                   selected = c("ascending")))
              )
            )  
          )   
        ))
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
    
    
    pc <- portfolios_this_user$data
    portfolio_choices <- pc$portfolio_id
    names(portfolio_choices) <- pc$portfolio_name
    filter_choices <- sort(unique(as_portfolio$project_status))
    select_box <- 
      shinydashboard::box(
        fluidPage(
          fluidRow(
            selectInput('selected_portfolio',
                        'Select any of your subscribed portfolios below',
                        choices = portfolio_choices,
                        selected = portfolio_choices,
                        multiple = TRUE)
          )
        ),
        title = 'Select portfolio(s)',
        status = 'warning',
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        width = 3
      )
    
    filter_box <-
      shinydashboard::box(
        fluidPage(
          fluidRow(
            selectInput('filter_portfolio',
                        'Filter your selected portfolio(s) by status',
                        choices = filter_choices,
                        selected = filter_choices,
                        multiple = TRUE)
          ),
          fluidRow(
            selectInput('filter_portfolio_region',
                        'Filter your selected portfolio(s) by region',
                        choices = sort(unique(as_portfolio$region_name)),
                        selected = sort(unique(as_portfolio$region_name)),
                        multiple = TRUE)
          )
        ),
        title = 'Filter portfolio(s)',
        status = 'warning',
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        width = 5
      )
    
    chart_box <- 
      shinydashboard::box(
        fluidPage(
          fluidRow(
            plotOutput('apv_plot', height = '240px')
          )
        ),
        title = 'Portfolio volume and spending by stage',
        width = 4,
        height = '320px',
        solidHeader = TRUE,
        status = "primary")
    
    if(okay){
      welcome_row <- NULL
    } else {
      welcome_row <- 
        fluidPage(
          fluidRow(
            column(welcome_width,
                   welcome_box),
            column(filter_width,
                   uiOutput('portfolios_text')
            )))
    }
    
    
    fluidPage(
      welcome_row,
      if(!okay){
        fluidRow(
          column(2),
          shinydashboard::box(
            tags$p(style = "font-size: 16px;",
                   paste0('During the development phase, you can log-in using any of the names below as the username.')
            ),
            DT::dataTableOutput('credentials_table'),
            title = 'Credentials',
            status = 'warning',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 7
          ),
          column(2))
      } else {
        fluidRow(select_box, filter_box,  chart_box)
      }
    )
  })
  output$credentials_table <- DT::renderDataTable({
    x <- users %>% dplyr::select(email)
    DT::datatable(x,
                  options = list(dom = 't'),
                  rownames = FALSE,
                  colnames = '')
  })
  
  
  # output$portfolios_text <- renderUI({
  #   okay <- ok()
  #   out <- NULL
  #   if(okay){
  #     # Get the portfolios available to the user
  #     p <- portfolios_this_user$data
  #     p <- p$portfolio_name
  #     n <- length(p)
  #     if(length(p) > 0){
  #       p <- paste0(p, collapse = ', ')
  #       out <- 
  #         valueBox(value = n,
  #                subtitle = 'Subscribed portfolios',
  #                icon = icon('tasks'),
  #                color = 'blue',
  #                width = 12)
  #     } 
  #   } 
  #   return(out)
  # })
  
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
    print(gg_spending_fish())
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
  
  # Parse the GET query string
  output$queryText <- renderText({
    # Add, for example, ?foo=123
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    out <- paste(names(query), query, sep = "=", collapse=", ")
    if(nchar(out) > 0){
      out <- paste0('PARSED QUERY STRING IS\n',
                    out)
      return(out)
    } else {
      return(NULL)
    }
  })
  
  # # Url
  # shinyURL.server()
  
  # On session end, close
  session$onSessionEnded(function() {
    message('Saving the current url')
    enableBookmarking(store = 'url')
    message('Session ended. Closing the connection pool.')
    tryCatch(pool::poolClose(pool), error = function(e) {message('')})
  })
  
}

shinyApp(ui, server, enableBookmarking = 'url')