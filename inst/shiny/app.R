library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(RColorBrewer)
source('global.R')

# Define acceptable password username combinations
users <- data.frame(username = letters[1:5],
                    password = 1:5)

# Height for main page charts
main_page_plot_height_num <- 250
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