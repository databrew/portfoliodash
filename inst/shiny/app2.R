library(shiny)
library(shinydashboard)
library(dplyr)

# Define acceptable password username combinations
users <- data.frame(username = letters[1:5],
                    password = 1:5)

header <- dashboardHeader(title="FIG SSA MEL Dashboard")
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
      fluidPage()
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
  
  output$longevity_menu <-
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
  })
  output$apv_plot <- renderPlot({
    barplot(10:1)
  })
  output$aps_plot <- renderPlot({
    plot(10:1, (10:1)^3)
  })
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
            status = "primary"),
          shinydashboard::box(
            plotOutput('apv_plot'),
            title = 'Active portfolio volume',
            width = 4,
            solidHeader = TRUE,
            status = "primary"),
          shinydashboard::box(
            plotOutput('aps_plot'),
            title = 'Active portfolio sp',
            width = 4,
            solidHeader = TRUE,
            status = "primary")),
        fluidRow(valueBox(value = 1, 
                          subtitle = 'Some subtitle', 
                          icon = NULL, 
                          color = "aqua", 
                          width = 4,
                          href = NULL),
                 valueBox(value = 1, 
                          subtitle = 'Some subtitle', 
                          icon = NULL, 
                          color = "aqua", 
                          width = 4,
                          href = NULL),
                 valueBox(value = 1, 
                          subtitle = 'Some subtitle', 
                          icon = NULL, 
                          color = "aqua", 
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
}

shinyApp(ui, server)