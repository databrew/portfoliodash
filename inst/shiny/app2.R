library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)

# Define acceptable password username combinations
users <- data.frame(username = letters,
                    password = 1:26)

header <- dashboardHeader(title="FIG SSA MEL Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    textOutput('submit_text'),
    uiOutput('username_ui'),
    uiOutput('password_ui'),
    uiOutput('submit_ui'),
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
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
  
  output$submit_text <- renderText({
    okay <- ok()
    message(okay)
    if(okay){
      paste0('Logged in as ',
             user())
    } else {
      'Log in:'
    }
  })
  
  # Evlaute whether a correct username / password has been entered
  ok <- reactiveVal(value = FALSE)
  observeEvent(input$submit,{
    out <- FALSE
    username <- input$username
    message(username)
    password <- input$password
    message(password)
    if(!is.null(username) & !is.null(password)){
      if(password == users$password[users$username == username]){
        out <- TRUE
      }
    }
    message(out)
    ok(out)
  }
  )
  
  # Create an observing user
  user <- reactiveVal(value = NULL)
  observeEvent(input$submit,{
    if(ok()){
      user(input$user)
      message(user())
    }
  })
  
  
}

shinyApp(ui, server)