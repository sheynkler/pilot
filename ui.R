
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shinythemes)
library(shiny)
library(shinyjs)
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  useShinyjs(),

  # Application title
  titlePanel("Time Serial Prediction Modeling: TV Viewing Prognose Calculator"),
  br(),

  # Sidebar with a slider input for number of bins
  
    fluidRow(
      column(3,
             div(id = "login_div",
               textInput("login", label = h3("Login:"), value = NULL),
             passwordInput("password", "Password:"),
             textOutput("login_error"),
             actionButton("login_button", "Login")
              
                      
                      ),
             selectInput("names_lm", label = h3("Choose a group"), choices = as.list(names_target)),
             dateRangeInput("dates", label = h3("Choose a date"), start = Sys.Date(), end = Sys.Date() + 1),
             h3("  Choose a time"),
             fluidRow(
               
               column(6,
                      numericInput("hh1", label = NULL, value = 1, min = 0, max = 23, width = 90)
                      ),
               column(6,
                      numericInput("hh2", label = NULL, value = 1, min = 0, max = 23, width = 90)
                      
               )
             ),
             
             
             
             h3("Selected:"),
               verbatimTextOutput("value")),
             
      column(9,
             
             shinyjs::hidden(div(id = "content",
               fluidRow(
               column(3,
                      (tableOutput("forcast"))
                      ),
               column(9,
                      plotOutput("forcast_plot"),
                      #h3(textOutput("forcast_sum")),
                      downloadButton("download_table", label = "Download predictive values as CSV file")
               )
             )))
             
      )
      
    
  )
))
