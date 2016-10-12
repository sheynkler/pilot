library(shinyjs)
source("timeFunctions.R")
source("jhfjzfjf.R")
library(shiny)
#load("fit_step.RData")
shinyServer(function(input, output, session) {
  
  time1 <- reactive({
    time1 <- input$dates[1]
    hh1 <- input$hh1
    hhh1 <- paste(hh1, "00", "00", sep = ":")
    time_hh <- paste(time1, hhh1)
    time1 <- as.POSIXct(strptime(time_hh, "%Y-%m-%d %H:%M:%S"))
    #time1 <- time1 + hh1*3600
    time1
  })
  time2 <- reactive({
    time2 <- input$dates[2]
    hh2 <- input$hh2
    hhh2 <- paste(hh2, "00", "00", sep = ":")
    time_hh <- paste(time2, hhh2)
    time2 <- as.POSIXct(strptime(time_hh, "%Y-%m-%d %H:%M:%S"))
    #time1 <- time1 + hh1*3600
    if(time2 < time1()) time2 <- time1()
    time2
  })


  output$value <- renderPrint({ 
    
    paste(time1() , time2(), sep = "   ")
    })
  
  predictors <- reactive({
    data_predictor_end <- create_predictor_2(time1(), time2())
    data_predictor_end
  })
  output$value_predictors <- renderPrint({ 
    
    summary(fit())
  })
  fit <- reactive({
    group <- input$names_lm
    names_lm <- paste0("fits/fit_", group , "_step.RData")
    load(names_lm)
    fit_step
  })
  forcast <- reactive({
    predict(fit(), predictors())
  })
  data_show <- reactive({
    forcast <- round(as.numeric(forcast()), digits = 2)
    forcast <- ifelse(forcast < 0, 0, forcast)
    t <- seq(time1(), time2(), by = "hour")
    data_show <- data.frame(time_start = substr(as.character(t),1,13), forcast)
    data_show
      
    })
  output$forcast <- renderTable({ 

    data_show()
  }, options = list(include.rownames = FALSE)
  )
  output$forcast_plot <- renderPlot({ 
    data_show <- data_show()
    if(nrow(data_show) > 1) barplot(data_show$forcast, col = "cornflowerblue", names.arg = data_show$time_start, main = "Predictive viewing time" ) else NULL
    
  })
  output$download_table <- downloadHandler(
    filename <- "Prognose.csv",
    content = function(con){
      write.csv(data_show(), con)
    }
  )
  login_pw <- reactive({
    if (input$login_button == 0) return()
    isolate({
      paste(input$login, input$password)
    })
  })
  
  
  observe({
    if(is.null(login_pw()) == F){
          if (input$login_button > 0 & login_pw() == login_pw_orig) {
      shinyjs::show(id = "content", anim = TRUE)
      shinyjs::hide(id = "login_div", anim = TRUE)
    }
    }
    

  })
  output$login_error <- renderText({
    if (input$login_button == 0) return (NULL)
    if (input$login_button > 0 & login_pw() != login_pw_orig) {
      "Login or password are false"
    }
  })
  
  

  

})
