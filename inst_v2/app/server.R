library(shiny)
library(DT)
shinyServer(function(input,output){
  
  dta <- eventReactive(input$searchButton,{
    
    if(is.null(input$searchText) |
               input$stock_grp != 'use search'){
      
      stock_list <- list(cars = c("TSLA","BMW.DE"),
                         tech = c("MSFT","GOOG"))
      
      srch <- stock_list[[input$stock_grp]]
      srch
      } else {
        srch <- unlist(strsplit(input$searchText,","))  
      }  
      
      smat <- seasonality(start_year = input$start_year,
                          start_month = input$start_month,
                          start_day = input$start_day,
                          end_year = input$end_year,
                          end_month = input$end_month,
                          end_day = input$end_day,
                          stocks = srch,
                          src = input$src,
                          mva = input$roll_window)
      smot <- tot_return(smat)
      li <- list()
      li$plot <- smat
      li$table <- smot
      li  
    
  })
  
  output$stockchart <- renderDygraph({
    if(is.null(dta())) return(NULL)
    dygraph(dta()$plot)%>%
      dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonthDay))
  })
  
  
  
  output$test <- renderDataTable(
    dta()$table)
  
})
