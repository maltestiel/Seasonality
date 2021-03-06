library(shiny)
shinyServer(function(input,output){
  
  dta <- eventReactive(input$searchButton,{
    srch <- unlist(strsplit(input$searchText,","))
    smat <- seasonality(start_year = input$start_year,
                        start_month = input$start_month,
                        start_day = input$start_day,
                        end_year = input$end_year,
                        end_month = input$end_month,
                        end_day = input$end_day,
                        stocks = srch,
                        src = input$src,
                        mva = input$roll_window)
  })
  
  output$stockchart <- renderDygraph({
    if(is.null(dta())) return(NULL)
    dygraph(dta())%>%
      dyAxis("x",valueFormatter=JS(getMonthDay), axisLabelFormatter=JS(getMonth))
  })
  
  
    
})
