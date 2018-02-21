## ui.R ##

dashboardPage(
  dashboardHeader(title = "Stockradar"),
  dashboardSidebar(
    sidebarSearchForm("searchText",
                      label = "select group or search..."
                      ,"searchButton"),
    selectInput("stock_grp","Select Group",
                c("use search","tech",
                  "cars"),
                selected = "use search"),
    selectInput("src","Select Source",
                c("yahoo","google",
                  "FRED","oanda"),
                selected = "yahoo"),
    numericInput("start_year","Select Start Year",
                 "2015"),
    numericInput("start_month","Select Start Month",
                 "01",
                 min = 1, max = 12),
    numericInput("start_day","Select Start Day",
                 "01",
                 min = 1, max = 31),
    numericInput("end_year","Select End Year",
                 "2017",
                 min = 1900, max = 2100),
    numericInput("end_month","Select End Month",
                 "04",
                 min = 1, max = 12),
    numericInput("end_day","Select End Day",
                 "30",
                 min = 1, max = 31),
    sliderInput("roll_window","Mov. Avg Window",
                min = 5, max = 50, value = 10)),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css", href = "custom.css")),
    tabItem(tabName = "home",
            fluidRow(
              box(title = "Stock Comparison", 
                  dygraphOutput("stockchart"),
                  width = 12)
            )
    ),
    tabItem(tabName = "home",
            fluidRow(
              box(title = "Stock Comparison", 
                  dataTableOutput("test"),
                  width = 12,
                  height = 500)
            )
    )
  )
)
