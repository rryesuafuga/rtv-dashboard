library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "RTV Dashboard Test"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Test", tabName = "test", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "test",
        h2("RTV Dashboard - Deployment Test"),
        p("If you see this, deployment is working!"),
        hr(),
        h3("System Info:"),
        verbatimTextOutput("sysinfo"),
        h3("Available Files:"),
        verbatimTextOutput("files")
      )
    )
  )
)

server <- function(input, output, session) {
  output$sysinfo <- renderPrint({
    list(
      R_Version = R.version.string,
      Working_Dir = getwd(),
      Sys_Date = Sys.Date()
    )
  })
  
  output$files <- renderPrint({
    list(
      Root_Files = list.files(),
      R_Files = list.files("R"),
      Data_Files = list.files("data")
    )
  })
}

shinyApp(ui = ui, server = server)

