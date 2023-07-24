#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(title = "Linear Modeling Dashboard"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput(
        inputId = "file1",
        label = "Choose CSV File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      Unknown Ui Code,
      checkboxInput(
        inputId = "header",
        label = "Header",
        value = TRUE
      ),
      radioButtons(
        inputId = "sep",
        label = "Separator",
        choices = c(Comma = ",", Semicolon = ";", Tab = "	"),
        selected = ","
      ),
      radioButtons(
        inputId = "quote",
        label = "Quote",
        choices = c(None = "", Double Quote = """, Single Quote = "'"),
        selected = """
      ),
      Unknown Ui Code,
      radioButtons(
        inputId = "disp",
        label = "Display",
        choices = c(Head = "head", All = "all"),
        selected = "head"
      )
    ),
    mainPanel = mainPanel(
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "lmPlot"),
      tableOutput(outputId = "contents")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmtPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
