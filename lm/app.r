#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# install.packages("ggplot2")
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling Dashboard"),
    h4("BSGP 7030 - Shiny Project"),
    h4("Anjali Byappanahalli"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Horizontal line ----
            tags$hr(),

            # Action button: linear model over scatter plot
            actionButton('run', label = "Linear Model data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
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
    # Original scatter plot
    output$scatterPlot <- renderPlot({
        ggplot() + 
        geom_point(aes(x = dataInput()$x, y = dataInput()$y),
        color = 'red') + 
        ggtitle("X vs Y") + 
        xlab('X') + 
        ylab('Y')
    })
    
    # Model data
    model <- eventReactive(input$run, {
        lm(formula = y ~ x,
        data = dataInput())
    })

    # Linear regression plot
    output$lmPlot <- renderPlot({
        coeffs <- coef(model(), 2)
        coef1 <- round(coeffs, 3)
        int <- round(coeffs[1], 3)
        slope <- round(coeffs[2], 3)
        r2 <- round(summary(model())$r.squared, 2)

        ggplot() + geom_point(aes(x = dataInput()$x, y = dataInput()$y), color = "red") + 
        geom_line(aes(x = dataInput()$x, y = predict(model(), datadata = dataInput())), 
        color = "blue") +
        ggtitle("X vs Y") +
        xlab("X") +
        ylab("Y") +
        geom_text(aes(x=12, y=14, label = paste("Intercept: ", int))) +
        geom_text(aes(x=12, y=13, label = paste("Slope: ", slope))) +
        geom_text(aes(x=12, y=12, label = paste("Coefficient: ", coef1))) +
        geom_text(aes(x=12, y=11, label = paste("R squared: ", r2)))
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
