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

    # Application title
    titlePanel("Price of Home vs. Square Footage Based on Neighborhood"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
            selectInput("neighborhood", 
                        label="Please select a Neighborhood to View",
                       choices=c("NAmes","Edwards", "BrkSide" ))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(readr)
    output$distPlot <- renderPlot({
      housing <-- input$file1
      read.csv(housing$datapath, header = input$header)
      
     # housing = read.csv("Stats1Project/train.csv")
      if(input$neighborhood == "NAmes"){
       names= housing %>% filter(Neighborhood == "NAmes") %>% ggplot(aes(x = (GrLivArea), y = SalePrice/1000)) + geom_point() + ggtitle("NAmes Neighborhood Price of Home vs. Square Footage")
        
      } else if(input$neighborhood == "Edwards"){
        edwards= housing %>% filter(Neighborhood == "Edwards") %>% ggplot(aes(x = (GrLivArea), y = SalePrice/1000)) + geom_point() + ggtitle("Edwards Neighborhood Price of Home vs. Square Footage")
        
      } else if(input$neighborhood == "Brkside"){
        brk= housing %>% filter(Neighborhood == "Brkside") %>% ggplot(aes(x = (GrLivArea), y = SalePrice/1000)) + geom_point() + ggtitle("Brkside Neighborhood Price of Home vs. Square Footage")

      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
