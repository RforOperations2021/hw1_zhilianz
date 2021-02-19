#Homework1
#Anno Zhang
#AndrewID: zhilianz


library(shiny)
library(ggplot2)
library(dplyr)
data <- read.csv("data.csv",col.names = c("State", "Cases","Deaths","Fatality","WeeklyNewCases","VaccineAdministered"))
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Public Safety in Pittsburgh"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("type",
                  "Select the data you want to view",
                  choices = c("MajorCrimeReports","Non-majorCrimeReports","OtherPoliceReports","SumofCrime","CrimePer100Person")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
      ggplot(data, aes(data$Neighborhood, data$CrimePer100Person), colur = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

