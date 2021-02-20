#Homework1
#Anno Zhang
#AndrewID: zhilianz


library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyBS)

#Read data from csv file
data <- read.csv("data.csv",col.names = c("State", "Cases","Deaths","Fatality","WeeklyNewCases","VaccineAdministered"))

ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 CASES IN US"),
    
    sidebarLayout(
        sidebarPanel(
            #Slider input to determine sample size
            sliderInput("size",
                        "How many states you want to select",
                        min=1, 
                        max = 40,
                        value = 10),
            
            #Radio button input to select data genre
            radioButtons("type",
                         "Select data you want to view",
                         choices = c("Cases","Deaths","Fatality","7 day New Cases","Vaccine Administered")
            ),
            
            #checkbox input to display table
            checkboxInput("table","Show Table",value = FALSE),
            
            #download button to download dataframe into csv file
            downloadButton("download","Download Data")
            
            
        ),
        
        mainPanel(
            plotOutput("plot"),
            DT::dataTableOutput("table")
            
        )
    )
)

server <- function(input, output) {
    
    observeEvent(input$size,{
        selected_data <<- data %>% sample_n(input$size, replace = FALSE)
        
        #Drawing different plots based on user's selection
        output$plot <- renderPlot({
            
            if(input$type == 'Cases'){
                slices <- selected_data$Cases
                pie(slices,label = selected_data$State,main = "Total cases proportion in selected states")
                
            }
            else if(input$type =='Deaths'){
                
                ggplot(selected_data, aes(x =  State, y =Deaths)) + geom_col(fill ="pink") +ggtitle("Death cases in selected states")
                
            }
            else if(input$type == 'Fatality'){
                ggplot(selected_data, aes(x =  Fatality, y = State)) + geom_point()+ggtitle("Fatality rate in selected states")
                
            }
            else if(input$type == '7 day New Cases'){
                ggplot(selected_data, aes(x = WeeklyNewCases, y = State)) + geom_col(fill ="lightblue")+ggtitle("Weekly new cases in selected states")
                
                
            }
            else if(input$type == "Vaccine Administered"){
                ggplot(selected_data, aes(x =  VaccineAdministered, y =State)) + geom_col(fill ="lightgreen")+ggtitle("Vaccine administered in selected states")
                
            }
            
        })
        
        output$table = DT::renderDataTable(
            if (input$table){
                DT::datatable(data = selected_data,
                              options = list(pageLength = 10),
                              rownames = FALSE
                )
            }
        )
    })
    
    
    
    output$download <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(data, file,row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

