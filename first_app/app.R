#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(southafricastats)
library(dplyr)
library(ggplot2)
library(DT)

mortality_noAllCause <- mortality_zaf %>%
  filter(indicator != "All causes")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Africa Mortality Statistics - shinyApp"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "province",
                     label = "Province",
                     choices = unique(mortality_zaf$province),
                     selected = "Gauteng",
                     multiple = T
                     ),
         checkboxInput(inputId = "showTable",
                       label = "Display Table",
                       value = F)
      ),
      
      # Show a plot
      mainPanel(
         plotOutput(outputId = "LinePlot"),
         dataTableOutput(outputId = "mortalityTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$LinePlot <- renderPlot({
     mortality_noAllCause %>% 
       filter(province %in% input$province) %>% 
        ggplot(aes(x = year, y = deaths, color = indicator)) + 
          geom_line(alpha = 0.8, size = 1.5) + 
          facet_wrap(~province) +
          theme_minimal(base_size = 18)
   })
   
   output$mortalityTable <- renderDataTable({
     if (input$showTable) {
       datatable(mortality_noAllCause %>% 
         filter(province %in% input$province)
       ) 
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

