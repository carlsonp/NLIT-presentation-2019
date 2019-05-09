library(shiny)
library(dplyr) # for easy dataframe manipulation
library(readr)
library(ggplot2) # for the "mpg" dataset
library(plotly) # for the interactive graph
library(shinyWidgets) # for the multi-select dropdown year filter

# save dataset for use in Tableau
# https://ggplot2.tidyverse.org/reference/mpg.html
# write_csv(mpg, path="mpg.csv")

mpg <- read_csv("mpg.csv")

# calculate average MPG by combining city and highway MPG (rough approximation)
mpg_df <- mpg %>% dplyr::mutate(avg_mpg = ((cty + hwy)/2))

ui <- fluidPage(
   
   titlePanel("Shiny Average MPG Example"),
   
   # Sidebar with a picker for year
   sidebarLayout(
      sidebarPanel(
          pickerInput("year",
                      label="Filter Year:",
                      choices = sort(unique(mpg_df$year)),
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE),
                      selected = sort(unique(mpg_df$year))
          )
      ),
      
      mainPanel(
         plotlyOutput("plotlyBargraph")
      )
   )
)

# Define server logic
server <- function(input, output) {
    
   getFilteredDF <- reactive({
       return(mpg_df %>% dplyr::filter(year %in% input$year))
   })
   
   getPlotlyDF <- reactive({
       return(getFilteredDF() %>% dplyr::group_by(class) %>%
                  summarize(avg_mpg_class = mean(avg_mpg)) %>% as.data.frame()
              )
   })
   
   output$plotlyBargraph <- renderPlotly({
      # https://plot.ly/r/bar-charts/
      plot_ly(
          x = getPlotlyDF()$class,
          y = getPlotlyDF()$avg_mpg_class,
          name = "Average MPG by Car Type",
          type = "bar"
      ) %>%
      layout(
          xaxis = list(title="Car Type"),
          yaxis = list(title="Average MPG")
      )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
