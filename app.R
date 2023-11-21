#shiny app
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)

vt <- read.csv("vt.csv", stringsAsFactors = FALSE)



ui <- fluidPage(h1("How to find Vancouver trees"),
                h5("This shiny app can be used to find where specific trees are in Vancouver while filtering based on diameter, height, and curb location. A plot is then generated according to counts in specific neighbourhoods when applicable and a table of the results is also shown.
                  The open dataset vancouver_trees was used:
                   Vancouver, British Columbia, Canada https://opendata.vancouver.ca/explore/dataset/street-trees/information
                   Includes information licensed from the Open Government Licence – Vancouver. License: https://opendata.vancouver.ca/pages/licence/"),
                titlePanel("Find trees according to diameter, height and location on/off curb"),
                sidebarLayout(
                sidebarPanel(
                  sliderInput("diameterInput", "Diameter", 0, 1000, c(300, 500), post = "m"),
                  sliderInput("height_range_idInput", "Height Range ID", 0, 10, c(2)),
                  checkboxGroupInput("curbInput", "Is Tree Located on Curb (Yes = Y or No = N)", choices = c("Y", "N"), selected = "Y")),
                  colourpicker::colourInput(paste("col", sep="_"), "Choose colour:", "black")),
                mainPanel(
                  tabsetPanel(
                  tabPanel("Plot", plotOutput("coolplot")),
                  tabPanel("Table",DT::dataTableOutput("results")),
                  br(), br())
                ))
#Feature: Added parameter to plot to allow user to change colour of bars, this can help visualize bars better and makes it more appealing
#Feature: Put plot and table in separate tabs, this makes the app more organized and visually appealing
#Feature: Multiple select for curb, this allows more variation and analysis of data as it offers more options for selection easily

server <- function(input, output){
  filtered <- reactive({
    if (is.null(input$curbInput)) {
      return(NULL)
    }
    vt %>%
      filter(diameter >= input$diameterInput[1],
           diameter <= input$diameterInput[2],
           height_range_id == input$height_range_idInput,
           curb %in% input$curbInput
  )
  })

  output$coolplot <- renderPlot({
  ggplot(filtered(), aes(neighbourhood_name)) +
    geom_bar(fill = input$col)

})


  output$results <- DT::renderDataTable({
  filtered()
  })
}

#Feature : Used DT package to create an interactive table, this helps user select specific variables they want to see faster.


shinyApp(ui = ui, server = server)



