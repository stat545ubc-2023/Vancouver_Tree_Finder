#shiny app
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)

vt <- read.csv("vt.csv", stringsAsFactors = FALSE)
#print(str(vt))


ui <- fluidPage(h1("How to find Vancouver trees"),
                h3("Use this app to find where specific trees are in Vancouver"),
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

#Feature : Used DT package to create an interactive table, this helps user select specific variables they want to see faster


shinyApp(ui = ui, server = server)



