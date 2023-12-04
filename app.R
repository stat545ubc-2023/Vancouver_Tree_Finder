#shiny app
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(colourpicker)
library(shinythemes)



vt <- read.csv("vt.csv", stringsAsFactors = FALSE)



ui <- fluidPage(theme = shinytheme("cerulean"),
                h1("How to find Vancouver trees"),
                img(src = "tree.png", height = 90, width = 90),
                img(src = "tree1.png", height = 90, width = 90),
                h5("This shiny app can be used to find where specific trees are in Vancouver while filtering based on diameter, height, and curb location. A plot is then generated according to counts in specific neighbourhoods when applicable and a table of the results is also shown.
                  The open dataset vancouver_trees was used:
                   Vancouver, British Columbia, Canada https://opendata.vancouver.ca/explore/dataset/street-trees/information
                   Includes information licensed from the Open Government Licence – Vancouver. License: https://opendata.vancouver.ca/pages/licence/"),
                titlePanel("Find trees according to diameter, height and location on/off curb"),
                sidebarLayout(
                sidebarPanel(
                  sliderInput("diameterInput", "Diameter", 0, 1000, c(300, 500), post = "m"),
                  sliderInput("height_range_idInput", "Height Range ID", 0, 10, c(2), post = "ft"),
                  checkboxGroupInput("curbInput", "Is Tree Located on Curb (Yes = Y or No = N)", choices = c("Y", "N"), selected = "Y")),
                      #conditionalPanel(input.curbInput == "Y",
                        #sliderInput("plant_areaInput", "Plant Area", 0, 20, c(5))),
                  colourpicker::colourInput(paste("col", sep="_"), "Choose colour:", "black")),
                #textOutput(),
                  #downloadButton(label = "Download"),
                  #downloadButton("downloadData", "Download.tsv"),
                mainPanel(
                  tabsetPanel(
                  tabPanel("Plot", plotOutput("coolplot")),
                  tabPanel("Table",DT::dataTableOutput("results")),
                  br(), br())
                ))
#Feature: Added parameter to plot to allow user to change colour of bars, this can help visualize bars better and makes it more appealing
#Feature: Put plot and table in separate tabs, this makes the app more organized and visually appealing
#Feature: Multiple select for curb, this allows more variation and analysis of data as it offers more options for selection easily

#Feature A4: Added an image for easier/quick understanding of the app
#Feature A4: Added a shiny theme for a more aesthetic looking app
#Feature A4:
#Addressed feedback by flipping the axes on my plot


server <- function(input, output){
  filtered <- reactive({
    if (is.null(input$curbInput)) {
      return(NULL)
    }
    vt %>%
      filter(diameter >= input$diameterInput[1],
           diameter <= input$diameterInput[2],
           height_range_id == input$height_range_idInput,
           curb %in% input$curbInput,
           #plant_area == input$plant_areaInput
  )
  })

  #output$text <- renderText()





  output$coolplot <- renderPlot({
  ggplot(filtered(), aes(neighbourhood_name)) +
    geom_bar(fill = input$col) +
      coord_flip()

})


  output$results <- DT::renderDataTable({
  filtered()
  })
}

  #output$download <- downloadHandler(
  #filename = function() {
    #paste0(input$vt, ".tsv")
  #},
  #content = function(file) {
    #vroom::vroom_write(data(), file)
  #}
#)




#Feature : Used DT package to create an interactive table, this helps user select specific variables they want to see faster.


shinyApp(ui = ui, server = server)



