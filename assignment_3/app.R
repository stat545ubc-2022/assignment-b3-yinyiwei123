# This is a app made for users to view the penguins weight and flipper length
library(shiny)
library(tidyverse)
library(ggplot2)
library(palmerpenguins)


data("penguins")
ui <- fluidPage(
#Feature1: add picture to the title page
  titlePanel(title = div(img(width = "15%", src="penguin.jpg"),"Penguins Weight & Flipper Length")),
  sidebarLayout(
    sidebarPanel(
#Feature2 : a data range filter allow user filter the penguins weight that they are interested 
      sliderInput("weightInput", "Weight", 2000,7000, c(3000,6000),post = "g"),
#Feature3 : two checkboxs filters allow users to select one or more penguins species and island that they are interested        
     checkboxGroupInput("penguinTypeInput","Species",
                   choices = c("Adelie", "Gentoo","Chinstrap"),
                   selected = "Adelie"),
      checkboxGroupInput("islandInput","Island",
                  choices = c("Torgersen","Biscoe","Dream"),
                  selected = "Torgersen")
      ),
#Feature4: output show a interactive plot
    mainPanel(plotOutput("plot",click = "plot_click"),
             verbatimTextOutput("info"))
  )
)

server <- function(input, output) {output$plot <- renderPlot({
  filtered<-penguins %>%
#extract the data based on the selection of weight, species and island 
    filter(body_mass_g >= input$weightInput[1],
           body_mass_g <= input$weightInput[2],
           species %in% input$penguinTypeInput,
           island %in% input$islandInput)
#create the output for the interactive plot,which will allow user use the click to find the weight and flipper length of penguins
  ggplot(filtered,aes(body_mass_g,flipper_length_mm)) +
    geom_point(aes(colour = species, shape = island),size = 3)+
    theme_bw() +
    labs(x = "Weight", y = "flipper length")
})
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
}
shinyApp(ui = ui, server = server)

