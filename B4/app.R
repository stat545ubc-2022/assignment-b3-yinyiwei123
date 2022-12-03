# This is a app made for users to view the penguins weight and flipper and bill length
library(shiny)
library(tidyverse)
library(ggplot2)
library(palmerpenguins)
data("penguins")
library(rsconnect)
ui <- fluidPage(
  #Feature1: add picture to the title page
  titlePanel(title = div(img(width = "15%", src="penguin.jpg"),"Penguins Weight & Flipper/Bill Length")),
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
                         selected = "Torgersen"),
      #Feature4 : Put help text to help users
      helpText("Note: please put input for all the needed area, interactive plot and filtered data will be on the right ")
    ),
    #Feature5:  Add tab panel to allow display for multiple plot and table output
    mainPanel(tabsetPanel(
      tabPanel("weight vs fliper length", plotOutput("plot1",click = "plot_click"),
               verbatimTextOutput("info")),
      tabPanel("weight vs bill length", plotOutput("plot2",click = "plot_click"),verbatimTextOutput("info1")),
      tabPanel("data table",tableOutput("outputtable")),
  )
)
)
)

server <- function(input, output) {
  filtered <-reactive({penguins %>%
    #extract the data based on the selection of weight, species and island 
    filter(body_mass_g >= input$weightInput[1],
           body_mass_g <= input$weightInput[2],
           species %in% input$penguinTypeInput,
           island %in% input$islandInput)})
  
  output$plot1 <- renderPlot({filtered() %>%
  #create the output for the interactive plot,which will allow user use the click to find the weight and flipper length of penguins
  ggplot(aes(body_mass_g,flipper_length_mm)) +
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
output$plot2 <- renderPlot({
  filtered() %>%
  #create the output for the interactive plot,which will allow user use the click to find the weight and bill length of penguins
  ggplot(aes(body_mass_g,bill_length_mm)) +
    geom_point(aes(colour = species, shape = island),size = 3)+
    theme_bw() +
    labs(x = "Weight", y = "flipper length")
})
#Show the table for the filtered data
output$info1 <- renderPrint({
  req(input$plot_click)
  x <- round(input$plot_click$x, 2)
  y <- round(input$plot_click$y, 2)
  cat("[", x, ", ", y, "]", sep = "")
})

output$outputtable <- renderTable({filtered()
})
}
shinyApp(ui = ui, server = server)

