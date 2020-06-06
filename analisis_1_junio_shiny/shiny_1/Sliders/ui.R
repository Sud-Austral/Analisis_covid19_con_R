# Live Sliders Example ui.R
# Customizing Sliders can include running animation sequence

# Shiny slider controls are extremely capable and can be
# easily customized. Features supported include:

#.The ability to input both single values and ranges
#.Custom formats for value display (e.g for currency)
#.The ability to animate the slider across a range of values

# Slider controls are created by calling the sliderInput()
# function. This ui.R file demonstrates using sliders with 
# a variety of options:
  

library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Sliders"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("integer", "Integer:", 
                min=0, max=1000, value=500),
    
    # Decimal interval with step value
    sliderInput("decimal", "Decimal:", 
                min = 0, max = 1, value = 0.5, step= 0.1),
    
    # Specification of range within an interval
    sliderInput("range", "Range:",
                min = 1, max = 1000, value = c(200,500)),
    
    # Provide a custom currency format for value display, with basic animation
    sliderInput("format", "Custom Format:", 
                min = 0, max = 10000, value = 0, step = 2500,
                format="$#,##0", locale="us", animate=TRUE),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    sliderInput("animation", "Looping Animation:", 1, 2000, 1, step = 10, 
                animate=animationOptions(interval=300, loop=T))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("values")
  )
))