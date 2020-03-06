# Exercise 3: interactive Shiny apps

# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")

# You will again be working with the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 1000 rows from the data set
# Store this sample in a variable `diamonds_sample`
diamonds_sample <- sample_n(diamonds, 1000)

# For convenience store the `range()` of values for the `price` column
# (of your sample)
diamond_range <- range(diamonds_sample$price)

# For convenience, get a vector of column names from the `diamonds` data set to
# use as select inputs
col_names <- colnames(diamonds_sample)

# To help keep the code organized, we'll store some UI elements in variables
# _before_ defining the UI.

# Define a variable `price_input` that is a `sliderInput()` with the following
# properties:
# - an inputId of `price_choice`
# - a label of "Price (in dollars)"
# - min and max valuesvalue based on the `price_range` calculated above
# - a current value equal to the price range
price_input <- sliderInput(inputId = "price_choice", label = "Price (in dollars)", min = diamond_range[1], max = diamond_range[2], value = diamond_range)

# Define a variable `feature_input` that is a `selectInput()` with the
# label "Feature of Interest". This dropdown should let the user pick one of
# the columns of the diamond data set. Use the `carat` column as a default
# Make sure to set an inputId to reference in your server!
feature_input <- selectInput(inputId = "feature_choice", label = "Feature of Interest", choices = col_names, selected = 'carat')

# Define a UI using a `fluidPage()` layout with the following content:
my_ui <- fluidPage(
  titlePanel(title = "Diamond Viewer"),
  price_input,
  feature_input,
  trendline_input <- checkboxInput(inputId = "show_trend", label = "Show Trendline", value = TRUE),
  plotOutput(outputId = "my_plot")
)
# Define a `server` function (with appropriate arguments)
# This function should perform the following:
my_server <- function(input_list, output_list) {
  output_list$my_plot <- renderPlot({
    
    selected_range <- input_list$price_choice
    
    filtered_data <- diamonds_sample %>% 
      filter(price > selected_range[1], price < selected_range[2])
    
    the_plot <- ggplot(data = filtered_data, mapping = aes_string(
      x = input_list$feature_choice,
      y = "price",
      color = "cut"
    )) +
      geom_point()
    
    if(input_list$show_trend == TRUE) {
      the_plot <- the_plot +
        geom_smooth(se = FALSE)
    }
    
    return(the_plot)
  }) 
}
    
    # Finally, if the "trendline" checkbox is selected, you should add (+)
    # a geom_smooth geometry (with `se=FALSE`) to your plot
    # Hint: use an if statement to see if you need to add more geoms to the plot

  
    # Be sure and return the completed plot!
  

# Create a new `shinyApp()` using the above ui and server
my_app <- shinyApp(my_ui, my_server)
