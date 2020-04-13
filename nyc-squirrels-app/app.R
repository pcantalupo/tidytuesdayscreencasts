#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

squirrel_variables = by_hectare %>% select(-(hectare:lat)) %>% colnames()
names(squirrel_variables) = squirrel_variables %>%
    str_replace_all("_", " ") %>%
    str_to_title()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Squirrels"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("minsquirrels",
                        "Number of Sq:",
                        min = 1,
                        max = 30,
                        value = 10),
            selectInput("variable",
                        "Variable:",
                        choices = squirrel_variables)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("parkplot")
        )
    )
)

# Define server logic required to draw ggplot
server <- function(input, output) {
    
    output$parkplot <- renderPlot({
        #print(input$variable)
        var = sym(input$variable)
        
        filtered = by_hectare %>% filter(n >= input$minsquirrels)
        
        midpoint = mean(filtered[[input$variable]])
        print(midpoint)
        
        filtered %>%
            ggplot() +
            geom_sf(data = central_park_sf) +
            geom_point(aes(long, lat, size = n, color = !!var)) +
            theme_void() +
            scale_color_gradient2(low = "blue", high = "red", mid="pink",
                                  labels=scales::percent, midpoint = midpoint) +
            labs(color = paste("%", input$variable),
                 size = "# of S") +
            coord_sf(datum = NA)
    },
    height=600,
    width=600)
}

# Run the application 
shinyApp(ui = ui, server = server)
