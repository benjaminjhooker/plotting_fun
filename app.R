#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws both cartesian and polar plots
ui <- fluidPage(
        
        # Application title
        titlePanel("Plotting and Pi from -n*pi to n*pi"),
        
        # Sidebar with a slider input for number of range, observations, and noise
        # radio buttons for the function 
        sidebarLayout(
                sidebarPanel(
                        sliderInput('range', 'Range: +/-n*pi',
                                    min = 1,
                                    max = 8,
                                    value = 1,
                                    step = 1),     
                        sliderInput("samps",
                                    "Observations as by = pi/n:",
                                    min = 1,
                                    max = 1000,
                                    value = 1),
                        radioButtons('func', 'Function', choices = c( 
                                'sin' = 'sin',
                                'cos' = 'cos',
                                'tan' = 'tan')),
                        sliderInput('noise',
                                    'Noise (sd):',
                                    min = 0,
                                    max = 1,
                                    value = 0)
                ),
                
                # Show a plot of the value inputs
                mainPanel(
                        plotOutput("cartplot"),
                        plotOutput('polarplot')
                )
        )
)

# Define server logic required to draw plots
server <- function(input, output) {
        
        output$cartplot <- renderPlot({
                x <- seq(-(input$range)*pi, (input$range)*pi, by = pi/input$samps)
                fun <- switch(input$func, sin = sin, cos = cos, tan = tan)
                y <- fun(x) + rnorm(x, 0, sd = input$noise)
                z <- sample(y)
                df <- data.frame('x' = x, 'y' = y, 'z' = z)
                ggplot(df, aes(x = x, y = y, color = z)) +
                        geom_point() +
                        theme_minimal()+
                        coord_cartesian() +
                        theme(legend.position = 'none') +
                        labs(title = paste('Cartesian plot of the', input$func,'function plus some noise'))
        })
        output$polarplot <- renderPlot({
                # generate polar plot based on input$bins from ui.R
                x <- seq(-(input$range)*pi, input$range*pi, by = pi/input$samps)
                fun <- switch(input$func, sin = sin, cos = cos, tan = tan)
                y <- fun(x) + rnorm(x, 0, sd = input$noise)
                # x <- sample(x)
                z <- sample(y)
                z1 <- sample(2:8,length(x), replace = T)/10
                df1 <- data.frame('x' = x, 'y' = y, 'z' = z, 'z1' = z1)
                # draw the polar plot with the specified number of obs
                ggplot(df1, aes(x = x, y = y, color = y, size = z)) +
                        theme_minimal() +
                        scale_color_gradient2(midpoint = 0, high = 'tan', 
                                              low = 'blue',
                                              mid = 'green')+
                        geom_point(alpha = z1) +
                        coord_polar() +
                        theme(axis.title = element_blank(),
                              axis.ticks = element_blank(),
                              axis.text = element_blank(),
                              legend.position = 'none',
                              panel.grid = element_blank())+
                        labs(title = paste('Polar plot of the',input$func,'function'))
                
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

