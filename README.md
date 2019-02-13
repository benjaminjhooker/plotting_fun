# Plotting Fun and Pi
A simple example of plotting in a shiny app using generated data based on pi.

There are two different widgets types (slider and radio buttons) that allow a dynamic plotting environment.  
The `noise` option seemed like more fun to plot more than essentially a line.  

As the range increases from the intial values [-1pi to 1pi] to broader values, the number of 'loops' in the
polar plot increases.

```{r, echo = FALSE}
x <- seq(-(2)*pi, (2)*pi, by = pi/100)

y <- sin(x) + rnorm(x, 0, sd = .05)
z <- sample(y)
df <- data.frame('x' = x, 'y' = y, 'z' = z)
ggplot(df, aes(x = x, y = y, color = z)) +
                        geom_point() +
                        theme_minimal()+
                        coord_cartesian() +
                        theme(legend.position = 'none') +
                        labs(title = paste('Cartesian plot of the sine function plus some noise'))
                        
```                        
