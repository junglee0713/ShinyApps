#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Shannon diversity"),
   
   # Sidebar with a slider input for number of communities
   sidebarLayout(
      sidebarPanel(
         sliderInput("num_comm",
                     "Number of communities:",
                     min = 10,
                     max = 500,
                     value = 100)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     library(tidyverse)
     library(viridis)
     num_comm <- input$num_comm
     max_num_species <- 10
     n <- sample(1:(max_num_species-1), size=num_comm, replace=T)
     s <- data.frame(index=1:num_comm, num_species=rep(NA, num_comm), 
                     shannon=rep(NA, num_comm), sd=rep(NA, num_comm))
     for (i in 1:length(n)) {
       breaks_num <- n[i]
       breaks <- c(0, sort(runif(breaks_num)), 1)
       props <- diff(breaks)
       s$index[i] <- i
       s$num_species[i] <- breaks_num+1
       s$shannon[i] <- -sum(props*log(props))
       s$sd[i] <- sd(props)
     }
     
     ggplot(s, aes(x=num_species, y=shannon)) + 
       geom_point(aes(color = sd)) + 
       scale_color_viridis() + 
       theme(aspect.ratio = 1) +
       labs(color = "Standard deviation of\nproportions",
            x = "Number of species",
            y = "Shannon index") +
       scale_x_discrete(limits = 2:max_num_species) +
       theme_bw() 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

