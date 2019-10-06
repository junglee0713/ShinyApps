#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- shinyUI(pageWithSidebar(
    
    headerPanel("Compute Shannon Diversity"),
    sidebarPanel(
        textInput("inputCount", 
                  "Enter a vector of counts (comma delimited)", 
                  "11, 15, 20, 8, 23, 1, 34, 12, 13, 54")
    ),
    
    mainPanel(
        h4('You entered'),
        #verbatimTextOutput("oid1"),
        verbatimTextOutput("oid2"),
        verbatimTextOutput("oid3"),
        h4('Shannon Diversity Comparison'),
        plotOutput("shannonPlot")
    )
))

server <- shinyServer(function(input, output) {
    
    output$oid1 <- renderPrint({
        cat("As string:\n")
        cat(input$inputCount)
    }
    )
    
    output$oid2 <- renderPrint({
        x <- as.numeric(unlist(strsplit(input$inputCount,",")))
        x <- x[x>0]
        cat("As numeric vector (zeros, if any, are excluded):\n")
        print(x)
    }
    )
    
    output$oid3 <- renderPrint({
        x <- as.numeric(unlist(strsplit(input$inputCount,",")))
        x <- x[x>0]
        y <- x/sum(x)
        cat("As proportions (zeros, if any, are excluded):\n")
        print(round(y, 3))
    }
    )
    
    output$shannonPlot <- renderPlot({
        library(tidyverse)
        library(ggsci)
        library(ggrepel)
        # x <- c(11, 15, 20, 8, 23, 1, 34, 12, 13, 54)
        x <- as.numeric(unlist(strsplit(input$inputCount,",")))
        x <- x[x>0]
        y <- x/sum(x)
        
        # comparison data set
        speciesMac <- 10
        domMin <- 0.3
        domMax <- 0.9
        domStep <- 0.1
        numSpeciesList <- 2^(2:speciesMac)
        domPropList <- seq(domMin, domMax, domStep)
        dfList <- list()
        count <- 0
        for (numSpecies in numSpeciesList) {
            count <- count + 1
            domProp <- "Even distribution"
            comp <- rep(1/numSpecies, numSpecies)
            shannon <- -sum(comp*log(comp))
            dfList[[count]] <- data.frame(numSpecies = numSpecies,
                                          domProp = domProp,
                                          shannon = shannon,
                                          stringsAsFactors = F)
            for (domProp in domPropList) {
                count <- count + 1
                comp <- c(domProp, rep((1 - domProp)/(numSpecies - 1), numSpecies - 1))
                shannon <- -sum(comp*log(comp))
                dfList[[count]] <- data.frame(numSpecies = numSpecies,
                                              domProp = as.character(domProp),
                                              shannon = shannon,
                                              stringsAsFactors = F)
            }
        }
        df <- bind_rows(dfList)
        userInputShannon <- data.frame(numSpecies = length(x), 
                                       domProp = "User input", 
                                       shannon = -sum(y*log(y)),
                                       stringsAsFactors = F)
        df <- rbind(df, userInputShannon) 
        
        g <- df %>% 
            ggplot(aes(numSpecies, shannon)) +
            geom_segment(aes(x = 0,
                             xend = length(y), 
                             y = df$shannon[df$domProp == "User input"],
                             yend = df$shannon[df$domProp == "User input"]),
                             color = "red",
                             lty = "dashed") +
            geom_segment(aes(x = length(y), 
                             xend = length(y), 
                             y = -Inf, 
                             yend = df$shannon[df$domProp == "User input"]),
                         color = "red",
                         lty = "dashed") +
            geom_point(aes(color = domProp)) +
            geom_text_repel(data = df %>% filter(domProp == "User input"),
                            aes(label = round(shannon, 2)),
                            segment.color = "grey50",
                            vjust = 1) +
            scale_color_d3() +
            scale_x_continuous(trans = "log2", breaks = c(2^(2:speciesMac), length(y))) +
            labs(x = "# of distinct species",
                 y = "Shannon diversity index",
                 color = "Proportion of\nmost dominant species\n(rest evenly distributed)") +
            theme_classic()
        
        print(g)
    }
    )
}
)

# Run the application 
shinyApp(ui = ui, server = server)
