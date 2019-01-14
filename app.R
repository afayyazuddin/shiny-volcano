#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

pasilla_results <- read_tsv("data/pasilla_results.txt") %>% na.omit()
max_p_value <- round(max(pasilla_results$minus_log10_p_value) + 10, 0)
min_fc <- round(min(pasilla_results$log2FoldChange) - 1, 0)
max_fc <- round(max(pasilla_results$log2FoldChange) + 1, 0)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Volcano"),

   # Add a description and instructions
   fluidRow(
     column(4,
            includeText("include.txt")
     )
   ),
   # specify range for fold change and p-value

   sidebarLayout(
      sidebarPanel(

        sliderInput("range_x", "log2 Fold Change:",
                    min = round(min_fc,2), max = round(max_fc,2),
                    value = c(min_fc, max_fc)),

        sliderInput("range_y", "-log10 p-value:",
                    min = 0, max = max_p_value,
                  value = c(0,max_p_value))


       ),

      # show volcano plot
      mainPanel(
        plotOutput(outputId = "scatterplot",
                   brush = "plot_brush"),
        # Show data table
        tableOutput(outputId = "resultstable")
      ),


  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$scatterplot <- renderPlot({

      # Draw a volcano plot of the pasilla dataset
      ggplot(data = pasilla_results, aes(x=log2FoldChange,
                                         y= minus_log10_p_value, col = factor(sig), alpha = 0.1)) +
        geom_point() +
        # add lines to indicate log2FC +/- 1.5
        geom_vline(xintercept=1.5, linetype="dotted") +
        geom_vline(xintercept=-1.5, linetype="dotted") +
        # add line to indicate padj = 0.05
        geom_hline(yintercept=-log(0.05, base=10), linetype="dotted") +
        theme(legend.position="none") +
        labs(x="log2 Fold Change", y="-log10 adjusted p-value") +
        ylim(input$range_y[1],input$range_y[2]) +
        xlim(input$range_x[1],input$range_x[2]) +
        scale_color_manual(values=c("blue", "black", "red"))
   })
   #Print data table
   output$resultstable <- renderTable({
     brushedPoints(pasilla_results, input$plot_brush) %>%
       select(genenames, log2FoldChange, minus_log10_p_value)
   },  striped = TRUE, spacing = "l", align = "lcr", digits = 2,
   caption = "Selected points.")

}

# Run the application
shinyApp(ui = ui, server = server)
