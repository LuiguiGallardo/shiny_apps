# Purpose: Shiny app to calculate Spearman correlation
# Date created: 31.08.2022
# Author: Luigui Gallardo-Becerra (bfllg77@gmail.com)

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Pearson and Spearman correlation",
    # Panel 1
    tabPanel("Input table",
             sidebarLayout(
               sidebarPanel(
                 
                 fileInput("input_file",
                           "Input file"),
                 
               ), # Sidebar panel 1
               mainPanel(
                 tableOutput("contents"),
               )
             )
    ), # Main panel 1
    
    # Panel 2
    tabPanel("Results table",
             sidebarLayout(
               sidebarPanel(
                 selectInput("method",
                             "Method (Pearson or Spearman)",
                             choices = c("Pearson",
                                         "Spearman")),
                 
                 sliderInput("coefficient",
                             "Coefficient value (Person's r or Spearman's rho)",
                             min = 0,
                             max = 1,
                             value = 0),
                 
                 sliderInput("pvalue",
                             "p-value",
                             min = 0,
                             max = 1,
                             value = 0,
                             step = 0.001),
               ), # Sidebar panel 2
               mainPanel(
                 tableOutput("correlation"),
               )
             )
    ), # Main panel 2
    
    # Panel 3
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("variable_x",
                             'Variable 1 (x)',
                             choices=NULL,
                             selected=NULL),
                 
                 selectInput("variable_y",
                             'Variable 2 (y)',
                             choices=NULL,
                             selected=NULL),
                 
                 selectInput("corr_position",
                             "Coefficient and p-value position (top or bottom)",
                             choices = c("top",
                                         "bottom")),
                 
                 textInput("title",
                           "Title",
                           value = ""),
                 
                 textInput("legend",
                           "Legend title",
                           value = ""),
                 
                 textInput("label_x",
                           "Label x-axis",
                           value = ""),
                 
                 textInput("label_y",
                           "Label y-axis",
                           value = ""),
                 
                 textInput("plot_filename",
                           "Plot filename (.svg)",
                           value = "correlation_plot"),
                 
                 downloadButton("downloadPlot",
                                "Download plot"),
                 
                 
               ), # Sidebar panel 2
               mainPanel(
                 plotOutput("plots")
               )
             )
    ), # Main panel 3
  )
)
