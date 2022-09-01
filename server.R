# Purpose: Shiny app to calculate Spearman correlation
# Date created: 31.08.2022
# Author: Luigui Gallardo-Becerra (bfllg77@gmail.com)

library(shiny)
library(shinythemes)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(dplyr)
library(ggpubr)

# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
    
    output$contents <- renderTable({
      input_file <<- input$input_file
      
      # Print NULL if no input file
      if (is.null(input_file))
        return(NULL)
      
      # Read input file
      input_table <<- read.delim(input_file$datapath,
                                 header = TRUE,
                                 row.names = 1)
      
      updateSelectInput(session,
                        "variable_x",
                        choices = colnames(input_table))
      
      updateSelectInput(session,
                        "variable_y",
                        choices = colnames(input_table))
      
      return(input_table)
    }
    )
    
    
    output$correlation <- renderTable({
      
      transposed_table <- t(subset(input_table, select = - c(group)))
      
      # Rho cutoff
      cor.cutoff = input$coefficient
      
      # P-value cutoff
      p.cutoff = input$pvalue
      
      # Correlation
      res.cor <- rcorr(t(transposed_table),
                       type = tolower(input$method))
      
      # This is the R value matrix
      matrix.cor <- res.cor$r 
      
      # This is the p-value matrix
      matrix.cor.p <- res.cor$P
      
      # Filter of coefficients and p-values
      # Filter of R (positive and negative)
      matrix.cor[which(matrix.cor>=(-cor.cutoff) & matrix.cor<=cor.cutoff)]=0 
      
      # Filter of p-value
      matrix.cor[which(matrix.cor.p>p.cutoff)]=0 
      
      # Reshape the dataframes and creation of final table
      melted_cormat <- melt(matrix.cor)
      melted_cormat.p <- melt(matrix.cor.p)
      
      merged_cormat <- merge(melted_cormat,
                             melted_cormat.p,
                             by = c("Var1", "Var2"))
      
      # Final filter: remove zeros, self-correlations and order taxa and function
      spearman_output <<- merged_cormat %>%
        filter(value.x != 0) %>%
        filter(Var1 != Var2) %>%
        rename("Correlation" = value.x,
               "p-value" = value.y)
      
      varchoices_1 <<- spearman_output["Var1"]
      varchoices_2 <<- spearman_output["Var2"]
      
      return(spearman_output)
    }
    )
    
    plot_reactive <- reactive({
      spearman_plot <<- ggplot(input_table,
                               aes(x = get(input$variable_x),
                                   y = get(input$variable_y))) +
        geom_point(aes(shape = group)) +
        geom_text(label = rownames(input_table),
                  vjust = -1) +
        theme_bw() +
        labs(title = input$title,
             shape = input$legend,
             x = input$label_x,
             y = input$label_y) +
        stat_smooth(level = 0.95,
                    method = "lm",
                    color = "darkgray") +
        stat_cor(method = tolower(input$method),
                 size = 5,
                 label.x.npc = "left",
                 label.y.npc = input$corr_position) +
        theme(plot.title = element_text(hjust = 0.5,
                                        size = 14,
                                        face = "bold"),
              panel.grid.major = element_line(colour = "gray"),
              panel.grid.minor = element_line(colour = "gray"))
      
      spearman_plot
    }
    )
    
    output$plots <- renderPlot({
      plot_reactive()
    }
    )
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0(input$plot_filename, ".svg")
      },
      content = function(file) {
        ggsave(
          file,
          plot_reactive(),
          width = 7,
          height = 7
        )
      }
    )
  }
)
