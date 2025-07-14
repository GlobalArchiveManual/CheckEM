#' Plot Basic Transformations of Predictor Variables for GAM Modelling
#'
#' This function generates diagnostic plots to visualise the distribution of predictor variables 
#' and the effects of basic transformations (square root and log transformations) on these variables. 
#' It creates scatter plots and histograms for the raw, square root, and log-transformed data. 
#' The function is useful for exploring the suitability of different transformations before 
#' fitting a Generalized Additive Model (GAM).
#' 
#' @param pred.vars A character vector of predictor variable names to be plotted.
#' @param dat A data frame containing the predictor variables and a grouping variable, 
#' either 'opcode' or 'sample', which is used for the scatter plots.
#'
#' @return The function generates and prints diagnostic plots but does not return a value.
#' @export
#' @import ggplot2 dplyr
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat' is a data frame containing the predictor variables and 'opcode' or 'sample' column
#' plot_transformations(pred.vars = c("var1", "var2"), dat = dat)
#' }

plot_transformations <- function(pred.vars, dat) {
  require(tidyverse)
  require(patchwork)
  
  for (i in pred.vars) {

    if (sum(str_count(names(dat), "opcode")) == 1) {
      
      # Raw data plots
      point <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = get(i)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = i) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist <- ggplot(data = dat) +
        geom_histogram(aes(x = get(i)), fill = NA, colour = "black") +
        labs(x = i) +
        theme_classic()
      
      # Square root transformation
      point.sqrt <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = sqrt(get(i))), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("sqrt(", i, ")")) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist.sqrt <- ggplot(data = dat) +
        geom_histogram(aes(x = sqrt(get(i))), fill = NA, colour = "black") +
        labs(x = paste0("sqrt(", i, ")")) +
        theme_classic()
      
      # Log transformation (x + 1)
      point.log <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = log(get(i) + 1)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("log(", i, ")")) +
        theme_classic()+
        theme(axis.text.x = element_blank())
      hist.log <- ggplot(data = dat) +
        geom_histogram(aes(x = log(get(i) + 1)), fill = NA, colour = "black") +
        labs(x = paste0("log(", i, ")")) +
        theme_classic() 
      
      # Print combined plots
      print((point + hist)/(point.sqrt + hist.sqrt)/(point.log + hist.log))
    }
    
    else if (sum(str_count(names(dat), "sample")) == 1) {
      
      # Raw data plots
      point <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = sample, y = get(i)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = i) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist <- ggplot(data = dat) +
        geom_histogram(aes(x = get(i)), fill = NA, colour = "black") +
        labs(x = i) +
        theme_classic()
      
      # Square root transformation
      point.sqrt <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = sample, y = sqrt(get(i))), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("sqrt(", i, ")")) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist.sqrt <- ggplot(data = dat) +
        geom_histogram(aes(x = sqrt(get(i))), fill = NA, colour = "black") +
        labs(x = paste0("sqrt(", i, ")")) +
        theme_classic()
      
      # Log transformation (x + 1)
      point.log <- ggplot2::ggplot(data = dat) + 
        geom_point(aes(x = sample, y = log(get(i) + 1)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("log(", i, ")")) +
        theme_classic()+
        theme(axis.text.x = element_blank())
      hist.log <- ggplot(data = dat) +
        geom_histogram(aes(x = log(get(i) + 1)), fill = NA, colour = "black") +
        labs(x = paste0("log(", i, ")")) +
        theme_classic() 
      
      # Print combined plots
      print((point + hist)/(point.sqrt + hist.sqrt)/(point.log + hist.log))
    }
    
  }
}
