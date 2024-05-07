#' A function to test basic transformations on predictor variables for GAM modelling
#'
#' @param pred.vars
#' @param dat  
#'
#' @return
#' @export
#'
#' @examples

plot_transformations <- function(pred.vars, dat) {
  require(tidyverse)
  
  for (i in pred.vars) {
    # raw data
    
    if (sum(str_count(names(dat), "opcode")) == 1) {
      point <- ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = get(i)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = i) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist <- ggplot(data = dat) +
        geom_histogram(aes(x = get(i)), fill = NA, colour = "black") +
        labs(x = i) +
        theme_classic()
      # square root
      point.sqrt <- ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = sqrt(get(i))), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("sqrt(", i, ")")) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist.sqrt <- ggplot(data = dat) +
        geom_histogram(aes(x = sqrt(get(i))), fill = NA, colour = "black") +
        labs(x = paste0("sqrt(", i, ")")) +
        theme_classic()
      # log (x + 1)
      point.log <- ggplot(data = dat) + 
        geom_point(aes(x = opcode, y = log(get(i) + 1)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("log(", i, ")")) +
        theme_classic()+
        theme(axis.text.x = element_blank())
      hist.log <- ggplot(data = dat) +
        geom_histogram(aes(x = log(get(i) + 1)), fill = NA, colour = "black") +
        labs(x = paste0("log(", i, ")")) +
        theme_classic() 
      print((point + hist)/(point.sqrt + hist.sqrt)/(point.log + hist.log))
    }
    
    else if (sum(str_count(names(dat), "sample")) == 1) {
      point <- ggplot(data = dat) + 
        geom_point(aes(x = sample, y = get(i)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = i) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist <- ggplot(data = dat) +
        geom_histogram(aes(x = get(i)), fill = NA, colour = "black") +
        labs(x = i) +
        theme_classic()
      # square root
      point.sqrt <- ggplot(data = dat) + 
        geom_point(aes(x = sample, y = sqrt(get(i))), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("sqrt(", i, ")")) +
        theme_classic() +
        theme(axis.text.x = element_blank())
      hist.sqrt <- ggplot(data = dat) +
        geom_histogram(aes(x = sqrt(get(i))), fill = NA, colour = "black") +
        labs(x = paste0("sqrt(", i, ")")) +
        theme_classic()
      # log (x + 1)
      point.log <- ggplot(data = dat) + 
        geom_point(aes(x = sample, y = log(get(i) + 1)), fill = NA, colour = "black", shape = 1) +
        labs(x = "", y = paste0("log(", i, ")")) +
        theme_classic()+
        theme(axis.text.x = element_blank())
      hist.log <- ggplot(data = dat) +
        geom_histogram(aes(x = log(get(i) + 1)), fill = NA, colour = "black") +
        labs(x = paste0("log(", i, ")")) +
        theme_classic() 
      print((point + hist)/(point.sqrt + hist.sqrt)/(point.log + hist.log))
    }

  }
}
