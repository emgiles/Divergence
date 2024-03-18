
require(tidyverse)
library(ggplot2)
library(gridExtra)
library(stats)
library(data.table)
library(pracma)
library(datasets)
source("polynomial_function.R")
#source("polynomial_function_windows.R")

# Read in the file with sliding window estimates of FST, pi and dxy
windowStats<-read.csv("fst_dxy_pi_windows/scurra_viridula_zebrina_pileup_05jan2024_called_filtered2_fmiss50_allsites_10chrms_only10_Fst_Dxy_pi_04.csv.gz",header=T)

#remove NAs 
windowStats_noNA <- na.omit(windowStats)
head(windowStats_noNA)

#subset chromosome
windowStats_noNA_ch1 <- subset(windowStats_noNA, scaffold=='ss1')

# Loop through each response variable and run the function
results_list <- list()
for (i in 9:14) {
    response_col_name <- names(windowStats_noNA_ch1)[i]  # Get column name of response variable
    results_list[[response_col_name]] <- perform_polynomial_regression(windowStats_noNA_ch1, i)
  }

# Create individual plot objects for each plot
plot_objects <- list()

for (response_col_name in names(windowStats_noNA_ch1)[9:14]) {
  results <- results_list[[response_col_name]]
  
  # Assign plots to individual variables
  plot_mse <- results$plot_mse
  plot_best_model1 <- results$plot_best_model1
  plot_best_model2 <- results$plot_best_model2
  
  # Store the plots in a list
  plot_objects[[response_col_name]] <- list(plot_mse = plot_mse, 
                                            plot_best_model1 = plot_best_model1, 
                                            plot_best_model2 = plot_best_model2)
}

# Accessing the plot for a specific response variable
response_variable_name <- "Fst_viridula_zebrina"
plot_wanted <- plot_objects[[response_variable_name]]$plot_best_model1
plot_wanted 

plot_combined <- results$combined_plot
plot_combined

# Save plots and summaries
for (response_col_name in names(windowStats_noNA_ch1)[9:14]) {
  results <- results_list[[response_col_name]]
  
  # Save plot_mse
  ggsave(
    paste("plot_mse_response_", response_col_name, ".png", sep = ""),
    plot = results$plot_mse,
    width = 10,  # Adjust width as needed
    height = 6,  # Adjust height as needed
    units = "in",
    dpi = 300,
    device = "png",
    limitsize = FALSE  # Prevent resizing the plot
  )
  
  # Save plot_best_model1
  ggsave(
    paste("plot_best_model1_response_", response_col_name, ".png", sep = ""),
    plot = results$plot_best_model1,
    width = 10,  # Adjust width as needed
    height = 6,  # Adjust height as needed
    units = "in",
    dpi = 300,
    device = "png",
    limitsize = FALSE  # Prevent resizing the plot
  )
  
  # Save plot_best_model2
  ggsave(
    paste("plot_best_model2_response_", response_col_name, ".png", sep = ""),
    plot = results$plot_best_model2,
    width = 10,  # Adjust width as needed
    height = 6,  # Adjust height as needed
    units = "in",
    dpi = 300,
    device = "png",
    limitsize = FALSE  # Prevent resizing the plot
  )
  
  # Save combined_plot
  ggsave(
    paste("combined_plot_response_", response_col_name, ".png", sep = ""),
    plot = results$combined_plot,
    width = 10,  # Adjust width as needed
    height = 6,  # Adjust height as needed
    units = "in",
    dpi = 300,
    device = "png",
    limitsize = FALSE  # Prevent resizing the plot
  )
  
  # Save summary_best_model
  summary_text <- capture.output(results$summary_best_model)
  writeLines(summary_text, paste("summary_best_model_response_", response_col_name, ".txt", sep = ""))
}



# Define a color palette for the response variables
response_colors <- c("#FFCCCC", "#FF6699", "#3333CC", "#FFCCCC", "#FF6699", "#3333CC")

