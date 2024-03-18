###Script with loop to generate combined fst and dxy plots for 10 chromosomes.

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

getwd()
setwd("/Users/emily/Dropbox/School/Thesis/Variants-Ch2/divergence")

# Read in the file with sliding window estimates of FST, pi and dxy
windowStats<-read.csv("fst_dxy_pi_windows/scurra_viridula_zebrina_pileup_05jan2024_called_filtered2_fmiss50_allsites_10chrms_only10_Fst_Dxy_pi_04.csv.gz",header=T)

#remove NAs 
windowStats_noNA <- na.omit(windowStats)
head(windowStats_noNA)

#subset chromosome
df1 <- subset(windowStats_noNA, scaffold=='ss1')
df2 <- subset(windowStats_noNA, scaffold=='ss2')
df3 <- subset(windowStats_noNA, scaffold=='ss3')
df4 <- subset(windowStats_noNA, scaffold=='ss4')
df5 <- subset(windowStats_noNA, scaffold=='ss5')
df6 <- subset(windowStats_noNA, scaffold=='ss6')
df7 <- subset(windowStats_noNA, scaffold=='ss7')
df8 <- subset(windowStats_noNA, scaffold=='ss8')
df9 <- subset(windowStats_noNA, scaffold=='ss9')
df10 <- subset(windowStats_noNA, scaffold=='ss10')

# List to store plots
plot_list <- list()

# List of dataframes
df_list <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# Loop through each dataframe
for (i in 1:length(df_list)) {
  # Extract the dataframe
  df <- df_list[[i]]
  
  # Subset the dataframe for DXY
  subset_df_dxy <- df[, c(1, 4, 9:11)]
  
  # Transform dataframe, columns as factors
  df_combined_dxy <- subset_df_dxy %>%
    pivot_longer(cols = starts_with("dxy"), names_to = "original_column") %>%
    mutate(original_column = factor(original_column)) %>%
    arrange(original_column)
  
  degree <- 15  # Set the degree of the polynomial regression model
  
  # Fit polynomial regression model for DXY
  polynomial_model_dxy <- lm(as.formula(paste("value ~ poly(mid, degree)")), data = df_combined_dxy)
  
  # Summary of the model for DXY
  summary(polynomial_model_dxy)
  
  # Create the plot for DXY
  plot_dxy <- ggplot(df_combined_dxy, aes(x = mid/1000000, y = value, color = factor(original_column))) +
    stat_smooth(method = 'lm', formula = y ~ poly(x, degree), se = FALSE) +
    labs(y="Dxy", x=paste("chrm",i), color = "") +
    scale_x_continuous(limits = c(0, 50), breaks = c(0, 25, 50)) +
    scale_y_continuous(limits = c(0, 0.10), breaks = c(0.00, 0.02, 0.04, 0.06, 0.08, 0.10)) +
    scale_color_manual(values=c("darkorchid1", "lightslateblue", "steelblue1"), 
                       labels=c("scurra|viridula", "scurra|zebrina", "viridula|zebrina")) +
    theme(axis.text.y=element_text(size=10),
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  # Save plot to list
  plot_list[[paste0("plot_dxy_df", i)]] <- plot_dxy
  
  # Subset the dataframe for Fst
  subset_df_fst <- df[, c(1, 4, 12:14)]
  
  # Transform dataframe, columns as factors
  df_combined_fst <- subset_df_fst %>%
    pivot_longer(cols = starts_with("Fst"), names_to = "original_column") %>%
    mutate(original_column = factor(original_column)) %>%
    arrange(original_column)
  
  degree <- 15  # Set the degree of the polynomial regression model
  
  # Fit polynomial regression model for Fst
  polynomial_model_fst <- lm(as.formula(paste("value ~ poly(mid, degree)")), data = df_combined_fst)
  
  # Summary of the model for Fst
  summary(polynomial_model_fst)
  
  # Create the plot for Fst
  plot_fst <- ggplot(df_combined_fst, aes(x = mid/1000000, y = value, color = factor(original_column))) +
    stat_smooth(method = 'lm', formula = y ~ poly(x, degree), se = FALSE) +
    labs(y="Fst", x="",color = "") +
    scale_x_continuous(limits = c(0, 50), breaks = c(0, 25, 50)) +
    scale_y_continuous(limits = c(0, 0.7), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) +
    scale_color_manual(values=c("darkorchid1", "lightslateblue", "steelblue1"),
                       labels=c("scurra|viridula", "scurra|zebrina", "viridula|zebrina")) +
    theme(axis.text.y=element_text(size=10),
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  # Save plot to list
  plot_list[[paste0("plot_fst_df", i)]] <- plot_fst
}


# Accessing the saved plots
plot_list[["plot_dxy_df1"]]
plot_list[["plot_fst_df1"]]
plot_list[["plot_dxy_df10"]]

# Combined plots
combined_1to5 <- ggarrange(plot_list[["plot_fst_df1"]],
                          plot_list[["plot_fst_df2"]],
                          plot_list[["plot_fst_df3"]],
                          plot_list[["plot_fst_df4"]],
                          plot_list[["plot_fst_df5"]],
                          plot_list[["plot_dxy_df1"]],
                          plot_list[["plot_dxy_df2"]],
                          plot_list[["plot_dxy_df3"]],
                          plot_list[["plot_dxy_df4"]],
                          plot_list[["plot_dxy_df5"]],
                          nrow=2, ncol=5,
                          common.legend=TRUE, legend="bottom",
                          align = "hv") 

combined_6to10 <- ggarrange(plot_list[["plot_fst_df6"]],
                           plot_list[["plot_fst_df7"]],
                           plot_list[["plot_fst_df8"]],
                           plot_list[["plot_fst_df9"]],
                           plot_list[["plot_fst_df10"]],
                           plot_list[["plot_dxy_df6"]],
                           plot_list[["plot_dxy_df7"]],
                           plot_list[["plot_dxy_df8"]],
                           plot_list[["plot_dxy_df9"]],
                           plot_list[["plot_dxy_df10"]],
                           nrow=2, ncol=5,
                           common.legend=TRUE, legend="bottom",
                           align = "hv") 
