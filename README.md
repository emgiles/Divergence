# Divergence
R (and a few awk) scripts for plotting divergence metrics across sliding windows

Inputs are the .csv and .tsv outputs from [genomics_general/](https://github.com/simonhmartin/genomics_general)

Place polynomial_function.R in working directory. 

Use polynomial_regression.R script as template to generate plots of polynomial regression with degree selection based on mean squared errors. 
Generates plots of outliers (99th quantiles) and labels outliers in dataframes.

Use Combined_Loop.R to generate combined polynomial regression plots of Fst and Dxy for multiple comparisons. Degree of polynomials must be set.

Use awk commands to parse coding_site_type results. 
