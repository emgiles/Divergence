# Divergence
R scripts for plotting divergence metrics across sliding windows

Input is the .csv output from [genomics_general/popgenWindows.py](https://github.com/simonhmartin/genomics_general)

Place polynomial_function.R in working directory. 

Use polynomial_regression.R script as template to generate plots of polynomial regression with degree selection based on mean squared errors. 
Generates plots of outliers (99th quantiles) and labels outliers in dataframes.

Use Combined_Loop.R to generate combined polynomial regression plots of Fst and Dxy for multiple comparisons. Degree of polynomials must be set.
