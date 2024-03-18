perform_polynomial_regression <- function(dataframe, response_index) {
  # Define predictor variable (mid) and response variable
  predictors <- names(dataframe)[4]
  response <- names(dataframe)[response_index]
  
  # Randomly shuffle data
  df.shuffled <- dataframe[sample(nrow(dataframe)), ]  # Shuffle entire dataframe
  
  # Define number of folds for k-fold cross-validation
  K <- 10 
  
  # Define degree of polynomials to fit
  degree <- 10
  
  # Create k equal-sized folds
  folds <- cut(seq(1, nrow(df.shuffled)), breaks = K, labels = FALSE)
  
  # Create object to hold MSE's of models
  mse <- matrix(data = NA, nrow = K, ncol = degree)
  
  # Perform K-fold cross-validation
  for (i in 1:K) {
    # Define training and testing data
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- df.shuffled[testIndexes, ]
    trainData <- df.shuffled[-testIndexes, ]
    
    # Use k-fold cv to evaluate models
    for (j in 1:degree) {
      formula <- paste(response, "~ poly(", predictors, ",", j, ")")
      if (anyNA(formula)) {
        next
      }
      
      # Ignore warnings about rank-deficient fits
      fit.train <- suppressWarnings(lm(as.formula(formula), data = trainData))
      
      fit.test <- predict(fit.train, newdata = testData)
      mse[i, j] <- mean((fit.test - testData[, response])^2) 
    }
  }
  
  # Find MSE for each degree 
  means <- colMeans(mse)
  polynomials <- 1:25  # Adjust the range of polynomials
  
  # Pad means with NA values if necessary to match the length of polynomials
  if (length(means) < length(polynomials)) {
    means <- c(means, rep(NA, length(polynomials) - length(means)))
  }
  
  # Find the best degree
  best_degree <- which.min(means)
  
  # Plot MSE vs. number of polynomials
  plot_mse <- ggplot(data = data.frame(polynomials, means), aes(x = polynomials, y = means)) +
    geom_line() +
    geom_point() +
    geom_point(aes(x = best_degree, y = means[best_degree]), color = "red", size = 3) + # Highlight the best degree in red
    ylab("mean square error") +
    scale_x_continuous(name = "number of polynomials", limits = c(1, 25), breaks = c(1, 5, 10, 15, 20, 25)) + 
    ggtitle(paste("Mean Square Error vs. Number of Polynomials for", response))
  
  # Fit best model
  best_model <- lm(as.formula(paste(response, "~ poly(", predictors, ",", best_degree, ", raw = TRUE)")), data = df.shuffled)
  summary_best_model <- summary(best_model)
  
  # Identify outliers based on 99th quantile
  quantile_99 <- quantile(df.shuffled[[response]], 0.99)
  df.shuffled$outlier <- ifelse(df.shuffled[[response]] > quantile_99, TRUE, FALSE)
  
  # Plot best model without outliers
  plot_best_model1 <- ggplot(data = df.shuffled, aes_string(x = predictors, y = response)) + 
    geom_point() +
    stat_smooth(method = 'lm', formula = y ~ poly(x, best_degree, raw = TRUE), size = 1, na.rm=TRUE) + 
    xlab(predictors) +
    ylab(response) +
    scale_x_continuous(limits = c(0, 50000000), breaks = c(0, 25000000, 50000000)) +
    theme(axis.text.y=element_text(size=10),
          legend.position = "none",
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  # Plot outliers without polynomial regression
  plot_best_model2 <- ggplot(data = df.shuffled, aes_string(x = predictors, y = response, color = "outlier")) + 
    geom_point() +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +  # Color outliers red
    xlab(predictors) +
    ylab(response) +
    scale_x_continuous(limits = c(0, 50000000), breaks = c(0, 25000000, 50000000)) +
    theme(axis.text.y=element_text(size=10),
          legend.position = "none",
          panel.background=element_blank(),
          axis.line=element_line(colour="black"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10))
  
  # Store necessary outputs in a list
  output_list <- list(plot_mse = plot_mse, 
                      summary_best_model = summary_best_model, 
                      plot_best_model1 = plot_best_model1, 
                      plot_best_model2 = plot_best_model2,
                      combined_plot = combined_plot)
  
  return(output_list)
  
}


