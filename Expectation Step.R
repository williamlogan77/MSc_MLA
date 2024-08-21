# Expectation function

# Function to calculate the posterior
# INPUTS :
#   data : data frame with the column 2 being of the vector of interest
#   mu_hat : vector of length 3 with the initial values for the mean
#   sd_hat : vector of length 3 with the initial values for the sd
#   lambda_hat : vector of length 3 with the initial values for lambda
# OUTPUTS :
#   posterior probabilities.

expectation <- function(data, mu_hat, sd_hat, lambda_hat) {

  # Define x as a vector of length of all fish
  x <- data[, 2]

  # Initialize empty dataframes/vector for storing the likelihood, numerator
  # and posterior distribution
  likelihood <- as.data.frame(matrix(NA, ncol = 3, nrow = 1000))
  numerator <- as.data.frame(matrix(NA, ncol = 3, nrow = 1000))
  posterior <- as.data.frame(matrix(NA, ncol = 3, nrow = 1000))

  # Calculate the likelihood and numerator and store it in afrorementioned emtpy
  # dataframes
  for (i in 1:3) {
    likelihood[i] <- dnorm(x, mu_hat[i], sd_hat[i])
    numerator[i] <- likelihood[i] * lambda_hat[i]
  }

  # Calculate the denominator and store it as dataframe with one column
  denominator <- (likelihood[1] * lambda_hat[1]) +
    (likelihood[2] * lambda_hat[2]) +
    (likelihood[3] * lambda_hat[3])

  # Calculate the posterior by dividing each numerator by the denominator and
  # store in it the previously defined dataframe
  for (i in 1:3) {
    posterior[i] <- numerator[i] / denominator
  }

  # Return the dataframe posterior
  return(posterior)
}