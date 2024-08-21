# A file containing the steps for Maximisation (recalculation of mu, sigma and
# lambda)

# Function calculating the estimate of mean for each group
# INPUTS:
# data: the dataset being used (grouped_data in this case)
# posterior: the entirety of the posterior values (should be the length of the
# group sample size)
# OUTPUTS:
# output: a vector containing the 3 calculated values
mu_hat <- function(data, posterior) {
    # Create a vector to place values when calculated
    output <- rep(NA, 3)
    # Loop through 1-3 (the three groups)
    for (k in 1:3) {
        # Multiply each relative length by the posterior value
        numerator <- sum((posterior[, k] * data[, 2]))
        # Calculate the denominator of the calculation
        denominator <- sum(posterior[k])

        # Loop through the lengths in the group and calculate the numerator and
        # add it to a variable (saves summing later)

        # Put the calculated value in the respective vector index
        output[k] <- numerator / denominator
    }
    return(output)
}

# Function calculating the estimate of standard deviation for each group
# INPUTS:
# data: the dataset being used (grouped_data in this case)
# posterior: the entirety of the posterior values (should be the length of the
# group sample size)
# OUTPUTS:
# output: a vector containing the 3 calculated values
sigma_hat <- function(data, posterior) {
    # Call the mu_hat function and store the estimated values in a variable for
    # use later
    mu_hat1 <- mu_hat(data, posterior)
    output <- rep(0, 3)

    # Similar to the last function, loop through each group and calculate the
    # numerator and denominator and store them in the correct places

    for (k in 1:3) {
        numerator <- sum((posterior[, k] * ((data[, 2] - mu_hat1[k])**2)))
        denominator <- sum(posterior[k])


        # Square root the numerator / denominator before putting them into
        # the correct output index
        output[k] <- sqrt(numerator / denominator)
    }
    return(output)
}

# Function calculating the probability for each group
# INPUTS:
# posterior: the entirety of the posterior values (should be the length of the
# group sample size)
# OUTPUTS:
# output: a vector containing the 3 calculated values
lambda_hat <- function(posterior) {
    # Same as other two functions, just a different calculation
    output <- rep(0, 3)
    for (k in 1:3) {
        output[k] <- mean(posterior[[k]])
    }
    return(output)
}