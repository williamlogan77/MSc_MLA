# General Initialisation Step
# Read in the data from the csv
load("./data/FishLengths.RData")

# Function: to initialize starting values for the EM algorithm
# using k-mean clustering
# INPUT:
#  data: data frame with 3 columns where 2nd column contains the vector of interest
# OUTPUT:
#   initial_mu_hat - vector of initial values for the mean
#   initial_sd_hat - vector of initial values for the mean
#   initial_lambda_hat - vector of initial values for the mean


initialisation <- function(data) {
    Length <- sort(data[, 2])

    # Checking for valid inputs
    check_inputs <- list(is.numeric(Length), is.data.frame(data))
    if (any(check_inputs == FALSE)) stop(" invalid arguments ")

    # Apply k-means with 3 centers
    cl <- kmeans(Length, 3)$cluster

    # Compute mean of each Cluster
    mu_hat1 <- mean(Length[cl == 1])
    mu_hat2 <- mean(Length[cl == 2])
    mu_hat3 <- mean(Length[cl == 3])
    initial_mu_hat <- sort(c(mu_hat1, mu_hat2, mu_hat3))

    # Compute standard deviaton of each cluster
    sigma_hat1 <- sd(Length[cl == 1])
    sigma_hat2 <- sd(Length[cl == 2])
    sigma_hat3 <- sd(Length[cl == 3])
    initial_sd_hat <- sort(c(sigma_hat1, sigma_hat2, sigma_hat3))

    # Compute probability of being in each cluster
    lambda_hat1 <- sum(cl == 1) / length(cl)
    lambda_hat2 <- sum(cl == 2) / length(cl)
    lambda_hat3 <- sum(cl == 3) / length(cl)
    initial_lambda_hat <- sort(c(lambda_hat1, lambda_hat2, lambda_hat3))

    return(list(
        "initial_mu_hat" = initial_mu_hat,
        "initial_sd_hat" = initial_sd_hat,
        "initial_lambda_hat" = initial_lambda_hat
    ))
}
