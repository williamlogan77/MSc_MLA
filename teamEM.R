# An innitial trial at the teamEM function
source("src/Initialisation Step.R")
source("src/Expectation Step.R")
source("src/Maximisation Step.R")

teamEM <- function(data, epsilon = 1e-6, maxit = 1000) {
  # Test inputs as explained in the Testing Pseudocode file
  check_inputs <- list(
    is.numeric(epsilon),
    is.numeric(maxit),
    is.data.frame(data),
    maxit > 0
  )
  if (any(check_inputs == FALSE)) stop("invalid arguments")

  # Create an inits variable and store the inital values for mu, sigma and
  # lambda in it
  inits <- initialisation(data)

  # Define a function that returns the Log likelihood as one value of the data
  # given a certain mu, sigma and lambda
  LogSum <- function(data, mu, sigma, lambda) {
    # Initialise an empty dataframe to store the values to be summed
    summation <- data.frame(
      ncol = 3, nrow = 1000,
      rep(0, 1000),
      rep(0, 1000),
      rep(0, 1000)
    )

    # Calculate the value that needs to be summed and store it in the summation
    # dataframe
    for (k in 1:3) {
      summation[, k] <- lambda[k] * dnorm(data[, 2], mu[k], sigma[k])
    }

    # Sum values across the rows, so that there is only one column of data
    one_col <- rowSums(summation)
    # Fianlly, take the log of each value and sum them. Note that this is a sum
    # rather than a multiplication due to the laws of logarithms.
    LogSum <- sum(log(one_col))

    # Return the calculated value
    return(LogSum)
  }

  # Store the inital values in the variables starting `new_` that are updated
  # every iteration of the convergence step
  new_mu <- inits$initial_mu_hat
  new_lambda <- inits$initial_lambda_hat
  new_sigma <- inits$initial_sd_hat

  # Initialise the counter variable to count iterations passed and create a
  # difference variable that changes with each convergence
  count <- 0
  diff <- Inf

  # Calculate the inital log likelihood with initial values and store it in
  # `prev_val`
  prev_val <- LogSum(
    data,
    inits$initial_mu_hat,
    inits$initial_sd_hat,
    inits$initial_lambda_hat
  )

  # Create an empty vector that will store all of the log likelihoods.
  log_likelihood <- c()

  # Initialise a while loop that runs on the condition that the absolute
  # difference is greater than our user set value epsilon.
  while (abs(diff) > epsilon) {
    # Stop the loop if the number of iterations reaches the user set max
    # iteration value.
    if (count == maxit) break

    # Calculate the update posterior value based on the changed data
    posterior <- expectation(
      data,
      new_mu,
      new_sigma,
      new_lambda
    )

    # Calculate the new mu, sigma, lambda based on the updated posterior values
    new_mu <- mu_hat(data, posterior)
    new_sigma <- sigma_hat(data, posterior)
    new_lambda <- lambda_hat(posterior)


    # Calculate the "next value" or `i+1`th value to compare to the previous
    # value
    next_val <- LogSum(data, new_mu, new_sigma, new_lambda)

    # Calculate difference between the `i+1`th value and the `i`th value
    diff <- next_val - prev_val

    # Store the `i+1`th value as the current value to compare against for the
    # next iteration
    prev_val <- next_val

    # Store the values in the vector.
    log_likelihood <- c(log_likelihood, next_val)

    # Add one to the count to keep track of iteration number
    count <- count + 1
  }

  # Sorting out output

  # Dataframe of estimate values
  estimates <- data.frame(
    "mu" = new_mu,
    "sigma" = new_sigma,
    "lambda" = new_lambda,
    row.names = c("Age1", "Age2", "Age3")
  )

  # Dataframe of initial values
  inits <- data.frame(
    "mu" = inits$initial_mu_hat,
    "sigma" = inits$initial_sd_hat,
    "lambda" = inits$initial_lambda_hat,
    row.names = c("Age1", "Age2", "Age3")
  )

  # Store converged as a boolean variable
  converged <- diff < epsilon

  # Dataframe for the posterior values
  posterior <- data.frame(
    "posteriors for group 1" = posterior[1],
    "posteriors for group 2" = posterior[2],
    "posteriors for group 3" = posterior[3]
  )

  # Store the log_likelihood as the variable name likelihood
  likelihood <- log_likelihood


  # Return the values as a list
  return(
    list(
      estimates = estimates,
      inits = inits,
      converged = converged,
      posterior = posterior,
      likelihood = likelihood
    )
  )
}