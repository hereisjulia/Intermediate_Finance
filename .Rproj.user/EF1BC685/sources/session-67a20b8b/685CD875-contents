#Code for Part B1

# Step 1: Review the information about OTPPB's history and investment objectives
# (This is a manual step and doesn't require code)

# Step 2: Load the data from B1.RData
load('/Users/shravanikarra/Downloads/B1.RData')

# Define the number of portfolio simulations
n_simulations <- 10000

# Create an empty matrix to store simulation results
portfolios <- matrix(nrow = n_simulations, ncol = 3)

# Step 3: Loop through simulations and calculate mean standard deviation frontier
for (i in 1:n_simulations) {
  # Generate random weights for asset classes
  weights <- runif(6)
  weights <- weights / sum(weights)  # Normalize to ensure they sum up to 1
  
  # Calculate portfolio mean return and standard deviation
  portfolio_mean <- sum(weights * B1$mean)
  portfolio_sd <- sqrt(t(weights) %*% (B1$cor %*% weights))
  portfolio_sharpe <- (portfolio_mean - B1$rf) / portfolio_sd  # Sharpe ratio
  
  # Store the results in the matrix
  portfolios[i, ] <- c(portfolio_mean, portfolio_sd, portfolio_sharpe)
}

# Create a data frame from the matrix
portfolios_df <- as.data.frame(portfolios)
colnames(portfolios_df) <- c("Mean Return", "Standard Deviation", "Sharpe Ratio")


# Step 4: Print the data frame
print(head(portfolios_df))

# Plot the efficient frontier

library(ggplot2)

ggplot(portfolios_df, aes(x = `Standard Deviation`, y = `Mean Return`, color = `Sharpe Ratio`)) +
  geom_point() +
  labs(title = "Efficient Frontier",
       x = "Standard Deviation (Risk)",
       y = "Mean Return",
       color = "Sharpe Ratio") +
  theme_minimal()


# Step 5: Compare the current asset allocation with the efficient frontier
# (This step is a manual analysis and doesn't require code)
