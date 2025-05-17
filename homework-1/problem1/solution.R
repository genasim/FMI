# Data preparation
print("prepare data")
data <- read.csv("Task-1.csv", header = TRUE)
X <- data[[1]]

n_total <- length(X)
n_region <- n_total / 2
region1 <- X[1:n_region]
region2 <- X[(n_region+1):n_total]

# A)
mu_hat <- mean(X)
mu_hat

# B)
sigma_hat <- sd(X)
threshold_5pct <- qnorm(0.05, mean = mu_hat, sd = sigma_hat)
threshold_5pct

# C)
threshold_95pct <- qnorm(0.95, mean = mu_hat, sd = sigma_hat)
decares_boosted <- which(X > threshold_95pct)

decares_boosted_region1 <- decares_boosted[decares_boosted <= n_region]
decares_boosted_region2 <- decares_boosted[decares_boosted >  n_region] - n_region

decares_boosted
decares_boosted_region1
decares_boosted_region2

# D)
mu1 <- mean(region1)
mu2 <- mean(region2)

profit1 <- mu1 - 9
profit2 <- mu2 - 9

list(
  region1 = list(mean_yield = mu1, expected_profit = profit1),
  region2 = list(mean_yield = mu2, expected_profit = profit2)
)

