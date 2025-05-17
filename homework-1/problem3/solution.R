n_last    <- 12000       # брой полици тази година
claims    <- 90          # брой предявени искове тази година
avg_cost  <- 1000        # средна изплатена сума на иск (лева)
n_next    <- 13000       # брой полици следващата година

# Оценка на вероятността за иск p
p_hat <- claims / n_last
p_hat

# a)
# Ако X ~ Bin(n_next, p_hat), то P(X > 110) = 1 - P(X <= 110)
prob_more_110 <- 1 - pbinom(110, size = n_next, prob = p_hat)
prob_more_110



# b)
q_999 <- qbinom(0.999, size = n_next, prob = p_hat)
max_expected_loss <- avg_cost * q_999
premium_b <- max_expected_loss / n_next
premium_b



# c)
q_99  <- qbinom(0.99, size = n_next, prob = p_hat)
required_revenue <- avg_cost * q_99 + 26000
premium_c <- required_revenue / n_next
premium_c




# results
prob_more_110  # вероятност за >110 искове
premium_b      # левове на полица, за да покрие щети с 99.9% вероятност
premium_c      # левове на полица, за да има ≥26 000 лв. печалба с 99% вероятност
