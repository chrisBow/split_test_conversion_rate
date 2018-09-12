library(dplyr)
library(tidyr)
library(ggplot2)


# --- Import data 30th Aug 2018 to 10th September 2018 ---

clicks <- c(a, b)
users <- c(a, b)
transactions <- c(a, b)
cost <- c(a, b)
revenue <- c(a, b)


# --- Calculate number of non-transactions ---

non_trans <- clicks - transactions
non_trans_users <- users - transactions




# --- Perform Chi-squared test ---

chisq.test(data.frame(transactions, non_trans_users), simulate.p.value = TRUE, B = 10000)


# --- Monte Carlo simulation with beta distributions ---

max_clicks_beta <- rbeta(10000, a, a)
cpa_beta <- rbeta(10000, b, b)
mc_compare <- data.frame(max_clicks_beta, cpa_beta)

mc_compare %>%
  gather(bid_strat, con_rate) %>%
  ggplot(aes(con_rate, fill = bid_strat)) + 
  geom_density(alpha = 0.5) +
  theme_minimal()



# --- Bayesian model for difference in average order value ---

library(BEST)

max_clicks_rev <- c(a...)
cpa_rev <- c(b...)

mean(max_clicks_rev) - mean(cpa_rev)

bayes_model_revenue <- BESTmcmc(max_clicks_rev, cpa_rev)

plot(bayes_model_revenue)



# --- Removing cost-per-conversion from each order ---

max_clicks_adjusted_rev <- max_clicks_rev - (a/n)
cpa_adjusted_rev <- cpa_rev - (b/n)

mean(max_clicks_adjusted_rev) - mean(cpa_adjusted_rev)

bayes_model_revenue <- BESTmcmc(max_clicks_adjusted_rev, cpa_adjusted_rev)

plot(bayes_model_revenue)