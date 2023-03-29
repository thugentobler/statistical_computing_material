# We load this snipped for each exercise  in this chapter
library(tidyverse)

diamonds <- mutate_if(diamonds, is.ordered, factor, ordered = FALSE)

#======================================================================================
# Exercise 1
#======================================================================================

fit <- lm(price ~ carat + color + cut + clarity, data = diamonds)
summary(fit)

# - Model quality: 91.6% of price variations are explained by covariates. 
#   The RMSE is 1157 USD.
# - Effects: All effects point into the intuitively right direction 
#   (larger stones are more expensive, worse color are less expensive etc.)
# - Does it make sense? One one hand yes (-> Effects). 
#   On the other hand: Additivity in color, cut and clarity are not realistic. 
#   Their effects should get larger with larger diamond size. 
#   This can be solved by adding interaction terms with carat or, much easier, 
#   to switch to a logarithmic response or using a log-link in a GLM.

#======================================================================================
# Exercise 2
#======================================================================================

fit_i <- lm(price ~ carat * (color + cut + clarity), data = diamonds)
summary(fit_i)

# Reason: See Exercise 1
# Params: For each effect of color, clarity, and cut, we get a "carat" effect: +17 parameters
# RMSE: Goes down from 1157 to 1033

#======================================================================================
# Exercise 3
#======================================================================================

library(splines)

fit_s <- lm(price ~ ns(carat, df = 5) + color + cut + clarity, data = diamonds)
summary(fit_s)

carat <- seq(0.2, 2, by = 0.01)
data <- diamonds[1, ] %>% 
  select(-carat) %>% 
  expand_grid(carat) %>% 
  mutate(prediction = predict(fit_s, .))

ggplot(data, aes(carat, prediction)) +
  geom_line(color = "darkorange")

# - A restricted cubic spline represents a numeric covariate by a smooth curve.
#   If k is the number of parameters to be invested, k-1 knots are placed.
#   Between two knots, a cubic curve is fitted with smooth transition at the knots.
#   Outside the outer knots, the curve is linear (why is this good?).
# - RMSE reduced from 1157 to 1072. The fitted curve is not so much different from a 
#   straight line, so the fit is not dramatically better here.
