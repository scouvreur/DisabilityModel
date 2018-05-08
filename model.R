# Clear workspace variables
rm(list = ls())
cat("\014")

# Set working directory
setwd("~/Dropbox/Documents/Projects/DataScience/DisabilityModel/")

# Load libraries
library(ggplot2, pscl)

load("~/Dropbox/Documents/Projects/DataScience/DisabilityModel/data.Rdata")

summary(pd$adl)
length(pd$adl)
median(pd$adl)
range(pd$adl)
print(adl_tab <- table(pd$adl))
round(prop.table(adl_tab), 3)
qplot(pd$adl, geom = 'histogram',
      xlab ='Number of difficulties',
      bins = 20)

summary(m1 <- glm(adl ~ 1, data = pd, family = poisson))

pred <- predict(m1, type="response")[1]
pred_probs <- apply(array(0:6), 1,
                    function(count) dpois(lambda = pred, x = count))

# Create a data frame containing the predicted and observed
# probabilities for each count of ADL:
observed_probs <- prop.table(table(pd$adl))
plot_data <- data.frame(count = rep(0:6, 2),
                        type = c(rep('observed', 7),
                                 rep('predicted', 7)),
                        y = c(observed_probs, pred_probs))

# Then plot this
ggplot(plot_data, aes(y = y, x = count, group = type, color = type)) +
  geom_line() +
  geom_point() +
  ggtitle('Predicted vs. observed probabilities of ADL count') +
  ylab('Probability') +
  xlab('Number of ADLs reported')

summary(m2 <- glm(adl ~ factor(sex) + age, data = pd, family = poisson))
round(exp(coef(m2)), 3)

with(m2, cbind(res.deviance = deviance,
               df = df.residual,
               p = pchisq(deviance,
                          df.residual,
                          lower.tail=FALSE)))

adl_freq <- table(zip$adl)
adl_freq
round(prop.table(adl_freq), 3)

summary(m3 <- zeroinfl(adl ~ indager + indsex + smok2 + ed2 +
                         limitill + livpart, data = zip))

irr <- exp(coef(m3)[1:7])
round(irr, 3)

# Estimate the equivalent Poisson regression model
m3_poisson <- glm(adl ~ indager + indsex + smok2 + ed2 +
                    limitill + livpart,
                  data = zip,
                  family = poisson)
# Compare this to the ZIP equivalent
vuong(m3_poisson, m3)

