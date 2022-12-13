#install.packages('tidymodels)

library('tidymodels')
library('broom.mixed')
`library('data.table')
theme_set(theme_bw())

urchins = fread('https://tidymodels.org/start/models/urchins.csv')
setnames(urchins, c('food_regime', 'initial_volume', 'width'))
urchins[, food_regime := factor(food_regime, levels = c('Initial', 'Low', 'High'))]

ggplot(
  urchins,
  aes(x = initial_volume, y = width, group = food_regime, col = food_regime)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = 'plasma', end = 0.7)

#declaring model with default engine
lmMod = linear_reg()

#estimating/training the model
lmFit = fit(lmMod, width ~ initial_volume * food_regime, data = urchins)
lmFit

#tidy model summary using tidy()
data.table(tidy(lmFit))

#using a model to predict
newPoints = data.table(expand.grid(
  initial_volume = 20, food_regime = c('Initial', 'Low', 'High')))

meanPred = data.table(predict(lmFit, new_data = newPoints))
meanPred

confIntPred = data.table(
  predict(lmFit, new_data = newPoints, type = 'conf_int'))
confIntPred

pData = cbind(newPoints, meanPred, confIntPred)

ggplot(pData, aes(x = food_regime)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = 0.2) +
  labs(y = 'urchin size')

#modeling with a different engine (e.g., bayesian engine)
#setting prior distribution
priorDist = rstanarm::student_t(df = 1)

set.seed(123)

#setting the engine of a linear_reg() object to accomodate a bayesian model
bayesMod = set_engine(
  linear_reg(), 'stan', prior_intercept = priorDist, prior = priorDist)

#train the model
bayesFit = fit(bayesMod, width ~ initial_volume * food_regime, data = urchins)

#printing model summary
print(bayesFit, digits = 5)

#printing tidy model summary
data.table(tidy(bayesFit, conf.int = TRUE))

#even though the underlying packages for each model are different, we can use the same code
#as we did with lmFit to summarize and plot predictions

#using a bayesian model to predict
meanPredBayes = data.table(predict(bayesFit, new_data = newPoints))
meanPredBayes

confIntPredBayes = data.table(
  predict(bayesFit, new_data = newPoints, type = 'conf_int'))
confIntPredBayes

pDataBayes = cbind(newPoints, meanPredBayes, confIntPredBayes)

ggplot(pDataBayes, aes(x = food_regime)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = 0.2) +
  labs(y = 'urchin size')
