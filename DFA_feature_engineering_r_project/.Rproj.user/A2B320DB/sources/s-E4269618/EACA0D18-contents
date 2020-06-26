library(tidymodels)
library(Hmisc)

nasa_features<-read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/nasa-features.csv")

nasa_features <- nasa_noIP_grouped[,2] %>%
  bind_cols(nasa_features)

nasa_vars <- nasa_features %>%
  select(-name)

nasa_split <- initial_split(nasa_vars)

nasa_recipe <-
  training(nasa_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  prep()

nasa_training <- juice(nasa_recipe)

nasa_testing <- 
  nasa_recipe %>%
  bake(testing(nasa_split))

nasa_rf_mod <-
  rand_forest(trees = 100) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

nasa_fit <- 
  nasa_rf_mod %>%
  fit(site_type ~., data = nasa_training)

nasa_results <- 
  nasa_testing %>%
  bind_cols(predict(nasa_fit, nasa_testing))

metrics(nasa_results, truth = site_type, estimate = .pred_class)

nasa_vars %>% ggplot(aes(F3)) +
  geom_histogram()

var_imp(nasa_fit)
