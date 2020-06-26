library(tidymodels)

nasa_rf_mod <-
  rand_forest(trees = 100) %>%
  set_engine("randomForest") %>%
  set_mode("classification")