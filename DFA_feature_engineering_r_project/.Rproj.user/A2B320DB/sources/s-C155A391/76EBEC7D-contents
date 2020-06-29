library(tidymodels)

nasa_rf_mod <-
  rand_forest(mtry = 13, trees = 1000) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

nasa_glm_mod <-
  logistic_reg() %>%
  set_engine("glm")

nasa_ranger_mod <-
  rand_forest(mtry = 15, trees = 1000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

nasa_nn_mod <-
  mlp() %>%
  set_engine("nnet") %>%
  set_mode("classification")

nasa_mars_mod <-
  mars() %>%
  set_engine("earth") %>%
  set_mode("classification")

nasa_knn_mod <-
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

nasa_dt_mod <-
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

## Tuning models
nasa_rf_tune <-
  rand_forest(mtry = tune(), 
              trees = 1000,
              min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

nasa_dt_tune <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("rpart") %>%
  set_mode("classification")

nasa_dt_tune_grid <-
  grid_regular(cost_complexity(),
               tree_depth(),
               levels = 5)
