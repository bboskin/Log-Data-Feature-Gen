library(tidymodels)
library(readr)
library(vip)
library(doParallel)

nasa_edu_net_fiveK<-read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/input-automata-csv/small-nasa-edu-net-fiveK.csv")

# 500 features ------------------------------------------------------------

# Data prep ---------------------------------------------------------------

nasa_fiveK_fivehundo_features <-
  read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/small-nasa-edu-net-5k-500.csv")

nasa_fiveK_fivehundo_features <-
  nasa_edu_net_fiveK %>%
  group_by(host) %>%
  summarise(site_type = first(site_type)) %>%
  select(site_type) %>%
  bind_cols(nasa_fiveK_fivehundo_features) %>%
  select(-name)


# Modeling ----------------------------------------------------------------

set.seed(1234)

nasa_fiveK_fivehundo_features_split <- initial_split(nasa_fiveK_fivehundo_features)

nasa_fiveK_fivehundo_features_recipe <-
  training(nasa_fiveK_fivehundo_features_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  prep()

nasa_fiveK_fivehundo_features_training <- juice(nasa_fiveK_fivehundo_features_recipe)

nasa_fiveK_fivehundo_features_testing <-
  nasa_fiveK_fivehundo_features_recipe %>%
  bake(testing(nasa_fiveK_fivehundo_features_split))

nasa_fiveK_fivehundo_features_fit <-
  nasa_rf_mod %>% 
  fit(site_type ~., data = nasa_fiveK_fivehundo_features_training)

nasa_fiveK_fivehundo_features_results <-
  nasa_fiveK_fivehundo_features_testing [,dim(nasa_fiveK_fivehundo_features_testing)[2]] %>%
  bind_cols(predict(nasa_fiveK_fivehundo_features_fit, nasa_fiveK_fivehundo_features_testing))

metrics(nasa_fiveK_fivehundo_features_results, truth = site_type, estimate = .pred_class)

nasa_fiveK_fivehundo_features_results %>%
  conf_mat(truth = site_type, estimate = .pred_class) 

vip(nasa_fiveK_fivehundo_features_fit) +
  ggtitle("Variable Importance Plot for 5K rows, 500 features Nasa data set")



# Tuning model parameters -------------------------------------------------
set.seed(1234)

nasa_fiveK_fivehundo_features_cv <- vfold_cv(nasa_fiveK_fivehundo_features_training)

nasa_fiveK_fivehundo_features_grid <- tune_grid(
  site_type ~.,
  model = nasa_rf_tune,
  resamples = nasa_fiveK_fivehundo_features_cv
)

# 1000 features ------------------------------------------------------------

# Data prep ---------------------------------------------------------------

nasa_fiveK_onethou_features <-
  read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/small-nasa-edu-net-5k-1000.csv")

nasa_fiveK_onethou_features <-
  nasa_edu_net_fiveK %>%
  group_by(host) %>%
  summarise(site_type = first(site_type)) %>%
  select(site_type) %>%
  bind_cols(nasa_fiveK_onethou_features) %>%
  select(-name)


# Modeling ----------------------------------------------------------------

set.seed(1234)

nasa_fiveK_onethou_features_split <- initial_split(nasa_fiveK_onethou_features)

nasa_fiveK_onethou_features_recipe <-
  training(nasa_fiveK_onethou_features_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  prep()

nasa_fiveK_onethou_features_training <- juice(nasa_fiveK_onethou_features_recipe)

nasa_fiveK_onethou_features_testing <-
  nasa_fiveK_onethou_features_recipe %>%
  bake(testing(nasa_fiveK_onethou_features_split))

nasa_fiveK_onethou_features_fit <-
  nasa_ranger_mod %>% 
  fit(site_type ~., data = nasa_fiveK_onethou_features_training)

nasa_fiveK_onethou_features_results <-
  nasa_fiveK_onethou_features_testing [,dim(nasa_fiveK_onethou_features_testing)[2]] %>%
  bind_cols(predict(nasa_fiveK_onethou_features_fit, nasa_fiveK_onethou_features_testing))

metrics(nasa_fiveK_onethou_features_results, truth = site_type, estimate = .pred_class)

nasa_fiveK_onethou_features_results %>%
  conf_mat(truth = site_type, estimate = .pred_class) 

vip(nasa_fiveK_onethou_features_fit) +
  ggtitle("Variable Importance Plot for 5K rows, 1000 features Nasa data set")


