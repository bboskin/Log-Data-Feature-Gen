library(tidymodels)
library(readr)
library(Hmisc)

nasa_new_onethou <- read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/nasa-edu-5k/new-filter-1000.csv")

nasa_new_onethou <-
  nasa_new_onethou %>%
  mutate(site_type = ifelse(grepl(".edu$", nasa_new_onethou$name), "edu", "net")) %>%
  select(-name)

nasa_new_onethou_split<-initial_split(nasa_new_onethou)

nasa_new_onethou_recipe <-
  training(nasa_new_onethou_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors())

nasa_new_onethou_fit <-
  workflow() %>%
  add_recipe(nasa_new_onethou_recipe) %>%
  add_model(nasa_rf_mod) %>%
  last_fit(nasa_new_onethou_split)



# Tuned decision tree model -----------------------------------------------

nasa_new_onethou_folds <- vfold_cv(training(nasa_new_onethou_split))

nasa_new_onethou_dt_wf <-
  workflow() %>%
  add_model(nasa_dt_tune) %>%
  add_recipe(nasa_new_onethou_recipe)

nasa_new_onethou_dt_resamples <-
  nasa_new_onethou_dt_wf %>%
  tune_grid(
    resamples = nasa_new_onethou_folds,
    grid = nasa_dt_tune_grid
  )

nasa_new_onethou_dt_best_tree <-
  nasa_new_onethou_dt_resamples %>% select_best("roc_auc")

nasa_new_onethou_dt_final_wf <-
  nasa_new_onethou_dt_wf %>% 
  finalize_workflow(nasa_new_onethou_dt_best_tree)

nasa_new_onethou_dt_final_fit <-
  nasa_new_onethou_dt_final_wf %>%
  last_fit(nasa_new_onethou_split)
  