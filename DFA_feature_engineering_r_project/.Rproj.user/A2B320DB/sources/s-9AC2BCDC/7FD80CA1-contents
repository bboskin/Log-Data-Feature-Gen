library(tidymodels)

small_nasa_eight_features <-
  read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/nasa-edu-net-1k.csv")


# Adding "edu" and "net" label column -------------------------------------

small_nasa_edu <-
  data.frame(grep(".edu$", small_nasa_eight_features$name), rep("edu", length(grep(".edu$", small_nasa_eight_features$name))))

small_nasa_net <-
  data.frame(grep("net$", small_nasa_eight_features$name), rep("net", length(grep("net$", small_nasa_eight_features$name))))

colnames(small_nasa_edu) <- c("index", "site_type")
colnames(small_nasa_net) <- c("index", "site_type")

small_nasa_edu_net_sites <- bind_rows(small_nasa_edu, small_nasa_net)

small_nasa_edu_net_sites <- small_nasa_edu_net_sites %>%
  arrange(index)

small_nasa_eight_features <-
  data.frame(small_nasa_eight_features, small_nasa_edu_net_sites[,2])

colnames(small_nasa_eight_features)[11] <- "site_type"


# Eight features Model ------------------------------------------------------------
set.seed(1234)

small_nasa_eight_vars <-
  small_nasa_eight_features %>%
  select(-name)

small_nasa_eight_split <- initial_split(small_nasa_eight_vars)

small_nasa_eight_recipe <-
  training(small_nasa_eight_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  prep()

small_nasa_eight_training <- juice(small_nasa_eight_recipe)

small_nasa_eight_testing <-
  small_nasa_eight_recipe %>%
  bake(testing(small_nasa_eight_split))

small_nasa_eight_fit <-
  nasa_rf_mod %>% 
  fit(site_type ~., data = small_nasa_eight_training)

small_nasa_eight_results <-
  small_nasa_eight_testing [,dim(small_nasa_eight_testing)[2]] %>%
  bind_cols(predict(small_nasa_eight_fit, small_nasa_eight_testing))

metrics(small_nasa_eight_results, truth = site_type, estimate = .pred_class)

small_nasa_eight_results %>%
  conf_mat(truth = site_type, estimate = .pred_class) 


# 500 features Model ------------------------------------------------------------

small_nasa_fivehundo_features <-
  read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/nasa-edu-net-1k-500.csv")

small_nasa_fivehundo_features <-
  data.frame(small_nasa_fivehundo_features, small_nasa_edu_net_sites[,2])

colnames(small_nasa_fivehundo_features)[502] <- "site_type"

small_nasa_fivehundo_vars <-
  small_nasa_fivehundo_features %>%
  select(-name)

small_nasa_fivehundo_split <- initial_split(small_nasa_fivehundo_vars)

small_nasa_fivehundo_recipe <-
  training(small_nasa_fivehundo_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  #step_nzv(all_predictors()) %>%
  #step_corr(all_predictors()) %>%
  prep()

small_nasa_fivehundo_training <- juice(small_nasa_fivehundo_recipe)

small_nasa_fivehundo_testing <-
  small_nasa_fivehundo_recipe %>%
  bake(testing(small_nasa_fivehundo_split))

small_nasa_fivehundo_fit <-
  nasa_rf_mod %>% 
  fit(site_type ~., data = small_nasa_fivehundo_training)

small_nasa_fivehundo_results <-
  small_nasa_fivehundo_testing [,dim(small_nasa_fivehundo_testing)[2]] %>%
  bind_cols(predict(small_nasa_fivehundo_fit, small_nasa_fivehundo_testing))

metrics(small_nasa_fivehundo_results, truth = site_type, estimate = .pred_class)

small_nasa_fivehundo_results %>%
  conf_mat(truth = site_type, estimate = .pred_class) 



# 1000 features model -----------------------------------------------------

small_nasa_onethousand_features <-
  read_csv("/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/output-automata-csv/nasa-edu-net-1k-1000.csv")

small_nasa_onethousand_features <-
  data.frame(small_nasa_onethousand_features, small_nasa_edu_net_sites[,2])

colnames(small_nasa_onethousand_features)[dim(small_nasa_onethousand_features)[2]] <- "site_type"

small_nasa_onethousand_vars <-
  small_nasa_onethousand_features %>%
  select(-name)

small_nasa_onethousand_split <- initial_split(small_nasa_onethousand_vars)

small_nasa_onethousand_recipe <-
  training(small_nasa_onethousand_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_predictors()) %>%
  #step_nzv(all_predictors()) %>%
  #step_corr(all_predictors()) %>%
  prep()

small_nasa_onethousand_training <- juice(small_nasa_onethousand_recipe)

small_nasa_onethousand_testing <-
  small_nasa_onethousand_recipe %>%
  bake(testing(small_nasa_onethousand_split))

small_nasa_onethousand_fit <-
  nasa_rf_mod %>% 
  fit(site_type ~., data = small_nasa_onethousand_training)

small_nasa_onethousand_results <-
  small_nasa_onethousand_testing [,dim(small_nasa_onethousand_testing)[2]] %>%
  bind_cols(predict(small_nasa_onethousand_fit, small_nasa_onethousand_testing))

metrics(small_nasa_onethousand_results, truth = site_type, estimate = .pred_class)

small_nasa_onethousand_results %>%
  conf_mat(truth = site_type, estimate = .pred_class) 

