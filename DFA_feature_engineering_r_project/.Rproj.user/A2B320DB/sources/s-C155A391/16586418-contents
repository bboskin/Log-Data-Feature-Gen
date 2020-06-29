library(tidymodels)

nasa_fiveK_baseline_df <- 
  nasa_edu_net_fiveK %>%
  group_by(host) %>%
  summarise(avg_bytes = mean(bytes),
            max_bytes = max(bytes),
            min_bytes = min(bytes),
            avg_time = mean(time),
            avg_date = mean(date),
            num = n(),
            site_type = first(site_type)
            ) %>%
  select(-host, -site_type) %>%
  bind_cols(nasa_fiveK_fivehundo_features)
  

nasa_fiveK_baseline_split<-initial_split(nasa_fiveK_baseline_df)

nasa_fiveK_baseline_recipe <-
  training(nasa_fiveK_baseline_split) %>%
  recipe(site_type~.) %>%
  step_zv(all_numeric()) %>%
  step_nzv(all_numeric()) %>%
  step_corr(all_numeric())

nasa_fiveK_baseline_fit <-
  workflow() %>%
  add_recipe(nasa_fiveK_baseline_recipe) %>%
  add_model(nasa_rf_mod) %>%
  last_fit(nasa_fiveK_baseline_split)

## Fit for baseline without five hundred features added
# nasa_fiveK_baseline_fit <-
#   workflow() %>%
#   add_formula(site_type~.) %>%
#   add_model(nasa_rf_mod) %>%
#   last_fit(nasa_fiveK_baseline_split)




