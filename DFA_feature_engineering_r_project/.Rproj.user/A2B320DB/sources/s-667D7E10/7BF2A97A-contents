library(tidymodels)
library(MASS)

calls <- read_csv("/Users/Jack/Documents/Github/Log-Data-Feature-Gen/featuredata.csv")

gender <- c("M", "M", "M", "M", "M", "M", "M", "M", "F", "F", "F", "F", "F", "F", "M", "M")
calls<- data.frame(calls, gender)

calls_vars<-
  calls[,-1]

call_split <- calls_vars %>%
  initial_split(prop = 10/16, strata=gender)

call_train <- training(call_split)
call_test <- testing(call_split)

rf_class_model <- 
  rand_forest(mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(gender ~ ., data = call_train)
  
results <- 
  call_test %>%
  bind_cols(predict(rf_class_model, call_test))

rf_class_model %>% predict(call_test)

