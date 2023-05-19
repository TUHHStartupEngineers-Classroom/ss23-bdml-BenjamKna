

library(tidyverse)
library(readxl)
library(rsample)
library(h2o)
library(recipes)


# Step 1 - Load the training & test dataset
product_backorders_tbl <- read_csv("product_backorders.csv")

set.seed(seed = 3030)
split_obj <- rsample::initial_split(product_backorders_tbl, prop = 0.85)
train_tbl <- training(split_obj)
test_tbl <- testing(split_obj)



# Step 2 - Specifiy the response and predictor variables

recipe_obj <- recipe(went_on_backorder ~., data = train_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(names(train_tbl)[train_tbl %>% sapply(is.character)], fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_tbl)


# Modeling
h2o.init()

# Split data into a training and a validation data frame
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl) # Leaderboard Frame


y <- "went_on_backorder" # response variable
x <- setdiff(names(train_h2o), y) # predictor 


# Step 3 - run AutoML specifying the stopping criterion
automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)



# Step 4 - View the leaderboard

# The leaderboard is a summary of the models produced by H2O AutoML

view(automl_models_h2o@leaderboard)

slotNames(automl_models_h2o)


# Step 5 - Predicting using Leader Model

predictions <- h2o.predict(automl_models_h2o@leader, newdata = as.h2o(test_tbl))

predictions_tbl <- predictions %>% as_tibble()

predictions_tbl

# Step 6 - Save the leader model

h2o.saveModel(automl_models_h2o@leader,path = "04_Modeling/h20_models/")


