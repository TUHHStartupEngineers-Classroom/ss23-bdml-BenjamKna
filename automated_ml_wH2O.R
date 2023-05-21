# H2O modeling
library(h2o)

employee_attrition_tbl          <- read_csv("datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl             <- read_excel("data_definitions.xlsx", sheet = 1, col_names = FALSE)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)



# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)


?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)


typeof(automl_models_h2o)
## "S4"

slotNames(automl_models_h2o)
## [1] "project_name"   "leader"         "leaderboard"    "event_log"      "modeling_steps" "training_info" 

automl_models_h2o@leaderboard
##                                              model_id       auc   logloss     aucpr mean_per_class_error      rmse        mse
## 1 StackedEnsemble_BestOfFamily_AutoML_20200820_190823 0.8585439 0.2992854 0.5869929            0.2406915 0.2978416 0.08870964
## 2          GBM_grid__1_AutoML_20200820_190823_model_3 0.8494016 0.3137896 0.5165541            0.2386968 0.3098134 0.09598435
## 3 DeepLearning_grid__1_AutoML_20200820_190823_model_1 0.8479056 0.3066365 0.6154288            0.2583112 0.3071528 0.09434283
## 4      XGBoost_grid__1_AutoML_20200820_190823_model_5 0.8439162 0.3057109 0.5299331            0.2061170 0.3071419 0.09433613
## 5    StackedEnsemble_AllModels_AutoML_20200820_190823 0.8425864 0.3211612 0.5205591            0.2539894 0.3107399 0.09655928
## 6      XGBoost_grid__1_AutoML_20200820_190823_model_6 0.8257979 0.3211936 0.5009608            0.2536569 0.3111129 0.09679122
##
## [30 rows x 7 columns] 

automl_models_h2o@leader





# Depending on the algorithm, the output will be different
h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()



h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1") %>% 
  h2o.saveModel(path = "04_Modeling/h20_models/")

h2o.loadModel("04_Modeling/h20_models/DeepLearning_grid__1_AutoML_20200820_190823_model_1")




# Choose whatever model you want
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_AutoML_20200820_190823")
stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)
## [1] "environment"

predictions_tbl <- predictions %>% as_tibble()
## # A tibble: 220 x 3
##    predict    No    Yes
##    <fct>   <dbl>  <dbl>
##  1 No      0.676 0.324 
##  2 No      0.863 0.137 
##  3 No      0.951 0.0492
##  4 No      0.832 0.168 
##  5 No      0.948 0.0521
##  6 No      0.862 0.138 
##  7 No      0.849 0.151 
##  8 No      0.823 0.177 
##  9 Yes     0.553 0.447 
## 10 No      0.936 0.0638
## # … with 210 more rows