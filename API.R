# This file will create the API with the plumber package to host our model!
# Patrick Seebold, ST558 Final Project
library(plumber)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(GGally)
library(leaflet)
library(yardstick)

# First we bring in our training data
data = read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# run our usual cleaning/preprocessing of the data
data_sub = data[, c("Diabetes_binary", "Education", "Sex", "GenHlth", "MentHlth", "PhysHlth")]
summary(data_sub)

data_sub$Diabetes_binary = factor(data_sub$Diabetes_binary, levels = c('1','0'),
                                  labels = c("Diabetes", "No Diabetes"))
data_sub$Sex = factor(data_sub$Sex, levels = c('0','1'),
                      labels = c("Female","Male"))
data_sub$Education = factor(data_sub$Education, 
                            levels = c('1','2','3','4','5','6'),
                            labels = c("Never attended school or only kindergarten",
                                       "Grades 1 through 8 (Elementary)",
                                       "Grades 9 through 11 (Some high school)",
                                       "Grade 12 or GED (High school graduate)",
                                       "College 1 year to 3 years (Some college or technical school)",
                                       "College 4 years or more (College graduate)"))
data_sub$GenHlth = factor(data_sub$GenHlth, levels = c('1','2','3','4','5'),
                          labels = c("Excellent", "Very Good", "Good",
                                     "Fair","Poor"))

# Now train the model - first set up our established recipe
rec1 = recipe(Diabetes_binary ~., data = data_sub) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(Education, Sex, GenHlth)

# now retrain the model as before, using tuned param (mtry = 5)
params_rf = tibble(mtry=5)
set.seed(42)
cv5 = vfold_cv(train, 5) # same CV for all models
metric = metric_set(mn_log_loss)

rf_spec = rand_forest(mtry = tune()) |>
  set_engine("ranger",importance = "impurity") |>
  set_mode("classification")

rf_wkf = workflow() |>
  add_recipe(rec1) |>
  add_model(rf_spec)

rf_wkf_final = rf_wkf |> 
  finalize_workflow(params_rf)

rf_model = rf_wkf_final |> 
  fit(data = data_sub)

#### Endpoints #####

#* API Endpoint 1: Model Predictions
#* @param input_data JSON array with five vars: Sex, Education, GenHlth, PhysHlth, MentHlth
#* @post /predict

# set default input_data in case user doesn't make a selection:
input_data = jsonlite::toJSON(data.frame(Sex = "Female",
                                Education = "Grade 12 or GED (High school graduate)",
                                GenHlth = "Excellent",
THIS DOESNT WORK YET                                PhysHlth = mean(data_sub$PhysHlth),
                                MentHlth = mean(data_sub$MentHlth)))
function(input_data) {
  input = as.data.frame(jsonlite::fromJSON(input_data))
  
  # Feed the baked data to the model
  pred = predict(rf_model, new_data = input, type = "class")
  
  # Return our predictions
  list(PredictedClass = pred)
}


# API Endpoint 2: Name and rendered Github page link
#* @get /readme
function(){
  "Patrick Seebold: https://pcseebol.github.io/FinalProject/EDA.pdf"
}

# API Endpoint 3: Confusion Matrix

