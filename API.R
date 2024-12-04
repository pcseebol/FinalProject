# This file will create the API with the plumber package to host our model!
# Patrick Seebold, ST558 Final Project
library(plumber)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(GGally)
library(leaflet)
library(yardstick)

# Start by reading in the data
data = read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# run our usual cleaning/preprocessing of the data:
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



# API endpoint 2: Name and Github link
#* @get /readme
function(){
  "This is our basic API"
}

#* Find natural log of a number
#* @param num Number to find ln of
#* @get /ln
function(num){
  log(as.numeric(num))
}

#query with http://localhost:PORT/ln?num=1

#* Find multiple of two numbers
#* @param num1 1st number
#* @param num2 2nd number
#* @get /mult
function(num1, num2){
  as.numeric(num1)*as.numeric(num2)
}

#query with http://localhost:PORT/mult?num1=10&num2=20

#* Plot of iris data
#* @serializer png
#* @param type base or ggally
#* @param color TRUE or FALSE (only for ggally)
#* @get /plotiris
function(type = "base", color = FALSE){
  if(tolower(type) == "ggally"){
    if(color){
      a <- GGally::ggpairs(iris, aes(color = Species))
      print(a)
    } else {
      a <- GGally::ggpairs(iris)
      print(a)
    }
  } else {
    pairs(iris)
  }
}
#http://localhost:PORT/plotiris?type=ggally


#* Plotting widget
#* @serializer htmlwidget
#* @param lat latitude
#* @param lng longitude
#* @get /map
function(lng = 174.768, lat = -36.852){
  m <- leaflet::leaflet() |>
    addTiles() |>  # Add default OpenStreetMap map tiles
    addMarkers(as.numeric(lng), as.numeric(lat))
  m  # Print the map
}

#query with http://localhost:PORT/map?lng=174&lat=-36


# Choose a predictor
#* @param predictor
#* @get /pred
function(predictor) {
  data <- iris
  if (is.numeric(data[[predictor]])) {
    value <- mean(data[[predictor]])
    message <- paste("The mean of", predictor, "is", value)
    return(message)
  } else if (predictor == "Species") {
    table <- table(data[[predictor]])
    return(paste0(names(table), ": ", table))
  } else {
    stop("Invalid predictor.")
  }
}
