# Following the teacher's recommendations, I tried to create alternative models with different 
# response (as well as a final one starting with ALL the parameters)

# Authors: Iker Ibarrola, Enetz Quindimil, Carlos Firvida, Iñigo Infante

# We load the libraries we will used to create the model:
library(dplyr)
library(pastecs)
library(ggplot2)
library(lattice)
library(car)
library(glue)


###################################
# DATASET LOADING & CHECKING
###################################

data <- read.csv("./breast_cancer_data.csv") # We read the dataset from the csv file

head(data) # We visualize the first six rows of the dataset

str(data) # We visualize the structure of the data (the different variables, their type and some initial values)


###################################
# DATASET MODIFICATION 
###################################

# We make some of the modificatons previously done in the descriptive analysis part:

data <- data %>% select(-id) # We remove id

data$diagnosis <- factor(data$diagnosis) # Create a factor from the diagnosis variabl:
levels(data$diagnosis) <- c("benign", "malign") # Instead of using 'b' & 'm', use more descriptive labels
summary(data$diagnosis) # We check that previously steps worked properly

# We divide the dataset into two parts: one to train the model (80%) and the other to test it (20%)
data_test <- data[473:569, ] # Data to test the model
data_train <- data[1:472, ] # Data to test the model


###################################
# MODELS TRYING TO PREDICT THE AREA MEAN
###################################
mod30 <- lm(area_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se+symmetry_se, data = data_train)
summary(mod30)

mod31 <- lm(area_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+symmetry_se, data = data_train)
summary(mod31)

mod32 <- lm(area_mean~texture_mean+smoothness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+symmetry_se, data = data_train)
summary(mod32)

mod33 <- lm(area_mean~texture_mean+smoothness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +symmetry_se, data = data_train)
summary(mod33)

mod34 <- lm(area_mean~texture_mean+smoothness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            , data = data_train)
summary(mod34)


mod35 <- lm(area_mean~smoothness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            , data = data_train)
summary(mod35)


###################################
#### Confidence Interval

confidence_interval <- data.frame(area_mean = data_train$area_mean)
confidence_interval <- cbind(confidence_interval, as.data.frame(
predict(mod35, data_train, interval = "confidence", level = 0.99)))

# Add a column to check whether the predicted values is in the range or not
confidence_interval$in_range <- ifelse(confidence_interval$area_mean > confidence_interval$lwr & 
                                         confidence_interval$area_mean < confidence_interval$upr, 
                                       TRUE, FALSE)
confidence_interval # In this case, as we are using the data used to train the model, the SE is low

# We can check the percentage of predictions that were in the given range of the confidence interval
confidence_interval_area <- round(sum(confidence_interval$in_range)/nrow(confidence_interval),4)


###################################
#### Prediction Interval

# Prediction with "new" data, not the one used to train the model (prediction interval) #
prediction_interval <- data.frame(area_mean = data_test$area_mean)
prediction_interval <- cbind(prediction_interval, as.data.frame(
  predict(mod35, data_test, interval = "prediction")))

# Add a column to check whether the predicted values is in the range or not
prediction_interval$in_range <- ifelse(prediction_interval$area_mean > prediction_interval$lwr & 
                                         prediction_interval$area_mean < prediction_interval$upr, 
                                       TRUE, FALSE)
prediction_interval # In this case, as there is more variability in predicting a new response, the 
# SE (difference between lower and upper) is larger than in the case of the confidence interval

# We can check the percentage of predictions that were in the given range of the prediction intervals
prediction_interval_area <- round(sum(prediction_interval$in_range)/nrow(prediction_interval), 4)


###################################
# MODELS TRYING TO PREDICT THE PEREMETER MEAN
###################################

mod40 <- lm(perimeter_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se+symmetry_se, data = data_train)
summary(mod40)

# Remove simmetry_se
mod41 <- lm(perimeter_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +texture_se+smoothness_se, data = data_train)
summary(mod41)

# Remove texture_se
mod42 <- lm(perimeter_mean~texture_mean+smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +smoothness_se, data = data_train)
summary(mod42)

# Remove texture_mean
mod43 <- lm(perimeter_mean~smoothness_mean+compactness_mean
            +concave.points_mean+symmetry_mean+fractal_dimension_mean
            +smoothness_se, data = data_train)
summary(mod43)

# All predictors under 0.05 

###################################
#### Confidence Interval

confidence_interval2 <- data.frame(perimeter_mean = data_train$perimeter_mean)
confidence_interval2 <- cbind(confidence_interval2, as.data.frame(
  predict(mod43, data_train, interval = "confidence", level = 0.99)))

# Add a column to check whether the predicted values is in the range or not
confidence_interval2$in_range <- ifelse(confidence_interval2$perimeter_mean > confidence_interval2$lwr & 
                                         confidence_interval2$perimeter_mean < confidence_interval2$upr, 
                                       TRUE, FALSE)
confidence_interval2 # In this case, as we are using the data used to train the model, the SE is low

# We can check the percentage of predictions that were in the given range of the confidence interval
confidence_interval_perimeter <- round(sum(confidence_interval2$in_range)/nrow(confidence_interval2),4)


###################################
#### Prediction Interval

# Prediction with "new" data, not the one used to train the model (prediction interval) #
prediction_interval2 <- data.frame(perimeter_mean = data_test$perimeter_mean)
prediction_interval2 <- cbind(prediction_interval2, as.data.frame(
  predict(mod43, data_test, interval = "prediction")))

# Add a column to check whether the predicted values is in the range or not
prediction_interval2$in_range <- ifelse(prediction_interval2$perimeter_mean > prediction_interval2$lwr & 
                                         prediction_interval2$perimeter_mean < prediction_interval2$upr, 
                                       TRUE, FALSE)
prediction_interval2 # In this case, as there is more variability in predicting a new response, the 
# SE (difference between lower and upper) is larger than in the case of the confidence interval

# We can check the percentage of predictions that were in the given range of the prediction intervals
prediction_interval_perimeter <- round(sum(prediction_interval2$in_range)/nrow(prediction_interval2),4)


###################################
# MODELS STARTING WITH ALL PARAMETERS AS PREDICTORS
###################################

# All the predictors except for the ones related to area, perimetes and radius
mod60 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + texture_se + 
              smoothness_se + compactness_se + concavity_se + concave.points_se + symmetry_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concavity_worst + concave.points_worst + 
              symmetry_worst + fractal_dimension_worst, data = data_train)
summary(mod60)

# Remove smoothness_se
mod61 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + texture_se + 
              compactness_se + concavity_se + concave.points_se + symmetry_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concavity_worst + concave.points_worst + 
              symmetry_worst + fractal_dimension_worst, data = data_train)
summary(mod61)

# Remove symmetry worst
mod62 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + texture_se + 
              compactness_se + concavity_se + concave.points_se + symmetry_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concavity_worst + concave.points_worst + 
              fractal_dimension_worst, data = data_train)
summary(mod62)

# Remove texture_se
mod63 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + symmetry_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concavity_worst + concave.points_worst + 
              fractal_dimension_worst, data = data_train)
summary(mod63)


# Remove concavity_worst
mod64 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + symmetry_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concave.points_worst + 
              fractal_dimension_worst, data = data_train)
summary(mod64)


# Remove symmetry_se
mod65 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concave.points_worst + 
              fractal_dimension_worst, data = data_train)
summary(mod65)


# Remove fractal_dimension_worst
mod66 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + compactness_worst + concave.points_worst, data = data_train)
summary(mod66)


# Remove compactness_worst
mod67 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se + texture_worst + 
              smoothness_worst + concave.points_worst, data = data_train)
summary(mod67)


# Remove texture_worst
mod68 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se + 
              smoothness_worst + concave.points_worst, data = data_train)
summary(mod68)


# Remove concave.points_worst
mod69 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se + 
              smoothness_worst, data = data_train)
summary(mod69)


# Remove smoothness_worst
mod70 <- lm(radius_mean ~ texture_mean + smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se, data = data_train)
summary(mod70)


# Remove texture_mean
mod71 <- lm(radius_mean ~ smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concavity_se + concave.points_se + 
              fractal_dimension_se, data = data_train)
summary(mod71)


# Remove concavity_se
mod72 <- lm(radius_mean ~ smoothness_mean + 
              compactness_mean + concavity_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concave.points_se + 
              fractal_dimension_se, data = data_train)
summary(mod72)


# Remove concavity_mean
mod73 <- lm(radius_mean ~ smoothness_mean + 
              compactness_mean + concave.points_mean + symmetry_mean + 
              fractal_dimension_mean + 
              compactness_se + concave.points_se + 
              fractal_dimension_se, data = data_train)
summary(mod73)

plot(mod73)

# No more to remove


###################################
#### Confidence Interval

# Prediction with the data used to train the model (confidence interval) #
confidence_interval <- data.frame(radius_mean = data_train$radius_mean)
confidence_interval <- cbind(confidence_interval, as.data.frame(
  predict(mod73, data_train, interval = "confidence", level = 0.99)))

# Add a column to check whether the predicted values is in the range or not
confidence_interval$in_range <- ifelse(confidence_interval$radius_mean > confidence_interval$lwr & 
                                         confidence_interval$radius_mean < confidence_interval$upr, 
                                       TRUE, FALSE)
confidence_interval # In this case, as we are using the data used to train the model, the SE is low

# We can check the percentage of predictions that were in the given range of the confidence interval
confidence_interval_all <- round(sum(confidence_interval$in_range)/nrow(confidence_interval),4)

###################################
#### Prediction Interval

# Prediction with "new" data, not the one used to train the model (prediction interval) #
prediction_interval <- data.frame(radius_mean = data_test$radius_mean)
prediction_interval <- cbind(prediction_interval, as.data.frame(
  predict(mod73, data_test, interval = "prediction")))

# Add a column to check whether the predicted values is in the range or not
prediction_interval$in_range <- ifelse(prediction_interval$radius_mean > prediction_interval$lwr & 
                                         prediction_interval$radius_mean < prediction_interval$upr, 
                                       TRUE, FALSE)
prediction_interval # In this case, as there is more variability in predicting a new response, the 
# SE (difference between lower and upper) is larger than in the case of the confidence interval

# We can check the percentage of predictions that were in the given range of the prediction intervals
prediction_interval_all <- round(sum(prediction_interval$in_range)/nrow(prediction_interval),4)


###################################
# DISPLAYING THE RESULTS
###################################

cat("(\n\n\n\n")
glue("confidence_interval_perimeter is {confidence_interval_perimeter}")
glue("confidence_interval_area is {confidence_interval_area}")
glue("confidence_interval_all is {confidence_interval_all}")

cat("(\n\n")

glue("prediction_interval_perimeter is {prediction_interval_perimeter}")
glue("prediction_interval_area is {prediction_interval_area}")
glue("prediction_interval_all is {prediction_interval_all}")

# We can see that the results are extremely similar, meaning that there is not so much difference
# between predicting the perimeter or the area and that our initial analysis was right at discarding
# most of the variables.

# Finally, if we recall the values for the model predicting the radius, this were the results:
    # Confidence_interval_radius: 0.2033
    # Prediction_interval_radius: 0.9381

# Once again, extremely similar values, concluding that the four of them predict the response
# very similarly

