library(stringr)

data <- read.csv("./data.csv")
head(data)
str(data)
dim(data)
sort(data$radius_mean)


# Ensure that there are no incorrect observations in the dataset (meaning that a variable is = 0 when it could not)
zero_removal <- function(data){
  for (column in names(data)) {
    amount <- sum(data[[column]] == 0, na.rm = TRUE)
    
    if (amount > 0){
      message <- stringr::str_glue("The column {column} had {amount} elements that were equal to 0")
      print(message)
      
      data[[column]][data[[column]] == 0] <- NA
    }
  }
  return(data)
}

# Assign the result to the data variable (update data variable)
data <- zero_removal(data)



