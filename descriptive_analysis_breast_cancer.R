
data <- read.csv("./breast_cancer_updated.csv")
head(data)
str(data)
dim(data)
sort(data$radius_mean)[4000:5000]
