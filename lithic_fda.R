# all data collected from https://archaeologydataservice.ac.uk/archives/view/bifaces/index.cfm


install.packages("dplyr")
install.packages("caret")
install.packages("mda")


library(dplyr)
library(caret)
library(mda)

# clear the board
rm(list = ls());

# this imports the spreadsheet and assigns it (using the arrow'<-') to the variable 'lithics'. 
# you will need to set your working directory on your own computer
lithic<-read.table("~/biface_database.txt", row.names=1, header=TRUE,sep="\t",quote="")



# select for HANDAXE u
lithic_HA<-droplevels(filter(lithic, BF_TYPE == "HANDAXE"))

# Drop non-metric data
lithic_HA <-droplevels(lithic_HA[,c(1:2,4:26,51)]) 
# but it's good to learn this now.

# Relevel data set and set reference population to South Africa.

lithic_HA$COUNTRY <- relevel(lithic_HA$COUNTRY, ref = "SOUTH AFRICA")

#####
# Analysis
#####

set.seed(123);

# Split the data into training (80%) and test set (20%)
training.samples <- lithic_HA$COUNTRY %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- lithic_HA[training.samples, ]
test.data <- lithic_HA[-training.samples, ]

# Fit the model - training set. This uses the fda package. 
model.train <- fda(COUNTRY ~., data = train.data, method = gen.ridge, keep.fitted = TRUE)
model.train

# Confusion table for trained data. 
# Rows = predicted classes, columns = actual classes, diagonal values = correctly predicted classes. 
CT<-model.train$confusion
CT

# Test data - run the test data agains our trained model
model.test <- fda(COUNTRY ~., data = test.data, method = gen.ridge, keep.fitted = TRUE)
model.test

# Confusion table for test data
# Rows = predicted classes, columns = actual classes, diagonal values = correctly predicted classes. 
CT<-model.test$confusion
CT

# visually compare the two. 
model.test

# Make predictions. What does predict() do?
predicted.classes <- predict(model.train, test.data)
predicted.classes

# calculate prediction accuracy and error rates - similar to confusion table above (extra verification)
actuals_preds <- data.frame(cbind(actuals=test.data$COUNTRY, predicteds=predicted.classes))  
actuals_preds


# min-max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy # 78% accurate!

#####
# Plots
#####

# Colors for the plot
colors = c('#8A9197FF','#FED439FF','#709AE1FF',"#D2AF81FF","#FD7446FF")

# The code below will plot the canonical variabels
# E = England, I = Israel, M = Morocco, S = South Africa, T = Tanzania


plot(model.train, coords = c(1,2), pcex=.50,
     colors = colors,
     pch = c("S","E","I","M","T"),asp=1)

