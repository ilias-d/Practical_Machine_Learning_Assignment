library(caret)
library(rattle)

# Download files
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "train.csv", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "test.csv", method = "curl")

# Open Files
training_set <- read.csv("train.csv")
testing_set <- read.csv("test.csv")

filter_string_belt <- "^roll_belt$|^pitch_belt$|^yaw_belt$|^roll_forearm$|^pitch_forearm$|^yaw_forearm$|^roll_arm$|^pitch_arm$|^yaw_arm$|^roll_dumbbell$|^pitch_dumbbell$|^yaw_dumbbell$|^classe$"
training_set_clean <- training_set[ , grepl(filter_string_belt, names(training_set))]
testing_set_clean <- testing_set[ , grepl(filter_string_belt, names(testing_set))]
training_set_clean$classe <- as.factor(training_set_clean$classe)

N <- 50
training_set_clean_A <- training_set_clean[training_set_clean$classe == "A", ]
training_set_clean_B <- training_set_clean[training_set_clean$classe == "B", ]
training_set_clean_C <- training_set_clean[training_set_clean$classe == "C", ]
training_set_clean_D <- training_set_clean[training_set_clean$classe == "D", ]
training_set_clean_E <- training_set_clean[training_set_clean$classe == "E", ]
training_set_clean <- rbind(training_set_clean_A[1:N,], training_set_clean_B[1:N,], training_set_clean_C[1:N,], training_set_clean_D[1:N,], training_set_clean_E[1:N,])


# Prediction with Trees
modFit_Trees <- train(classe ~ ., method="rpart", data = training_set_clean)
print(modFit_Trees$finalModel)
fancyRpartPlot(modFit_Trees$finalModel)
prediction_Trees_train_set <- predict(modFit_Trees)
table(prediction_Trees_train_set, training_set_clean$classe)
prediction_Trees <- predict(modFit_Trees, newdata = testing_set_clean)

# Prediction with forests
modFit_Forest <-train(classe ~ ., method="rf", data = training_set_clean, prox=TRUE)
modFit_Forest
prediction_Forest_train_set <- predict(modFit_Forest)
table(prediction_Forest_train_set, training_set_clean$classe)
prediction_Forest <- predict(modFit_Trees, newdata = testing_set_clean)

# Prediction with Boosting
modFit_Boosting <-train(classe ~ ., method="gbm", data = training_set_clean, verbose=FALSE)
print(modFit_Boosting)
prediction_Boosting_train_set <- predict(modFit_Boosting)
table(prediction_Boosting_train_set, training_set_clean$classe)
prediction_Boosting <- predict(modFit_Boosting, newdata = testing_set_clean)

# Summarize Results
Prediction_Table <- t(rbind(prediction_Trees, prediction_Forest, prediction_Boosting))
Prediction_Table







