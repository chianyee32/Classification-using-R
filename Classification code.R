
#                               3152 Assignment 2
#                              Name: Chian Yee On
#                             Student ID: 33402302


rm(list = ls())
Phish = read.csv("PhishingData.csv")
set.seed(33402302) # Your Student ID is the random seed
L = as.data.frame(c(1:50))
L = L[sample(nrow(L), 10, replace = FALSE),]
Phish = Phish[(Phish$A01 %in% L),]
PD = Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows

#install.packages("tree")
library(tree)
#install.packages("e1071")
library(e1071)
#install.packages(("ROCR"))
library(ROCR)
#install.packages("randomForest")
library(randomForest)
#install.packages("adabag")
library(adabag)
#install.packages("rpart")
library(rpart)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("caret")
library(caret)

# ==================================================================================================
#
#                                       Question 1
# Explore the data: What is the proportion of phishing sites to legitimate sites? Obtain
# descriptions of the predictor (independent) variables – mean, standard deviations, etc.
# for real-valued attributes. Is there anything noteworthy in the data? Are there any
# attributes you need to consider omitting from your analysis? (1 Mark)
# 
# ==================================================================================================

str(PD)

summary(PD)

# Ensure the 'Class' column is a factor
PD$Class = as.factor(PD$Class)

# Calculate and Print the Proportion of Phishing vs. Legitimate Sites
cat("Proportion of Phishing vs. Legitimate Sites:\n")
print(table(PD$Class))

# Label 0 corresponds to a legitimate URL, label 1 to a phishing URL

class_proportion = prop.table(table(PD$Class)) 
print(class_proportion)

# Obtain Descriptions of the Predictor (Independent) Variables
cat("\nSummary of Predictor Variables:\n")
print(summary(PD))

# Calculate mean and standard deviation for numeric predictors
numeric_num = PD %>% select(where(is.numeric))

# Calculate the standard deviation for each numeric column
Standard_Deviation = sapply(numeric_num, sd, na.rm = TRUE)
print("Standard Deviation of Numeric Variables:")
print(Standard_Deviation)

# Calculate the mean for each numeric column
Mean = sapply(numeric_num, mean, na.rm = TRUE)
print("Mean of Numeric Variables:")
print(Mean)

# Combine Mean and Standard Deviation into a single summary data frame
numeric_summary = data.frame(
  Variable = names(numeric_num),
  Mean = Mean,
  Standard_Deviation = Standard_Deviation
)
print("Summary of Numeric Variables (Mean and Standard Deviation):")
print(numeric_summary)

# Check for Missing Values
cat("\nNumber of Missing Values:\n")
missing_values = sum(is.na(PD))
print(missing_values)

# ==================================================================================================
#
#                                       Question 2
# Document any pre-processing required to make the data set suitable for the model fitting
# that follows. (1 Mark)
# 
# ==================================================================================================

PD = na.omit(PD)
summary(PD)

# ==================================================================================================
#
#                                       Question 3
# Divide your data into a 70% training and 30% test set by adapting the following code
# (written for the iris data). Use your student ID as the random seed.
# 
# ==================================================================================================

set.seed(33402302) #Student ID as random seed
train.row = sample(1:nrow(PD), 0.7*nrow(PD))
PD.train = PD[train.row, ]
PD.test = PD[-train.row, ]

# Omitting the NA values
PD.train = na.omit(PD.train)
PD.test = na.omit(PD.test)

# Ensure Class is a factor in the training set
PD.train$Class = as.factor(PD.train$Class)
PD.test$Class = as.factor(PD.test$Class)

# ==================================================================================================
#
#                                       Question 4
#
# Implement a classification model using each of the following techniques. For this question
# you may use each of the R functions at their default settings if suitable. (5 Marks)
# • Decision Tree
# • Naïve Bayes
# • Bagging
# • Boosting
# • Random Forest
# 
# ==================================================================================================

# Calculate a decision tree
PD.tree = tree(Class ~., data = PD.train)

summary(PD.tree)

# Calculate naive bayes
PD.bayes = naiveBayes(Class ~. , data = PD.train)

summary(PD.bayes)

# Bagging
PD.bag = bagging(Class ~. , data = PD.train, mfinal=5)

summary(PD.bag)

#Boosting
PD.boost = boosting(Class ~. , data = PD.train, mfinal=10)

summary(PD.boost)

# Random Forest
PD.rf = randomForest(Class ~. , data = PD.train, na.action = na.exclude)

summary(PD.rf)

# ==================================================================================================
#
#                                       Question 5
#
# Using the test data, classify each of the test cases as ‘phishing (1)’ or ‘legitimate (0)’.
# Create a confusion matrix and report the accuracy of each model. (1 Mark)
# 
# ==================================================================================================

#===============#
# Decision Tree #
#===============#

# do predictions as classes and draw a table
PD.predtree = predict(PD.tree, PD.test, type = "class")
tree_conf = table(Predicted_Class = PD.predtree, Actual_Class = PD.test$Class)
cat("\n#Decision Tree Confusion\n")
print(tree_conf)

tree_sum = sum(tree_conf)
tree_sum2 = sum(diag(tree_conf))

tree_acc = (round(tree_sum2 / tree_sum, 4)) * 100
tree_acc

#===============#
#  Naive Bayes  #
#===============#

PD.predbayes = predict(PD.bayes, PD.test)
naive_conf = table(Predicted_Class = PD.predbayes, Actual_Class = PD.test$Class)
cat("\n#NaiveBayes Confusion\n")
print(naive_conf)

nb_sum = sum(naive_conf)
nb_sum2 = sum(diag(naive_conf))

naive_acc = (round(nb_sum2 / nb_sum, 4)) * 100
naive_acc

#===============#
#    Bagging    #
#===============#

PDpred.bag = predict.bagging(PD.bag, PD.test)
bag_conf = PDpred.bag$confusion
cat("\n#Bagging Confusion\n")
print(bag_conf)

bag_sum = sum(bag_conf)
bag_sum2 = sum(diag(bag_conf))

bag_acc = (round(bag_sum2 / bag_sum, 4)) * 100
bag_acc

#===============#
#    Boosting   #
#===============#

PDpred.boost = predict.boosting(PD.boost, newdata=PD.test)
boost_conf = PDpred.boost$confusion
cat("\n#Boosting Confusion\n")
print(boost_conf)

boost_sum = sum(boost_conf)
boost_sum2 = sum(diag(boost_conf))

boost_acc = (round(boost_sum2 / boost_sum, 4)) * 100
boost_acc

#===============#
# Random Forest #
#===============#

PDpredrf = predict(PD.rf, PD.test)
random_conf = table(Predicted_Class = PDpredrf, Actual_Class = PD.test$Class)
cat("\n#Random Forest Confusion\n")
print(random_conf)

random_sum = sum(random_conf)
random_sum2 = sum(diag(random_conf))

random_acc = (round(random_sum2 / random_sum, 4)) * 100
random_acc

# ==================================================================================================
#
#                                       Question 6
#
# Using the test data, calculate the confidence of predicting ‘phishing’ for each case and
# construct an ROC curve for each classifier. You should be able to plot all the curves on the
# same axis. Use a different colour for each classifier. Calculate the AUC for each classifier.
# (1 Mark)
#
# ==================================================================================================m

# Create a blank plot
plot(0, 0, type = "n", xlab = "False Positive Rate", ylab = "True Positive Rate", 
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curves for Different Classifiers")
abline(0, 1, col = "black")

# Decision Tree
PD.pred.tree = predict(PD.tree, PD.test, type = "vector")
PDDpred = ROCR::prediction(PD.pred.tree[, 2], PD.test$Class)
PDDperf = performance(PDDpred, "tpr", "fpr")
plot(PDDperf, add = TRUE, col = "blue")
PDDauc = performance(PDDpred, "auc")@y.values[[1]]
print(paste("Decision Tree AUC:", PDDauc))

# Naive Bayes
PDpred.bayes = predict(PD.bayes, PD.test, type = "raw")
PDBpred = ROCR::prediction(PDpred.bayes[, 2], PD.test$Class)
PDBperf = performance(PDBpred, "tpr", "fpr")
plot(PDBperf, add = TRUE, col = "blueviolet")
PDBauc = performance(PDBpred, "auc")@y.values[[1]]
print(paste("Naive Bayes AUC:", PDBauc))

# Bagging
PDpred.bag = predict.bagging(PD.bag, PD.test)
PDBagpred = ROCR::prediction(PDpred.bag$prob[, 2], PD.test$Class)
PDBagperf = performance(PDBagpred, "tpr", "fpr")
plot(PDBagperf, add = TRUE, col = "red")
PDBagauc = performance(PDBagpred, "auc")@y.values[[1]]
print(paste("Bagging AUC:", PDBagauc))

# Boosting
PDpred.boost = predict.boosting(PD.boost, newdata = PD.test)
PDBoostpred = ROCR::prediction(PDpred.boost$prob[, 2], PD.test$Class)
PDBoostperf = performance(PDBoostpred, "tpr", "fpr")
plot(PDBoostperf, add = TRUE, col = "green")
PDBoostauc = performance(PDBoostpred, "auc")@y.values[[1]]
print(paste("Boosting AUC:", PDBoostauc))

# Random Forest 
PDpred.rf = predict(PD.rf, PD.test, type = "prob")
PDFpred = ROCR::prediction(PDpred.rf[, 2], PD.test$Class)
PDFperf = performance(PDFpred, "tpr", "fpr")
plot(PDFperf, add = TRUE, col = "darkgreen")
PDFauc = performance(PDFpred, "auc")@y.values[[1]]
print(paste("Random Forest AUC:", PDFauc))

# Add legend
legend("bottomright", legend = c(
  paste("Decision Tree (AUC =", round(PDDauc, 4), ")"),
  paste("Naive Bayes (AUC =", round(PDBauc, 4), ")"),
  paste("Bagging (AUC =", round(PDBagauc, 4), ")"),
  paste("Boosting (AUC =", round(PDBoostauc, 4), ")"),
  paste("Random Forest (AUC =", round(PDFauc, 4), ")")
), col = c("blue", "blueviolet", "red", "green", "darkgreen"), lty = 1, cex = 0.8)

# ==================================================================================================
#
#                                       Question 7
#
# Create a table comparing the results in Questions 5 and 6 for all classifiers. Is there a
# single “best” classifier? (1 Mark)
#
# ==================================================================================================

# Create a comparison table
comparison_table = data.frame(
  Classifier = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
  AUC = c(PDDauc, PDBauc, PDBagauc, PDBoostauc, PDFauc),
  Accuracy = c(tree_acc, naive_acc, bag_acc, boost_acc, random_acc)
)

# Print the comparison table
print(comparison_table)

# Determine the best classifier based on AUC
best_auc = comparison_table[which.max(comparison_table$AUC), ]
print(paste("Best Classifier Based on AUC:", best_auc$Classifier))

# Determine the best classifier based on Accuracy
best_accuracy = comparison_table[which.max(comparison_table$Accuracy), ]
print(paste("Best Classifier Based on Accuracy:", best_accuracy$Classifier))

# ==================================================================================================
#
#                                       Question 8
#
# Examining each of the models, determine the most important variables in predicting
# whether a web site will be phishing or legitimate. Which variables could be omitted from
# the data with very little effect on performance? Give reasons. (2 Marks)
#
# ==================================================================================================

#Attribute importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(PD.tree))

cat("\n#Baging Attribute Importance\n")
print(PD.bag$importance)

cat("\n#Boosting Attribute Importance\n")
print(PD.boost$importance)

cat("\n#Random Forest Attribute Importance\n")
print(PD.rf$importance)

cat("\n#Naive Bayes Attribute Importance\n")
print("Naive Bayes just computes the probability of each variable supplied, hence it is impossible to assess the significance of the variables it uses.")

# ==================================================================================================
#
#                                       Question 9
#
# Starting with one of the classifiers you created in Question 4, create a classifier that is
# simple enough for a person to be able to classify whether a site is phishing or legitimate
# by hand. Describe your model with either a diagram or written explanation. What factors
# were important in your decision? State why you chose the attributes you used. Using the
# test data created in Question 3, evaluate model performance using the measures you
# calculated for Questions 5 and 6. How does it compare to those in Question 4? (4 Marks)
#
# ==================================================================================================

# Train a simple decision tree classifier
simple_tree = rpart(Class ~ ., data = PD.train, method = "class", control = rpart.control(cp = 0.01))

# Prune the tree to simplify it
min = which.min(simple_tree$cptable[,"xerror"])
pruned_tree = prune(simple_tree, cp = simple_tree$cptable[min, "CP"])

# Plot the pruned tree
rpart.plot(pruned_tree, type = 3, extra = 101, under = TRUE, fallen.leaves = TRUE, main = "Pruned Decision Tree")

# Describe the model
print("Pruned Decision Tree Description:")
print(pruned_tree)

# Evaluate model performance using test data
pred_pruned_tree = predict(pruned_tree, PD.test, type = "class")
pruned_tree_conf = table(Predicted_Class = pred_pruned_tree, Actual_Class = PD.test$Class)
cat("\n#Pruned Decision Tree Confusion Matrix\n")
print(pruned_tree_conf)

pruned_sum = sum(pruned_tree_conf)
pruned_sum2 = sum(diag(pruned_tree_conf))

pruned_tree_acc = (round(pruned_sum2 / pruned_sum, 4)) * 100
pruned_tree_acc

# Compute AUC for pruned tree
pred_pruned_tree_prob = predict(pruned_tree, PD.test, type = "prob")
pruned_tree_pred = ROCR::prediction(pred_pruned_tree_prob[, 2], PD.test$Class)
pruned_tree_perf = performance(pruned_tree_pred, "tpr", "fpr")
pruned_tree_auc = performance(pruned_tree_pred, "auc")@y.values[[1]]
pruned_tree_auc

# Comparison with previous classifiers
comparison_table = rbind(
  comparison_table,
  data.frame(Classifier = "Pruned Decision Tree", AUC = pruned_tree_auc, Accuracy = pruned_tree_acc)
)

print("Comparison Table with Pruned Decision Tree:")
print(comparison_table)

# ==================================================================================================
#
#                                       Question 10
#
# Create the best tree-based classifier you can. You may do this by adjusting the
# parameters, and/or cross-validation of the basic models in Question 4. Show that your
# model is better than the others using the measures you calculated for Questions 5 and 6.
# Describe how you created your improved model, and why you chose that model. What
# factors were important in your decision? State why you chose the attributes you used. (4
# Marks)
#
# ==================================================================================================

# Predict using the existing Random Forest model on the test set
PDpredrf = predict(PD.rf, PD.test, type = "class")
random_conf = table(Predicted_Class = PDpredrf, Actual_Class = PD.test$Class)
cat("\n# Random Forest Confusion Matrix\n")
print(random_conf)

# Calculate accuracy
random_sum = sum(random_conf)
random_sum2 = sum(diag(random_conf))
random_acc = (round(random_sum2 / random_sum, 4)) * 100
cat("Random Forest Accuracy: ", random_acc, "%\n")

# Calculate AUC for the existing Random Forest model
PDpred_rf_prob = predict(PD.rf, PD.test, type = "prob")
new_predrf = ROCR::prediction(PDpred_rf_prob[, 2], PD.test$Class)
new_prefrf = performance(new_predrf, "tpr", "fpr")
new_auc = performance(new_predrf, "auc")@y.values[[1]]
cat("Random Forest AUC: ", new_auc, "\n")

# Plot the ROC curve for Random Forest
plot(new_prefrf, col = "red", main = "ROC Curves for Different Classifiers", xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0, 1, col = "black")

# Add ROC curves for other models
plot(PDDperf, add = TRUE, col = "blue")
plot(PDBperf, add = TRUE, col = "blueviolet")
plot(PDBagperf, add = TRUE, col = "darkgreen")
plot(PDBoostperf, add = TRUE, col = "brown")

# Add legend to the ROC plot
legend("bottomright", legend = c(
  paste("Decision Tree (AUC =", round(PDDauc, 4), ")"),
  paste("Naive Bayes (AUC =", round(PDBauc, 4), ")"),
  paste("Bagging (AUC =", round(PDBagauc, 4), ")"),
  paste("Boosting (AUC =", round(PDBoostauc, 4), ")"),
  paste("Random Forest (AUC =", round(new_auc, 4), ")")
), col = c("blue", "blueviolet", "darkgreen", "brown", "red"), lty = 1, cex = 0.8)

# Create a comparison table
comparison_table = data.frame(
  Classifier = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
  AUC = c(PDDauc, PDBauc, PDBagauc, PDBoostauc, new_auc),
  Accuracy = c(tree_acc, naive_acc, bag_acc, boost_acc, random_acc)
)

cat("Comparison Table with Random Forest:\n")
print(comparison_table)

# ==================================================================================================
#
#                                       Question 11
#
# Using the insights from your analysis so far, implement an Artificial Neural Network
# classifier and report its performance. Comment on attributes used and your data preprocessing required. 
# How does this classifier compare with the others? Can you give any reasons? (4 Marks)
#
# ==================================================================================================

#install.packages("neuralnet")
library(neuralnet)

options(digits = 4)

# Load the dataset
ann_PD = PD

# Ensure 'Class' is a factor and convert to numeric for ANN
ann_PD$Class = as.factor(ann_PD$Class)
ann_PD$Class = as.numeric(ann_PD$Class)

# Remove rows with missing values
ann_PD = ann_PD[complete.cases(ann_PD), ]

# Split the data into training and test sets
set.seed(33402302)
ind = sample(2, nrow(ann_PD), replace = TRUE, prob = c(0.7, 0.3))
ann_PD_train = ann_PD[ind == 1, ]
ann_PD_test = ann_PD[ind == 2, ]

# Scale the numeric features
preProc = preProcess(ann_PD_train[, -ncol(ann_PD_train)], method = c("center", "scale"))
ann_PD_train[, -ncol(ann_PD_train)] <- predict(preProc, ann_PD_train[, -ncol(ann_PD_train)])
ann_PD_test[, -ncol(ann_PD_test)] <- predict(preProc, ann_PD_test[, -ncol(ann_PD_test)])

# Train the ANN model using the neuralnet package
set.seed(33402302)
PD.nn = neuralnet(Class ~ ., data = ann_PD_train, hidden = 3, linear.output = FALSE)

# Plot the neural network
plot(PD.nn)

# Predict using the ANN model on the test set
pred_ann = compute(PD.nn, ann_PD_test[, -ncol(ann_PD_test)])
pred_ann_class = ifelse(pred_ann$net.result > 0.5, 1, 0)

# Create a confusion matrix
ann_conf = confusionMatrix(factor(pred_ann_class), factor(ann_PD_test$Class))
print(ann_conf)

# Calculate AUC
pred_ann_prob = pred_ann$net.result
ann_pred = ROCR::prediction(pred_ann_prob, ann_PD_test$Class)
ann_perf = performance(ann_pred, "tpr", "fpr")
ann_auc = performance(ann_pred, "auc")@y.values[[1]]
print(paste("AUC for ANN:", ann_auc))

# Comparison table
comparison_table = data.frame(
  Classifier = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "ANN"),
  AUC = c(PDDauc, PDBauc, PDBagauc, PDBoostauc, PDFauc, ann_auc),
  Accuracy = c(tree_acc, naive_acc, bag_acc, boost_acc, random_acc, ann_conf$overall['Accuracy'] * 100)
)

print("Comparison Table with ANN:")
print(comparison_table)

# ==================================================================================================
#
#                                       Question 12
#
# Fit a new classifier to the data, test and report its performance in the same way as for
# previous models. You can choose a new type of classifier not covered in the course, or a
# new version of any of the classifiers we have studied. Either way, you will be
# implementing a new R package. As a starting point, you might refer to James et al. (2021),
# or look online. When writing up, state the new classifier and package used. Include a web
# link to the package details. Give a brief description of the model type and how it works.
# Comment on the performance of your new model. (4 Marks)
#
# ==================================================================================================

# Load necessary libraries
# install.packages("xgboost")
library(xgboost)

# Ensure 'Class' is a factor and convert to numeric for xgboost
PD$Class = as.factor(PD$Class)
PD$Class = as.numeric(PD$Class) - 1  # Convert to 0 and 1

# Split the data into training and test sets
set.seed(33402302)
train.target = sample(1:nrow(PD), 0.7 * nrow(PD))
xgb_train_set = PD[train.target, ]
xgb_test_set = PD[-train.target, ]

# Convert to matrix form, separating features and target
xgb_train_data = xgb.DMatrix(data = as.matrix(xgb_train_set[, -ncol(xgb_train_set)]), label = xgb_train_set$Class)
xgb_test_data = xgb.DMatrix(data = as.matrix(xgb_test_set[, -ncol(xgb_test_set)]), label = xgb_test_set$Class)

# Set parameters for XGBoost
params = list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.1,
  nthread = 2
)

# Train the model
xgb_model = xgb.train(
  params = params,
  data = xgb_train_data,
  nrounds = 100,
  watchlist = list(val = xgb_test_data),
  early_stopping_rounds = 10,
  verbose = 0
)

# Predict using the XGBoost model on the test set
pred_xgb = predict(xgb_model, xgb_test_data)
pred_xgb_class = ifelse(pred_xgb > 0.5, 1, 0)

# Create a confusion matrix
xgb_conf = confusionMatrix(factor(pred_xgb_class), factor(xgb_test_set$Class))
print(xgb_conf)

# Calculate AUC
xgb_pred = ROCR::prediction(pred_xgb, xgb_test_set$Class)
xgb_perf = performance(xgb_pred, "tpr", "fpr")
xgb_auc = performance(xgb_pred, "auc")@y.values[[1]]
print(paste("XGBoost AUC:", xgb_auc))

# Comparison table
comparison_table = data.frame(
  Classifier = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "XGBoost"),
  AUC = c(PDDauc, PDBauc, PDBagauc, PDBoostauc, PDFauc, xgb_auc),
  Accuracy = c(tree_acc, naive_acc, bag_acc, boost_acc, random_acc, xgb_conf$overall['Accuracy'] * 100)
)

print("Comparison Table with XGBoost:")
print(comparison_table)

