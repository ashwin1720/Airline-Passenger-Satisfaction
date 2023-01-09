#  First Name       : Sai Harish Kumar
#  Last Name        : Vitta   
#  Purpose          : Apply Ramdom Forest to the breast cancer data 

# Step 0: Clear the Environment
dev.off()
rm(list=ls())

# Read the File
file<-file.choose()
bc_raw <- read.csv(file)

# Remote NAs
bc_raw <- na.omit(bc_raw)

# Categorize the Columns
bc <- bc_raw
bc$Gender <- ifelse(bc$Gender == 'Male', 1, 0)
bc$Customer.Type <- ifelse(bc$Customer.Type == 'Loyal Customer', 1, 0)
bc$Type.of.Travel <- ifelse(bc$Type.of.Travel == 'Business travel', 1, 0)
bc$Class <- ifelse(bc$Class == 'Eco', 1, 0)
bc$satisfaction <- ifelse(bc$satisfaction == 'satisfied', 1, 0)

# Extract the Feature Names
colnames = colnames(bc)

# Normalize All Columns into Range of 0 to 1
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x, na.rm = T)) / (max(x, na.rm = T)-min(x, na.rm = T)))
}

# Accuracy/Precision metrics   
err_metric=function(comparision.matrix)
{
  true.negative =comparision.matrix[1,1]
  true.positive =comparision.matrix[2,2]
  false.positive =comparision.matrix[1,2]
  false.negative =comparision.matrix[2,1]
  precision =(true.positive)/(true.positive+false.positive)
  recall_score =(false.positive)/(false.positive+true.negative)
  
  f1_score <- F1_Score(y_pred = predicted.values, y_true = test$satisfaction, positive = "1")
  accuracy_model  =(true.positive+true.negative)/(true.positive+true.negative+false.positive+false.negative)
  False_positive_rate =(false.positive)/(false.positive+true.negative)
  False_negative_rate =(false.negative)/(false.negative+true.positive)
  
  print(paste("Precision value of the model: ",round(precision,2)))
  print(paste("Accuracy of the model: ",round(accuracy_model,2)))
  print(paste("Recall value of the model: ",round(recall_score,2)))
  print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
  print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
  print(paste("f1 score of the model: ",round(f1_score,2)))
}

# Calculate the Standard Deviation
sde <- c()
for(col in colnames) { sde <- append(sde, sd(bc[, col])) }
# Correlation through Spearman formula (For Ordinal Data)
correlation <- c()
for (i in seq(1, length(colnames))) {
  cov <- cov(bc[, i], bc[, "satisfaction"])
  col_correlation <- cov / (sde[i] * sde[25])
  correlation <- append(correlation, col_correlation)
}
related.features <- ifelse((correlation > 0.1 | correlation < -0.1), TRUE, FALSE)
bc_filtered <- bc[related.features]


# For without Correlation Work
bc_filtered <- bc
# Factor the Target Column
bc_filtered$satisfaction <- as.factor(bc_filtered$satisfaction)

# Data Sampling 
set.seed(1)
idx<-sort(sample(nrow(bc_filtered),as.integer(.70*nrow(bc_filtered))))
training<-bc_filtered[idx,]
test<-bc_filtered[-idx,]

# =============================================
#           Apply Random Forest
# =============================================
#install.packages('randomForest')
library('randomForest')
tree_mod <- randomForest(x=training[, c(-25)], y = training$satisfaction)
predicted.values <- predict(tree_mod, test[, c(-25)])
print("RANDOM FOREST: ")
comparision.matrix = table(test$satisfaction, predicted.values)
err_metric(comparision.matrix)
# =============================================
#           Apply Gaussian Naive Bayes
# =============================================
#install.packages("e1071")
library(e1071)
classifier_cl <- naiveBayes(satisfaction~., data = training)
predicted.values <- predict(classifier_cl, test[-25], type = "class")
print("NAIVE BAYES: ")
comparision.matrix = table(test$satisfaction, predicted.values)
err_metric(comparision.matrix)
print("\n\n")
# =============================================
#           Apply CART
# =============================================
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)  		
CART_clasifier<-rpart( satisfaction~.,data=training)
predicted.values<-predict(CART_clasifier,test[-25], type="class")
rpart.plot(CART_clasifier)
print("CART: ")
comparision.matrix = table(test$satisfaction, predicted.values)
err_metric(comparision.matrix)

# =============================================
#           Apply KNN
# =============================================

for (colname in colnames) {
  bc[, colname] <- normalize(bc[, colname])
}
# install.packages("kknn")
library(kknn)
predict_k1 <- kknn(formula= satisfaction~., training , test[,c(-25)], k=5,kernel ="rectangular")
predicted.values <- fitted(predict_k1)
print("KNN: ")
comparision.matrix = table(test$satisfaction, predicted.values)
err_metric(comparision.matrix)
