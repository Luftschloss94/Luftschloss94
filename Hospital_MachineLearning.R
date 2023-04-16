#' Author: Fernando Gamboa 
#' Title: A2 - Hospital readmissions 
#' Date: 08/April/2023 

#Sections:
#1 Data cleaning and first EDA
#2 Logistic Regression Model
#3 Decision Tree Model
#4 RandomForest Model
#5 Evaluating the best model 
#6 Top 100 patients to be readmitted EDA and Insights 


## Set the working directory
setwd("~/Desktop/R Studio/Hult_R/personalFiles")

#packages 
install.packages("tidyverse")
install.packages("readr")
install.packages("tidyr")
install.packages("data.table")
install.packages("lattice")

#Libraries required 
library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(vtreat)
library(caret)
library(rpart.plot)
library(MLmetrics)
library(pROC)
library(ranger)
library(psych)

#Opening the required files 

Hospital_Test <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv", na.strings = c("", "NA", "?")) 
Hospital_Train <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv", na.strings = c("", "NA", "?"))
Meds_Test <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv", na.strings = c("", "NA", "?"))
Meds_Train <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv", na.strings = c("", "NA", "?"))
Patient_Test <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv", na.strings = c("", "NA", "?"))
Patient_Train <- read.csv("https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv", na.strings = c("", "NA", "?"))

#Section 1:Data cleaning and first EDA

#Join all Train data 
#train 
train_data <- left_join(Hospital_Train,Meds_Train, by = "tmpID") #Join Hospital with Meds on train 
train_data <- left_join(train_data, Patient_Train, by = "tmpID") #Join train data with Patient data 

#Validate that the information can be displayed correctly 
head(train_data)

# Structure 
str(train_data)

#Review number of columns 
ncol(train_data)

#blanks 
colSums(train_data == "") #Admission_type_id and admission_source have some null values, could be dropped 

#NA
colSums(is.na(train_data)) #medical specialty 41.32% NA, payer_code 53.08% NAs. Drop this 2 columns 
colMeans(is.na(train_data))

#Drop Medical Specialty and Payer Code 
drops <- c('medical_specialty', 'payer_code')
train_data <- train_data[,!(names(train_data) %in% drops)]
colSums(is.na(train_data)) #Validate
ncol(train_data) #Columns dropped from 44 to 42 

#Clean NA through Feature Engineering / Imputation 
#Admission_type_id, race, discharge disposition, admin source, diagnose 1, 2 and 3 for  imputation 
# Select columns that need to be cleaned 
clean_columns <- c("race", "admission_type_id", "discharge_disposition_id", 
                     "admission_source_id", "diag_1_desc", "diag_2_desc", "diag_3_desc")

# Use replace function in loop to replace data 
for (col in clean_columns) {
  mode_prop <- names(sort(table(train_data[[col]]), decreasing = TRUE))[1]
  train_data[[col]] <- ifelse(is.na(train_data[[col]]), mode_prop, train_data[[col]])
}

colSums(is.na(train_data)) #Validate

#Detecting Possible Outliers 
detection <- c("time_in_hospital","num_procedures","number_outpatient","number_inpatient")

#Boxplot
#Number outpatient and inpatient have outliers, this may be treated within the prediction models 
boxplot(train_data[,c(detection)])

#Age  
#Age is skewed to the right, with most patients being 60-80 years old 
hist(train_data$age, main="Age Distribution") 

#Weight
#Weight is distributed normally 
hist(train_data$wgt, main="Weight Distribution") 

#General information graphics to review data 
pairs.panels(train_data[c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses")])

#test 
test_data <- left_join(Hospital_Test, Meds_Test, by = "tmpID") #Join Hospital with Meds on test 
test_data <- left_join(test_data, Patient_Test, by = "tmpID") #Join test data with Patient on test 

#Validate that the information can be displayed correctly 
head(test_data)

#Review number of columns 
ncol(test_data)

#Drop same columns as Train data 
drops <- c('medical_specialty', 'payer_code')
test_data <- test_data[,!(names(test_data) %in% drops)]
ncol(test_data) #Validate


#Clean NA through Feature Engineering / Imputation - Apply same process to Test Data
#Admission_type_id, race, discharge disposition, admin source, diagnose 1, 2 and 3 for  imputation 
# Select columns that need to be cleaned 
clean_test <- c("race", "admission_type_id", "discharge_disposition_id", 
                   "admission_source_id", "diag_1_desc", "diag_2_desc", "diag_3_desc")

# loop over the columns and replace NAs with the mode
for (col in clean_test) {
  mode_test <- names(sort(table(test_data[[col]]), decreasing = TRUE))[1]
  test_data[[col]] <- ifelse(is.na(test_data[[col]]), mode_test, test_data[[col]])
}

colSums(is.na(test_data)) #Validate


#Readmitted_y is our Y variable for the models to evaluate 

#Data Preprocessing and Preparation 
#Preparing data samples and segmenting data 

# Examine the oversampled data for more equal class balances 
table(train_data$readmitted_y)

# Identify the informative and target
# This dataframes will be used for design treatment 
names(train_data)
targetVariable <- names(train_data)[42]
informativeVariable <- names(train_data)[1:41] #Select relevant columns only 
informative_test <- names(train_data)[1:41] #Test Data mirrors train columns 

# Segment data 
set.seed(1000)
idx <- sample(1: nrow(train_data), .1*nrow(train_data))
trainSet <- train_data[idx, ] #Prep Data
testSet <- train_data[-idx, ] #Non Prep

# EDA 
summary(trainSet)

# Names of columns
names(trainSet)

# Design a "C"ategorical variable plan 
hospital_plan <- designTreatmentsC(trainSet, 
                          informativeVariable,
                          targetVariable, 1)

# Apply treatment to X variables 
treatedXVar <- prepare(hospital_plan, testSet)

#This model will be used to validate data with predictions 
treatedTest  <- prepare(hospital_plan, test_data)

# Do a second partition to prevent overfitting 

# SplitPercent 
splitPercent <- round(nrow(train_data) %*% .9)

set.seed(1500)
idx <- sample(1: nrow(train_data), splitPercent )
train <- treatedXVar[idx, ]
validate <- treatedXVar[-idx, ] #review this part of the formula

# Section 2: Logistic Regression Model

# Loggistic Regression AND FIT 

# Fit the model into a logistic regression 
fit <- lm(readmitted_y ~ ., train)

# Review Regression 
summary(fit)

# Do a Backward Variable to reduce multi-colinearity 
bestFit <- step(fit, direction='backward')

#Review Best fit
summary(bestFit)

#Compare change in columns 
#Model reduced from 120 to 41 columns 
length(coefficients(fit))
length(coefficients(bestFit))

# Get predictions
Predictions <- predict(bestFit,  validate, type='response')
tail(Predictions)

# Classify and Cutoff 
cutoff      <- 0.5
HospitalClasses <- ifelse(Predictions >= cutoff,1, 0)

results <- data.frame(actual  = testSet[-idx,]$readmitted_y,
                      classes = HospitalClasses,
                      probs   = Predictions)
head(results)

# Confusion Matrix 
(confMat <- ConfusionMatrix(results$classes, results$actual))

# Accuracy 
sum(diag(confMat)) / sum(confMat)
Accuracy(results$classes, results$actual)

# Visualization of classes 
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'darkgreen')

# ROC; chg boolean to 1/0
ROCobj <- roc(results$classes, results$actual*1)
plot(ROCobj)

# Increase the cutoff to improve balanced accuracy
# You can do this programmatically following this blog: https://rpubs.com/raysunau/optimal_cutoff_point
newCutoff <- .39
newClasses <- ifelse(Predictions >= newCutoff, 1,0)
#Improved confusion matrix with new cutoff 
(confMat <- ConfusionMatrix(newClasses, results$actual))

#accuracy goes down to 63.50%, but confusion matrix improves 
Accuracy(newClasses, results$actual)

# Calculate the performance metrics
accuracy <- sum(diag(confMat)) / sum(confMat)
precision <- diag(confMat) / colSums(confMat)
recall <- diag(confMat) / rowSums(confMat)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the performance metrics
cat(paste0("Accuracy: ", round(accuracy, 2), "\n"))
cat(paste0("Precision: ", round(precision, 2), "\n"))
cat(paste0("Recall: ", round(recall, 2), "\n"))
cat(paste0("F1 Score: ", round(f1_score, 2), "\n"))


# Visualization of classes for New Cutoff 
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = newCutoff), color = 'darkgreen')

# Section 3: Decision Tree Model

# Decision Tree Model

#Identify factors with only 1 unique value 
sapply(train_data, function(x) length(unique(x)))

#Drop acetohexamide, troglitazone, citoglipton, acetohexamide, examide

drop_tree <- c('acetohexamide', 'troglitazone', 'citoglipton', 'acetohexamide', 'examide')
train_data <- train_data[,!(names(train_data) %in% drop_tree)]

# Fit a decision tree with caret
set.seed(1300)
fit_tree <- train(as.factor(readmitted_y) ~., #formula based
             data = train_data, #data in
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1, .25)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

#Examine
fit_tree

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit_tree)

# Plot a pruned tree, best form to visualize 
pdf('bestTreeH.pdf')
prp(fit_tree$finalModel, extra = 1)
dev.off()

# Make some predictions on the training set
trainCaret <- predict(fit_tree, train_data)
head(trainCaret)

# Get the conf Matrix
confusion_tree <- confusionMatrix(trainCaret, as.factor(train_data$readmitted_y))
confusion_tree
precision <- confusion_tree$byClass["Precision"]
precision
f1_score <- confusion_tree$byClass["F1"]
f1_score

#Before running predict again, equal levels for discharge
test_data$discharge_disposition_id <- factor(test_data$discharge_disposition_id, levels = "unique")

# Run prediction model one more time 
testCaret <- predict(fit_tree,test_data)


# Section 4:RandomForest Model

#Random Forest Model 

# Fit a random forest model with Ranger
Hospital_SampleFit <- ranger(as.factor(readmitted_y) ~ .,
                     data  = treatedXVar, 
                     num.trees = 120,
                     importance = 'permutation',
                     mtry  = 1, 
                     probability = T)
Hospital_SampleFit


# Reviewing improved variable importance
VarImpDF <- data.frame(variables = names(ranger::importance(Hospital_SampleFit)),
                       importance = ranger::importance(Hospital_SampleFit),
                       row.names = NULL)

#Sorting in decreasing order 
#only choose top 10 variables 
VarImpDF <- VarImpDF[order(VarImpDF$importance, decreasing = T),][1:10,]

#Visualize the variable importance

ggplot(VarImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()

# TrainClass Prediction 
trainClass <- predict(Hospital_SampleFit, treatedXVar)
# In ranger objects, the predictions are within a list and need to be declared
head(trainClass$predictions)

# Using the prediction probability list element, classify with 0.50 cutoff 
classOutcome <- ifelse(trainClass$predictions[,2]>=0.5,'TRUE','FALSE')
#Confusion Matrix
confusion_forest <- confusionMatrix(as.factor(classOutcome), 
                as.factor(treatedXVar$readmitted_y))
confusion_forest
precision <- confusion_forest$byClass["Precision"]
precision
f1_score <- confusion_forest$byClass["F1"]
f1_score


### Apply to the validation test set
oneHundredPatients <- predict(Hospital_SampleFit, treatedXVar)
oneHundredPatients

# Accuracy Comparison from MLmetrics
classOutcomeTest <- ifelse(oneHundredPatients$predictions[,2]>=0.5,
                           'TRUE','FALSE')

#Accuracy for the model 
Accuracy(as.factor(classOutcomeTest), 
         as.factor(treatedXVar$readmitted_y))


# Section 5: Evaluating the best model 

#Model Evaluation 
#Since Accuracy is a more solid indicator when determining the model success in predictability, Logistic Regression is chosen 

evaluation <- data.frame(models = c("Logistic Regression", "Decision Tree", "RandomForest"),
                        Accuracy = c(0.6721,0.6472,0.6071),
                        F1_Score = c(0.67,0.7566,0.7539),
                        Precision = c(0.62,0.6462,0.6051)
                                  )
evaluation
#Logistic Regression came out as the model with the highest accuracy 
#Validate the model with Test Data 

#Split
splitPercent_test <- round(nrow(test_data) %*% .9)

set.seed(1200)
idx_t <- sample(1: nrow(test_data), splitPercent_test )
test_train <- treatedTest[idx_t, ]
validate_test <- treatedTest[-idx_t, ]

#Fit for test 
fit_test <- lm(readmitted_y ~ ., test_train)

# Review Regression 
summary(fit_test)

# Do a Backward Variable to reduce multi-colinearity 
bestFit_test <- step(fit_test, direction='backward')

#Review Best fit
summary(bestFit_test)

#Compare change in columns 
#Model reduced from 120 to 41 columns 
length(coefficients(fit_test))
length(coefficients(bestFit_test))


# Get predictions
tPredictions <- predict(bestFit_test,  treatedTest, type='response')
tail(tPredictions)
head(tPredictions)

# Noticed that probability for some case was above 100%, used this to bound the data into 0 and 1, and limit the incorrect prob
tPredictions_logistic <- exp(tPredictions) / (1 + exp(tPredictions))
tail(tPredictions_logistic)

#Do cutoff for test data set predictions 
cutoff_t      <- 0.5
TestClasses <- ifelse(tPredictions_logistic >= cutoff_t,1, 0)

#Results data frame 
results_test <- data.frame(actual  = validate_test$readmitted_y,
                           tmpID  =  validate_test$tmpID,
                           classes = TestClasses,
                           probs   = tPredictions_logistic)

results_test 

#Visualize
ggplot(results_test, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff_t), color = 'darkgreen')

# ROC; chg boolean to 1/0
ROCobj_t <- roc(results_test$classes, results_test$actual*1)
plot(ROCobj_t)

#Improve cutoff

#Do cutoff for test data set predictions 
cutoff_t      <- 0.6
TestClasses <- ifelse(tPredictions_logistic >= cutoff_t,1, 0)

#Results data frame 
results_test <- data.frame(actual  = validate_test$readmitted_y,
                           tmpID  =  validate_test$tmpID,
                           classes = TestClasses,
                           probs   = tPredictions_logistic)

results_test 



#Visualize 2 
ggplot(results_test, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff_t), color = 'darkgreen')

# ROC; chg boolean to 1/0 - 2
ROCobj_t <- roc(results_test$classes, results_test$actual*1)
plot(ROCobj_t)

# Confusion Matrix 
(confMat <- ConfusionMatrix(results_test$classes, results_test$actual))

# Accuracy 
sum(diag(confMat)) / sum(confMat)
Accuracy(results_test$classes, results_test$actual)


#Filter for only TRUE
top_100 <- filter(results_test, actual == "TRUE")
top_100

#Sort in descending order
top_100 <- top_100[order(-top_100$probs),]
top_100

#Top 100 Patientes EDA 
#Examine table columns and data 
#Slice data to only obtain probability and ID 
top_100 <- top_100[1:100,c(2,4)]
top_100

#Join full data

top_100_EDA <- left_join(top_100, test_data, by = 'tmpID')
top_100_EDA

#Inspect overal characteristics of DataFrame 
colnames(top_100_EDA)  #List of column names
nrow(top_100_EDA)  #How many rows are in data frame?
dim(top_100_EDA)  #Dimensions of the data frame?
head(top_100_EDA)  #See the first 6 rows of data frame
str(top_100_EDA)  #See list of columns and data types (numeric, character, etc)
summary(top_100_EDA)  #Statistical summary of data

#Validate if there are any missing values 
colSums(is.na(top_100_EDA))

#Review the Feature Importance Variable to identify the most relevant variables 

# Get the variable importance for the variables 
varImp_test <- varImp(fit_test, scale = FALSE)

# Convert variable importance to a dataframe with the Top 10 variables 
# Couldn't make the code work to make the variable importance into a dataframe with 2 columns, had to hardcode
varImp_df <- data.frame(variable = c("number_inpatient", "number_outpatient", "discharge_disposition_id", "number_diagnoses","num_lab_procedures","admission_source_id","diag_1","number_emergency","num_medications","metformin_catP"),
                           importance = c(6.9932569,3.88433093,3.54301286,3.02426969,2.55022442,2.05858215,2.01292115,1.94448766,1.88301178,1.87513802))

# Print the dataframe
varImp_df$importance <- as.numeric(varImp_df$importance)
varImp_df$importance <- round(varImp_df$importance, digits = 2)
varImp_df

#Visualization
ggplot(varImp_df, aes(x=importance, y = reorder(variable, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()

#Actual EDA

#Number inpatient = Patients that stayed at the hospital during the visit
#Number outpatient = Patient that visited hospital but didn't stayed 
#Number Emergency = Patients that went to emergencies during their visit 

#Insight #1 
#Not emplpoyed in ppt as there was no trend among readmissions and lab procedures 
insight_1 <- top_100_EDA %>% group_by(probs) %>% 
  summarise(num_lab_procedures = sum(num_lab_procedures)
            ,.groups = "drop") %>%
  as.data.frame()

#Probability of Readmission by Lab Procedure
ggplot(insight_1, aes(x = num_lab_procedures, y = probs)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  labs(x = "Number of Lab Procedures", y = "Probability", 
       title = "Probability of Readmission per Lab Procedure")

#Insight #2 Metformin vs Insulin level
#Patients who consume Metformin mantain steady levels of insulin or have low insulin in body
#This helps to reduce readmissions 
insight_2 <- subset(top_100_EDA, select = c("tmpID", "probs", "insulin", "metformin"))
table_insight <- aggregate(metformin  ~ insulin , data = insight_2, FUN = length)
table_insight

#Visualization 
ggplot(table_insight, aes(x = insulin, y = metformin)) +
  geom_bar(stat = "identity", fill = 'lightblue3') + 
  labs(x = "Insulin levels", y = "Metformin use", title = "Relation between insulin levels and Metformin use")

#Insight #3 Average time in hosptial per diagnosis
#Most patients that stay the long are diabetic patients and patients with respiratory diseases 
insight_3 <- top_100_EDA %>% 
  group_by(diag_1_desc) %>% 
  summarise(avg_time_in_hospital = mean(time_in_hospital), .groups = "drop") %>%
  arrange(desc(avg_time_in_hospital)) %>%
  head(10) 
insight_3

# Create bar graph
ggplot(insight_3, aes(x = reorder(diag_1_desc, avg_time_in_hospital), y = avg_time_in_hospital)) +
  geom_bar(stat = "identity", fill = 'lightblue3') +
  xlab("Diagnosis") +
  ggtitle("Total Time in Hospital for Top Diagnoses") +
  theme_bw() + 
  coord_flip()

#Insight #4 Diagnosis 1 vs probability of readmission 
#Top ten probability don't include diabetic patiences, however pulmonary diseases occupy 4 spots. 
insight_4 <- top_100_EDA %>% 
  group_by(diag_1_desc) %>% 
  summarise(probs = mean(probs), .groups = "drop") %>%
  arrange(desc(probs)) %>%
  head(10) 
insight_4

# Create bar graph
ggplot(insight_4, aes(x = reorder(diag_1_desc, probs), y = probs)) +
  geom_bar(stat = "identity", fill = 'lightblue3') +
  xlab("Diagnosis") +
  ggtitle("Probability of Readmission per Diagnosis") +
  theme_bw() + 
  coord_flip()

#Insight 5
# Inpatient visits frequency and type of source ID 
#Most patients opt for emergency revisions, second most visits go to referred physicians. 
insight_5 <- top_100_EDA %>% 
  group_by(admission_source_id) %>% 
  summarise(number_inpatient = n(), .groups = "drop")
insight_5

ggplot(top_100_EDA, aes(x = number_inpatient)) + 
  geom_histogram(binwidth = 1, fill = 'lightblue3', color = 'white') +
  ggtitle("Number of Inpatient visits") +
  xlab("Number of Inpatient visits") +
  ylab("Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  

ggplot(insight_5, aes(x = admission_source_id, y = number_inpatient)) +
  geom_bar(stat = "identity", fill = "lightblue3", color = 'white') +
  labs(x = "Admission Source ID", y = "Number of Inpatients") +
  ggtitle("Number of Inpatients by Admission Source ID") +
  theme_minimal()+
  theme(panel.grid = element_blank())


# Save results in CSV 
write_csv(top_100,"/Users/fernandogamboa/Desktop/R Studio/Hult_R/Final_A2_Results_FGAMBOA.csv")

--------------------------------------------------------------------------------------------
#End 
