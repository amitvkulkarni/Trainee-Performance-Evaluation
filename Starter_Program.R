library(dplyr)
library(tidyr)
library(caret)
library(tidymodels)
library(factoextra)
library(h2o)
library(corrplot)


h2o.init()
# Loading the raw data
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

delta <- train_data %>% 
  anti_join(test_data, by = "trainee_id")


train_data <- train_data %>% 
  anti_join(delta, by = "trainee_id")



print("Initiating Data Preparation")
# Impute missing values in age with mean in both train and test data ###################################################
train_data <- train_data %>% 
  mutate(age = replace(x = age, is.na(age), mean(age, na.rm = TRUE)))

test_data <- test_data %>% 
  mutate(age = replace(x = age, is.na(age), mean(age, na.rm = TRUE)))


train_data <- train_data %>% 
  mutate(trainee_engagement_rating = replace(x = trainee_engagement_rating, is.na(trainee_engagement_rating),
                                             median(trainee_engagement_rating, na.rm = TRUE)))

test_data <- test_data %>% 
  mutate(trainee_engagement_rating = replace(x = trainee_engagement_rating, is.na(trainee_engagement_rating), 
                                             median(trainee_engagement_rating, na.rm = TRUE)))

print("Missing data imputation is completed")
# Impute missing values in age with mean in both train and test data ###################################################

print("Initiating New feature creation")
# New feature creation in both train and test ###################################################

train_data <- train_data %>% 
  mutate(ratingPerProgram = trainee_engagement_rating/total_programs_enrolled)

test_data <- test_data %>% 
  mutate(ratingPerProgram = trainee_engagement_rating/total_programs_enrolled)

train_data <- train_data %>% 
  group_by(trainee_id) %>% 
  mutate(attempts = n())


test_data <- test_data %>% 
  group_by(trainee_id) %>% 
  mutate(attempts = n())

train_data <- train_data %>% 
  group_by(education) %>% 
  mutate(per_education = n()/nrow(train_data))

test_data <- test_data %>% 
  group_by(education) %>% 
  mutate(per_education = n()/nrow(test_data))


train_data <- train_data %>% 
  group_by(total_programs_enrolled) %>% 
  mutate(per_enrolled = n()/nrow(train_data))

test_data <- test_data %>% 
  group_by(total_programs_enrolled) %>% 
  mutate(per_enrolled = n()/nrow(test_data))


train_data <- train_data %>% 
  group_by(difficulty_level) %>% 
  mutate(per_diff = n()/nrow(train_data))

test_data <- test_data %>% 
  group_by(difficulty_level) %>% 
  mutate(per_diff = n()/nrow(test_data))



train_data <- train_data %>% 
  group_by(program_duration) %>% 
  mutate(per_duration = n()/nrow(train_data))

test_data <- test_data %>% 
  group_by(program_duration) %>% 
  mutate(per_duration = n()/nrow(test_data))


train_data <- train_data %>% 
  group_by(trainee_engagement_rating) %>% 
  mutate(per_rating = n()/nrow(train_data))


test_data <- test_data %>% 
  group_by(trainee_engagement_rating) %>% 
  mutate(per_rating = n()/nrow(test_data))


train_data <- train_data %>% 
  group_by(program_type) %>% 
  mutate(per_programType = n()/nrow(train_data))

test_data <- test_data %>% 
  group_by(program_type) %>% 
  mutate(per_programType = n()/nrow(test_data))


train_data <- train_data %>% 
  group_by(program_id) %>% 
  mutate(per_programID = n()/nrow(train_data))



test_data <- test_data %>% 
  group_by(program_id) %>% 
  mutate(per_programID = n()/nrow(test_data))


train_data <- train_data %>% 
  mutate(total_learnin_hrs = program_duration * total_programs_enrolled)

test_data <- test_data %>% 
  mutate(total_learnin_hrs = program_duration * total_programs_enrolled)


train_data <- train_data %>% 
  group_by(education, program_type, gender) %>% 
  mutate(edu_type_gender = n())

test_data <- test_data %>% 
  group_by(education, program_type, gender) %>% 
  mutate(edu_type_gender = n())



train_data <- train_data %>%
  group_by(education, program_duration, gender) %>%
  mutate(edu_duration_gender = n()) %>%
  mutate(per_edu_duration_gender = n()/sum(edu_duration_gender))

test_data <- test_data %>%
  group_by(education, program_duration, gender) %>%
  mutate(edu_duration_gender = n()) %>%
  mutate(per_edu_duration_gender = n()/sum(edu_duration_gender))


train_data <- train_data %>%
  group_by(city_tier, education, total_programs_enrolled) %>%
  mutate(city_edu_enrolled = n())

test_data <- test_data %>%
  group_by(city_tier, education, total_programs_enrolled) %>%
  mutate(city_edu_enrolled = n())

train_data <- train_data %>%
  group_by(education, difficulty_level) %>%
  mutate(edu_diff = n())

test_data <- test_data %>%
  group_by(education, difficulty_level) %>%
  mutate(edu_diff = n())


train_data <- train_data %>% 
  group_by(trainee_id) %>% 
  mutate(numberoftest = n()) %>% 
  group_by(is_pass, trainee_id) %>% 
  mutate(per_pass = n()/numberoftest)

pass_mapping <- train_data %>% 
  select(trainee_id, per_pass,numberoftest, is_pass) %>% 
  filter(is_pass == "1")

pass_mapping <- unique(pass_mapping, by = "trainee_id")

test_data <- merge(test_data,pass_mapping, by.x = "trainee_id", by.y = "trainee_id", all.x = T, all.y = F)
test_data <- test_data[, -c(32:33)]

# test_data <- test_data %>% 
#   rename("per_pass" = "per_pass.x")

plot(train_data$per_pass,train_data$is_pass)
plot(train_data$numberoftest,train_data$per_pass)

########################################################################################################################
print("Feature creation completed")

hotels_df <- train_data %>%
  # select("trainee_id","is_pass", "age","total_programs_enrolled", 
  #        "per_enrolled","per_diff","per_rating","per_programType","edu_duration_gender",
  #        "difficulty_level_hard","difficulty_level_hard","difficulty_level_intermediate",
  #        "difficulty_level_intermediate","city_tier_X2","city_tier_X3","education_High.School.Diploma",
  #        "education_Masters","education_Matriculation","education_No.Qualification") %>%
  select("trainee_id","is_handicapped","city_tier","is_pass", "age","education","program_duration",
         "ratingPerProgram","trainee_engagement_rating","total_programs_enrolled", "test_id","attempts",
         "per_education","per_enrolled","per_diff","per_duration","per_rating","per_programType","per_programID",
         "total_learnin_hrs","edu_type_gender","edu_duration_gender","per_edu_duration_gender","city_edu_enrolled",
         "edu_diff","per_pass") %>%
  mutate_if(is.character, factor)

#str(hotels_df)
hotels_df$is_pass <- as.factor(hotels_df$is_pass)
hotels_df$city_tier <- as.factor(hotels_df$city_tier)
#hotels_df$test_id <- as.factor(hotels_df$test_id)


# Build receipe and prep the data for modelling
hotel_rec <- recipe(is_pass ~ ., data = hotels_df) %>%
  step_downsample(is_pass,ratio = 1.7) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(age, program_duration, attempts,per_education,per_enrolled,per_diff,per_duration,per_rating,
                 per_programType,per_programID,total_learnin_hrs,
                 city_edu_enrolled,edu_diff, 
                 edu_type_gender,edu_duration_gender,per_edu_duration_gender,per_pass) %>%
  prep()

print("Recipe creation completed")
print("Down sample | dummified | Normalized --> Completed")


hotel_rec_juice <- juice(hotel_rec)
hotel_rec_juice_mat <- as.matrix(hotel_rec_juice[,-which(names(hotel_rec_juice) %in% c("is_pass"))])
print("Variables for model building before filteration")
names(hotel_rec_juice)


# Find highly corelated variables and remove them from dataframe
#hotel_rec_juice <- fnRemoveCorrelatedVars(hotel_rec_juice_mat,hotel_rec_juice)

print("Variables after highly correlated variabled are removed")
names(hotel_rec_juice)

# Variable selection based on GLM with pvalue <0.5

# impVariables <- fnVarImp_glm(hotel_rec_juice)
# hotel_rec_juice <- hotel_rec_juice[,impVariables]
# print("Variables that are significant pavlue <0.05")
# names(hotel_rec_juice)

# split into training and testing
set.seed(23489)
train_index <- sample(1:nrow(hotel_rec_juice), 0.9 * nrow(hotel_rec_juice))
case_train <- hotel_rec_juice[train_index, ]
case_test <- hotel_rec_juice[-train_index, ]

case_train.hex <- case_train
case_train.hex <- as.h2o(case_train.hex)

case_test.hex <- case_test
case_test.hex <- as.h2o(case_test.hex)

table(case_train$is_pass)
print("Initiating PCA model build")  

case_train_mat <- as.matrix(case_train[,-which(names(case_train) %in% c("is_pass"))])
model_pca <- prcomp(case_train_mat, scale. = T)
summary(model_pca)

print("Plotting scree plot")
#fviz_eig(model_pca)

case_train_component <- data.frame(case_train$is_pass, model_pca$x)
case_train_component <- case_train_component[,1:ncol(case_train_component)]

case_train_component <- case_train_component %>% 
  rename("is_pass" = "case_train.is_pass")

# Test data
case_test_mat <- predict(model_pca, newdata = case_test)
case_test_component <- data.frame(case_test$is_pass, case_test_mat)
case_test_component <- case_test_component[,1:ncol(case_test_component)]

case_test_component <- case_test_component %>% 
  rename("is_pass" = "case_test.is_pass")


test_bake <- bake(hotel_rec, new_data = test_data)
test_bake.hex <- test_bake
test_bake.hex <- as.h2o(test_bake.hex)

test_bake_component <- predict(model_pca, newdata = test_bake)  
test_bake_component <- test_bake_component[,1:ncol(test_bake_component)]


case_train_component.hex <- as.h2o(case_train_component)
case_test_component.hex <- as.h2o(case_test_component)

case_test_bake.hex <- as.h2o(test_bake_component)

x <- names(case_train[,-which(names(case_train) %in% c("is_pass"))])
x_pca <- names(case_train_component[,-which(names(case_train_component) %in% c("is_pass"))])
y <- "is_pass"


fnXGBoost <- function(x,y, training_frame, validation_frame, test_frame){
  
  print("Initiating XGBoost build --> Started")
  Model_xgb <- h2o.xgboost(x = x, y = y,
                           training_frame = training_frame, 
                           validation_frame = validation_frame,
                           distribution = "bernoulli",
                           booster = "gbtree", 
                           normalize_type = "tree",
                           keep_cross_validation_predictions = TRUE,
                           fold_assignment = "Modulo",
                           nfolds = 10,
                           learn_rate = 0.3,
                           seed = 1234)
  
  print("Initiating XGBoost build --> Completed")
  print(h2o.performance(Model_xgb))
  
  
  yhat_submissio <- h2o.predict(Model_xgb, test_frame)
  
  pred_sub <- as.data.frame(yhat_submissio$predict)
  id_sub <- test_data$id 
  
  sub_df <- cbind(id_sub, pred_sub)
  sub_df <- sub_df %>% 
    rename("id" = "id_sub", "is_pass" = "predict")
  
  print(nrow(sub_df))
  table(sub_df$is_pass)
  per_sub_df <- sub_df %>% 
    group_by(is_pass) %>% 
    summarise(cnt = n()) %>% 
    mutate(per = cnt / sum(cnt)*100)
  
  print(per_sub_df)
  write.csv(sub_df, "submission_PCA_XGBoost.csv")
  #print("Date written to file --> Completed")
  
}


################################## Model Builds ######################################################

fnXGBoost(x,y,case_train.hex,case_test.hex, test_bake.hex)

################################## Model Builds ######################################################
