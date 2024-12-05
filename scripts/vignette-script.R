# Commented Vignette Script

# Loading necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(sparsesvd)
library(nnet)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(leafsync)
library(maps)
library(nnet)
library(randomForest)
library(vip)
library(SparseM)
library(Matrix)

# Read in datasets
PersonData <- read_rds('./Data/raw/PersonData_111A.Rds')
HHData <- read_rds('./Data/raw/HHData_111A.Rds')
hh_bgDensity <- read_rds('./Data/raw/hh_bgDensity.Rds')
county_shp <- st_read("./Data/raw/counties/counties.shp")

# Merge datasets
personHHData <- left_join(PersonData, HHData) %>% left_join(hh_bgDensity)

# Preprocess data
data <- personHHData  %>% 
  # filter out observations for variables that have values of DK (respondents responded with Don't Know)
  # which take values of 9, 98, 998 or RF (respondent refused to answer) which takes values 9, 99, 999
  filter(WorkDaysWk != 8 , WorkDaysWk !=9,
         TypicalHoursWk != 998, TypicalHoursWk!= 999,
         TransitTripsWk != 98, TransitTripsWk != 99,
         WalkTripsWk != 98, WalkTripsWk != 99,
         BikeTripsWk != 98, BikeTripsWk != 99) %>% 
  # convert variables represented by 0 or 1 into factors 
  mutate(HH_anyTransitRider = as.factor(HH_anyTransitRider),
         HH_homeType = as.factor(HH_homeType),
         HH_homeowner = as.factor(HH_homeowner),
         HH_isHispanic = as.factor(HH_isHispanic),
         HH_intEnglish = as.factor(HH_intEnglish),
         HH_income = as.factor(HH_income),
         Male = as.factor(Male),
         persHisp = as.factor(persHisp),
         persAfricanAm = as.factor(persAfricanAm),
         persNativeAm = as.factor(persNativeAm),
         persAsian = as.factor(persAsian),
         persPacIsl = as.factor(persPacIsl),
         persOthr = as.factor(persOthr),
         persDKrace = as.factor(persDKrace),
         persRFrace = as.factor(persRFrace),
         bornUSA = as.factor(bornUSA),
         DriverLic = as.factor(DriverLic),
         TransitPass = as.factor(TransitPass),
         Employed = as.factor(Employed),
         WorkFixedLoc = as.factor(WorkFixedLoc),
         WorkHome =as.factor(WorkHome),
         WorkNonfixed = as.factor(WorkNonfixed),
         FlexSched = as.factor(FlexSched),
         FlexPrograms = as.factor(FlexPrograms),
         Disability = as.factor(Disability),
         DisLicensePlt = as.factor(DisLicensePlt),
         Student = as.factor(Student),
         workday = as.factor(workday),
         hhid = paste(hhid, pnum),
         SchoolMode = as.factor(SchoolMode),
         WorkMode = as.factor(WorkMode),
         EducationCompl = as.factor(EducationCompl))

# checks which columns are numeric 
numeric_columns <- sapply(data, is.numeric)

numeric_data <- data[, numeric_columns]

numeric_data <- numeric_data %>% select(-CTFIP, -bg_density)

scaled_data <- scale(numeric_data)

hhid <- data$hhid
bg_group <- data$bg_group
scaled_data <- cbind(hhid, bg_group, scaled_data)

scaled_data_clean <- na.omit(scaled_data) %>% 
  as.data.frame() %>% 
  select(-pnum)




# Loading necessary packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(sparsesvd)
library(nnet)

#Read in datasets
PersonData <- read_rds('../data/PersonData_111A.Rds')
HHData <- read_rds('../data/HHData_111A.Rds')
hh_bgDensity <- read_rds('../data/hh_bgDensity.Rds')

# Merge datasets
personHHData <- left_join(PersonData, HHData) %>%
  left_join(hh_bgDensity)

# Determine which columns are numeric
numeric_columns <- sapply(personHHData, is.numeric)

# Select only numeric variables
numeric_data <- personHHData[, numeric_columns]

# Remove county FIP code, household id, and bg_density (identification and response variables)
numeric_data <- numeric_data %>% select(-CTFIP, -hhid, -bg_density)

# Standardize data
scaled_data <- scale(numeric_data)

# Add back in household id column and bg_group
hhid <- personHHData$hhid
bg_group <- as.factor(personHHData$bg_group)
scaled_data <- cbind(hhid, bg_group, scaled_data)

# Remove rows with NA values
scaled_data_clean <- na.omit(scaled_data) %>% 
  as.data.frame()


# Partition data
set.seed(14531)
partitions <- scaled_data_clean %>% 
  initial_split(prop = 0.8)

# Separate id and response variable in testing and training data
test_dtm <- testing(partitions) %>% 
  select(-hhid, -bg_group)
test_labels <- testing(partitions) %>% 
  select(hhid, bg_group)

train_dtm <- training(partitions) %>% 
  select(-hhid, -bg_group)
train_labels <- training(partitions) %>%
  select(hhid, bg_group)


#Convert training data to sparse matrix to use sparsesvd function to perform PCA
train_dtm_sparse <- as.matrix(train_dtm) %>% 
  as("sparseMatrix")

#Perform PCA on sparse training data matrix and turn into dataframe
train_svd <- sparsesvd(train_dtm_sparse, rank = 18)
training_projected <- as.data.frame(train_svd$u %*% diag(train_svd$d))

#assign column names
colnames(training_projected) <- paste0("PC", 1:ncol(training_projected))

#function to reproject new data onto training PCA
reproject_fn <- function(.dtm, train_projected) {
  .dtm_sparse <- as(.dtm, "sparseMatrix")
  test_projected <- as.matrix(.dtm_sparse %*% train_projected$v %*% diag(1 / train_projected$d))
  colnames(test_projected) <- paste0("PC", 1:ncol(test_projected))
  return(test_projected)
}

#project test data onto training PCA
test_projected <- reproject_fn(test_dtm, train_svd)

#explained variance plot
singular_values <- train_svd$d
variance_explained <- (singular_values^2) / sum(singular_values^2)

plot(variance_explained, type = "b", xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", main = "Scree Plot")
abline(h = 0.1, col = "red", lty = 2)

#cumulative variance plot
cumulative_variance <- cumsum(variance_explained)

plot(cumulative_variance, type = "b", xlab = "Principal Component",
     ylab = "Cumulative Variance Explained", main = "Cumulative Variance")

# set the threshold for the cumulative variance (80%)
threshold <- 0.8
reduced_pcs <- which(cumulative_variance >= threshold)[1]

# print the number of PCs to keep
cat("Number of PCs to retain:", reduced_pcs)

# Plot PC1 vs PC2
plot(training_projected$PC1, training_projected$PC2, xlab = "PC1", ylab = "PC2",
     main = "PCA - PC1 vs PC2", pch = 19, col = "blue")


# Add the categorical variable to the PC scores
plot_pca_train <- cbind(training_projected, train_labels)  # Replace with actual variable

# Plot with ggplot2
library(ggplot2)
ggplot(plot_pca_train, aes(x = PC1, y = PC2, color = as.factor(bg_group))) +
  geom_point() +
  labs(title = "PCA: PC1 vs PC2 by bg_group",
       x = "PC1", y = "PC2") +
  theme_minimal()+
  scale_color_manual(values = c('green', 'blue', 'red', 'yellow'))

## Logistic Regression

# Get the reduced PCA data that you will feed into logistic regression model
training_projected_reduced <- training_projected[, 1:reduced_pcs]
test_projected_reduced <- test_projected[, 1:reduced_pcs]

reduced_training <- cbind(training_projected_reduced, bg_group = train_labels$bg_group)

reduced_testing <- cbind(as.data.frame(test_projected_reduced), bg_group = test_labels$bg_group)

# Fit logistic regression model with the PCA reduced training data


library(nnet)
log_regmodel <- multinom(bg_group ~ ., data = reduced_training)

# Let's see how well our model was able to classify the different household densities

logreg_test_predictions <- predict(multinom_model, newdata = reduced_testing)

## Saving data
