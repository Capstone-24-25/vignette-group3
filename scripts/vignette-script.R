# Commented Line-By-Line Vignette Script

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
personHHData <- left_join(PersonData, HHData) %>%
  left_join(hh_bgDensity)

# Pre-process the combined dataset `personHHData`
data <- personHHData  %>% 
  # Filter out observations for variables that have values of DK (respondents responded with Don't Know)
  # which take values of 9, 98, 998. Also filters out RF (respondent refused to answer) 
  # which takes values 9, 99, 999.
  filter(WorkDaysWk != 8 , WorkDaysWk !=9,
         TypicalHoursWk != 998, TypicalHoursWk!= 999,
         TransitTripsWk != 98, TransitTripsWk != 99,
         WalkTripsWk != 98, WalkTripsWk != 99,
         BikeTripsWk != 98, BikeTripsWk != 99) %>% 
  # Convert binary variables into factors
  mutate(HH_anyTransitRider = as.factor(HH_anyTransitRider),
         HH_homeType = as.factor(HH_homeType),
         HH_homeowner = as.factor(HH_homeowner),
         HH_isHispanic = as.factor(HH_isHispanic),
         HH_intEnglish = as.factor(HH_intEnglish),
         HH_income = as.factor(HH_income),
         Male = as.factor(Male),
         persWhite = as.factor(persWhite),
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


# Checks which columns are numeric 
numeric_columns <- sapply(data, is.numeric)

numeric_data <- data[, numeric_columns] %>% select(-CTFIP, -bg_density)

scaled_data <- scale(numeric_data)

hhid <- data$hhid
bg_group <- data$bg_group
scaled_data <- cbind(hhid, bg_group, scaled_data)

scaled_data_clean <- na.omit(scaled_data) %>% 
  as.data.frame() %>%
  # Take out the `pnum` variable to help with redudancy
  select(-pnum)

# Check which columns are numeric
numeric_columns <- sapply(data, is.numeric)

# Filter into numeric data
numeric_data <- data[, numeric_columns]
numeric_data <- numeric_data %>% select(-CTFIP, -bg_density)

# Scale the numeric data
scaled_data <- scale(numeric_data)

# Re-assign the `hhid` variable to fix an error
hhid <- data$hhid

# Make `bg_group` variable into a data frame to convert it from numeric back to categorical
bg_group <- as.data.frame(data$bg_group)

# Update the `scaled_data` object to a data frame with columns `hhid`, `bg_group`, and `scaled_data`
scaled_data <- cbind(hhid, bg_group, scaled_data) %>% 
  # Re-assign `bg_group` variable to fix an error
  mutate(bg_group = data$bg_group)

# Re-assign the `scaled_data` object into `scaled_data_clean` with final clean data
scaled_data_clean <- na.omit(scaled_data) %>% # Remove missing observations
  as.data.frame() %>% # Transform into a data frame
  # Remove outcome variable
  select(-`data$bg_group`)


# Exploratory Data Analysis

# Static Map Plot
county_bg_aggreg <- data %>% 
  group_by(County, CTFIP, bg_group) %>%  # group by `county`, `CTFIP`, and also `bg_group`
  mutate(count = n()) %>% 
  summarise_at(vars(-hhid), mean)
county_bg_shp <- county_shp %>% 
  merge(data.frame(bg_group = c("Urban", "Suburban", "Exurban", "Rural"))) %>% 
  left_join(county_bg_aggreg)
# get the CA county data
county <- ggplot2::map_data("county", region = "california")
county_bg <- merge(county, data.frame(bg_group = c("Urban", "Suburban", "Exurban", "Rural")))
county_bg_all <- county_bg_aggreg %>% 
  mutate(subregion = tolower(County)) %>% 
  full_join(county_bg, by = c("subregion", "bg_group"))
ggplot(county_bg_all) +
  geom_polygon(aes(x = long, y = lat, group = subregion, fill = Sum_PMT), colour = "white") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  facet_wrap(vars(bg_group), nrow = 2) +  # multi-panel plots using facet_wrap(), plot in 2 rows
  ggtitle("Total PMT in California at County-level") + 
  theme_void() +
  theme(legend.position="bottom")

# Sum of Trips by Residential Area Plot
urban_TripMap <-  mapview(filter(county_bg_shp, bg_group == "Urban"),
                          zcol = "Sum_Trips", legend = TRUE, popup = NULL,
                          layer.name = "Urban Trips")
suburb_TripMap <- mapview(filter(county_bg_shp, bg_group == "Suburban"),
                          zcol = "Sum_Trips", legend = TRUE, popup = NULL,
                          layer.name = "Suburban Trips")
exurb_TripMap <- mapview(filter(county_bg_shp, bg_group == "Exurban"),
                         zcol = "Sum_Trips", legend = TRUE, popup = NULL,
                         layer.name = "Exurban Trips")
rural_TripMap <- mapview(filter(county_bg_shp, bg_group == "Rural"),
                         zcol = "Sum_Trips", legend = TRUE, popup = NULL,
                         layer.name = "Rural Trips")
latticeview(urban_TripMap, suburb_TripMap, exurb_TripMap, rural_TripMap, sync = "all")


# Data Partitioning

# Set seed for reproducibility
set.seed(14531)

# Partition the data into an 80/20 split 
partitions <- scaled_data_clean %>% 
  initial_split(prop = 0.8)

# Separate id and response variable in testing data
test_dtm <- testing(partitions) %>% 
  select(-hhid, -bg_group)
test_labels <- testing(partitions) %>% 
  select(hhid, bg_group)

# Separate id and response variable in training data
train_dtm <- training(partitions) %>% 
  select(-hhid, -bg_group)
train_labels <- training(partitions) %>%
  select(hhid, bg_group)


# Principal Component Analysis 

# Set seed for reproducibility
set.seed(14531)

# Perform PCA on training data
# Ensure the training and testing data is numeric and convert to a matrix
train_features_matrix <- as.matrix(train_features)
test_features_matrix <- as.matrix(test_features)
# Convert training data matrix into a sparse matrix
train_features_sparse <- as(train_features_matrix, "sparseMatrix")
# Perform PCA on sparse training data matrix using the `sparsevd()` function
train_svd <- sparsesvd(train_features_sparse, rank = 18)
# Turn projected data into a data frame
training_projected <- as.data.frame(train_svd$u %*% diag(train_svd$d))
# Assign column names
colnames(training_projected) <- paste0("PC", 1:ncol(training_projected))

# Create explained variance plot to visualize variance explained by principal components
singular_values <- train_svd$d
variance_explained <- (singular_values^2) / sum(singular_values^2)
plot(variance_explained, type = "b", xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", main = "Scree Plot")
abline(h = 0.1, col = "red", lty = 2)

# Create cumulative variance plot to visualize variance explained by principal components
cumulative_variance <- cumsum(variance_explained)
plot(cumulative_variance, type = "b", xlab = "Principal Component",
     ylab = "Cumulative Variance Explained", main = "Cumulative Variance")

# Set the threshold for the cumulative variance (80%)
threshold <- 0.8
reduced_pcs <- which(cumulative_variance >= threshold)[1]

# Print the number of PCs to keep
cat("Number of PCs to retain:", reduced_pcs)

# Plot PC1 vs PC2
plot(training_projected$PC1, training_projected$PC2, xlab = "PC1", ylab = "PC2",
     main = "PCA - PC1 vs PC2", pch = 19, col = "blue")

# Project testing data onto training PCA
# Set seed for reproducibility
set.seed(14531)
# Define reprojection function `reproject_fn`
reproject_fn <- function(.dtm, train_projected) {
  .dtm_sparse <- as(.dtm, "sparseMatrix")
  test_projected <- as.matrix(.dtm_sparse %*% train_projected$v %*% diag(1 / train_projected$d))
  colnames(test_projected) <- paste0("PC", 1:ncol(test_projected))
  return(test_projected)
}
# Project the testing matrix onto `training_svd`
test_projected <- reproject_fn(test_features_matrix, train_svd)


# Logistic Regression





# Random Forest

# Look at number of missing data per column
colSums(is.na(data))

# Will see that there is a lot of missing data for DisLicensePlt, SchoolMode, and WorkMode
# We will impute these missing values with 'Not Disabled', 'Not in School', and 'Not Working' respectively


# Set seed for reproducibility
set.seed(14531)



# We will preprocess the original dataset differently for random forest, so we will clean the original dataset now
rf_data <- data %>% 
  select(-County, -CTFIP, -MPO, -City, -bg_density) %>% 
  mutate(DisLicensePlt = as.factor(ifelse(is.na(DisLicensePlt), 'Not Disabled', DisLicensePlt)),
         SchoolMode = as.factor(ifelse(is.na(SchoolMode), 'Not in School', SchoolMode)),
         WorkMode = as.factor(ifelse(is.na(WorkMode), 'Not Working', SchoolMode))) %>% 
  na.omit()

# Split the dataset with 80/20 split
partitions <- initial_split(rf_data, prop = 0.8)

# Create training and testing datasets
train_data <- training(partitions)
test_data <- testing(partitions)

# Specify the random forest model for classification 
# Set-up tuning for mtry, trees, and min_n parameters
rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")

# Define a workflow, add model
# add formula with bg_group as response variable and using all other variables as predictors
rf_workflow <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_formula(bg_group ~ .)

# Set up cross-validation with 5 folds 
# stratify by bg_group to make sure all levels of bg_group are included in each fold
train_folds <- vfold_cv(train_data, v = 5, strata = bg_group)

# Set up a grid to test different parameter combinations
rf_grid <- grid_regular(mtry(range = c(5, 30)),
                        #number of variables randomly selected at each split
                        trees(range = c(100, 500)), 
                        #number of trees in the forest
                        min_n(range = c(5, 10)),
                        #minimum number of observations in each node
                        levels = 5)

# Tune the model with the training folds and the grid
tune_results <- tune_grid(
  rf_workflow,
  resamples = train_folds,
  grid = rf_grid
)

# Extract the best parameters for the model using accuracy as the metric
best_params <- select_best(tune_results, metric = "accuracy")
print(best_params)

# Finalize the workflow with the best parameters
final_rf <- finalize_workflow(rf_workflow, best_params)

# Train the final model on the entire training data
final_rf_fit <- fit(final_rf, data = train_data)

# Get predictions on the test data 
# combine with the original data to see how well the model performs
predictions <- predict(final_rf_fit, test_data) %>% bind_cols(test_data)

#RF Model Evaluation
# Use confusion matrix to see how well the model classified the different household densities
confusion_matrix <- predictions %>% 
  conf_mat(truth = bg_group, estimate = .pred_class)
print(confusion_matrix)

# Calculate accuracy metric
accuracy <- predictions %>% 
  metrics(truth = bg_group, estimate = .pred_class) %>% 
  filter(.metric == "accuracy")
print(accuracy)

# Plot variable importance to see the top 10 variables that had the most impact on the model
rf_model %>% 
  vip() +
  theme_minimal()
