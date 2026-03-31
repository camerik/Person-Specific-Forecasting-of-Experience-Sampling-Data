# --------------------------------------------------------------------------------------------------------------
#------------------------------- Initialization ---------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
# Initialize renv, load functions and set seed for reproducability

# Install and load required packages
packages <- c("dplyr", "tidyr", "zoo", "imputeTS", "purrr", "Metrics", "ggplot2", "fpp3", "fable", "glmnet", "coin",
              "openesm", "quantregForest", "kableExtra", "xtable", "boot", "ggdist")
lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})

create_renv <- FALSE
if (create_renv) {
  renv::clean()
  renv::project()
  renv::snapshot(force = T)
  renv::dependencies()
  renv::status()
  readLines(".renvignore")
  renv::restore()
}

# Ensure reproducability
random_seed <- 47
set.seed(random_seed)

# Load file with additional, outsourced functions
functions_file_path <- "./functions_MLM.R"
#functions_file_path <- "git-ordner/Person-Specific-Forecasting-of-Experience-Sampling-Data/functions_MLM.R"
source(functions_file_path)

# Define output path and create output dir
figures_output_dir <- "./figures/"
#figures_output_dir <- "/Users/cameri/Desktop/Psychologie-Master/Masterarbeit/Masterarbeit2.0/Latex-Code/Figures/"
dir.create(figures_output_dir, showWarnings = FALSE)

# Load dataset from openesm or from RData file
data_path <- "./raw_data.RData"
load_from_file <- TRUE
if (load_from_file) {
  load(data_path)
} else {
  raw_data = openesm::get_dataset("0008_westhoff")$data
}






# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------- Preprocessing ---------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# Check initial number of ids
n_id_1_initial = length(unique(raw_data$id))

# Drop columns not required for forecasting
cols_to_drop <- c("scheduled_time", "response_time",
                  "location_latitude", "location_longitude", "start_date", 
                  "end_date", "duration_in_seconds", "finished", "sleep_duration") 
raw_data <- raw_data %>% 
  select(-all_of(cols_to_drop))

# Transform weekday to numerical variable, impute NAs and transform to 
# cyclic coordinates to ensure proper relations (Sunday as close to Monday as Tuesday)
# Source: Back and Jackson (2022)
weekday_map <- c("Monday"=0,"Tuesday"=1,"Wednesday"=2,"Thursday"=3,"Friday"=4,"Saturday"=5,"Sunday"=6)
raw_data <- raw_data %>%
  mutate(weekday = weekday_map[weekday]) %>% # Numeric Encoding
  mutate(weekday = zoo::na.locf(weekday, na.rm = FALSE, fromLast = TRUE)) %>% # Impute Missing Days
  mutate( # Sine and Cosine encoding
    sin_weekday = sin(weekday / 7 * 2 * pi),
    cos_weekday = cos(weekday / 7 * 2 * pi)
  ) %>%
  select(-weekday) # Drop the original weekday column

# CAVE: daily variable file for NAs per day fill missings with values of sleep quality of that day
raw_data <- raw_data %>%
  group_by(id, day) %>%                      
  mutate(across(
    all_of(c("sleep_quality")),
    ~ if (any(!is.na(.x))) {
      rep(first(na.omit(.x)), length(.x))
    } else {
      .x 
    }
  )) %>%
  ungroup()

# Different categorizations of item/feature names in vectors (e.g., feature names is all colnames 
# except id and counter)
feature_names = setdiff(colnames(raw_data), c("id", "counter"))
beep_feature_names = setdiff(colnames(raw_data), c("id", "counter", "sin_weekday", "cos_weekday", "sleep_quality", "day"))
daily_feature_names = c("sin_weekday", "cos_weekday", "sleep_quality", "day")
features_to_lag = setdiff(colnames(raw_data), c("id", "counter", "sin_weekday", "cos_weekday", "day", "beep", "sleep_quality"))



# ---------------------------- Low Variance -----------------------------------------------------------------------------------
# Check for and exclude ids with low variance: 
# Low Variance Definition: Either variance < 1 or ≥ 10 unique answer categories
def_low_var <- "one"
if (def_low_var == "ten_unique") {
  # Number of unique answer categories per id per item
  unique_counts <- raw_data %>%
    group_by(id) %>%
    summarise(across(all_of(beep_feature_names), ~ n_distinct(.)), .groups = "drop")
  
  # Keep only ids with count ≥ 10 in every item/feature 
  ids_var <- unique_counts %>%
    filter(if_all(everything(), ~ . >= 10)) %>%
    pull(id)
  raw_data <- raw_data %>% 
    filter(id %in% ids_var)
  
} else if (def_low_var == "one") {
  # Variance per feature
  var_features <- raw_data %>%
    group_by(id) %>%
    summarise(across(all_of(beep_feature_names), ~ sd(., na.rm = T)), groups = "drop") 
  
  # Keep only ids with variance >= 1 in every item/feature 
  ids_var <- var_features %>%
    filter(if_all(everything(), ~ . >= 1)) %>%
    pull(id)
  raw_data <- raw_data %>%
    filter(id %in% ids_var)
  
} else {
  print("Cutoff not implemented!")
}

# Check: how many ids were excluded? (9 for "one" or 38 for "ten_unique")
n_id_2_var_check = length(unique(raw_data$id)) # excludes 38 participants



# --- MISSINGNESS ----------------------------------------------------------------------------------------------------
# Pivot to long format
raw_data_long = raw_data %>%
  pivot_longer(cols = all_of(feature_names), names_to = "item", values_to = "value")

# Compute n missing rows per ID  
raw_data_long_missing_rows <- raw_data %>%
  mutate(
    row_missing = if_else(
      if_all(setdiff(beep_feature_names, "beep"), is.na), TRUE, FALSE
    )
  )

# Compute consecutive missing rows per id and exclude ids with more than 5 consecutive missing rows (one whole day)
cutoff_cons_miss = 5
consecutive_missing <- raw_data_long_missing_rows %>%
  arrange(id, counter) %>%
  group_by(id) %>%
  summarise(
    max_consec_missing = {
      r <- rle(row_missing)
      if (any(r$values)) max(r$lengths[r$values]) else 0
    },
    .groups = "drop"
  )

ggplot(consecutive_missing, aes(x = max_consec_missing)) +
  geom_bar() +
  geom_vline(xintercept = cutoff_cons_miss, color = "red", linewidth = 1) +
  labs(
    x = "Max Consecutive Missing Rows per ID",
    y = "Number of Participants",
    title = "Distribution of Consecutive Missing Rows"
  ) +
  theme_minimal()

valid_ids_consec <- consecutive_missing %>%
  filter(max_consec_missing <= cutoff_cons_miss) %>%
  pull(id)

# Filter both wide and long datarames
raw_data_long <- raw_data_long %>%
  filter(id %in% valid_ids_consec)
raw_data <- raw_data %>%
  filter(id %in% valid_ids_consec)

n_id_3_cons_missings = length(unique(raw_data_long$id)) # Excludes 2 participants



# ------------------------Holdout Splits ---------------------------------------------------------------------------------
# Split dataset into Train, Validation (for HPO) and Test-Dataset while considering temporal order
n_obs <- length(unique(raw_data$counter))
n_val <- 15 # 3 days
n_test <- 15 # 3 days 
n_train <- n_obs - (n_val + n_test)
train_counters <- seq(1, n_train, 1)
val_counters <- seq(n_train + 1, n_train + n_val, 1)
train_val_counters <- union(train_counters, val_counters)
test_counters <- seq(n_train + n_val + 1, n_obs, 1)

# Remove IDs with NAs in test data (important for model evaluation)
# This is required due to the lagged feature structure and our model's
# inability to handle NAs
ids_with_nas <- raw_data_long %>%
  filter(counter %in% test_counters) %>%
  group_by(id) %>%
  summarise(has_na = any(is.na(value))) %>%
  filter(has_na) %>%
  pull(id)

raw_data_long <- raw_data_long %>% 
  filter(!(id %in% ids_with_nas))
raw_data <- raw_data %>% 
  filter(!(id %in% ids_with_nas))
n_id_4_test_na = length(unique(raw_data_long$id)) # Removes 66 participants

# Check for IDs with missings in val data (important for HPO)
ids_with_nas_val <- raw_data_long %>%
  filter(counter %in% val_counters) %>%
  group_by(id) %>% 
  summarise(has_na = any(is.na(value))) %>%
  filter(has_na) %>%
  pull(id)

# Remove IDs with missings in val data
raw_data_long <- raw_data_long %>%
  filter (!( id %in% ids_with_nas_val))
raw_data <- raw_data %>% 
  filter(!(id %in% ids_with_nas_val))
n_id_5_val_na = length(unique(raw_data_long$id)) # removes 15 participants



# ---------------------Define Target Item and Relevant Cols-------------------------------------------------
id_col <- "id"
time_col <- "counter"
target_item <- "depressed"



# --------------------- Descriptive statistics---------------------------------------------------------------
# Average time series length before interpolation 
ts_length <- raw_data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    n_timepoints = sum(!is.na(.data[[target_item]])),
    .groups = "drop"
  )
n_surveys <- sum(ts_length$n_timepoints)
mean_ts_length <- mean(ts_length$n_timepoints)
sd_ts_length <- sd(ts_length$n_timepoints)
range_ts_length <- range(ts_length$n_timepoints)

# Range of answer categories (all items have the same possible answer categories 0-100)
range_answer_cat =  raw_data %>%
  dplyr::select(all_of(features_to_lag)) %>%
  unlist() %>%
  range(na.rm = TRUE)

# Calculate in-person statistics and plot counts of answers per item over all ids (Westhoff et al., 2024)
data_statistics <- raw_data_long %>%
  dplyr::group_by(id, item) %>%
  dplyr::summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
for (id_i in unique(raw_data_long$id)) {
  print(raw_data_long %>%
    dplyr::filter(., id == id_i) %>%
    dplyr::filter(item %in% features_to_lag) %>%
    ggplot(aes(x = value)) +
    ggtitle(paste("ID:",id_i)) +
    geom_histogram(bins = 20, na.rm = TRUE) +
    facet_wrap(~ item) +
    theme_classic())
}



#------------------------------- Prepare Data for HPO --------------------------------------------------
# Interpolate
interpolation_type = "Kalman"
data_long_hpo <- interpolate(raw_data_long, train_counters, interpolation_type)

# Plot imputed values for example IDS
example_ids <- c(72425, 73479, 72291)
plot_interpolated_examples(example_ids, raw_data_long, data_long_hpo, target_item)

# Check if there are any NAs left
sum(is.na(data_long_hpo %>% filter(counter %in% train_counters)))

# Source for AR(1)-DF-Test and Stationarity Transformations:
# Ryan et al. (2025) (adf_flow in diagnose_trend_type)
# Compute detrending components to detrend data
trend_parameter_hpo <- diagnose_trend_type(data_long_hpo %>% filter(counter %in% train_counters))

# Diff and detrend whole dataset
data_long_hpo_dd <- diff_and_detrend(data_long_hpo, trend_parameter_hpo)

# Compute mean and sd for scaling data
std_stats_hpo = data_long_hpo_dd %>% 
  filter(counter %in% train_counters) %>%
  group_by(id, item) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Scale Data
data_long_hpo_dd_std <- data_long_hpo_dd %>%
  left_join(std_stats_hpo, by = c("id", "item")) %>%
  mutate(value = (value - mean_value) / sd_value) %>%
  dplyr::select(-mean_value, -sd_value)

# Create a data frame for HPO (wide format)
df_hpo <- data_long_hpo_dd_std %>%
  pivot_wider(names_from = item, values_from = value) %>%
  arrange(id, counter)

# Test if data is standardized correctly
mean(data_long_hpo_dd_std %>% 
       filter(id == unique(data_long_hpo_dd_std$id)[1], item == target_item, counter %in% train_counters) %>% 
       dplyr::pull(value))



#-------------------------------- Prepare Data for Eval ------------------------------------------------------
# Interpolate
data_long_eval <- interpolate(raw_data_long, train_val_counters, interpolation_type)

# Check if there are any NAs left
sum(is.na(data_long_eval %>% filter(counter %in% train_val_counters)))

# Remove IDs with missings in val data
data_long_eval <- data_long_eval %>%
  filter (!(id %in% ids_with_nas_val)) 

# Compute detrending components to detrend data
trend_parameter_eval <- diagnose_trend_type(data_long_eval %>% filter(counter %in% train_val_counters))

# Diff and detrend whole dataset
data_long_eval_dd <- diff_and_detrend(data_long_eval, trend_parameter_eval)

# Compute mean and sd for scaling data
std_stats_eval = data_long_eval_dd %>% 
  filter(counter %in% train_val_counters) %>%
  group_by(id, item) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Scale Data
data_long_eval_dd_std <- data_long_eval_dd %>%
  left_join(std_stats_eval, by = c("id", "item")) %>%
  mutate(value = (value - mean_value) / sd_value) %>%
  dplyr::select(-mean_value, -sd_value)

# Create a data frame for HPO (wide format)
df_eval <- data_long_eval_dd_std %>%
  pivot_wider(names_from = item, values_from = value) %>%
  arrange(id, counter)

# Test if data is standardized correctly
mean(data_long_eval_dd_std %>% 
       filter(id == unique(data_long_eval_dd_std$id)[1], item == target_item, counter %in% train_val_counters) %>% 
       dplyr::pull(value))

# Extract the raw test data to compare to predictions
y_train_raw <- data_long_hpo %>% 
  filter(item == target_item, counter %in% train_val_counters)
y_test_raw <- data_long_hpo %>% 
  filter(item == target_item, counter %in% c(val_counters, test_counters))

# Compute variance in target item per id for trainval and for test split
outcome_SD_train <- y_train_raw %>%
  group_by(id) %>%
  summarise(SD = sd(value, na.rm = TRUE))
outcome_SD_test <- y_test_raw %>%
  filter(counter %in% test_counters) %>%
  group_by(id) %>%
  summarise(SD = sd(value, na.rm = TRUE))

print(paste("Number of participants after preprocessing:",length(unique(data_long_eval$id))))





#-------------------------------------------------------------------------------------------------------------
#--------------------------------------------------- AR(1) Model ---------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# The AR-Baseline model does only require the target as a predictor
df_ar <- data_long_eval_dd_std  %>% 
  dplyr::filter(item == target_item) %>% 
  as_tsibble(key = c(id, item), index = counter)

# Store results
test_predictions_ar <- tibble()
train_predictions_ar <- tibble()
test_metrics_ar <- tibble()
train_metrics_ar <- tibble()

# Fit AR(1) model per ID, store forecasts and PIs
for (id_i in unique(df_hpo$id)) {
  df_id <- df_ar %>% 
    dplyr::filter(id == id_i)
  
  # Train set
  df_train <- df_ar %>%
    dplyr::filter(counter %in% train_val_counters, id == id_i) %>%
    dplyr::arrange(counter)
  
  # Test set
  df_test <- df_ar %>% 
    dplyr::filter(counter %in% test_counters, id == id_i) %>%
    dplyr::arrange(counter)
  
  # Raw values
  y_obs <- data_long_eval %>% 
    filter(item == target_item, id == id_i, counter %in% test_counters) %>% 
    arrange(counter) %>% 
    pull(value)
  y_obs_train <- data_long_eval %>% 
    filter(item == target_item, id == id_i, counter %in% train_val_counters) %>% 
    arrange(counter) %>% 
    pull(value)
  
  # Fit AR(1)-Model
  fit<- df_train %>% 
    model(AR1 = ARIMA(value ~ pdq(1,0,0)))
  
  # Get in-sample predictions via augment 
  train_pred_ar_i <- fit %>%
    augment() %>%
    filter(!is.na(.fitted)) %>%
    transmute(
      id = id_i,
      counter,
      y_pred_train = .fitted
    ) %>%
    mutate(
      # Back-transform fitted values
      y_pred_train = undo_transformations(
        y_pred_train, id_i, target_item,
        std_stats_eval, trend_parameter_eval,
        counter
      ),
      # Add y_obs_train
      y_obs_train = y_obs_train)
  
  y_pred_train <- train_pred_ar_i$y_pred_train
  train_predictions_ar <- bind_rows(train_predictions_ar, train_pred_ar_i)
  
  # Forecast with fixed model parameters + Bootstrapped Prediction Interval
  fc <- fit %>%
    forecast(new_data = df_test, bootstrap = TRUE, times = 500) %>%
    hilo(level = 95) %>%
    unpack_hilo(`95%`)
  
  mean_preds <- undo_transformations(fc$.mean, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
  pred_lower <- undo_transformations(fc$`95%_lower`, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
  pred_upper <- undo_transformations(fc$`95%_upper`, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
  
  # Store results per ID
  test_predictions_ar_i <- tibble(
    id       = id_i,
    counter  = df_test$counter,
    y_obs    = y_obs,
    y_preds     = mean_preds,
    pred_lower  = pred_lower,
    pred_upper  = pred_upper
  )
  
  # Combine all IDs
  test_predictions_ar <- bind_rows(test_predictions_ar, test_predictions_ar_i)
  
  # Compute and store test set metrics
  test_metrics_ar_i <- compute_metrics(y_obs, mean_preds, pred_lower, pred_upper)
  test_metrics_ar_i <- test_metrics_ar_i %>% 
    mutate(id = id_i, counter = test_counters)
  test_metrics_ar <- bind_rows(test_metrics_ar, test_metrics_ar_i) 
  
  # Compute and store train set metrics
  train_metrics_ar_i <- compute_metrics(y_obs_train, y_pred_train)
  train_metrics_ar_i <- train_metrics_ar_i %>% 
    mutate(id = id_i)
  train_metrics_ar <- bind_rows(train_metrics_ar, train_metrics_ar_i) 
}



# ------- Analysis of potential overfitting ------------------------------------------------------

# Extract test set RMSE per ID
test_metrics_ar_single <- test_metrics_ar %>% 
  dplyr::distinct(id, .keep_all = TRUE) 
overfitting_ar <- create_overfitting_plot(train_metrics_ar, test_metrics_ar_single, outcome_SD_train, outcome_SD_test)
print(overfitting_ar)

ggsave(
  filename = paste0(figures_output_dir, "potentialoverfittingAR1.pdf"),
  plot     = overfitting_ar,
  width    = 10,
  height   = 6,
  dpi      = 300
)





#-----------------------------------------------------------------------------------------------------------------
#--------- Elastic Net Regression (ENR) with lagged features and bootstrapping for uncertainty estimation --------
#-----------------------------------------------------------------------------------------------------------------


#------------------------------------------------- HP-Optimization -----------------------------------------------
val_metrics_enr_hpo <- tibble()

# Optimize the number of lagged features
for (n_lags_i in seq(1, 7, 1)) { 
  df_hpo_i <- create_lagged_features(
    df=df_hpo, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  # Optimize regularization ( 0 = ridge, 1 = lasso, 0 < alpha < 1 = elastic net)
  for (alpha_i in seq(0, 1, 0.5)) { 
    # Fit glm and predict targets per id
    for (id_i in unique(df_hpo_i$id)) {
      X_train <- as.matrix(df_hpo_i %>% 
                             filter(counter %in% train_counters, id == id_i) %>% 
                             dplyr::select(-target_item, -id, -counter))
      y_train <- as.matrix(df_hpo_i %>% 
                             filter(counter %in% train_counters, id == id_i) %>% 
                             dplyr::select(target_item, -id))
      X_val <- as.matrix(df_hpo_i %>% 
                           filter(counter %in% val_counters, id == id_i) %>% 
                           dplyr::select(-target_item, -id, -counter))
      y_val <- as.vector(as.matrix(y_test_raw %>% 
                                      filter(counter %in% val_counters, id == id_i) %>% 
                                      dplyr::select(value)))
      
      # Internally, GLMNet fits a sequence of models with different lambdas, which we use to find the best value
      # Note that we do not use cv.glmnet, as this does not take into account that the data is a time series
      # Note II: No setting random seed required as glmnet and opt-method is deterministic.
      glm_fit <- glmnet(
        X_train, y_train,
        alpha = alpha_i,
        standardize = FALSE,
      )
      preds_hpo <- predict(glm_fit, newx = X_val) # Shape ( one row = all lambdas for one time point, one column = one time series (lambda-specific))

      # Undo transformations
      preds_hpo <- undo_transformations(preds_hpo, id_i, target_item, std_stats_hpo, trend_parameter_hpo, val_counters, is_matrix=TRUE)
      
      # Find best lambda depending on min rmse
      rmse_preds_hpo = sqrt(colMeans((y_val - preds_hpo)^2))
      best_lambda_index = which.min(rmse_preds_hpo)
      best_lambda_i = glm_fit$lambda[best_lambda_index]
      preds_hpo_i = as.vector(preds_hpo[,best_lambda_index])
      
      # Compute accuracy metrics
      val_metrics_i <- compute_metrics(y_val, preds_hpo_i) 
      val_metrics_i <- val_metrics_i %>% mutate(id = id_i, 
                                                n_lags=n_lags_i,
                                                alpha=alpha_i,
                                                lambda = best_lambda_i)
      val_metrics_enr_hpo <- bind_rows(val_metrics_enr_hpo, val_metrics_i)
      
      # Logging
      print(sprintf("ID: %.0f   N_Lags: %.0f   Alpha: %.1f   Best Lambda: %.3f   RMSE: %.3f", id_i, n_lags_i, alpha_i, best_lambda_i, val_metrics_i$RMSE))
    }
  }
}

# Save HPs per id which maximize accuracy (i.e., minimize RMSE)
optim_criteria <- "RMSE" 
opt_hps <- val_metrics_enr_hpo %>%
  group_by(id) %>%
  filter(.data[[optim_criteria]] == min(.data[[optim_criteria]], na.rm = TRUE)) %>%
  slice_head(n = 1) %>%  
  ungroup() %>% 
  dplyr::select(id, n_lags, lambda, alpha, RMSE)

# HP Table
opt_hps_apa <- opt_hps %>%
  mutate(
    id = round(id, 0),
    n_lags = round(n_lags, 0),
    lambda = formatC(lambda, format = "f", digits = 2),
    alpha = round(alpha, 0),
    RMSE  = round(RMSE, 2)
  )
hpo_enr_tab <- xtable(opt_hps_apa,caption= paste0(target_item, "Optimal Hyperparameters for ENR per Participant"), label="tab:opt_hps")
print(hpo_enr_tab, include.rownames = FALSE, sanitize.text.function = identity, comment = FALSE)




#----------------------------------------------------------------------------------------------------------------
#--------------------------------------------- ENR Without PIs --------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
test_metrics_enr <- tibble() 
train_metrics_enr <- tibble()
all_predictions_enr <- tibble()
train_predictions_enr <- tibble()
sensitivity_results_enr <- tibble()

# Fit ENR and predict target_item per id
for (id_i in unique(df_hpo$id)) {
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  # Create wide format dataset with lagged features
  df_eval_i <- create_lagged_features( 
    df=df_eval, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
  y_train_raw_i <- as.matrix(y_train_raw %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(value))
  y_train_raw_i <- y_train_raw_i[(n_lags_i + 1):length(train_val_counters)]
  
  # Fit model
  glm_fit_i <- glmnet(X_train, y_train, alpha = alpha_i, lambda = lambda_i, standardize = FALSE)
  
  # Save number of non zero coefficients and coefficients
  glm_summary <- capture.output(print(glm_fit_i)) %>% 
    paste(collapse = "\n")
  
  # Extract coefficients
  coef_df <- as.data.frame(as.matrix(coef(glm_fit_i)))
  colnames(coef_df) <- "estimate"
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  
  # Save predictions
  preds <- as.vector(predict(glm_fit_i, newx = X_test, s = lambda_i))
  preds_train <- as.vector(predict(glm_fit_i, newx = X_train, s = lambda_i))
  
  # Undo transformations
  preds <- undo_transformations(preds, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  preds_train <- undo_transformations(preds_train, id_i, target_item, std_stats_eval, trend_parameter_eval, train_val_counters)
  
  # Remove NAs arised in n_lag first preds
  preds_train <- na.omit(preds_train) 
  
  # Store preds for residual plots - Test set
  pred_store_i <- tibble(
    id = id_i,
    counter = test_counters,
    y_obs = as.numeric(y_test),
    y_pred = as.numeric(preds)
  )
  
  all_predictions_enr <- bind_rows(
    all_predictions_enr,
    pred_store_i
  )
  
  # Store preds for residual plots - Train set
  train_pred_i <- tibble(
    id = id_i,
    counter = train_val_counters[(n_lags_i + 1):length(train_val_counters)],
    y_obs_train = as.numeric(y_train_raw_i),
    y_pred_train = as.numeric(preds_train)
  )
  train_predictions_enr <- bind_rows(train_predictions_enr, train_pred_i)
  
  # Compute Accuracy metrics for id and add to all metrics in test_metrics_enr (as well as optimal HPs)
  test_metrics_i <- compute_metrics(y_test, preds)
  test_metrics_i <- test_metrics_i %>% 
    mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i)
  test_metrics_enr <- bind_rows(test_metrics_enr, test_metrics_i)
  
  train_metrics_i <- compute_metrics(y_train_raw_i, preds_train)
  train_metrics_i <- train_metrics_i %>% 
    mutate(id = id_i)
  train_metrics_enr <- bind_rows(train_metrics_enr, train_metrics_i)
  
  # Sensitivity Analysis
  sensitivity_results_i <- sensitivity_analysis(preds, y_test, test_counter, id_i, n_lags_i)
  sensitivity_results_enr <- bind_rows(sensitivity_results_enr, sensitivity_results_i)
}



# -------- Analysis of potential Overfitting  -------------------------------------------------------
overfitting_enr <- create_overfitting_plot(train_metrics_enr, test_metrics_enr, outcome_SD_train, outcome_SD_test)
print(overfitting_enr)
ggsave(
  filename = paste0(figures_output_dir, "potentialoverfittingENR.pdf"),
  plot     = overfitting_enr,
  width    = 10,
  height   = 6,
  dpi      = 300
)



# ------------------------- Analysis of Residuals of ENR-------------------------------------------------
# Compute mean of residuals per participant 
all_predictions_enr <- all_predictions_enr %>% 
  mutate(resid = y_obs - y_pred)

all_predictions_enr %>%
  group_by(id) %>%
  summarise(mean_resid = mean(resid))

# Compute aggregated mean 
all_predictions_enr %>%
  summarise(mean_resid = mean(resid))

# Compute ACF of person-specific residuals 
max_lag <- 3
acf_resid <- all_predictions_enr %>%
  arrange(id, counter) %>%
  group_by(id) %>%
  summarise(
    n = n(),
    acf = list(stats::acf(resid, plot = T, lag.max = min(max_lag, n - 1))$acf[-1]),
    acf_lag1 = ifelse(length(acf[[1]]) >= 1, acf[[1]][1], NA_real_),
    acf_lag2 = ifelse(length(acf[[1]]) >= 2, acf[[1]][2], NA_real_),
    acf_lag3 = ifelse(length(acf[[1]]) >= 3, acf[[1]][3], NA_real_),
    .groups = "drop"
  )
acf_resid

# Visualize 
all_predictions_enr %>%
  arrange(id, counter) %>%
  ggplot(aes(x = counter, y = resid)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~ id, scales = "free_x") +
  labs(x = "Counter", y = "Residual")



#-----------------------------Sensitivity Analysis---------------------------------------------------------------
sensitivity_results_enr <- sensitivity_results_enr %>%
  mutate(zone = factor(zone,
                       levels = c("leakage_zone", "clean_zone"),
                       labels = c("leakage", "clean")
  ),
  id = as.factor(id))



#------------------ Sensitivity Analysis: Results --------------------------------------------------
# RMSE per ID per zone
sensitivity_wide <- sensitivity_results_enr %>%
  pivot_wider(names_from = zone, values_from = RMSE) 

# Plot
ggplot(sensitivity_results_enr, aes(x = zone, y = RMSE, fill = zone)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "RMSE in leakage vs. clean zone across participants",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal()


# Observed mean difference
obs_diff <- mean(sensitivity_wide$leakage - sensitivity_wide$clean, na.rm = TRUE)
cat("Observed mean difference (leakage - clean):", round(obs_diff, 4), "\n")

# Test if leakage < clean paired by id using Monte Carlo resampling as approximation
perm_test <- symmetry_test(
  RMSE ~ zone | id,     
  data = sensitivity_results_enr,
  alternative = "less", 
  distribution = approximate(nresample = 5000)
)
perm_test





#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------- Bootstrapping for Prediction Intervals -------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
n_bootstrap <- 500 
test_metrics_boot <- tibble()
test_predictions_boot <- tibble()

# Fit glm and predict target item per id
for (id_i in unique(df_eval$id)) {
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  # Create lagged dataset
  df_eval_i <- create_lagged_features( 
    df=df_eval, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  # Split lagged dataset
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
  
  # Create matrix for storing bootstrapped predictions(rows = n_bootstrap, columns = n_test_counter)
  all_preds <- matrix(NA, nrow = n_bootstrap, ncol = nrow(X_test))
  
  # Ensure reproducability
  set.seed(random_seed)

  # Bootstrapping
  for(i in 1:n_bootstrap) {
    # Draw bootstrap sample
    sample_idx <- sample(1:nrow(X_train), size = nrow(X_train), replace = TRUE) 
    X_sample <- X_train[sample_idx, , drop = FALSE]
    y_sample <- y_train[sample_idx]
    
    # Fit glm to bootstrap sample
    glm_fit_boot_i <- glmnet(X_sample, y_sample, alpha = alpha_i, standardize = FALSE, trace = FALSE, lambda=lambda_i)
    
    # Predict on test set
    preds <- predict(glm_fit_boot_i, newx = X_test, s = lambda_i)
    preds <- undo_transformations(preds, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
    all_preds[i, ] <- as.vector(preds)
  }
  
  # Compute mean and median predictions
  mean_preds <- apply(all_preds, 2, mean)
  median_preds <- apply(all_preds, 2, median)
  
  # Compute prediction intervals (2.5% and 97.5%)
  pred_lower <- apply(all_preds, 2, quantile, probs = 0.025)
  pred_upper <- apply(all_preds, 2, quantile, probs = 0.975)
  
  # Compute metrics
  test_metrics_i <- compute_metrics(y_test, median_preds, pred_lower, pred_upper)
  test_metrics_i <- test_metrics_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda=lambda_i)
  test_metrics_boot <- bind_rows(test_metrics_boot, test_metrics_i)
  
  # Save test predictions for plotting
  test_predictions_boot <- bind_rows(test_predictions_boot, tibble(
    mean_preds= mean_preds,
    y_preds= median_preds, # median preds get renamed here for simple plotting
    y_obs= as.vector(y_test),
    pred_lower= pred_lower,
    pred_upper= pred_upper,
    id= id_i,
  ))
  
  # Logging
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_i$RMSE[1]))
}






#----------------------------------------------------------------------------------------------------------------------------
#------------------------------------- RESIDUAL Bootstrapping for Prediction Intervals --------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
test_metrics_resid_boot <- tibble()
test_predictions_resid_boot <- tibble()

# Fit glm and predict target item per id
for (id_i in unique(df_hpo$id)) {
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  # Create lagged dataset
  df_eval_i <- create_lagged_features( 
    df=df_eval, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  # Split lagged dataset
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
  
  # Ensure reproducability
  set.seed(random_seed)
  
  # Residual Bootstrapping (with undoing scaling and diff and detrending inside function)
  boot <- resid_bootstrap(
    X_train = X_train,
    y_train = y_train,
    X_test  = X_test,
    alpha   = alpha_i,
    lambda  = lambda_i,
    B       = n_bootstrap,
    standardize = FALSE,
    id=id_i, 
    target_item=target_item, 
    std_stats=std_stats_eval, 
    trend_parameter=trend_parameter_eval, 
    counters = test_counters
  )
  
  # Extract predictive distribution (n_test x B)
  all_preds <- boot$mu_boot

  mean_preds <- apply(all_preds, 1, mean)
  median_preds <- apply(all_preds, 1, median)
  
  pred_lower <- apply(all_preds, 1, quantile, probs = 0.025, na.rm = TRUE)
  pred_upper <- apply(all_preds, 1, quantile, probs = 0.975, na.rm = TRUE)
  
  # Store preds and PIs
  test_predictions_resid_boot_i <- tibble(
    id = id_i,
    counter = test_counters,
    y_obs = as.vector(y_test),
    mean_preds = mean_preds,
    y_preds = median_preds,
    pred_lower = pred_lower,
    pred_upper = pred_upper
  )
  
  test_predictions_resid_boot <- bind_rows(test_predictions_resid_boot, test_predictions_resid_boot_i)
  
  # Compute metrics
  test_metrics_resid_i <- compute_metrics(y_test, median_preds, pred_lower, pred_upper)
  test_metrics_resid_i <- test_metrics_resid_i %>% 
    mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i)
  test_metrics_resid_boot <- bind_rows(test_metrics_resid_boot, test_metrics_resid_i)
  
  # Logging
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_resid_i$RMSE[1]))
}





#----------------------------------------------------------------------------------------------------------------------------
#------------------------------------------ BLOCK Bootstrapping for Prediction Intervals ------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
l_block <- 10
test_metrics_block_boot <- tibble()
test_predictions_block_boot <- tibble()

# Fit glm and predict target item per id
for (id_i in unique(df_hpo$id)) {
  # Get optimal HPs per id
  alpha_i  <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  # Create lagged feature dataset
  df_eval_i <- create_lagged_features(
    df = df_eval,
    id_col = id_col,
    time_col = time_col,
    target_col = target_item,
    n_lags = n_lags_i,
    numeric_features = features_to_lag
  )
  
  # Split lagged dataset
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% 
                         dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% 
                         dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% 
                        dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% 
                        dplyr::select(value))
  
  # Mean block length l for geometric block bootstrap
  n_sim <- nrow(X_train)
  
  # Backtransform 
  mean___ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_value
  sd___   <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_value
  
  transformation <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(transformation)
  params <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(params)
  
  # Stationary Bootstrap (Politis & Romano, 1992) via tsboot (sim="geom")
  # bootstraps the observations (X, y)
  # first column = y, remaining columns = X
  # Note that if sim is "geom" then l is the mean of the geometric distribution used to generate the block lengths.
  tseries_train <- cbind(y_train, X_train)
  boot_ts <- tsboot(
    tseries   = tseries_train,
    statistic = statistic_forecast,
    R         = n_bootstrap,
    l         = l_block ,
    sim       = "geom",
    n.sim     = n_sim,
    X_test    = X_test,
    alpha     = alpha_i,
    lambda    = lambda_i,
    mean___   = mean___,
    sd___     = sd___,
    test_counters   = test_counters,
    transformation  = transformation,
    params          = params
  )
  
  # Extract predictive distribution (tsboot returns bootstrap replicates in boot_ts$t : R x length(statistic))
  all_preds <- boot_ts$t
  
  # Compute mean and median predictions
  mean_preds   <- apply(all_preds, 2, mean, na.rm = TRUE)
  median_preds <- apply(all_preds, 2, median, na.rm = TRUE)
  
  # Compute prediction intervals (2.5% and 97.5%)
  pred_lower <- apply(all_preds, 2, quantile, probs = 0.025, na.rm = TRUE)
  pred_upper <- apply(all_preds, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  # Compute metrics
  test_metrics_i <- compute_metrics(y_test, median_preds, pred_lower, pred_upper) %>%
    mutate(id = id_i, n_lags = n_lags_i, alpha = alpha_i, lambda = lambda_i,
           block_boot = TRUE, sim = "geom", l = l_block)
  test_metrics_block_boot <- bind_rows(test_metrics_block_boot, test_metrics_i)
  
  # Save test predictions for plotting
  test_predictions_block_boot <- bind_rows(
    test_predictions_block_boot,
    tibble(
      mean_preds   = mean_preds,
      y_preds = median_preds, 
      y_obs       = y_test,
      pred_lower   = pred_lower,
      pred_upper   = pred_upper,
      id           = id_i
    )
  )
  
  # Logging
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_i$RMSE[1]))
}





#----------------------------------------------------------------------------------------------------------------------------
#------- Quantile Random Forest Regression (RFR) with lagged features and bootstrapping for uncertainty estimation ----------
#---------------------------------------------------------------------------------------------------------------------------- 

#------------------------------- HP-Optimization ----------------------------------------------------------------------------
val_metrics_rfr_hpo <- tibble()
opt_hpo_rfr <- tibble()


for (n_lags_i in seq(1, 7, 1)) { # optimize n lags
  df_hpo_i <- create_lagged_features(
    df=df_hpo, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  X_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(target_item, -id))
  X_val <- as.matrix(df_hpo_i %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_val <- as.matrix(y_test_raw %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(value))
  
  # Define Grid for HPO based on n_lags
  p <- ncol(X_train)
  hpo_grid <- expand.grid(
    mtry     = unique(round(c( p/5, p/4, p/3))), #Number of variables randomly sampled as candidates at each split.
    nodesize = c(3, 5, 10, 15), # Minimum size of terminal nodes. default = 5 
    ntree = c(200, 500, 1000)
  )
  
  # Optimize per person
  for (id_i in unique(df_hpo$id)) {
    # Loop over hyperparameter combinations 
    for (h in 1:nrow(hpo_grid)) {
      mtry_i     <- hpo_grid$mtry[h]
      nodesize_i <- hpo_grid$nodesize[h]
      ntree_i     <- hpo_grid$ntree[h]
      
      # Ensure reproducability
      set.seed(random_seed)
      
      qrf_fit_h <- quantregForest(
        x = X_train,
        y = y_train,
        mtry = mtry_i,
        nodesize = nodesize_i,
        ntree = ntree_i,
        replace = T
      )
      
      # Extract median prediction, undo transformations and compute metrics
      preds_hpo_rfr <- predict(qrf_fit_h, X_val, what = 0.5)
      preds_hpo_rfr <- undo_transformations(preds_hpo_rfr, id_i, target_item, std_stats_hpo, trend_parameter_hpo, val_counters)
      val_metrics_i <- compute_metrics(y_val, preds_hpo_rfr) 
      val_metrics_i <- val_metrics_i %>% mutate(id = id_i, 
                                                n_lags=n_lags_i,
                                                mtry     = mtry_i,
                                                nodesize = nodesize_i,
                                                n_tree = ntree_i)
      val_metrics_rfr_hpo <- bind_rows(val_metrics_rfr_hpo, val_metrics_i)
      
      # Logging
      message(sprintf("ID: %.0f   N_Lags: %.0f   mtry: %.2f   n_tree: %.0f   nodesize: %.0f  RMSE: %.3f", id_i, n_lags_i, mtry_i, ntree_i, nodesize_i, val_metrics_i$RMSE))
    }
  }
}

# Save HPs per id which maximize accuracy (i.e., minimize RMSE)
optim_criteria <- "RMSE" 
opt_hpo_rfr <- val_metrics_rfr_hpo %>%
  group_by(id) %>%
  filter(.data[[optim_criteria]] == min(.data[[optim_criteria]], na.rm = TRUE)) %>%
  slice_head(n = 1) %>%  
  ungroup() %>% 
  dplyr::select(id, n_lags, mtry, nodesize, n_tree, RMSE)

# LaTeX Table
opt_hpo_rfr %>%
  mutate(
    RMSE     = round(RMSE, 2),
    across(c(mtry, nodesize, n_lags, n_tree), as.integer)
  ) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c(
      "ID",
      "$n_{lags}$",
      "$n_{tree}$",
      "$mtry$",
      "$nodesize$",
      "RMSE"
    ),
    caption = "Optimal hyperparameters (RFR) per ID"
  ) %>%
  kable_styling(
    latex_options = "hold_position",
    font_size = 10,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE)



#--------------------------------------- Predictions for RFR --------------------------------------------------------------############
# Fit random forest regression and predict target_item per id
test_metrics_rfr <- tibble() 
train_metrics_rfr <- tibble()
test_predictions_rfr <- tibble()
train_predictions_rfr <- tibble()
sensitivity_results_rfr <- tibble()

for (id_i in unique(df_hpo$id)) {
  # Get optimal hyperparameters per id 
  opt_row <- opt_hpo_rfr %>% filter(id == id_i)
  n_lags_i     <- opt_row$n_lags
  opt_mtry     <- opt_row$mtry
  opt_nodesize <- opt_row$nodesize
  opt_ntree <- opt_row$n_tree
  
  # Create lagged dataset 
  df_eval_i <- create_lagged_features(
    df = df_eval, 
    id_col = id_col,
    time_col = time_col,
    target_col = target_item,
    n_lags = n_lags_i,
    numeric_features=features_to_lag
  )
  
  # Split lagged dataset
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
  y_train_raw_i <- as.matrix(y_train_raw %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(value))
  y_train_raw_i <- y_train_raw_i[(n_lags_i + 1):length(y_train_raw_i)]
  
  # Ensure reproducability
  set.seed(random_seed)
  
  qrf_fit <- quantregForest(
    x        = X_train,
    y        = y_train,
    ntree    = opt_ntree,              
    mtry     = opt_mtry,         
    nodesize = opt_nodesize      
  )
  
  # Predict point est. + PI 
  median_preds <- predict(qrf_fit, X_test, what = 0.5)
  preds_train <- predict(qrf_fit, X_train, what = 0.5)
  pred_lower   <- predict(qrf_fit, X_test, what = 0.025)
  pred_upper   <- predict(qrf_fit, X_test, what = 0.975)
  
  # Undo Transformations
  median_preds <- undo_transformations(median_preds, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  preds_train <- undo_transformations(preds_train, id_i, target_item, std_stats_eval, trend_parameter_eval, train_val_counters)
  pred_lower <- undo_transformations(pred_lower, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  pred_upper <- undo_transformations(pred_upper, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  
  # Remove NAs arised in n_lag first preds
  preds_train <- na.omit(preds_train)
  
  # Evaluate (test metrics)
  test_metrics_i <- compute_metrics(
    y_obs      = y_test,
    y_pred     = median_preds,
    pred_lower = pred_lower,
    pred_upper = pred_upper
  ) %>% 
    mutate(
      id      = id_i,
      counter = test_counters,
      n_lags  = n_lags_i,
      mtry    = opt_mtry,
      nodesize= opt_nodesize,
      n_trees = opt_ntree
    )
  test_metrics_rfr <- bind_rows(test_metrics_rfr, test_metrics_i)
  
  # Evaluate (train metrics)
  train_metrics_i <- compute_metrics(y_train_raw_i, preds_train)
  train_metrics_i <- train_metrics_i %>% mutate(id = id_i)
  train_metrics_rfr <- bind_rows(train_metrics_rfr, train_metrics_i)
  
  # Save test predictions for plotting
  test_predictions_rfr <- bind_rows(test_predictions_rfr, tibble(
    id=id_i,
    counter = test_counters,
    y_preds=median_preds,
    y_obs=as.vector(y_test),
    pred_lower=pred_lower,
    pred_upper=pred_upper
  ))
  
  # Save train predictions for plotting
  train_predictions_rfr <- bind_rows(train_predictions_rfr, tibble(
    id=id_i,
    counter = train_val_counters[(n_lags_i + 1):length(train_val_counters)],
    y_obs_train = y_train_raw_i,
    y_pred_train = preds_train
  ))
  
  sensitivity_results_i <- sensitivity_analysis(median_preds, y_test, test_counter, id_i, n_lags_i)
  sensitivity_results_rfr <- bind_rows(sensitivity_results_rfr, sensitivity_results_i)
}



#------------------ Sensitivity Analysis RFR: Results --------------------------------------------------
sensitivity_results_rfr <- sensitivity_results_rfr %>%
  mutate(zone = factor(zone,
                       levels = c("leakage_zone", "clean_zone"),
                       labels = c("leakage", "clean")
  ),
  id = as.factor(id))

# RMSE per ID per zone
sensitivity_wide_rfr <- sensitivity_results_rfr %>%
  pivot_wider(names_from = zone, values_from = RMSE) 

# Plot
ggplot(sensitivity_results_rfr, aes(x = zone, y = RMSE, fill = zone)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "RMSE in leakage vs. clean zone across participants",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal()


# Observed mean difference
obs_diff_rfr <- mean(sensitivity_wide_rfr$leakage - sensitivity_wide_rfr$clean, na.rm = TRUE)
cat("Observed mean difference (leakage - clean):", round(obs_diff_rfr, 4), "\n")

# Test if leakage < clean paired by id using Monte Carlo resampling as approximation
perm_test_rfr <- symmetry_test(
  RMSE ~ zone | id,      
  data = sensitivity_results_rfr,
  alternative = "less",
  distribution = approximate(nresample = 5000)
)
perm_test_rfr


#---------------- Analysis of Potential Overfitting -----------------------------------------
test_metrics_rfr_single <- test_metrics_rfr %>% dplyr::distinct(id, .keep_all = TRUE)
overfitting_rfr <- create_overfitting_plot(train_metrics_rfr, test_metrics_rfr_single, outcome_SD_train, outcome_SD_test)
print(overfitting_rfr)
ggsave(
  filename = paste0(figures_output_dir, "potentialoverfittingRFR.pdf"),
  plot     = overfitting_rfr,
  width    = 10,
  height   = 6,
  dpi      = 300
)






#------------------------------------------------------------------------------------------------------------------
#-----------------------------------Plots and Tables --------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# Histograms of RMSE with median + range tables
method_levels <- c(
  "Quantile RFR",
  "ENR",
  "AR(1)"
)

palette <- c(
  "AR(1)" = "dodgerblue3",
  "ENR" = "grey",
  "Quantile RFR" = "indianred1"
)

metrics_long <- bind_rows(
  test_metrics_ar   %>% transmute(id, Method = "AR(1)", RMSE),
  test_metrics_enr  %>% transmute(id, Method = "ENR", RMSE),
  test_metrics_rfr  %>% transmute(id, Method = "Quantile RFR", RMSE)
) %>%
  mutate(Method = factor(Method, levels = method_levels))

metrics_long_unique <- metrics_long %>%
  dplyr::distinct(id, Method, .keep_all = TRUE)

xmin <- min(metrics_long_unique$RMSE)
xmax <- max(metrics_long_unique$RMSE)

raincloudplot <- ggplot(metrics_long_unique, aes(x = RMSE, y = Method, fill = Method)) +
  
  # density plot
  stat_halfeye(
    adjust = 0.6,
    width = 0.4,
    height = 0.5,
    .width = 0,
    justification = -0.3,
    alpha = 0.75,
    point_colour = NA
  ) +
  
  # boxplot
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.5
  ) +

  # raw data
  geom_point(
    position = position_jitter(width=0, height=0.1),
    alpha = 0.5,
    size = 1
  ) +
  scale_fill_manual(values = palette) +
  xlim(xmin, xmax) +
  theme_minimal() +
  labs(
    x = "RMSE",
    y = ""
  ) +
  theme(
    axis.title.y = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.position = "none") 

print(raincloudplot)

# Save
ggsave(paste0(figures_output_dir, "rmse_distribution_plot_",target_item,".pdf"), raincloudplot, width = 5, height = 3, bg = "white")

# Table RMSE
rmse_summary <- metrics_long %>%
  group_by(Method) %>%
  summarise(
    Median = median(RMSE, na.rm = TRUE),
    Min = min(RMSE, na.rm = TRUE),
    Max = max(RMSE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Median = round(Median, 2),
    Min = round(Min, 2),
    Max = round(Max, 2),
    Range = paste0(Min, " - ", Max)
  ) %>%
  select(Method, Median, Range)

rmse_summary

apply(rmse_summary, 1, function(x) {
  cat(sprintf("%.2f & %s \\\\\n",
              as.numeric(x["Median"]),
              x["Range"]))
})

#----------------------------------------------------------------------------------------
# Accuracy and UQ Comparison: standard bootstrapping vs.residual bootstrap vs. Block Bootstrap aggregated over participants

uq_comparison_table <- tibble(
  Method = c(
    "AR(1) RB",
    "ENR SB",
    "ENR RB",
    "ENR BB",
    "Quantile RFR"
  ),
  `C - Median` = c(
    median(test_metrics_ar$Coverage, na.rm = TRUE),
    median(test_metrics_boot$Coverage, na.rm = TRUE),
    median(test_metrics_resid_boot$Coverage, na.rm = TRUE),
    median(test_metrics_block_boot$Coverage, na.rm = TRUE),
    median(test_metrics_rfr$Coverage, na.rm = TRUE)
  ),
  `C- Range` = c(
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_ar$Coverage, na.rm = TRUE),
      max(test_metrics_ar$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_boot$Coverage, na.rm = TRUE),
      max(test_metrics_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_resid_boot$Coverage, na.rm = TRUE),
      max(test_metrics_resid_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_block_boot$Coverage, na.rm = TRUE),
      max(test_metrics_block_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_rfr$Coverage, na.rm = TRUE),
      max(test_metrics_rfr$Coverage, na.rm = TRUE)
    )
  ),
  `IW- Median` = c(
    median(test_metrics_ar$interval_width, na.rm = TRUE),
    median(test_metrics_boot$interval_width, na.rm = TRUE),
    median(test_metrics_resid_boot$interval_width, na.rm = TRUE),
    median(test_metrics_block_boot$interval_width, na.rm = TRUE),
    median(test_metrics_rfr$interval_width, na.rm = TRUE)
  ),
  `IW- Range` = c(
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_ar$interval_width, na.rm = TRUE),
      max(test_metrics_ar$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_boot$interval_width, na.rm = TRUE),
      max(test_metrics_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_resid_boot$interval_width, na.rm = TRUE),
      max(test_metrics_resid_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_block_boot$interval_width, na.rm = TRUE),
      max(test_metrics_block_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_rfr$interval_width, na.rm = TRUE),
      max(test_metrics_rfr$interval_width, na.rm = TRUE)
    )
  ),
  `WS - Median` = c(
    mean(test_metrics_ar$Winkler_Score, na.rm = TRUE),
    mean(test_metrics_boot$Winkler_Score, na.rm = TRUE),
    mean(test_metrics_resid_boot$Winkler_Score , na.rm = TRUE),
    mean(test_metrics_block_boot$Winkler_Score , na.rm = TRUE),
    mean(test_metrics_rfr$Winkler_Score, na.rm = TRUE)
  ),
  `WS - Range` = c(
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_ar$Winkler_Score, na.rm = TRUE),
      max(test_metrics_ar$Winkler_Score, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_boot$Winkler_Score, na.rm = TRUE),
      max(test_metrics_boot$Winkler_Score, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_resid_boot$Winkler_Score, na.rm = TRUE),
      max(test_metrics_resid_boot$Winkler_Score, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_block_boot$Winkler_Score, na.rm = TRUE),
      max(test_metrics_block_boot$Winkler_Score, na.rm = TRUE)
    ),
    sprintf(
      "%.2f--%.2f",
      min(test_metrics_rfr$Winkler_Score, na.rm = TRUE),
      max(test_metrics_rfr$Winkler_Score, na.rm = TRUE)
    )
  )
)

uq_comparison_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    caption = paste0("Uncertainty Quantification Evaluation across Models ", target_item),
    align = "lcccc"
  ) %>%
  add_header_above(
    c(" " = 1, "Coverage" = 2, "Interval Width" = 2, "Winkler-Score" == 2)
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(5, width = "3cm")

#---------------------------------------------------------------------------------------------------------
#------------ Accuracy and UQ - person-specific-----------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


#----- AR(1) Model----------------------------------------------------------------------------------------
AR_UQ <- test_metrics_ar %>%
  mutate(Method = "AR(1) RB") %>%
  summarise(
    RMSE           = mean(RMSE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE),
    Winkler_Score  = mean(Winkler_Score, na.rm = TRUE),
    .by = c(id, Method)
  ) %>%
  pivot_wider(
    names_from  = Method,
    values_from = c(RMSE, Coverage, interval_width, Winkler_Score),
    names_glue  = "{Method} - {.value}"
  ) %>%
  arrange(id)

AR_UQ %>%
  kable(format="latex", booktabs=TRUE, digits=2, longtable=TRUE,
        caption= paste0(target_item,"AR(1): Person-Specific Accuracy and Uncertainty Quantification (Residual Bootstrap)")) %>%
  kable_styling(latex_options=c("repeat_header"), font_size=8)


#--------- ENR Model ---------------------------------------------------------------------------------
# First, get all UQ methods of the ENR Model in one (wide) data frame
ENR_UQ <- bind_rows(
  test_metrics_boot       %>% mutate(Method = "ENR SB"),
  test_metrics_resid_boot %>% mutate(Method = "ENR RB"),
  test_metrics_block_boot %>% mutate(Method = "ENR BB")
) %>%
  mutate(Method = factor(Method, levels = c(
    "ENR SB",
    "ENR RB",
    "ENR BB"
  ))) %>%
  group_by(id, Method) %>%
  summarise(
    RMSE           = mean(RMSE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE),
    Winkler_Score  = mean(Winkler_Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(id, Method) %>%
  group_by(id) %>%
  mutate(
    add_space = row_number() == n()  
  ) %>%
  ungroup()


ENR_UQ %>%
  select(-add_space) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 2,
    longtable = TRUE,
    caption = paste0(target_item,
                     ": ENR Person-Specific Accuracy and Uncertainty Quantification"),
    align = c("l","c","c","c","c","c")
  ) %>%
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 8
  ) %>%
  row_spec(
    which(ENR_UQ$add_space),
    extra_latex_after = "\\addlinespace"
  )





#--------------- Quantile RFR Model-------------------------------------------------------------------------------
RFR_UQ <- test_metrics_rfr %>%
  mutate(Method = "Quantile RFR") %>%
  summarise(
    RMSE           = mean(RMSE, na.rm = TRUE),
    Coverage       = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE),
    Winkler_Score  = mean(Winkler_Score, na.rm = TRUE),
    .by = c(id, Method)
  ) %>%
  pivot_wider(
    names_from  = Method,
    values_from = c(RMSE, Coverage, interval_width, Winkler_Score),
    names_glue  = "{Method} - {.value}"
  ) %>%
  arrange(id)

RFR_UQ %>%
  kable(format="latex", booktabs=TRUE, digits=2, longtable=TRUE,
        caption=paste0(target_item,"RFR: Person-Specific Accuracy and Uncertainty Quantification (Quantile Random Forest)"))%>%
  kable_styling(latex_options=c("repeat_header"), font_size=8)



#--------------------------------------------------------------------------------------------------------------
#----------------------------- Residual Plots -----------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
metrics_train <- list(train_metrics_ar, train_metrics_enr, train_metrics_rfr)
predictions_train <- list(train_predictions_ar, train_predictions_enr, train_predictions_rfr)
names_train <- c("AR(1)", "ENR", "Quantile RFR")


# Loop for plots for different bootstrap variations
for (i in seq(1, 3, 1)) {
  name_i <- names_train[[i]]
  metrics_i <- metrics_train[[i]]
  preds_i <- predictions_train[[i]]
  
  
  for (id_i in unique(df_hpo$id))  {
    # Extract In-Sample Predictions for this ID
    y_obs <- (preds_i %>% dplyr::filter(., id == id_i))$y_obs_train
    y_pred <- (preds_i %>% dplyr::filter(., id == id_i))$y_pred_train
    
    # Residual Plots - Training Set
      
      preds_i_with_res <- preds_i %>% 
        dplyr::filter(., id == id_i) %>%
        dplyr::mutate(resid = y_obs_train - y_pred_train)
      
      Res_plot_train <-  ggplot(preds_i_with_res, aes(x = y_pred_train, y = resid)) +
          geom_point(
            aes(color = abs(resid)),
            alpha = 0.45,
            size = 2
          ) +
          geom_smooth(
            formula = 'y ~ x',
            method = "loess",
            se = FALSE,
            color = "#2c3e50",
            linewidth = 1.1
          ) +
          geom_hline(
            yintercept = 0,
            linetype = "dashed",
            color = "black",
            linewidth = 0.7
          ) +
        scale_color_gradient(
          low = "dodgerblue3",
          high = "indianred1",
          limits = c(0, 50),
          oob = scales::squish,   
          name = "|Residual|"
        ) +
          labs(
            x = "Predicted",
            y = "Residual (Observed – Predicted)",
            title = paste(name_i, "In-Sample Residual Plot — ID", id_i) 
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "right",
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(xlim = c(0,100), ylim = c(-70, 70)) +
          guides(color = guide_colorbar(barwidth = 1, barheight = 14))
      
      
      print(Res_plot_train)
      
      model_name <- gsub("[^[:alnum:]]", "_", name_i)
      folder_name <- paste0(figures_output_dir, "/residual_plots/")
      dir.create(folder_name, showWarnings = FALSE)
      file_name <- paste0(folder_name, "In_Sample_resid_", model_name, target_item, "_ID_", id_i, ".pdf")
      
      # Save Plots in Figures folder
      ggsave(
        filename = file_name,
        plot = Res_plot_train,
        width = 8,
        height = 6,
        dpi = 300,
        bg = "white" 
      )
    }
}





#------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------  Forecasting Plots with PIs and Historical Data + Out-Of-Sample (Test-Set) Residual Plots -----------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------
metrics <- list(test_metrics_ar, test_metrics_boot, test_metrics_resid_boot, test_metrics_block_boot, test_metrics_rfr)
predictions <- list(test_predictions_ar, test_predictions_boot, test_predictions_resid_boot, test_predictions_block_boot, test_predictions_rfr)
names <- c( "AR1 - Residual Bootstrap", "Standard Bootstrap", "Residual Bootstrap", "Block Bootstrap", "Quantile RFR")



#------------------------------------ Get representative IDs for expressive plots -------------------------
# RMSE per model
rmse_all <- bind_rows(
  test_metrics_ar  %>% distinct(id, RMSE) %>% mutate(Model = "AR(1)"),
  test_metrics_enr     %>% distinct(id, RMSE) %>% mutate(Model = "ENR"),
  test_metrics_rfr %>% distinct(id, RMSE) %>% mutate(Model = "RFR")
) 

# Rank per model
rmse_ranks <- rmse_all %>%
  group_by(Model) %>%
  mutate(rk = rank(RMSE)) %>%
  ungroup()

# Mean rank over all models
difficulty <- rmse_ranks %>%
  summarise(mean_rank = mean(rk), .by = id)

# Easy / typical / hard IDs 
best_id <- difficulty %>% slice_min(mean_rank, n = 1)
worst_id <- difficulty %>% slice_max(mean_rank, n = 1)

median_val <- median(difficulty$mean_rank)
typical_id <- difficulty %>%
  mutate(dist = abs(mean_rank - median_val)) %>%
  slice_min(dist, n = 1) %>%
  select(id, mean_rank) %>%
  slice(1)

list(
  best    = best_id,
  typical = typical_id,
  worst    = worst_id
)

# Save representative IDS
representative_ids <- c(best_id$id, typical_id$id, worst_id$id)
representative_ids



#----------------------------- Create Plots ----------------------------------------------------------
for (id_i in representative_ids) {
  # Extract training + test predictions 
  y_train <- (data_long_eval %>% dplyr::filter(., id == id_i, counter %in% train_val_counters, item == target_item))$value
  n_train <- length(y_train)
  
  # Historical data frame
  df_hist <- data.frame(
    time = 1:n_train,
    y_obs = y_train,
    type = "Historical"
  )
  
  forecast_plot <-  ggplot() +
    geom_point(
      data = df_hist,
      aes(x = time, y = y_obs), 
      color = "grey40",
      shape = 16
    ) +
    geom_line(
      data = df_hist,
      aes(x = time, y = y_obs), 
      color = "grey40",
      linewidth = 1
    ) +
    labs(
      x = "Time step",
      y = "Target value",
      color = "",
      fill  = "",
      size=20
    ) +
    theme_minimal() +
    ylim(0, 100) +
    theme(axis.text = element_text(size=12), axis.title = element_text(size=14))
  
  # Print and safe each plot
  print(forecast_plot)
  ggsave(
    filename = paste0(figures_output_dir, "historical_data_", id_i, ".pdf"),
    plot = forecast_plot, width = 4, height = 3)
}

# Loop for different bootstrap variations
for (i in seq(1, 5)) {
  name_i <- names[[i]]
  metrics_i <- metrics[[i]]
  preds_i <- predictions[[i]]
  
  for (id_i in representative_ids) {
    # Extract training + test predictions for this ID
    y_train <- (data_long_eval %>% dplyr::filter(id == id_i, counter %in% val_counters, item == target_item))$value
    y_obs  <- (preds_i %>% dplyr::filter(id == id_i))$y_obs
    y_preds <- (preds_i %>% dplyr::filter(id == id_i))$y_preds
    lower <- (preds_i %>% dplyr::filter(id == id_i))$pred_lower
    upper <- (preds_i %>% dplyr::filter(id == id_i))$pred_upper
    
    base_time <- length(train_counters)+1
    n_train <- length(y_train)
    n_test  <- length(y_obs)
    
    time_hist <- base_time:(base_time+n_train-1)
    time_test <- (base_time+n_train-1):(base_time+n_train+n_test-1)
    
    # Historical data frame
    df_hist <- data.frame(
      time = time_hist,
      y_obs = y_train,
      type = "Historical"
    )
    
    # Test + forecast data frame
    df_test <- data.frame(
      time = time_test,
      y_obs = c(tail(y_train, n=1), y_obs),
      y_preds = c(tail(y_train, n=1), y_preds),
      lower = c(tail(y_train, n=1), lower),
      upper = c(tail(y_train, n=1), upper),
      type = "Test"
    )
    
    
    forecast_plot <-  ggplot() +
      geom_ribbon(
        data = df_test,
        aes(x = time, ymin = lower, ymax = upper), 
        fill = "indianred1",
        alpha = 0.2
      ) +
      geom_line(
        data = df_test,
        aes(x = time, y = y_preds),
        color = "indianred1",
        linewidth = 1
      ) +
      geom_point(
        data = df_hist,
        aes(x = time, y = y_obs), 
        color = "grey40",
        shape = 16
      ) +
      geom_line(
        data = df_test,
        aes(x = time, y = y_obs),
        color = "dodgerblue3",
        linewidth = 1,
        alpha = 0.6
      ) +
      geom_point(
        data = df_test,
        aes(x = time, y = y_obs), 
        color = "dodgerblue3",
        shape = 16,
        alpha = 0.6
      ) +
      geom_line(
        data = df_hist,
        aes(x = time, y = y_obs), 
        color = "grey40",
        linewidth = 1
      ) +
      # Add clinically relevant cutoff value
      geom_hline(
        yintercept = 18,
        linetype = "solid",
        color = "grey",
        linewidth = 0.8
      ) +
      labs(
        x = "Time step",
        y = "Target value",
        color = "",
        fill  = "",
        size=20
      ) +
      theme_minimal() +
      theme(axis.text = element_text(size=12), axis.title = element_text(size=14))
    
    # Print and safe each plot
    print(forecast_plot)
    ggsave(
      filename = paste0(figures_output_dir, name_i, "forecast_id", id_i, ".pdf"),
      plot = forecast_plot, width = 4, height = 3)
  }
}
# TODO: In diskussion beschreiben dass zusammenhang zwischen features und target nicht richtig geschätzt wurde bei den personen die nicht funktioneirne
# ABER: auch mit random forests nicht und diese nehemen keinen linearen zusammenhang an! residual plots in trainingsset anschauen --------------------------------------------------
