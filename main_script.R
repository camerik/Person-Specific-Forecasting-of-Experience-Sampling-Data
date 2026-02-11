############################################### Initialization ##########################################################
# Initialize renv, load functions and set seed for reproducability
# TODO: Ask: Mean or Median Prediction for Bootstrapping/Random Forests?

# Install and load required packages
packages <- c("dplyr", "tidyr", "zoo", "imputeTS", "purrr", "Metrics", "ggplot2", "glmnet", "coin",
              "openesm", "quantregForest", "kableExtra", "xtable", "boot")
lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})
source("git-ordner/Person-Specific-Forecasting-of-Experience-Sampling-Data/functions_MLM.R")

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

# Load dataset from openesm or from RData file
load_from_file <- TRUE
if (load_from_file) {
  load("raw_data.RData")
} else {
  raw_data = openesm::get_dataset("0008_westhoff")$data
}

############################################# Preprocessing #############################################################
# Check initial number of ids
n_id_1_initial = length(unique(raw_data$id))

# Drop columns not required for forecasting
cols_to_drop <- c("scheduled_time", "response_time",
                  "location_latitude", "location_longitude", "start_date", 
                  "end_date", "duration_in_seconds", "finished", "sleep_duration") 
raw_data <- raw_data %>% select(-all_of(cols_to_drop))

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
    all_of(c("sleep_quality")),# nur auffüllen, wenn es mind. einen observed Wert gibt
    # denselben Wert auf alle beeps des Tages verteilen
    ~ if (any(!is.na(.x))) {
      rep(first(na.omit(.x)), length(.x))
    } else {
      .x # sonst NA lassen (wird probleme geben bei interpolation, PRO TAG interpolieren)
    }
  )) %>%
  ungroup()

# Character string with item/feature names (all colnames except id and counter)
feature_names = setdiff(colnames(raw_data), c("id", "counter"))
beep_feature_names = setdiff(colnames(raw_data), c("id", "counter", "sin_weekday", "cos_weekday", "sleep_quality", "day"))
daily_feature_names = c("sin_weekday", "cos_weekday", "sleep_quality", "day")
features_to_lag = setdiff(colnames(raw_data), c("id", "counter", "sin_weekday", "cos_weekday", "day", "beep", "sleep_quality"))




# --- LOW VARIANCE -----------------------------------------------------------------------------------
# Check for and exclude ids with low variance: 
# Cutoff: variance < 1 or ≥ 10 unique answer categories
def_low_var <- "one"
if (def_low_var == "ten_unique") {
  # n unique answer categories per id per item
  unique_counts <- raw_data %>%
    group_by(id) %>%
    summarise(across(all_of(beep_feature_names), ~ n_distinct(.)), .groups = "drop")
  
  # keep only ids with count ≥ 10 in every item/feature 
  ids_to_keep <- unique_counts %>%
    filter(if_all(everything(), ~ . >= 10)) %>% # is already grouped by id, so i can use every column because column counter has > 10 unique categories per id
    pull(id)
  
  raw_data <- raw_data %>% filter(id %in% ids_to_keep)
  
  # check: how many ids were excluded? 
  n_id_2_var_check = length(unique(raw_data$id)) # excludes 38 participants
  
} else if (def_low_var == "one") {
  var_features <- raw_data %>%
    group_by(id) %>%
    summarise(across(all_of(beep_feature_names), ~ sd(., na.rm = T)), groups = "drop") 
  
  ids_var <- var_features %>%
    filter(if_all(everything(), ~ . >= 1)) %>%
    pull(id)
  
  raw_data <- raw_data %>%
    filter(id %in% ids_var)
  
  n_id_2_var_check <- length(unique(raw_data$id)) # excludes 9 participants
} else {
  print("cutoff not implemented")
}




# --- MISSINGNESS (Number of Rows) NOT NECESSARY IN THIS DATASET SINCE IT HAS ALREADY BEEN PREPROCESSED---------------------------------------------------------------------------------
# pivot longer
raw_data_long = raw_data %>%
  pivot_longer( cols = all_of(feature_names), names_to = "item", values_to = "value")

# compute n missing rows per ID  
raw_data_long_missing_rows <- raw_data %>%
  mutate(
    row_missing = if_else(
      if_all(setdiff(beep_feature_names, "beep"), is.na), TRUE, FALSE
    )
  )
missing_rows_per_id <- raw_data_long_missing_rows %>%
  group_by(id) %>%
  summarise(
    n_missing_rows = sum(row_missing),
    .groups = "drop"
  )

# # Remove ids with too many missing rows (based on 2*std more than the mean of missing rows per id)
# cutoff <- mean(missing_rows_per_id$n_missing_rows) +
#   2 * sd(missing_rows_per_id$n_missing_rows)
# 
# 
# # plot distribution of max missing rows and cut off value
# ggplot(missing_rows_per_id, aes(x = n_missing_rows)) +
#   geom_bar() +
#   geom_vline(xintercept = cutoff, color = "red", linewidth = 1) +
#   labs(
#     x = "Number of Missing Rows (per ID)",
#     y = "Count of Participants",
#     title = "Distribution of Missing Rows per ID"
#   ) +
#   theme_minimal()

# valid_ids <- missing_rows_per_id %>%
#   filter(n_missing_rows <= cutoff) %>%
#   pull(id)
# 
# raw_data_long <- raw_data_long %>%
#   filter(id %in% valid_ids)
# 
# raw_data <- raw_data %>%
#   filter(id %in% valid_ids)
# 
# n_id_3_missings = length(unique(raw_data$id)) # excludes 6 participants




# --- MISSINGNESS (Consecutive Rows) ---------------------------------------------------------------------------------
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

# filter datasets 
raw_data_long <- raw_data_long %>%
  filter(id %in% valid_ids_consec)
raw_data <- raw_data %>%
  filter(id %in% valid_ids_consec)

n_id_4_cons_missings = length(unique(raw_data_long$id)) # excludes 2 participants




# --- HOLDOUT SPLITS ---------------------------------------------------------------------------------
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
ids_with_nas <- raw_data_long %>%
  filter(counter %in% test_counters) %>%
  group_by(id) %>% # over all items!
  summarise(has_na = any(is.na(value))) %>%
  filter(has_na) %>%
  pull(id)

raw_data_long <-  raw_data_long %>% filter(!(id %in% ids_with_nas))
raw_data <- raw_data %>% filter(!(id %in% ids_with_nas))
n_id_5_test_na = length(unique(raw_data_long$id)) # removes 66 participants


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
n_id_6_val_na = length(unique(raw_data_long$id)) # removes 15 participants


# ---------------------Define Target Item and Relevant Cols-------------------------------------------------
id_col <- "id"
time_col <- "counter"
target_item <-"positive_physical_health_behavior"
# target_item <- "depressed"


# ---------------------Descriptive statistics---------------------------------------------------------------
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
  dplyr::select(all_of(feature_names)) %>%
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

# Plot 4 example features
for (id_i in unique(raw_data_long$id)) {
  print(raw_data_long %>%
          dplyr::filter(id == id_i) %>%
          dplyr::filter(item %in% feature_names[13:16]) %>%
          ggplot(aes(x = value)) +
          ggtitle(paste("ID:",id_i)) +
          geom_histogram(bins = 20, na.rm = TRUE) +
          facet_wrap(~ item) +
          theme_classic())
}


ACF_plot(raw_data_long, chosen_item = target_item)

#################################### Prepare Data for HPO ##########################################
# Interpolate
interpolation_type = "Kalman"
data_long_hpo <- interpolate(raw_data_long, train_counters, interpolation_type)


# Plot imputed values for example IDS

example_id <- c(72425, 73479, 72291)


for (id_i in example_id) {
  
  # raw with NAs
  df_raw <- raw_data_long %>%
    dplyr::filter(id == id_i, item == target_item) %>%
    dplyr::select(counter, value_raw = value) %>%
    dplyr::arrange(counter)
  
  # imputed
  df_imp <- data_long_hpo %>%
    dplyr::filter(id == id_i, item == target_item) %>%
    dplyr::select(counter, value_imp = value) %>%
    dplyr::arrange(counter)
  
  # align by counter 
  df_plot <- dplyr::left_join(df_raw, df_imp, by = "counter") %>%
    dplyr::arrange(counter)
  
  if (any(is.na(df_plot$value_raw))) {
    print(
      ggplot_na_imputations(
        x_with_na = df_plot$value_raw,
        x_with_imputations = df_plot$value_imp,
        title = paste("ID", id_i, target_item),
        xlab = "Counter",
        ylab = "Value"
      )
    )
  } else {
    message("ID ", id_i, ": no missing values in raw series for this item.")
  }
}


# Check if there are any NAs left
sum(is.na(data_long_hpo %>% filter(counter %in% train_counters)))

# Source for AR(1)-DF-Test and Stationarity Transformations:
# Ryan et al. (2025) (adf_flow in diagnose_trend_type)
# Compute detrending components to detrend data
trend_parameter_hpo <- diagnose_trend_type(data_long_hpo %>% filter(counter %in% train_counters))

# Diff and detrend whole dataset
data_long_hpo_dd <- diff_and_detrend(data_long_hpo, trend_parameter_hpo)

# Compute mean and sd for scaling data
std_stats_hpo = data_long_hpo_dd %>% filter(counter %in% train_counters) %>%
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
mean(data_long_hpo_dd_std %>% filter(id == unique(data_long_hpo_dd_std$id)[1], item == target_item, counter %in% train_counters) %>% dplyr::pull(value))


################################### Prepare Data for Eval ###########################################
# Interpolate
# data_long_eval <- interpolate(raw_data_long, train_val_counters, interpolation_type)
data_long_eval <- data_long_hpo

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
std_stats_eval = data_long_eval_dd %>% filter(counter %in% train_val_counters) %>%
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
mean(data_long_eval_dd_std %>% filter(id == unique(data_long_eval_dd_std$id)[1], item == target_item, counter %in% train_val_counters) %>% dplyr::pull(value))

# Extract the raw test data to compare to predictions
y_train_raw <- data_long_hpo %>% filter(item == target_item, counter %in% train_val_counters)
y_test_raw <- data_long_hpo %>% filter(item == target_item, counter %in% c(val_counters, test_counters))

print(paste("Number of participants after preprocessing:",length(unique(data_long_eval$id))))


#########################################################################################################################
# Elastic Net Regression (ENR) with lagged features and bootstrapping for uncertainty estimation
#########################################################################################################################


################################################### HP-Optimization #####################################################
val_metrics <- tibble() # tibble for accuracy metrics per HPO combination
i_iter = 0
for (n_lags_i in seq(1, 7, 1)) { # Optimize the number of lagged features
  df_hpo_i <- create_lag_features(
    df=df_hpo, 
    id_col=id_col, 
    time_col=time_col, 
    target_col=target_item, 
    n_lags=n_lags_i,
    numeric_features=features_to_lag
  )
  
  for (alpha_i in seq(0, 1, 0.5)) { # Optimize regularization ( 0 = ridge, 1 = lasso, 0 < alpha < 1 = elastic net)
    # Fit glm and predict targets per id
    for (id_i in unique(df_hpo_i$id)) {
      X_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
      y_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(target_item, -id))
      X_test <- as.matrix(df_hpo_i %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
      y_test <- as.vector(as.matrix(y_test_raw %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(value)))
      
      set.seed(random_seed)
      
      # lambda opt
      glm_fit <- glmnet(
        X_train, y_train,
        alpha = alpha_i,
        standardize = FALSE,
      )
      
      preds_hpo <- predict(glm_fit, newx = X_test) # Shape ( one row = all lambdas for one time point, one column = one time series (lambda-specific))
      
      # do NOT use cv.glmnet --> does not take into account that preds are time series
      # • “lambda.min”: the λ at which the smallest MSE is achieved. (with CV)
      # • “lambda.1se”: the largest λ at which the MSE is within one standard error of the smallest MSE (default).
      
      # Undo transformations
      preds_hpo <- undo_transformations(preds_hpo, id_i, target_item, std_stats_hpo, trend_parameter_hpo, val_counters, is_matrix=TRUE)
      
      # Find best lambda depending on min rmse
      rmse_preds_hpo = sqrt(colMeans((y_test - preds_hpo)^2))
      best_lambda_index = which.min(rmse_preds_hpo)
      best_lambda_i = glm_fit$lambda[best_lambda_index]
      preds_hpo_i = as.vector(preds_hpo[,best_lambda_index])
      
      # compute accuracy metrics
      val_metrics_i <- compute_metrics(preds_hpo, y_test) # TODO: außerhalb der schleife überprüfen ob code richtig läuft
      val_metrics_i <- val_metrics_i %>% mutate(id = id_i, 
                                                n_lags=n_lags_i,
                                                alpha=alpha_i,
                                                lambda = best_lambda_i)
      val_metrics <- bind_rows(val_metrics, val_metrics_i)
      
      print(sprintf("ID: %.0f   N_Lags: %.0f   Alpha: %.1f   Lambda: %.3f   RMSE: %.3f", id_i, n_lags_i, alpha_i, best_lambda_i, val_metrics_i$RMSE))
    }
  }
}


# Save hps per id which maximize accuracy
optim_criteria <- "RMSE" #TODO change to sMAPE or MAE?
opt_hps <- val_metrics %>%
  group_by(id) %>%
  filter(.data[[optim_criteria]] == min(.data[[optim_criteria]], na.rm = TRUE)) %>%
  slice_head(n = 1) %>% # debug: multiple HP combo solutions for min-RMSE per id, arbitrarily take the first HP combo 
  ungroup() %>% 
  dplyr::select(id, n_lags, lambda, alpha, RMSE, sMAPE, MAE)


# Format numeric columns APA-style and print table ready for latex document
opt_hps_apa <- opt_hps %>%
  mutate(
    RMSE  = round(RMSE, 3),
    sMAPE = round(sMAPE, 3),
    MAE   = round(MAE, 3),
    lambda = formatC(lambda, format = "f", digits = 2)
  )
apa_tab <- xtable(opt_hps_apa,caption="Optimal Hyperparameters per Participant", label="tab:opt_hps")
print(apa_tab, include.rownames = FALSE, sanitize.text.function = identity, comment = FALSE)


################################################# Linear Forecast #######################################################

# Fit glm and predict target_item per id
test_metrics <- tibble() # for out of sample accuracy metrics
train_metrics <- tibble() # for in sample accuracy metrics
glm_results <- tibble()
all_predictions <- tibble()
train_predictions <- tibble()
sensitivity_results <- tibble()  
for (id_i in unique(df_hpo$id)) { # df_hpo, because df_eval includes ids without hpo because of missings in validation set 
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  # create wide format dataset with lagged features
  df_eval_i <- create_lag_features( # own function to create lagged df 
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
  y_train_raw_i <- y_train_raw_i[(n_lags_i + 1):length(y_train_raw_i)]
  
  set.seed(random_seed)
  
  glm_fit_i <- glmnet(X_train, y_train, alpha = alpha_i, lambda = lambda_i, standardize = FALSE) #data is already z-transformed
  
  # save number of non zero coefficients and coefficients
  glm_summary <- capture.output(print(glm_fit_i)) %>% paste(collapse = "\n")
  
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
  preds_train <- na.omit(preds_train) # remove NAs arised in n_lag first preds
  
  # Store preds for residual plots - Test set
  pred_store_i <- tibble(
    id = id_i,
    counter = test_counters,
    y_true = as.numeric(y_test),
    y_pred = as.numeric(preds)
  )
  
  all_predictions <- bind_rows(
    all_predictions,
    pred_store_i
  )
  
  # Store preds for residual plots - Train set
  train_pred_store_i <- tibble(
    id = id_i,
    counter = train_val_counters[(n_lags_i + 1):length(train_val_counters)],
    y_obs_train = as.numeric(y_train_raw_i),
    y_pred_train = as.numeric(preds_train)
  )
  
  train_predictions <- bind_rows(
    train_predictions,
    train_pred_store_i
  )
  
  
  test_metrics_i <- compute_metrics(preds, y_test) # accuracy metrics per id
  test_metrics_i <- test_metrics_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i) # add optimized HPO
  test_metrics <- bind_rows(test_metrics, test_metrics_i) # accuracy metrics matrix (all ids)
  #n_coef <- bind_rows(n_coef, n_nonzero_coef_i)
  #coefficients <- bind_rows(coefficients, coefficients_i)
  
  train_metrics_i <- compute_metrics(preds_train, y_train_raw_i)
  train_metrics_i <- train_metrics_i %>% mutate(id = id_i)
  train_metrics <- bind_rows(train_metrics, train_metrics_i)
  
  glm_results <- bind_rows(
    glm_results,
    tibble(
      id = id_i,
      alpha = alpha_i,
      n_lags = n_lags_i,
      lambda=lambda_i,
      glm_summary = glm_summary,
      coef_table = list(coef_df)   
    ))
  
  # Sensitivitätsanalyse
  sensitivity_results_i <- sensitivity_analysis(preds, y_test, test_counter, id_i, n_lags_i)
  sensitivity_results <- bind_rows(sensitivity_results, sensitivity_results_i)
}

# -------- Analysis of Potential Overfitting  ---------------------------

# Compute variance in target item per id
target_variance <- y_test_raw %>%
  filter(counter %in% test_counters) %>%
  group_by(id) %>%
  summarise(SD = sd(value, na.rm = TRUE))

# Merge RMSEs for train and test
combined_metrics <- train_metrics %>%
  select(id, RMSE) %>% rename(RMSE_Train = RMSE) %>%
  inner_join(
    test_metrics %>% select(id, RMSE) %>% rename(RMSE_Test = RMSE),
    by = "id"
  ) %>%
  inner_join(
    target_variance,  # add variance column
    by = "id"
  ) %>%
  rename(Outcome_SD = SD)

# Sort ascending by Target_Std to see relationship to Test_RMSE
combined_metrics <- combined_metrics %>%
  arrange(RMSE_Test) %>% # arrange(Target_Std) %>%                   
  mutate(id = factor(id, levels = id))

# Convert from wide to long format for plotting
long_metrics <- combined_metrics %>%
  pivot_longer(
    cols = c(RMSE_Train, RMSE_Test, Outcome_SD),
    names_to = "Metric",
    values_to = "Value"
  )

# Plot side-by-side bar plot
ggplot(long_metrics, aes(x = factor(id), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "RMSE and SD in Outcome item per ID",
       x = "ID", y = "Value") +
  scale_fill_manual(values = c("RMSE_Train" = "dodgerblue3",
                               "RMSE_Test"  = "indianred1",
                               "Outcome_SD" = "seagreen3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Simple correlation between Target_Std and Test RMSE
correlation <- cor(combined_metrics$Outcome_SD, combined_metrics$RMSE_Test, use = "complete.obs")
correlation

# Respective Plot
ggplot(combined_metrics, aes(x = Outcome_SD, y = RMSE_Test)) +
  geom_point(color = "red", size = 3) +           # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # regression line with 95% CI
  labs(title = "Correlation between Target Std and Test RMSE",
       x = "Target Standard Deviation",
       y = "Test RMSE") +
  theme_minimal()


# ------------Analysis of residuals of elastic net regression-------------------------------------------------
# compute mean of residuals per participant 
all_predictions <- all_predictions %>% 
  mutate(resid = y_true - y_pred)

all_predictions %>%
  group_by(id) %>%
  summarise(mean_resid = mean(resid))

# compute aggregated mean 
all_predictions %>%
  summarise(mean_resid = mean(resid))

# compute ACF of person-specific residuals 
max_lag <- 3

acf_resid <- all_predictions %>%
  arrange(id, counter) %>%
  group_by(id) %>%
  summarise(
    n = n(),
    acf = list(stats::acf(resid, plot = T, lag.max = min(max_lag, n - 1))$acf[-1]),
    # [-1] removes lag 0 
    acf_lag1 = ifelse(length(acf[[1]]) >= 1, acf[[1]][1], NA_real_),
    acf_lag2 = ifelse(length(acf[[1]]) >= 2, acf[[1]][2], NA_real_),
    acf_lag3 = ifelse(length(acf[[1]]) >= 3, acf[[1]][3], NA_real_),
    .groups = "drop"
  )

acf_resid
# Visualize 
all_predictions %>%
  arrange(id, counter) %>%
  ggplot(aes(x = counter, y = resid)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~ id, scales = "free_x") +
  labs(x = "Counter", y = "Residual")



#--------------------------------------------------------------------------------------------
# Sensitivity Analysis
sensitivity_results <- sensitivity_results %>%
  mutate(zone = factor(zone,
                       levels = c("leakage_zone", "clean_zone"),
                       labels = c("leakage", "clean")
  ),
  id = as.factor(id))


#------------------ Sensitivity Analysis: Results --------------------------------------------------
# RMSE per ID per zone
sensitivity_wide <- sensitivity_results %>%
  pivot_wider(names_from = zone, values_from = RMSE) 

# Plot
ggplot(sensitivity_results, aes(x = zone, y = RMSE, fill = zone)) +
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

perm_test <- symmetry_test(
  RMSE ~ zone | id,      # paired by id
  data = sensitivity_results,
  alternative = "less",  # leakage < clean
  distribution = approximate(nresample = 5000)  # approximate = Monte Carlo resampling
)
perm_test

# Echten wert mit Verteilung der RMSE-Mittelwertsdifferenzen über ids hinweg vergleichen. 
# 5000 mal permutieren (innerhalb jeder person random leakage vs clean vertauschen)
# wahrscheinlichkeitsdichte der vertielung --> echte mittelwertsdifferenz wie wahrscheinlich wenn 
# die durch permutation entstandene verteilung (unter annahme der nullhypothese) gilt? 


# analysis of penalization 
# see glm_summary --> n_nonzero coefficients, etc


########################################################################################################################
####################################### Bootstrapping for Prediction Intervals #########################################
########################################################################################################################
n_bootstrap <- 500 

# Fit glm and predict target item per id
test_metrics_boot <- tibble() #accuracy metrics
test_predictions_boot <- tibble() # mean_preds, median_preds, y_test, PI, and PI eval 
for (id_i in unique(df_eval$id)) { #df_hpo, cause df_eval includes ids with missings in validation set --> no HPO possible
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  df_eval_i <- create_lag_features( #create lagged dataset
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
  
  # create matrix for bootstrapped preds
  all_preds <- matrix(NA, nrow = n_bootstrap, ncol = nrow(X_test))
  
  set.seed(random_seed)
  
  # Bootstrapping
  for(i in 1:n_bootstrap) {
    # ziehe  nrow(X_train) viele Zeilen aber mit zurücklegen 
    sample_idx <- sample(1:nrow(X_train), size = nrow(X_train), replace = TRUE) 
    X_sample <- X_train[sample_idx, , drop = FALSE] #should have 90 - n_lags rows
    y_sample <- y_train[sample_idx] #should have 90 - n_lags values
    
    # Fit glm to bootstrap sample
    glm_fit_boot_i <- glmnet(X_sample, y_sample, alpha = alpha_i, standardize = FALSE, trace = FALSE, lambda=lambda_i) # Already STD
    
    # Predict on test set using a set lambda
    preds <- predict(glm_fit_boot_i, newx = X_test, s = lambda_i)
    preds <- undo_transformations(preds, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
    
    all_preds[i, ] <- as.vector(preds) # rows = n_bootstrap durchläufe, columns = n_counter in test set
  }
  
  # Compute mean and median predictions
  mean_preds <- apply(all_preds, 2, mean)
  median_preds <- apply(all_preds, 2, median)
  
  # Compute prediction intervals (2.5% and 97.5%)
  pred_lower <- apply(all_preds, 2, quantile, probs = 0.025)
  pred_upper <- apply(all_preds, 2, quantile, probs = 0.975)
  
  # Compute metrics
  test_metrics_i <- compute_metrics(y_test, mean_preds, pred_lower, pred_upper)
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_i$RMSE[1]))
  test_metrics_i <- test_metrics_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda=lambda_i)
  test_metrics_boot <- bind_rows(test_metrics_boot, test_metrics_i)
  
  # Save test predictions for plotting
  test_predictions_boot <- bind_rows(test_predictions_boot, tibble(
    mean_preds=mean_preds,
    median_preds=median_preds,
    y_test=y_test,
    pred_lower=pred_lower,
    pred_upper=pred_upper,
    id=id_i,
  ))
}

########################################################################################################################
####################################### RESIDUAL Bootstrapping for Prediction Intervals #########################################
########################################################################################################################
# Fit glm and predict target item per id
test_metrics_resid_boot <- tibble() #accuracy metrics
test_predictions_resid_boot <- tibble() # mean_preds, median_preds, y_test, PI, and PI eval 
for (id_i in unique(df_hpo$id)) { #df_hpo, cause df_eval includes ids with missings in validation set --> no HPO possible
  # Get optimal HPs per id
  alpha_i <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  df_eval_i <- create_lag_features( #create lagged dataset
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
  
  set.seed(random_seed)
  
  # Residual Bootstrapping WITH Undoing scaling and diff and detrending inside function
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
  
  # Bootstrap-Verteilung 
  all_preds <- boot$mu_boot         # n_test x B
  #mu_point <- boot$mu_point       # n_test
  
  mean_preds <- apply(all_preds, 1, mean)
  median_preds <- apply(all_preds, 1, median)
  
  pred_lower <- apply(all_preds, 1, quantile, probs = 0.025, na.rm = TRUE)
  pred_upper <- apply(all_preds, 1, quantile, probs = 0.975, na.rm = TRUE)
  
  # bootstrap mean as point forecast (or median?)
  
  
  #store preds and PIs for plotting
  test_predictions_resid_boot_i <- tibble(
    id = id_i,
    counter = test_counters,
    y_test = as.numeric(y_test),
    mean_preds = as.numeric(mean_preds),
    median_preds = as.numeric(median_preds),
    pred_lower = as.numeric(pred_lower),
    pred_upper = as.numeric(pred_upper)
  )
  
  test_predictions_resid_boot <- bind_rows(test_predictions_resid_boot, test_predictions_resid_boot_i)
  
  # Compute metrics
  test_metrics_resid_i <- compute_metrics(y_test, mean_preds, pred_lower, pred_upper)
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_resid_i$RMSE[1]))
  test_metrics_resid_i <- test_metrics_resid_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i)
  test_metrics_resid_boot <- bind_rows(test_metrics_resid_boot, test_metrics_resid_i)
}

########################################################################################################################
####################################### BLOCK Bootstrapping for Prediction Intervals ###################################
########################################################################################################################
l_block <- 10
test_metrics_block_boot <- tibble()       # accuracy metrics
test_predictions_block_boot <- tibble()   # mean_preds, median_preds, y_test, PI, and PI eval
for (id_i in unique(df_hpo$id)) { # df_hpo, cause df_eval includes ids with missings in validation set --> no HPO possible
  
  # Get optimal HPs per id
  alpha_i  <- (opt_hps %>% filter(id == id_i))$alpha
  n_lags_i <- (opt_hps %>% filter(id == id_i))$n_lags
  lambda_i <- (opt_hps %>% filter(id == id_i))$lambda
  
  df_eval_i <- create_lag_features(
    df = df_eval,
    id_col = id_col,
    time_col = time_col,
    target_col = target_item,
    n_lags = n_lags_i,
    numeric_features = features_to_lag
  )
  
  # Split lagged dataset
  X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
  X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
  y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
  
  # mean block length l for geometric block bootstrap
  n_sim <- nrow(X_train)  # length of train+val data set 
  
  # --- Backtransform params (same as your original code) ---
  mean___ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_value
  sd___   <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_value
  
  transformation <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(transformation)
  params <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(params)
  
  # -------------------------------------------------------------------
  # Stationary Bootstrap (Politis & Romano, 1992) via tsboot(sim="geom")
  # bootstraps the observations (X, y)
  # first column = y, remaining columns = X
  # -------------------------------------------------------------------
  
  tseries_train <- cbind(y_train, X_train)  # matrix: n_sim x (1+p)
  
  # Run tsboot: geom block lengths with mean l_block; n.sim = n_sim (see above)
  boot_ts <- tsboot(
    tseries   = tseries_train,
    statistic = statistic_forecast,
    R         = n_bootstrap,
    l         = l_block , # If sim is "geom" then l is the mean of the geometric distribution used to generate the block lengths.
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
  
  # tsboot returns bootstrap replicates in boot_ts$t : R x length(statistic)
  all_preds <- boot_ts$t  # matrix: n_bootstrap x nrow(X_test)
  
  # Compute mean and median predictions
  mean_preds   <- apply(all_preds, 2, mean, na.rm = TRUE)
  median_preds <- apply(all_preds, 2, median, na.rm = TRUE)
  
  # Compute prediction intervals (2.5% and 97.5%)
  pred_lower <- apply(all_preds, 2, quantile, probs = 0.025, na.rm = TRUE)
  pred_upper <- apply(all_preds, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  # Compute metrics
  test_metrics_i <- compute_metrics(y_test, mean_preds, pred_lower, pred_upper) %>%
    mutate(id = id_i, n_lags = n_lags_i, alpha = alpha_i, lambda = lambda_i,
           block_boot = TRUE, sim = "geom", l = l_block)
  message(sprintf("ID: %d   RMSE: %.3f", id_i, test_metrics_i$RMSE[1]))
  test_metrics_block_boot <- bind_rows(test_metrics_block_boot, test_metrics_i)
  
  # Save test predictions for plotting
  test_predictions_block_boot <- bind_rows(
    test_predictions_block_boot,
    tibble(
      mean_preds   = mean_preds,
      median_preds = median_preds,
      y_test       = y_test,
      pred_lower   = pred_lower,
      pred_upper   = pred_upper,
      id           = id_i
    )
  )
}


#########################################################################################################################
# Quantile Random Forest Regression (RFR) with lagged features and bootstrapping for uncertainty estimation
#########################################################################################################################

############################## HP-Optimization #############################################
opt_hpo_RFR <- tibble()
for (id_i in unique(df_hpo$id)) {
  for (n_lags_i in seq(1, 7, 1)) { # optimize n lags
    df_hpo_i <- create_lag_features(
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
    
    set.seed(random_seed)
    
    # Define Grid for HPO based on n_lags
    p <- ncol(X_train)
    hpo_grid <- expand.grid(
      mtry     = unique(round(c( p/5, p/4, p/3))), #Number of variables randomly sampled as candidates at each split.
      nodesize = c(3, 5, 10, 15), # Minimum size of terminal nodes. default = 5 
      ntree = c(200, 500, 1000)
    )
    
    #  create tibble to store HPs
    hpo_RFR_results <- tibble()
    
    #  loop over hyperparameter combinations 
    for (h in 1:nrow(hpo_grid)) {
      
      mtry_i     <- hpo_grid$mtry[h]
      nodesize_i <- hpo_grid$nodesize[h]
      ntree_i     <- hpo_grid$ntree[h]
      
      set.seed(random_seed)
      
      qrf_fit_h <- quantregForest(
        x = X_train,
        y = y_train,
        mtry = mtry_i,
        nodesize = nodesize_i,
        ntree = ntree_i,
        replace = T # gleiche Logik wie bei bootstrapping
        # importance = Should importance of predictors be assessed? --> TODO: genauer anschauen? könnte hilfreich für 
        # Interpretation sein 
      )
      
      preds_hpo_RFR <- predict(qrf_fit_h, X_val, what = 0.5) #median
      
      # Undo transformations
      preds_hpo_RFR <- undo_transformations(preds_hpo_RFR, id_i, target_item, std_stats_hpo, trend_parameter_hpo, val_counters)
      
      rmse_h <- sqrt(mean((y_val - preds_hpo_RFR)^2, na.rm = TRUE))
      
      hpo_RFR_results <- bind_rows(
        hpo_RFR_results,
        tibble(
          mtry     = mtry_i,
          nodesize = nodesize_i,
          n_tree = ntree_i,
          RMSE     = rmse_h
        )
      )
    }
  }
  
  # select optimal hyperparameters 
  best_hpo <- hpo_RFR_results %>% 
    arrange(RMSE) %>% 
    slice(1)
  
  opt_mtry     <- best_hpo$mtry
  opt_nodesize <- best_hpo$nodesize
  
  # store the optimal hyperparameters in a tibble
  opt_hpo_RFR <- bind_rows(
    opt_hpo_RFR,
    tibble(
      id       = id_i,
      n_lags   = n_lags_i,
      n_tree  =  ntree_i,
      mtry     = opt_mtry,
      nodesize = opt_nodesize,
      RMSE     = best_hpo$RMSE
    )
  )
  
  print(sprintf("ID: %.0f   N_Lags: %.0f   mtry: %.2f   n_tree: %.0f   nodesize: %.0f  RMSE: %.3f", id_i, n_lags_i, opt_mtry, ntree_i, opt_nodesize, best_hpo$RMSE))
}



# Results-Table
opt_hpo_RFR %>%
  mutate(
    RMSE = round(RMSE, 2),
    mtry = as.integer(mtry),
    nodesize = as.integer(nodesize),
    n_lags = as.integer(n_lags),
    n_tree = as.integer(n_tree)
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
    latex_options = c("hold_position"),
    font_size = 10,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  as.character() %>%
  paste0(
    "\\captionsetup{labelformat=empty}\n",
    "\\raggedright\n",
    .
  )

################################### Predictions for RFR ########################################################################
# Fit random forest regression and predict target_item per id
test_metrics_RFR <- tibble() 
train_metrics_RFR <- tibble()
test_predictions_RFR <- tibble()
train_predictions_RFR <- tibble()
sensitivity_results_RFR <- tibble()
for (id_i in unique(df_hpo$id)) {
  # Get optimal hyperparameters per id 
  opt_row <- opt_hpo_RFR %>% filter(id == id_i)
  n_lags_i     <- opt_row$n_lags
  opt_mtry     <- opt_row$mtry
  opt_nodesize <- opt_row$nodesize
  opt_ntree <- opt_row$n_tree
  
  # Create lagged dataset 
  df_eval_i <- create_lag_features(
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
  
  set.seed(random_seed)
  
  qrf_fit <- quantregForest(
    x        = X_train,
    y        = y_train,
    ntree    = opt_ntree,              
    mtry     = opt_mtry,         
    nodesize = opt_nodesize      
  )
  
  #  Predict point est. + PI 
  median_preds <- predict(qrf_fit, X_test, what = 0.5)
  preds_train <- predict(qrf_fit, X_train, what = 0.5) # changed name so it matches preds_train of AR(1) and ENR model (for residual plots) 
  pred_lower   <- predict(qrf_fit, X_test, what = 0.025)
  pred_upper   <- predict(qrf_fit, X_test, what = 0.975)
  
  
  # Undo Transformations
  median_preds <- undo_transformations(median_preds, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  preds_train <- undo_transformations(preds_train, id_i, target_item, std_stats_eval, trend_parameter_eval, train_val_counters)
  pred_lower <- undo_transformations(pred_lower, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  pred_upper <- undo_transformations(pred_upper, id_i, target_item, std_stats_eval, trend_parameter_eval, test_counters)
  
  
  preds_train <- na.omit(preds_train) # remove NAs arised in n_lag first preds
  #  Evaluate (test metrics)
  test_metrics_i <- compute_metrics(
    y_obs      = y_test,
    y_pred     = median_preds,
    pred_lower = pred_lower,
    pred_upper = pred_upper
  ) %>% 
    mutate(
      id      = id_i,
      n_lags  = n_lags_i,
      mtry    = opt_mtry,
      nodesize= opt_nodesize,
      n_trees = opt_ntree
    )
  
  test_metrics_RFR <- bind_rows(test_metrics_RFR, test_metrics_i)
  
  # Evaluate (train metrics)
  
  train_metrics_i <- compute_metrics(preds_train, y_train_raw_i)
  train_metrics_i <- train_metrics_i %>% mutate(id = id_i)
  train_metrics_RFR <- bind_rows(train_metrics_RFR, train_metrics_i)
  
  
  # Save test predictions for plotting
  test_predictions_RFR <- bind_rows(test_predictions_RFR, tibble(
    id=id_i,
    counter = test_counters,
    median_preds=median_preds,
    y_test=y_test,
    pred_lower=pred_lower,
    pred_upper=pred_upper
  ))
  
  # Save train predictions for plotting
  train_predictions_RFR <- bind_rows(train_predictions_RFR, tibble(
    id=id_i,
    counter = train_val_counters[(n_lags_i + 1):length(train_val_counters)],
    y_obs_train = y_train_raw_i,
    y_pred_train = preds_train
  ))
  
  sensitivity_results_i <- sensitivity_analysis(median_preds, y_test, test_counter, id_i, n_lags_i)
  sensitivity_results_RFR <- bind_rows(sensitivity_results_RFR, sensitivity_results_i)
}


#####################################################################################################################
################ Plots and Tables ##########################################################################
#####################################################################################################################

# UQ Comparison: bootstrapping vs. bootstrap using resampled residuals
# TODO: Anpassen, Winkler Score Einfügen und visuell trennen von ENR und RFR und AR
uq_comparison_table <- tibble(
  Method = c(
    "ENR Standard Bootstrap",
    "ENR Bootstrap - Resampled Residuals",
    "ENR Block Bootstrap",
    "Quantile Random Forest Regression"
  ),
  `Coverage (Mean)` = c(
    mean(test_metrics_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_resid_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_block_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_RFR$Coverage, na.rm = TRUE)
  ),
  `Coverage (Range)` = c(
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_boot$Coverage, na.rm = TRUE),
      max(test_metrics_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_resid_boot$Coverage, na.rm = TRUE),
      max(test_metrics_resid_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_block_boot$Coverage, na.rm = TRUE),
      max(test_metrics_block_boot$Coverage, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_RFR$Coverage, na.rm = TRUE),
      max(test_metrics_RFR$Coverage, na.rm = TRUE)
    )
  ),
  `Interval width (Mean)` = c(
    mean(test_metrics_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_resid_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_block_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_RFR$interval_width, na.rm = TRUE)
  ),
  `Interval width (Range)` = c(
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_boot$interval_width, na.rm = TRUE),
      max(test_metrics_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_resid_boot$interval_width, na.rm = TRUE),
      max(test_metrics_resid_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_block_boot$interval_width, na.rm = TRUE),
      max(test_metrics_block_boot$interval_width, na.rm = TRUE)
    ),
    sprintf(
      "%.3f--%.3f",
      min(test_metrics_RFR$interval_width, na.rm = TRUE),
      max(test_metrics_RFR$interval_width, na.rm = TRUE)
    )
  )
)
uq_comparison_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = "Comparison of UQ methods",
    align = "lcccc"
  ) %>%
  add_header_above(
    c(" " = 1, "Coverage" = 2, "Interval width" = 2)
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) %>%
  column_spec(1, width = "4cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(5, width = "3cm")


############################# Residual Plots ##################################################################
############Residual Plots - Model Fits #######################################################################
metrics_train <- list(train_metrics, train_metrics_RFR)
predictions_train <- list(train_predictions, train_predictions_RFR)
names_train <- c("ENR", "QuantRegForests")
# Loop for plots for different bootstrap variations
for (i in seq(1, 2, 1)) {
  name_i <- names_train[[i]]
  metrics_i <- metrics_train[[i]]
  preds_i <- predictions_train[[i]]
  
  for (example_id in c(72425, 73479, 72291)) {
    # Extract training + test predictions for this ID
    y_obs <- (preds_i %>% filter(id == example_id))$y_obs_train
    y_pred <- (preds_i %>% filter(id == example_id))$y_pred_train
    n_train <- length(y_train)
    
    # Historical data frame
    df_hist <- data.frame(
      time = 1:n_train,
      true = y_train,
      type = "Historical"
    )
    
    # Residual Plots - Training Set
    for (example_id in c(72425, 73479, 72291)) {
      
      preds_i_with_res <- preds_i %>% # hab ich schon definiert, hier eig doppelter code
        dplyr::filter(id == example_id) %>%
        dplyr::mutate(resid = y_obs_train - y_pred_train)
      
      print(
        ggplot(preds_i_with_res, aes(x = y_pred_train, y = resid)) +
          geom_point(
            aes(color = abs(resid)),
            alpha = 0.45,
            size = 2
          ) +
          geom_smooth(
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
            low = "#74add1",
            high = "#d73027",
            name = "|Residual|"
          ) +
          labs(
            x = "Predicted",
            y = "Residual (Observed – Predicted)",
            title = paste("Residual Plot — ID", example_id) # TODO: paste name model
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold"),
            legend.position = "right",
            panel.grid.minor = element_blank()
          ) +
          coord_cartesian(ylim = c(-70, 70)) +
          guides(color = guide_colorbar(barwidth = 1, barheight = 14))
      )
    }
  }
}

#############################  Forecasting with Historical Data + Test Set Residual Plots ######################################################
metrics <- list(test_metrics_boot, test_metrics_resid_boot, test_metrics_block_boot, test_metrics_RFR)
predictions <- list(test_predictions_boot, test_predictions_resid_boot, test_predictions_block_boot, test_predictions_RFR)
names <- c("Standard Bootstrap", "Residual Sampling Bootstrap", "Block Bootstrap", "QuantRegForests")
# Loop for plots for different bootstrap variations
for (i in seq(1, 4, 1)) {
  name_i <- names[[i]]
  metrics_i <- metrics[[i]]
  preds_i <- predictions[[i]]
  
  for (example_id in c(72425, 73479, 72291)) {
    # Extract training + test predictions for this ID
    y_train <- (data_long_eval %>% filter(id == example_id, counter %in% train_val_counters, item == target_item))$value
    y_test  <- (preds_i %>% filter(id == example_id))$y_test
    y_pred_median <- (preds_i %>% filter(id == example_id))$median_preds
    lower <- (preds_i %>% filter(id == example_id))$pred_lower
    upper <- (preds_i %>% filter(id == example_id))$pred_upper
    
    n_train <- length(y_train)
    n_test  <- length(y_test)
    
    # Historical data frame
    df_hist <- data.frame(
      time = 1:n_train,
      true = y_train,
      type = "Historical"
    )
    
    # Test + forecast data frame
    df_test <- data.frame(
      time = (n_train + 1):(n_train + n_test),
      true = y_test,
      median_forecast = y_pred_median,
      lower = lower,
      upper = upper,
      type = "Test"
    )
    
    # Merge for plotting
    plot_df <- df_test
    
    print(
      ggplot() +
        geom_ribbon(
          data = plot_df,
          aes(x = time, ymin = lower, ymax = upper, fill = "95% Prediction Interval"),
          alpha = 0.2
        ) +
        geom_line(
          data = plot_df,
          aes(x = time, y = median_forecast, color = "Median forecast"),
          linewidth = 1
        ) +
        geom_point(
          data = plot_df,
          aes(x = time, y = true, color = "Observed values"),
          shape = 16
        ) +
        geom_line(
          data = plot_df,
          aes(x = time, y = true, color = "Observed values"),
          linetype = "dashed"
        ) +
        geom_line(
          data = df_hist,
          aes(x = time, y = true, color = "Historical data"),
          linewidth = 1
        ) +
        scale_color_manual(values = c(
          "Median forecast" = "blue",
          "Observed values" = "black",
          "Historical data" = "grey40"
        )) +
        scale_fill_manual(values = c(
          "95% Prediction Interval" = "red"
        )) +
        labs(
          title = paste("Forecast with UQ for",name_i,"for ID:", example_id),
          x = "Time step",
          y = "Target value",
          color = "",
          fill  = ""
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  }
  
  
  print(ggplot(preds_i, aes(x = y_test, y = median_preds)) +
          geom_point(alpha = 0.5) +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
          facet_wrap(~ id, scales = "free") +
          labs(
            x = "Observed",
            y = "Predicted",
            title = paste("Observed vs Predict per ID for", name_i)
          ) +
          theme_minimal())
  
  
  # Residual Plots 
  for (example_id in c(72425, 73479, 72291)) {
    
    preds_i_with_res <- preds_i %>% # hab ich schon definiert, hier eig doppelter code
      dplyr::filter(id == example_id) %>%
      dplyr::mutate(resid = y_test - median_preds)
    
    print(
      ggplot(preds_i_with_res, aes(x = median_preds, y = resid)) +
        geom_point(
          aes(color = abs(resid)),
          alpha = 0.45,
          size = 2
        ) +
        geom_smooth(
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
          low = "#74add1",
          high = "#d73027",
          name = "|Residual|"
        ) +
        labs(
          x = "Predicted",
          y = "Residual (Observed – Predicted)",
          title = paste("OOS Residual Plot — ID", example_id) # TODO: paste name model
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "right",
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(ylim = c(-70, 70)) +
        guides(color = guide_colorbar(barwidth = 1, barheight = 14))
    )
  }
}

# TODO: In diskussion beschreiben dass zusammenhang zwischen features und target nicht richtig geschätzt wurde bei den personen die nicht funktioneirne
# ABER: auch mit random forests nicht und diese nehemen keinen linearen zusammenhang an! residual plots in trainingsset anschauen 

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################



#----------------------------------------------------------------------------------------------------
#TODO: Alle modelle einfügen
# tabelle accuracy und UQ
tab <- tibble(
  Modell = c("AR(1)","RLR (Bootstrap)", "RFR" ),
  
  RMSE_mean = c( mean(ar1_benchmark_metrics$RMSE)[1],
                 mean(test_metrics_boot$RMSE)[1],
                 mean(test_metrics_RFR$RMSE)[1]
  ),
  
  RMSE_min = c(
    range(ar1_benchmark_metrics$RMSE)[1],
    range(test_metrics_boot$RMSE)[1],
    range(test_metrics_RFR$RMSE)[1]
  ),
  
  RMSE_max = c(
    range(ar1_benchmark_metrics$RMSE)[2],
    range(test_metrics_boot$RMSE)[2],
    range(test_metrics_RFR$RMSE)[2]
  ),
  
  Coverage = c(
    mean(ar1_benchmark_metrics$Coverage),
    mean(test_metrics_boot$Coverage),
    mean(test_metrics_RFR$Coverage)
  ),
  Interval_width_mean = c(
    mean(ar1_benchmark_metrics$interval_width),
    mean(test_metrics_boot$interval_width),
    mean(test_metrics_RFR$interval_width)
  ),
  
  Interval_width_min = c(
    range(ar1_benchmark_metrics$interval_width)[1],
    range(test_metrics_boot$interval_width)[1],
    range(test_metrics_RFR$interval_width)[1]
  ),
  
  Interval_width_max = c(
    range(ar1_benchmark_metrics$interval_width)[2],
    range(test_metrics_boot$interval_width)[2],
    range(test_metrics_RFR$interval_width)[2]
  )
)

# convert to LaTeX
kable(tab, format = "latex", booktabs = TRUE, digits = 3)


# ---------------- results per id ----------------------
# --- specify model name 

# aggregate RLR Bootstrap aggregieren 
df_RLRB <- test_metrics_boot %>%
  group_by(id) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    Coverage = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE)
  ) %>%
  mutate(Model = "RLRB")

# aggregate RFR 
df_RFR <- test_metrics_RFR %>%
  group_by(id) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    Coverage = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE)
  ) %>%
  mutate(Model = "RFR")

# bind
df_all <- bind_rows(df_RLRB, df_RFR)


# pivot wider  one row per  ID, Spalten = Modell
df_wide <- df_all %>%
  pivot_wider(
    names_from = Model,
    values_from = c(RMSE, Coverage, interval_width)
  )

# combine Bootstrap / RFR
results_combined <- df_wide %>%
  mutate(
    RMSE = paste0(round(RMSE_RLRB, 3), " / ", round(RMSE_RFR, 3)),
    Coverage = paste0(round(Coverage_RLRB, 3), " / ", round(Coverage_RFR, 3)),
    Mean_Interval_Width = paste0(
      round(interval_width_RLRB, 3), 
      " / ", 
      round(interval_width_RFR, 3)
    )
  ) %>%
  select(id, RMSE, Coverage, Mean_Interval_Width)

results_combined


results_combined %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,   # wichtig: "/" bleibt erhalten, keine komischen Farben!
    col.names = c(
      "ID",
      "RMSE (RLRB / RFR)",
      "Coverage (RLRB / RFR)",
      "Interval Width (RLRB / RFR)"
    )
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) %>%
  add_header_above(c(" " = 1, "Vorhersagegüte" = 1, "Unsicherheit" = 2))



#combine with AR(1) results

combined_wide <- results_combined %>%
  separate(RMSE, into = c("RMSE_RLRB", "RMSE_RFR"), sep = " / ") %>%
  separate(Coverage, into = c("Coverage_RLRB", "Coverage_RFR"), sep = " / ") %>%
  separate(Mean_Interval_Width, into = c("Width_RLRB", "Width_RFR"), sep = " / ") %>%
  mutate(
    RMSE_AR1 = ar1_benchmark_metrics$RMSE[match(id, ar1_benchmark_metrics$id)],
    Coverage_AR1 = ar1_benchmark_metrics$Coverage[match(id, ar1_benchmark_metrics$id)],
    Width_AR1 = ar1_benchmark_metrics$Mean_Interval_Width[match(id, ar1_benchmark_metrics$id)]
  )

combined_wide <- combined_wide %>%
  select(
    id,
    RMSE_RLRB, RMSE_RFR, RMSE_AR1,
    Coverage_RLRB, Coverage_RFR, Coverage_AR1,
    Width_RLRB, Width_RFR, Width_AR1
  )


combined_wide %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    col.names = c(
      "ID",
      "RLRB", "RFR", "AR(1)",
      "RLRB", "RFR", "AR(1)",
      "RLRB", "RFR", "AR(1)"
    )
  ) %>%
  add_header_above(
    c(
      " " = 1,
      "RMSE" = 3,
      "Coverage" = 3,
      "Intervallbreite" = 3
    )
  ) %>%
  kable_styling(latex_options = "hold_position")
  kable_styling(latex_options = "hold_position")