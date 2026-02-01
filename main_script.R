############################################### Questions #######################################################
#TODO:Ask bzgl. split bereits VOR ausschluss von ids mit geringer varianz, missings etc? 
# TODO:
# Opt_hpo_resuluts anschauen, immer noch überall n_lag 7? 
#   Anzahl bäume? Nodesize? 

# Keine runde sache wenn ich über warnsysteme etc spreche und dann einen datensatz zu psych flexibilität nehme? 
#   
#   Gab es anker bei fragestellung? Slider zu Beginn mittig platziert? 
  

############################################### Initialization ##########################################################
# Initialize renv, load functions and set seed for reproducability
# renv::init()
packages <- c("dplyr", "tidyr", "zoo", "purrr", "Metrics", "ggplot2", "glmnet", "coin",
              "openesm", "quantregForest", "kableExtra")
lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})
source("git-ordner/Uncertainty-Quantification-in-ESM-Time-Series-Forecast/functions_MLM.R")

# renv::clean()
# renv::project()
# renv::snapshot(force = T)
# renv::dependencies()
# renv::status()
# readLines(".renvignore")
#renv::restore()

#.libPaths()

set.seed(42)

# Load dataset from openesm, from csv file or RData file 
#data_file = openesm::get_dataset("0008_westhoff")
#raw_data = data_file$data
#load("raw_data.RData")

#raw_data = read.csv("0008_westhoff.csv")
load("raw_data.RData")
n_id_1 = length(unique(raw_data$id))

############################################# Preprocessing #############################################################
# Drop columns not required for forecasting
cols_to_drop <- c("scheduled_time", "response_time",
                  "location_latitude", "location_longitude", "start_date", 
                  "end_date", "duration_in_seconds", "finished", "sleep_duration") 
raw_data <- raw_data %>% select(-all_of(cols_to_drop))

# Transform weekday to numerical variable and impute NAs
weekday_map <- c("Monday"=0,"Tuesday"=1,"Wednesday"=2,"Thursday"=3,"Friday"=4,"Saturday"=5,"Sunday"=6)
raw_data$weekday <- weekday_map[raw_data$weekday]
for (i in 2:(nrow(raw_data))) {
  if (is.na(raw_data$weekday[i])) {
    raw_data$weekday[i] = ifelse(
      raw_data$beep[i] == 1, 
      (raw_data$weekday[i - 1] + 1) %% 7, 
      raw_data$weekday[i - 1]
    )
  }
}

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
raw_data <- raw_data %>% select(-day) # Remove day afterwards

# Character string with item/feature names (all colnames except id and counter)
feature_names = setdiff(colnames(raw_data), c("id", "counter"))
beep_feature_names = setdiff(colnames(raw_data), c("id", "counter", "weekday", "sleep_quality", "day"))
daily_feature_names = c("weekday", "sleep_quality")
features_to_lag = setdiff(colnames(raw_data), c("id", "counter", "weekday", "day", "beep", "sleep_quality"))
features_to_lag

# --- LOW VARIANCE -----------------------------------------------------------------------------------
# Check for and exclude ids with low variance: 
# Cutoff: variance < 1 or ≥ 10 unique answer categories
def_low_var <- "one"
if (def_low_var == "ten_unique") {
     # n unique answer categories per id per item
     unique_counts <- raw_data %>%
       group_by(id) %>%
       summarise(across(all_of(feature_names), ~ n_distinct(.)), .groups = "drop")
     
     # keep only ids with count ≥ 10 in every item/feature 
     ids_to_keep <- unique_counts %>%
       filter(if_all(everything(), ~ . >= 10)) %>% # is already grouped by id, so i can use every column because column counter has > 10 unique categories per id
       pull(id)
     
     raw_data <- raw_data %>% filter(id %in% ids_to_keep)
     
     # check: how many ids were excluded? 
     n_id_2 = length(unique(raw_data$id))
     # 38 exclusions!
     } else if (def_low_var == "one") {
     # check for ids with items with var ≤ 1 (hier sd ≤1)
     var_features <- raw_data %>%
       group_by(id) %>%
       summarise(across(all_of(feature_names), ~ sd(., na.rm = T)), groups = "drop") 
       
     ids_var <- var_features %>%
       filter(if_all(everything(), ~ . >= 1)) %>%
       pull(id)
     
     raw_data <- raw_data %>%
       filter(id %in% ids_var)
     
     n_id_2 <-  length(unique(raw_data$id))
     # excludes 9 participants
} else {
  print("cutoff not implemented")
}

# --- MISSINGNESS ---------------------------------------------------------------------------------
# Remove ids with too many missing rows (based on 2*std more than the mean of missing rows per id)
beep_feature_names

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

cutoff <- mean(missing_rows_per_id$n_missing_rows) +
  2 * sd(missing_rows_per_id$n_missing_rows)


# plot distribution of max missing rows and cut off value
ggplot(missing_rows_per_id, aes(x = n_missing_rows)) +
  geom_bar() +
  geom_vline(xintercept = cutoff, color = "red", linewidth = 1) +
  labs(
    x = "Number of Missing Rows (per ID)",
    y = "Count of Participants",
    title = "Distribution of Missing Rows per ID"
  ) +
  theme_minimal()

valid_ids <- missing_rows_per_id %>%
  filter(n_missing_rows <= cutoff) %>%
  pull(id)

raw_data_long <- raw_data_long %>%
  filter(id %in% valid_ids)

raw_data <- raw_data %>%
  filter(id %in% valid_ids)

n_id_3 = length(unique(raw_data$id))
# excludes 6 participants


# Compute consecutive missing rows per id and exclude ids with 
# more than 5 consecutive missing rows (one whole day)
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


n_id_4 = length(unique(raw_data_long$id))
# excludes one participant

# check 
length(unique(raw_data$id)) == length(unique(raw_data_long$id))

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
#check 
n_id_5 = length(unique(raw_data_long$id))
# n_id_5=37


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
n_id_6 = length(unique(raw_data_long$id))


# ---------------------Descriptive statistics---------------------------------------------------------------
# range of answer categories (all items have the same possible answer categories 0-100)
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


# plot 4 example features
raw_data_long %>%
  dplyr::filter(item %in% feature_names[13:16]) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  facet_wrap(~ item) +
  theme_classic()

# plot person specific counts for example id
example_id <- 68177

raw_data_long %>%
  dplyr::filter(id == example_id, item == feature_names[2]) %>%
  ggplot(ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 20, na.rm = TRUE) +
  ggplot2::theme_classic()


#ACF_plot(raw_data_long, chosen_item = "positive_physical_health_behavior")
ACF_plot(raw_data_long, chosen_item = "depressed")

#---------------------------------------------------------------------------------------------------

#################################### Prepare Data for HPO ##########################################

# Interpolate
interpolation_type = "spline"

raw_data_long_imp <- interpolate(raw_data_long, train_counters, interpolation_type)

# Check if there are any NAs left
sum(is.na(raw_data_long_imp %>% filter(counter %in% train_counters)))



# Source for AR(1)-DF-Test and Stationarity Transformations:
# Ryan et al. (2025) (adf_flow in diagnose_trend_type)
# Compute detrending components to detrend data
trend_parameter_hpo <- diagnose_trend_type(raw_data_long_imp %>% filter(counter %in% train_counters))

# Diff and detrend whole dataset
raw_data_long_imp_hpo <- diff_and_detrend(raw_data_long_imp, trend_parameter_hpo)

# Compute mean and sd for scaling data
std_stats_hpo = raw_data_long_imp_hpo %>% filter(counter %in% train_counters) %>%
  group_by(id, item) %>%
  summarise(
    mean_hpo = mean(value, na.rm = TRUE),
    sd_hpo   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Scale Data
raw_data_long_hpo <- raw_data_long_imp_hpo %>%
  left_join(std_stats_hpo, by = c("id", "item")) %>%
  mutate(value = (value - mean_hpo) / sd_hpo) %>%
  dplyr::select(-mean_hpo, -sd_hpo)

# Create a data frame for HPO (wide format)
df_hpo <- raw_data_long_hpo %>%
  pivot_wider(names_from = item, values_from = value) %>%
  arrange(id, counter)

# Test if data is standardized correctly
mean(raw_data_long_hpo %>% filter(id == unique(raw_data_long_hpo$id)[1], item == "depressed", counter %in% train_counters) %>% dplyr::pull(value))


################################### Prepare Data for Eval ###########################################
# Interpolate
raw_data_long_imp2 <- interpolate(raw_data_long, train_val_counters, interpolation_type)

# Check if there are any NAs left
sum(is.na(raw_data_long_imp2 %>% filter(counter %in% train_val_counters)))

# Remove IDs with missings in val data
raw_data_long_imp2 <- raw_data_long_imp2 %>%
  filter (!(id %in% ids_with_nas_val)) 
length(unique(raw_data_long_imp2$id))

# Compute detrending components to detrend data
trend_parameter_eval <- diagnose_trend_type(raw_data_long_imp2 %>% filter(counter %in% train_val_counters))

# Diff and detrend whole dataset
raw_data_long_imp_eval <- diff_and_detrend(raw_data_long_imp2, trend_parameter_eval)

# Compute mean and sd for scaling data
std_stats_eval = raw_data_long_imp_eval %>% filter(counter %in% train_val_counters) %>%
  group_by(id, item) %>%
  summarise(
    mean_eval = mean(value, na.rm = TRUE),
    sd_eval   = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# Scale Data
raw_data_long_eval <- raw_data_long_imp_eval %>%
  left_join(std_stats_eval, by = c("id", "item")) %>%
  mutate(value = (value - mean_eval) / sd_eval) %>%
  dplyr::select(-mean_eval, -sd_eval)

# Create a data frame for HPO (wide format)
df_eval <- raw_data_long_eval %>%
  pivot_wider(names_from = item, values_from = value) %>%
  arrange(id, counter)

# Test if data is standardized correctly
mean(raw_data_long_eval %>% filter(id == unique(raw_data_long_eval$id)[1], item == "depressed", counter %in% train_val_counters) %>% dplyr::pull(value))



#########################################################################################################################
# Regularized Linear Regression with lagged features and bootstrapping for uncertainty estimation
########################################################################################################################
id_col <- "id"
time_col <- "counter"
# target_item <- "positive_physical_health_behavior"
target_item <- "depressed"


y_test_raw <- raw_data_long_imp %>% filter(item == target_item, counter %in% c(val_counters, test_counters))


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
        # CAVE: wird unten überschrieben mit train_val
        X_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
        y_train <- as.matrix(df_hpo_i %>% filter(counter %in% train_counters, id == id_i) %>% dplyr::select(target_item, -id))
        # CAVE: test dataset means val dataset in HPO
        X_test <- as.matrix(df_hpo_i %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
        y_test <- as.vector(as.matrix(y_test_raw %>% filter(counter %in% val_counters, id == id_i) %>% dplyr::select(value)))
        
        set.seed(47)
        
        # lambda opt
        glm_fit <- glmnet(
          X_train, y_train,
          alpha = alpha_i,
          standardize = FALSE,
        )
        print(glm_fit)
        
        
        preds_hpo <- predict(glm_fit, newx = X_test) # Shape ( one row = all lambdas for one time point, one column = one time series (lambda-specific))
        
        # do NOT use cv.glmnet --> does not take into account that preds are time series
        # • “lambda.min”: the λ at which the smallest MSE is achieved. (with CV)
        # • “lambda.1se”: the largest λ at which the MSE is within one standard error of the smallest MSE (default).
        
        
        # Undo standardization
        mean_ <- (std_stats_hpo %>% filter(id == id_i, item == target_item))$mean_hpo
        sd_ <- (std_stats_hpo %>% filter(id == id_i, item == target_item))$sd_hpo
        preds_hpo <- preds_hpo * sd_ + mean_
        
        
        # Undo detrending/differencing
        transformation_hpo <- trend_parameter_hpo %>%
          filter(id == id_i, item == target_item) %>%
          pull(transformation)
        params_hpo <- trend_parameter_hpo %>%
          filter(id == id_i, item == target_item) %>%
          pull(params)
        preds_hpo <- undo_diff_and_detrend_matrix(preds_hpo, val_counters, transformation_hpo, params_hpo)
        
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
      }
    }
  }


length(unique(df_hpo_i$id))



# Save hps per id which maximize accuracy
optim_criteria <- "RMSE" #TODO change to sMAPE or MAE?
opt_hps <- val_metrics %>%
  group_by(id) %>%
  filter(.data[[optim_criteria]] == min(.data[[optim_criteria]], na.rm = TRUE)) %>%
  slice_head(n = 1) %>% # debug: multiple HP combo solutions for min-RMSE per id, arbitrarily take the first HP combo 
  ungroup() %>% 
  dplyr::select(id, n_lags, lambda, alpha, RMSE, sMAPE, MAE)


# save as apa style table
 #Format numeric columns APA-style
 opt_hps_apa <- opt_hps %>%
   mutate(
     RMSE  = round(RMSE, 3),
     sMAPE = round(sMAPE, 3),
     MAE   = round(MAE, 3),
     # lambda = format(lambda, scientific = TRUE, digits = 3)
    lambda = formatC(lambda, format = "f", digits = 2)
  )

#apa_tab <- xtable(
#  opt_hps_apa,
#  caption = "Optimal Hyperparameters per Participant",
#  label   = "tab:opt_hps"
#)
#
#print(
#  apa_tab,
#  include.rownames = FALSE,
#  sanitize.text.function = identity,
#  comment = FALSE
#)
#

################################################# Linear Forecast #######################################################

# Fit glm and predict target_item per id
test_metrics <- tibble() # for accuracy metrics
glm_results <- tibble()
all_predictions <- tibble()
sensitivity_results <- tibble()  # für Sensitivitätsanalyse (Leakage vs Clean Zone)
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
              
              ## so one row contains id, counter, target_item at that counter-time,
              # lagged values of ALL features INCLUDING the target_item 
              
              #designmatrix is created per id, so it contains as many lags as specified in n_lags_i!
              # Designmatrix (all features = all relevant items + their lags!) Good to know:  -target_item only removes target_item at timepoint t.
              X_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
              # target item matrix (matrix, because every value within counter %in% train_val_counter is stored in here)
              y_train <- as.matrix(df_eval_i %>% filter(counter %in% train_val_counters, id == id_i) %>% dplyr::select(target_item))
              # test design matrix (all features = all relevant items + their lags) --> 
              #CAVE: for counter 1- n_lags: lag variables stem from training + val set!
              X_test <- as.matrix(df_eval_i %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(-target_item, -id, -counter))
              # test target item matrix forming (X,y) with X_test (no target item values from training_val set)
              y_test <- as.matrix(y_test_raw %>% filter(counter %in% test_counters, id == id_i) %>% dplyr::select(value))
              
              set.seed(47)
              
              glm_fit <- glmnet(X_train, y_train, alpha = alpha_i, lambda = lambda_i, standardize = FALSE) #data is already z-transformed
              print(glm_fit)
              

              # save number of non zero coefficients and coefficients
              glm_summary <- capture.output(print(glm_fit)) %>% paste(collapse = "\n")
              
              # coef
              coef_df <- as.data.frame(as.matrix(coef(glm_fit)))
              colnames(coef_df) <- "estimate"
              coef_df$term <- rownames(coef_df)
              rownames(coef_df) <- NULL
              
              # save predictions
              preds <- as.vector(predict(glm_fit, newx = X_test, s = lambda_i)) 
              
              # Undo standardization
              mean_ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_eval
              sd_ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_eval
              preds <- preds * sd_ + mean_

              # Undo detrending/differencing
              transformation <- trend_parameter_eval %>%
                filter(id == id_i, item == target_item) %>%
                pull(transformation)
              params <- trend_parameter_eval %>%
                filter(id == id_i, item == target_item) %>%
                pull(params)
              preds <- undo_diff_and_detrend_single(preds, test_counters, transformation, params)
             
              #store preds
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
              
             
              test_metrics_i <- compute_metrics(preds, y_test) # accuracy metrics per id
              test_metrics_i <- test_metrics_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i) # add optimized HPO
              test_metrics <- bind_rows(test_metrics, test_metrics_i) # accuracy metrics matrix (all ids)
              #n_coef <- bind_rows(n_coef, n_nonzero_coef_i)
              #coefficients <- bind_rows(coefficients, coefficients_i)
              
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
              
              errors_df <- tibble(
                id = id_i,
                counter = test_counters,
                y_true = as.numeric(y_test),
                y_pred = as.numeric(preds),
              ) %>%
                mutate(
                  zone = if_else(row_number() <= n_lags_i, "leakage_zone", "clean_zone") # TODO: leakage eig nur in zone n_lags-1? NEIN, passt, siehe IPAD
                )
              
              # Fehler (RMSE) pro Zone
              zone_metrics <- errors_df %>%
                group_by(id, zone) %>%
                summarise(
                  RMSE = sqrt(mean((y_true - y_pred)^2, na.rm = TRUE)),
                  .groups = "drop"
                )
              
              sensitivity_results <- bind_rows(sensitivity_results, zone_metrics) 
}


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


# Auswertung Sensitivitätsanalyse
# RMSE pro id pro zone 
sensitivity_wide <- sensitivity_results %>%
  pivot_wider(names_from = zone, values_from = RMSE)

# Signifikanztest: Leakage-Effekt vorhanden? n = 20 --> non-parametrischer Test (Permutationstest)

# Visualisierung

ggplot(sensitivity_results, aes(x = zone, y = RMSE, fill = zone)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "RMSE in leakage vs. clean zone across participants",
    x = NULL,
    y = "RMSE"
  ) +
  theme_minimal()


# Wide-Format:
sensitivity_wide <- sensitivity_results %>%
  select(id, zone, RMSE) %>%
  pivot_wider(names_from = zone, values_from = RMSE)

# observed mean difference
obs_diff <- mean(sensitivity_wide$leakage- sensitivity_wide$clean, na.rm = TRUE)
cat("Observed mean difference (leak - clean):", round(obs_diff, 4), "\n")

#obs_diff_2 <- mean((sensitivity_results %>% filter(zone == "leakage"))$RMSE - (sensitivity_results %>% filter(zone == "clean"))$RMSE)
#obs_diff_2

# TODO: Ask if Permutationtest is valid in this scenario?
# bei HPO tlw. ids mit n_lag = 1 --> RMSE in leakage zone basiert auf einer differenz, während für 
# die gleiche id der RMSE der clean zone auf 14 Differenzen beruht.


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
#TODO: Ask wie detailliert Ergebnisse der Regularisierung berichtet werden, z.B. Anzahl Parameter ≠ 0 bei lasso etc


########################################################################################################################
####################################### Bootstrapping for Prediction Intervals #########################################
########################################################################################################################
# Problem: wenn ich block bootstrapping wie in Xu & Xie (2021) anwende: 
# pro Block 20-50 obs. 
#--> z.B. 3 * 30 bootstrap Blöcke 
# --> angenommen opt. n_lag = 3 --> ensemble model wird auf 27 obs. gefittet
# 3 ensemble models, welche insgesamt auf 81 beobachtungen basieren 
# --> TODO: neue datenstruktur? oder kann ich nach counter filtern? 
# dann hätte ich 3 Blöcke aus denen mit zurücklegen gezogen werden kann. 
# 

n_bootstrap <- 500 

# Fit glm and predict target item per id
test_metrics_boot <- tibble() #accuracy metrics
test_predictions_boot <- tibble() # mean_preds, median_preds, y_test, PI, and PI eval 
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
  
  # create matrix for bootstrapped preds
  all_preds <- matrix(NA, nrow = n_bootstrap, ncol = nrow(X_test))
  # TODO col.names<-(all_preds) counteranzahl ab 1 + n_lags pro id 
  # row = bootstrap Durchlauf
  # column = counter zest set 
  
  # Bootstrapping
  for(i in 1:n_bootstrap) {
    # ziehe  nrow(X_train) viele Zeilen aber mit zurücklegen 
    sample_idx <- sample(1:nrow(X_train), size = nrow(X_train), replace = TRUE) 
    X_sample <- X_train[sample_idx, , drop = FALSE] #should have 90 - n_lags rows
    y_sample <- y_train[sample_idx] #should have 90 - n_lags values
    
    # Fit glm to bootstrap sample
    glm_fit <- glmnet(X_sample, y_sample, alpha = alpha_i, standardize = FALSE, trace.it = T, lambda=lambda_i) # Already STD
    
    # Predict on test set using a set lambda
    preds <- predict(glm_fit, newx = X_test, s = lambda_i)
    
    # Undo standardization
    mean___ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_eval
    sd___ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_eval
    preds <- preds * sd___ + mean___
    
    # Undo detrending/differencing
    transformation <- trend_parameter_eval %>%
      filter(id == id_i, item == target_item) %>%
      pull(transformation)
    params <- trend_parameter_eval %>%
      filter(id == id_i, item == target_item) %>%
      pull(params)
    preds <- undo_diff_and_detrend_single(preds, test_counters, transformation, params)
    all_preds[i, ] <- as.vector(preds)
  }
  
  #??apply # 2 indicates columns
  # Compute mean and median predictions
  #reminder all_preds:  rows = n_bootstrap durchläufe, columns = n_counter in test set
  mean_preds <- apply(all_preds, 2, mean) #mean aller bootstrap durchläufe
  median_preds <- apply(all_preds, 2, median)
  
  # Compute prediction intervals (2.5% and 97.5%)
  pred_lower <- apply(all_preds, 2, quantile, probs = 0.025)
  pred_upper <- apply(all_preds, 2, quantile, probs = 0.975)
  
  # Compute metrics
  test_metrics_i <- compute_metrics(y_test, mean_preds, pred_lower, pred_upper)
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



# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_boot$id)[5:8]) {
  
  y_test <- (test_predictions_boot %>% filter(id == example_id))$y_test
  y_pred_mean <- (test_predictions_boot %>% filter(id == example_id))$mean_preds
  y_pred_median <- (test_predictions_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_boot %>% filter(id == example_id))$pred_upper
  
  
  # Prepare a tidy dataframe and convert to long format for plotting
  boot_plot_df <- data.frame(
    time = 1:length(y_test),
    true = y_test,
    mean_forecast = y_pred_mean,
    # median_forecast = y_pred_median,
    lower = lower,
    upper = upper
  )
  
  
  # Plot Forecast with uncertainty intervals
  print(ggplot(boot_plot_df, aes(x = time)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
          geom_line(aes(y = mean_forecast, color = "Mean forecast"), linewidth = 1) +
          # geom_line(aes(y = median_forecast, color = "Median forecast"), linewidth = 1) +
          geom_point(aes(y = true, color = "True values"), shape = 16) +
          geom_line(aes(y = true, color = "True values"), linetype = "dashed") +
          scale_color_manual(values = c("Mean forecast" = "red",
                                        "Median forecast" = "blue",
                                        "True values" = "black")) +
          labs(title = "Forecast with Bootstrapped Prediction Interval",
               x = "Time step",
               y = "Target value",
               color = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}




# plot observed vs predicted

ggplot(test_predictions_boot, aes(x = y_test, y = mean_preds)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ id, scales = "free") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = "Observed vs Predict per ID"
  ) +
  theme_minimal()


test_predictions_boot <- test_predictions_boot %>%
  mutate(resid = y_test - mean_preds)

ggplot(test_predictions_boot, aes(x = mean_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual plot"
  ) +
  theme_minimal()


ggplot(test_predictions_boot, aes(x = mean_preds, y = resid)) +
  
  # Punktwolke
  geom_point(aes(color = abs(resid)),
             alpha = 0.45, size = 2) +
  
  # Smooth line (zeigt systematische Biases)
  geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 1.1) +
  
  # Zero-line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  
  scale_color_gradient(low = "#74add1", high = "#d73027",
                       name = "|Residual|") +
  
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot",
    subtitle = "systematic over- and underestimation"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )






########### plot RLR-BS with historical data#######
# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_boot$id)[9:12]) {
  #TODO: historical data mit plotten
  # Extract training + test predictions for this ID
  y_train <- (raw_data_long_imp2 %>% filter(id == example_id, counter %in% train_val_counters, item == target_item))$value
  y_test  <- (test_predictions_boot %>% filter(id == example_id))$y_test
  y_pred_mean   <- (test_predictions_boot %>% filter(id == example_id))$mean_preds
  y_pred_median <- (test_predictions_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_boot %>% filter(id == example_id))$pred_upper
  
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
    mean_forecast = y_pred_mean,
    median_forecast = y_pred_median,
    lower = lower,
    upper = upper,
    type = "Test"
  )
  
  # Merge for plotting
  boot_plot_df <- df_test
  
  print(
    ggplot() +
      geom_ribbon(
        data = boot_plot_df,
        aes(x = time, ymin = lower, ymax = upper, fill = "95% Prediction Interval"),
        alpha = 0.2
      ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = mean_forecast, color = "Mean forecast"),
        linewidth = 1
      ) +
       geom_line(
        data = boot_plot_df,
        aes(x = time, y = median_forecast, color = "Median forecast"),
        linewidth = 1
       ) +
      geom_point(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        shape = 16
      ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        linetype = "dashed"
      ) +
      geom_line(
        data = df_hist,
        aes(x = time, y = true, color = "Historical data"),
        linewidth = 1
      ) +
      scale_color_manual(values = c(
        "Mean forecast" = "red",
        "Median forecast" = "blue",
        "Observed values" = "black",
        "Historical data" = "grey40"
      )) +
      scale_fill_manual(values = c(
        "95% Prediction Interval" = "red"
      )) +
      labs(
        title = paste0("Regularized linear Regression with bootstrapped PI — ID ", example_id),
        x = "Time step",
        y = "Target value",
        color = "",
        fill  = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
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
    


      # Predict on test set 
      B <- 500 
      
      boot <- resid_bootstrap(
        X_train = X_train,
        y_train = y_train,
        X_test  = X_test,
        alpha   = alpha_i,
        lambda  = lambda_i,
        B       = B,
        seed    = 47,
        standardize = FALSE
      )
      
      # Bootstrap-Verteilung 
      mu_boot <- boot$mu_boot         # n_test x B
      mu_point <- boot$mu_point       # n_test
      
      pred_lower <- apply(mu_boot, 1, quantile, probs = 0.025, na.rm = TRUE)
      pred_upper <- apply(mu_boot, 1, quantile, probs = 0.975, na.rm = TRUE)
      
      # bootstrap mean as point forecast (or median?)
      mu_mean <- rowMeans(mu_boot, na.rm = TRUE)
      
      
      # Undo standardization
      mean_ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_eval
      sd_   <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_eval
      
      mu_mean  <- mu_mean  * sd_ + mean_
      pred_lower <- pred_lower * sd_ + mean_
      pred_upper <- pred_upper * sd_ + mean_
      
      # Undo detrending/differencing
      transformation <- trend_parameter_eval %>%
        filter(id == id_i, item == target_item) %>%
        pull(transformation)
      
      params <- trend_parameter_eval %>%
        filter(id == id_i, item == target_item) %>%
        pull(params)
      
      mu_mean  <- undo_diff_and_detrend_single(mu_mean,  test_counters, transformation, params)
      pred_lower <- undo_diff_and_detrend_single(pred_lower, test_counters, transformation, params)
      pred_upper <- undo_diff_and_detrend_single(pred_upper, test_counters, transformation, params)
      
  
    #store preds and PIs for plotting
   test_predictions_resid_boot_i <- tibble(
      id = id_i,
      counter = test_counters,
      y_test = as.numeric(y_test),
      mean_preds = as.numeric(mu_mean),
      pred_lower = as.numeric(pred_lower),
      pred_upper = as.numeric(pred_upper)
    )
    
    
   test_predictions_resid_boot <- bind_rows(test_predictions_resid_boot, test_predictions_resid_boot_i)
   
    
  
  # Compute metrics
  test_metrics_resid_i <- compute_metrics(y_test, mu_mean, pred_lower, pred_upper)
  test_metrics_resid_i <- test_metrics_resid_i %>% mutate(id = id_i, n_lags=n_lags_i, alpha=alpha_i, lambda = lambda_i)
  test_metrics_resid_boot <- bind_rows(test_metrics_resid_boot, test_metrics_resid_i)
  
  
}

# UQ Comparison: bootstrapping vs. bootstrap using resampled residuals
uq_comparison_table <- tibble(
  Method = c(
    "Standard Bootstrap",
    "Bootstrap - Resampled Residuals"
  ),
  `Coverage (Mean)` = c(
    mean(test_metrics_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_resid_boot$Coverage, na.rm = TRUE)
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
    )
  ),
  `Interval width (Mean)` = c(
    mean(test_metrics_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_resid_boot$interval_width, na.rm = TRUE)
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
    )
  )
)

uq_comparison_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 3,
    caption = "Comparison of UQ methods: Standard Bootstrap vs. Bootstrap - Resampled Residuals",
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


# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_boot$id)[5:8]) {
  
  y_test <- (test_predictions_resid_boot %>% filter(id == example_id))$y_test
  y_pred_mean <- (test_predictions_resid_boot %>% filter(id == example_id))$mean_preds
  # y_pred_median <- (test_predictions_resid_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_resid_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_resid_boot %>% filter(id == example_id))$pred_upper
  
  
  # Prepare a tidy dataframe and convert to long format for plotting
  boot_plot_df <- data.frame(
    time = 1:length(y_test),
    true = y_test,
    mean_forecast = y_pred_mean,
   # median_forecast = y_pred_median,
    lower = lower,
    upper = upper
  )
  
  
  # Plot Forecast with uncertainty intervals
  print(ggplot(boot_plot_df, aes(x = time)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
    geom_line(aes(y = mean_forecast, color = "Mean forecast"), linewidth = 1) +
   # geom_line(aes(y = median_forecast, color = "Median forecast"), linewidth = 1) +
    geom_point(aes(y = true, color = "True values"), shape = 16) +
    geom_line(aes(y = true, color = "True values"), linetype = "dashed") +
    scale_color_manual(values = c("Mean forecast" = "red",
                                  "Median forecast" = "blue",
                                  "True values" = "black")) +
    labs(title = "Forecast with Bootstrapped Prediction Interval",
         x = "Time step",
         y = "Target value",
         color = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}




# plot observed vs predicted

ggplot(test_predictions_resid_boot, aes(x = y_test, y = mean_preds)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ id, scales = "free") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = "Observed vs Predict per ID"
  ) +
  theme_minimal()


test_predictions_resid_boot <- test_predictions_resid_boot %>%
  mutate(resid = y_test - mean_preds)

ggplot(test_predictions_resid_boot, aes(x = mean_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual plot"
  ) +
  theme_minimal()




ggplot(test_predictions_resid_boot, aes(x = mean_preds, y = resid)) +
  
  # Punktwolke
  geom_point(aes(color = abs(resid)),
             alpha = 0.45, size = 2) +
  
  # Smooth line (zeigt systematische Biases)
  geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 1.1) +
  
  # Zero-line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  
  scale_color_gradient(low = "#74add1", high = "#d73027",
                       name = "|Residual|") +
  
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


########### plot RLR-RESID-BS with historical data#######
# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_boot$id)[9:12]) {
  #TODO: historical data mit plotten
  # Extract training + test predictions for this ID
  y_train <- (raw_data_long_imp2 %>% filter(id == example_id, counter %in% train_val_counters, item == target_item))$value
  y_test  <- (test_predictions_resid_boot %>% filter(id == example_id))$y_test
  y_pred_mean   <- (test_predictions_resid_boot %>% filter(id == example_id))$mean_preds
 # y_pred_median <- (test_predictions_resid_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_resid_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_resid_boot %>% filter(id == example_id))$pred_upper
  
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
    mean_forecast = y_pred_mean,
   # median_forecast = y_pred_median,
    lower = lower,
    upper = upper,
    type = "Test"
  )
  
  # Merge for plotting
  boot_plot_df <- df_test
  
  print(
    ggplot() +
      geom_ribbon(
        data = boot_plot_df,
        aes(x = time, ymin = lower, ymax = upper, fill = "95% Prediction Interval"),
        alpha = 0.2
      ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = mean_forecast, color = "Mean forecast"),
        linewidth = 1
      ) +
     # geom_line(
      #  data = boot_plot_df,
      #  aes(x = time, y = median_forecast, color = "Median forecast"),
      #  linewidth = 1
     # ) +
      geom_point(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        shape = 16
      ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        linetype = "dashed"
      ) +
      geom_line(
        data = df_hist,
        aes(x = time, y = true, color = "Historical data"),
        linewidth = 1
      ) +
      scale_color_manual(values = c(
        "Mean forecast" = "red",
       # "Median forecast" = "blue",
        "Observed values" = "black",
        "Historical data" = "grey40"
      )) +
      scale_fill_manual(values = c(
        "95% Prediction Interval" = "red"
      )) +
      labs(
        title = paste0("Elastic Net Regression with residual bootstrapped PI — ID ", example_id),
        x = "Time step",
        y = "Target value",
        color = "",
        fill  = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
}

mean(test_metrics_boot$RMSE)
median(test_metrics_boot$RMSE)
range(test_metrics_boot$RMSE)
range(test_metrics_boot$Coverage)

range(test_metrics_resid_boot$Coverage)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#--------------Random forest regression------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------
#HPO per participant

#optimal HP per ID
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
  
  
  # design matrices per id
  X_train <- as.matrix(df_hpo_i %>% 
                         filter(counter %in% train_counters, id == id_i) %>% 
                         select(-target_item, -id, -counter))
  
  y_train <- as.matrix(df_hpo_i %>% 
                         filter(counter %in% train_counters, id == id_i) %>% 
                         select(target_item))
  
  # 
  X_val <- as.matrix(df_hpo_i %>% 
                       filter(counter %in% val_counters, id == id_i) %>% 
                       select(-target_item, -id, -counter))
  
  y_val <- as.matrix(y_test_raw %>% 
                       filter(counter %in% val_counters, id == id_i) %>% 
                       select(value))
  
 
   p <- ncol(X_train)
  hpo_grid <- expand.grid(
    mtry     = unique(round(c( p/5, p/4, p/3))), #Number of variables randomly sampled as candidates at each split.
    nodesize = c(3, 5, 10, 15), # Minimum size of terminal nodes. default = 5 
    ntree = c(200, 500, 1000)
    # maxnodes = # wird auch häufig festgelegt, kein klassischer hyperparameter
    # max tree depth gibt es in randomforest paket nicht
  )
  
  # maxnodes Maximum number of terminal nodes trees in the forest can have. If not given,
  #trees are grown to the maximum possible (subject to limits by nodesize). If set
  #larger than maximum possible, a warning is issued.
  
  #  create tibble to store HPs
  hpo_RFR_results <- tibble()
  
  #  loop over hyperparameter combinations 
  for (h in 1:nrow(hpo_grid)) {
    
    mtry_i     <- hpo_grid$mtry[h]
    nodesize_i <- hpo_grid$nodesize[h]
    ntree_i     <- hpo_grid$ntree[h]
    
    set.seed(47)
    
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
    
    
    # Undo standardization
    mean_ <- (std_stats_hpo %>% filter(id == id_i, item == target_item))$mean_hpo
    sd_ <- (std_stats_hpo %>% filter(id == id_i, item == target_item))$sd_hpo
    preds_hpo_RFR <- preds_hpo_RFR * sd_ + mean_
    #print(preds_hpo_RFR)
    
    # Undo detrending/differencing 
    transformation_hpo <- trend_parameter_hpo %>%
      filter(id == id_i, item == target_item) %>%
      pull(transformation)
    params_hpo <- trend_parameter_hpo %>%
      filter(id == id_i, item == target_item) %>%
      pull(params)
    preds_hpo_RFR<- undo_diff_and_detrend_single(preds_hpo_RFR, val_counters, transformation_hpo, params_hpo)
    
    
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
  
  print("One Iteration of RFR HPO Done.")
  
}
# ergebnistabelle

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

#----------------

# Fit random forest regression and predict target_item per id
test_metrics_RFR <- tibble() 
test_predictions_RFR <- tibble()
sensitivity_results_RFR <- tibble()

for (id_i in unique(df_hpo$id)) {
  
  # get optimal hyperparameters per id 
  opt_row <- opt_hpo_RFR %>% filter(id == id_i)
  n_lags_i     <- opt_row$n_lags
  opt_mtry     <- opt_row$mtry
  opt_nodesize <- opt_row$nodesize
  
  # create lagged dataset 
  df_eval_i <- create_lag_features(
    df = df_eval, 
    id_col = id_col,
    time_col = time_col,
    target_col = target_item,
    n_lags = n_lags_i,
    numeric_features=features_to_lag
  )
  
  #  design matrices 
  X_train <- as.matrix(df_eval_i %>% 
                         filter(counter %in% train_val_counters, id == id_i) %>% 
                         select(-target_item, -id, -counter))
  
  y_train <- as.matrix(df_eval_i %>% 
                         filter(counter %in% train_val_counters, id == id_i) %>% 
                         select(target_item))
  
  X_test <- as.matrix(df_eval_i %>% 
                        filter(counter %in% test_counters, id == id_i) %>% 
                        select(-target_item, -id, -counter))
  
  y_test <- as.matrix(y_test_raw %>% 
                        filter(counter %in% test_counters, id == id_i) %>% 
                        select(value))
  
  # fit final models with opt. HP 
  
  set.seed(47)
  
  qrf_fit <- quantregForest(
    x        = X_train,
    y        = y_train,
    ntree    = 500,              
    mtry     = opt_mtry,         
    nodesize = opt_nodesize      
  )
  
  #  Predict point est. + PI 
  median_preds <- predict(qrf_fit, X_test, what = 0.5)
  pred_lower   <- predict(qrf_fit, X_test, what = 0.025)
  pred_upper   <- predict(qrf_fit, X_test, what = 0.975)
  
  #  Undo standardization 
  mu_i <- std_stats_eval %>% filter(id == id_i, item == target_item) %>% pull(mean_eval)
  sigma_i <- std_stats_eval %>% filter(id == id_i, item == target_item) %>% pull(sd_eval)
  
  preds <- median_preds * sigma_i + mu_i
  
  pred_lower <- pred_lower * sigma_i + mu_i
  pred_upper <- pred_upper * sigma_i + mu_i
  
  # Undo detrending/differencing 
  transformation_i <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(transformation)
  
  params_i <- trend_parameter_eval %>%
    filter(id == id_i, item == target_item) %>%
    pull(params)
  
  preds       <- undo_diff_and_detrend_single(preds,       test_counters, transformation_i, params_i)
  pred_lower  <- undo_diff_and_detrend_single(pred_lower,  test_counters, transformation_i, params_i)
  pred_upper  <- undo_diff_and_detrend_single(pred_upper,  test_counters, transformation_i, params_i)
  
  #  Evaluate 
  test_metrics_i <- compute_metrics(
    y_obs      = y_test,
    y_pred     = preds,
    pred_lower = pred_lower,
    pred_upper = pred_upper
  ) %>% 
    mutate(
      id      = id_i,
      n_lags  = n_lags_i,
      mtry    = opt_mtry,
      nodesize= opt_nodesize
    )
  
  test_metrics_RFR <- bind_rows(test_metrics_RFR, test_metrics_i)
  
  
  # Save test predictions for plotting
  test_predictions_RFR <- bind_rows(test_predictions_RFR, tibble(
    median_preds=preds,
    y_test=y_test,
    pred_lower=pred_lower,
    pred_upper=pred_upper,
    id=id_i,
  ))
  
  #  Zone-wise RMSE 
  zone_metrics_RFR <- errors_df %>%
    group_by(id, zone) %>%
    summarise(
      RMSE = sqrt(mean((y_true - y_pred)^2, na.rm = TRUE)),
      .groups = "drop"
    )
  
  sensitivity_results_RFR <- bind_rows(sensitivity_results_RFR, zone_metrics_RFR) 
}




########### plot forecasts of RFR with historical data#######
# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_RFR$id)[6:10]) {
  #TODO: historical data mit plotten
  # Extract training + test predictions for this ID
  y_train <- (raw_data_long_imp2 %>% filter(id == example_id, counter %in% train_val_counters, item == target_item))$value
  y_test  <- (test_predictions_RFR %>% filter(id == example_id))$y_test
  y_pred_median <- (test_predictions_RFR %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_RFR %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_RFR %>% filter(id == example_id))$pred_upper
  
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
  boot_plot_df <- df_test
  
  
  # Plot
  print(
    ggplot() +
      geom_ribbon(
        data = boot_plot_df,
        aes(x = time, ymin = lower, ymax = upper, fill = "95% Prediction Interval"),
        alpha = 0.2
      ) +
   #  geom_line(
   #    data = boot_plot_df,
   #    aes(x = time, y = mean_forecast, color = "Mean forecast"),
   #    linewidth = 1
   #  ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = median_forecast, color = "Median forecast"),
        linewidth = 1
      ) +
      geom_point(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        shape = 16
      ) +
      geom_line(
        data = boot_plot_df,
        aes(x = time, y = true, color = "Observed values"),
        linetype = "dashed"
      ) +
      geom_line(
        data = df_hist,
        aes(x = time, y = true, color = "Historical data"),
        linewidth = 1
      ) +
      scale_color_manual(values = c(
        "Mean forecast" = "red",
        "Median forecast" = "blue",
        "Observed values" = "black",
        "Historical data" = "grey40"
      )) +
      scale_fill_manual(values = c(
        "95% Prediction Interval" = "red"
      )) +
      labs(
        title = paste0("Regularized linear Regression with bootstrapped PI — ID ", example_id),
        x = "Time step",
        y = "Target value",
        color = "",
        fill  = ""
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
}  
### residual plot rlr bs
#----------------------------------------------------------------------------------------
test_predictions_resid_boot <- test_predictions_resid_boot %>%
  mutate(resid = y_test[,1] - median_preds)

ggplot(test_predictions_resid_boot, aes(x = median_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot RLR - BS"
  ) +
  theme_minimal()




ggplot(test_predictions_resid_boot, aes(x = median_preds, y = resid)) +
  
# Punktwolke
geom_point(aes(color = abs(resid)),
           alpha = 0.45, size = 2) +
  
  # Smooth line 
  geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 1.1) +
  
  # Zero-line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  
  scale_color_gradient(low = "#74add1", high = "#d73027",
                       name = "|Residual|") +
  
  labs(
    x = "Predicted value",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot RLR-BS"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

#----------------------------------------------------------------------------------------
# resudual plot RFR 

test_predictions_RFR <- test_predictions_RFR %>%
  mutate(resid = y_test[,1] - median_preds)

ggplot(test_predictions_RFR, aes(x = median_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual plot RFR"
  ) +
  theme_minimal()




ggplot(test_predictions_RFR, aes(x = median_preds, y = resid)) +
  
  # Punktwolke
  geom_point(aes(color = abs(resid)),
             alpha = 0.45, size = 2) +
  
  # Smooth line (zeigt systematische Biases)
  geom_smooth(method = "loess", se = FALSE, color = "#2c3e50", linewidth = 1.1) +
  
  # Zero-line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
  
  scale_color_gradient(low = "#74add1", high = "#d73027",
                       name = "|Residual|") +
  
  labs(
    x = "Predicted value",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot RFR"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

#----------------------------------------------------------------------------------------------------

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

# --- 1. RLR Bootstrap aggregieren ---
df_RLRB <- test_metrics_boot %>%
  group_by(id) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    Coverage = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE)
  ) %>%
  mutate(Model = "RLRB")

# --- 2. RFR aggregieren ---
df_RFR <- test_metrics_RFR %>%
  group_by(id) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    Coverage = mean(Coverage, na.rm = TRUE),
    interval_width = mean(interval_width, na.rm = TRUE)
  ) %>%
  mutate(Model = "RFR")

# --- 3. Zusammenführen ---
df_all <- bind_rows(df_RLRB, df_RFR)


# --- 3. Breit pivotieren: eine Zeile pro ID, Spalten = Modell ---
df_wide <- df_all %>%
  pivot_wider(
    names_from = Model,
    values_from = c(RMSE, Coverage, interval_width)
  )

# --- 4. Werte kombinieren: "Bootstrap / RFR" ---
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




