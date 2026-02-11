
# Load in packages
packages <- c("dplyr", "tidyr", "zoo", "purrr", "Metrics", "ggplot2", "fpp3", "fable") 

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})

# data_long_eval = Kalman imputed missings, mean stationary, z-scaled based on train+val

df_ar <- data_long_eval_dd_std  %>% 
  dplyr::filter(item == target_item) %>% 
  as_tsibble(key = c(id, item), index = counter)

# Forecasting horizon
H <- 15  

# Store results
test_predictions_ar <- tibble()
train_predictions_ar <- tibble()
test_metrics_ar <- tibble()

# Fit AR(1) model per ID, store forecasts and PIs

for (id_i in unique(df_hpo$id)) { # only ids from df_hpo!
  
  df_id <- df_ar %>% 
    dplyr::filter(id == id_i)
  
  # train set
  df_train <- df_ar %>%
    dplyr::filter(counter %in% train_val_counters, id == id_i) %>%
    dplyr::arrange(counter)
  
  # test set
  df_test <- df_ar %>% 
    dplyr::filter(counter %in% test_counters, id == id_i) %>%
    dplyr::arrange(counter)
  

    # fit AR(1)-Model
    fit<- df_train %>% 
      model(AR1 = ARIMA(value ~ pdq(1,0,0)))
    
    # get in-sample predictions via augment 
    train_pred_ar_i <- fit %>%
      augment() %>%
      transmute(
        id = id_i,
        counter,
        y_pred_train = .fitted
      ) %>%
      mutate(
        # back-transform fitted values (and optionally y_obs_train)
        y_pred_train = undo_transformations(
          y_pred_train, id_i, target_item,
          std_stats_eval, trend_parameter_eval,
          counter
        ),
        # add y_obs_train
        y_obs_train = y_obs_train)

    
    train_predictions_ar <- bind_rows(train_predictions_ar, train_pred_ar_i)
    
    
    # Forecast with fixed model parameters + Bootstrapped Prediction Interval
    fc <- fit %>%
      forecast(h = H, bootstrap = TRUE, times = 500) %>%
      hilo(level = 95) %>%
      unpack_hilo(`95%`)
    
    
    mean_preds <- undo_transformations(fc$.mean, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
    pred_lower <- undo_transformations(fc$`95%_lower`, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
    pred_upper <- undo_transformations(fc$`95%_upper`, id_i, target_item, std_stats_eval, trend_parameter_eval, df_test$counter)
    y_obs <- data_long_eval %>% filter(item == target_item, id == id_i, counter %in% test_counters) %>% arrange(counter) %>% pull(value)
    y_obs_train <- data_long_eval %>% filter(item == target_item, id == id_i, counter %in% train_val_counters) %>% arrange(counter) %>% pull(value)
  
  
  # Store results per ID
  test_predictions_ar_i <- tibble(
    id       = id_i,
    counter  = df_test$counter,
    y_obs  = y_obs,
    mean_preds     = mean_preds,
    pred_lower  = pred_lower,
    pred_upper  = pred_upper
  )
  
  # Combine all IDs
  test_predictions_ar <- bind_rows(test_predictions_ar, test_predictions_ar_i)

  
  # Compute and store test metrics
  test_metrics_ar_i <- compute_metrics(y_obs, mean_preds, pred_lower, pred_upper)
  test_metrics_ar_i <- test_metrics_ar_i %>% mutate(id = id_i)
  test_metrics_ar <- bind_rows(test_metrics_ar, test_metrics_ar_i) 
  
}



#---------------------------------------------------------------------------------------
### residual plot AR(1)
#----------------------------------------------------------------------------------------
ggplot(test_predictions_ar, aes(x = y_obs, y = mean_preds)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ id, scales = "free") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = "Observed vs Predict per ID"
  ) +
  theme_minimal()


test_predictions_ar <- test_predictions_ar %>%
  mutate(resid = y_obs - mean_preds)

ggplot(test_predictions_ar, aes(x = mean_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot AR(1)"
  ) +
  theme_minimal()




ggplot(test_predictions_ar, aes(x = mean_preds, y = resid)) +
  
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
    title = "OOS Residual Plot AR(1)",
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

