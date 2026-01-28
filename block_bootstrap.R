
n_bootstrap <- 500

l_block <- 10

# Fit glm and predict target item per id
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
        X_train <- as.matrix(
          df_eval_i %>% 
            filter(counter %in% train_val_counters, id == id_i) %>% 
            dplyr::select(-target_item, -id, -counter)
        )
        y_train <- as.numeric(
          df_eval_i %>% 
            filter(counter %in% train_val_counters, id == id_i) %>% 
            dplyr::pull(target_item)
        )
        X_test <- as.matrix(
          df_eval_i %>% 
            filter(counter %in% test_counters, id == id_i) %>% 
            dplyr::select(-target_item, -id, -counter)
        )
        y_test <- as.numeric(
          y_test_raw %>% 
            filter(counter %in% test_counters, id == id_i) %>% 
            dplyr::pull(value)
        )
        
        # mean block length l for geometric block bootstrap
        n_sim <- nrow(X_train)  # length of train+val data set 
        
        
        # --- Backtransform params (same as your original code) ---
        mean___ <- (std_stats_eval %>% filter(id == id_i, item == target_item))$mean_eval
        sd___   <- (std_stats_eval %>% filter(id == id_i, item == target_item))$sd_eval
        
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
         
       View(tseries_train)
       
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



# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_block_boot$id)[5:8]) {
  
  y_test <- (test_predictions_block_boot %>% filter(id == example_id))$y_test
  y_pred_mean <- (test_predictions_block_boot %>% filter(id == example_id))$mean_preds
  y_pred_median <- (test_predictions_block_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_block_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_block_boot %>% filter(id == example_id))$pred_upper
  
  
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

ggplot(test_predictions_block_boot, aes(x = y_test, y = mean_preds)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ id, scales = "free") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = "Observed vs Predict per ID"
  ) +
  theme_minimal()


test_predictions_block_boot <- test_predictions_block_boot %>%
  mutate(resid = y_test - mean_preds)

ggplot(test_predictions_block_boot, aes(x = mean_preds, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted",
    y = "Residual (Observed – Predicted)",
    title = "Residual plot"
  ) +
  theme_minimal()


ggplot(test_predictions_block_boot, aes(x = mean_preds, y = resid)) +
  
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






########### plot Elastic Net- Block bootstrap with historical data#######
# Plot individual plots for 5 unique IDs
for (example_id in unique(test_metrics_block_boot$id)[9:12]) {
  #TODO: historical data mit plotten
  # Extract training + test predictions for this ID
  y_train <- (raw_data_long_imp2 %>% filter(id == example_id, counter %in% train_val_counters, item == "positive_physical_health_behavior"))$value
  y_test  <- (test_predictions_block_boot %>% filter(id == example_id))$y_test
  y_pred_mean   <- (test_predictions_block_boot %>% filter(id == example_id))$mean_preds
  y_pred_median <- (test_predictions_block_boot %>% filter(id == example_id))$median_preds
  lower <- (test_predictions_block_boot %>% filter(id == example_id))$pred_lower
  upper <- (test_predictions_block_boot %>% filter(id == example_id))$pred_upper
  
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


# UQ Comparison: bootstrapping vs. bootstrap using resampled residuals
uq_comparison_table <- tibble(
  Method = c(
    "Standard Bootstrap",
    "Bootstrap - Resampled Residuals"
  ),
  `Coverage (Mean)` = c(
    mean(test_metrics_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_resid_boot$Coverage, na.rm = TRUE),
    mean(test_metrics_block_boot$Coverage, na.rm = TRUE)
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
    )
  ),
  `Interval width (Mean)` = c(
    mean(test_metrics_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_resid_boot$interval_width, na.rm = TRUE),
    mean(test_metrics_block_boot$interval_width, na.rm = TRUE)
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
    )
  )
)


# Create table

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

