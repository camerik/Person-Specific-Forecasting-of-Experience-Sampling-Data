
# Load in packages
packages <- c("dplyr", "tidyr", "zoo", "purrr", "Metrics", "ggplot2", "fpp3", "fable") 

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})

# --- Full dataset ---
# raw_data_long_imp_2 = spline imputed missings in train+val data

ts_all <- raw_data_long_imp2 %>% 
  filter(item == "positive_physical_health_behavior") %>% 
  as_tsibble(key = c(id, item), index = counter)

ts_train_val <- ts_all %>% 
  filter(counter %in% train_val_counters)
# horizon
H <- 15  

# store results
ar1_fc_all <- list()

for (id_k in unique(df_hpo$id)) { # nur ids au df_hpo!
  
  df_id <- ts_all %>% filter(id == id_k)
  df_train <- ts_train_val %>% filter(id == id_k)
  
  # test points: first 15 post-training observations
  test_points <- df_id %>% 
    filter(counter > max(train_val_counters)) %>% 
    slice(1:H)
  
  # Speicherobjekte
  fc_mean     <- numeric(H)
  fc_lower95  <- numeric(H)
  fc_upper95  <- numeric(H)
  
  current_series <- df_train
  
  for (h in 1:H) {
    
    # --- (1) AR(1)-Modell ---
    model_h <- current_series %>% 
      model(AR1 = ARIMA(value ~ pdq(1,0,0)))
    
    # --- (2) Forecast + Prediction Interval ---
    fc_h <- model_h %>%
      forecast(h = 1, bootstrap = TRUE, times = 1000) %>%
      hilo(level = 95)
    
    fc_h2 <- fc_h %>% unpack_hilo(`95%`)
    
    # Speichern
    fc_mean[h]    <- fc_h2$.mean
    fc_lower95[h] <- fc_h2$`95%_lower`
    fc_upper95[h] <- fc_h2$`95%_upper`
    
    # --- (3) Append true value ---
    current_series <- current_series %>% 
      add_row(
        counter = test_points$counter[h],
        id      = id_k,
        item    = "positive_physical_health_behavior",
        value   = test_points$value[h]
      )
  }
  
  # Store results per ID
  ar1_fc_all[[as.character(id_k)]] <- tibble(
    id       = id_k,
    counter  = test_points$counter,
    true     = test_points$value,
    forecast = fc_mean,
    lower95  = fc_lower95,
    upper95  = fc_upper95
  )
}

# Combine all IDs
ar1_benchmark_fc <- bind_rows(ar1_fc_all)


# --- Intervallbreite + Coverage pro beep ---
ar1_benchmark_fc <- ar1_benchmark_fc %>%
  mutate(
    interval_width = upper95 - lower95,
    coverage = true >= lower95 & true <= upper95
  )

# --- Aggregierte Metriken pro ID ---
ar1_benchmark_metrics <- ar1_benchmark_fc %>%
  group_by(id) %>%
  summarise(
    RMSE = sqrt(mean((forecast - true)^2)),
    Coverage = mean(coverage, na.rm = TRUE),
    interval_width = mean(interval_width)
  )


ar1_benchmark_metrics %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c(
      "ID",
      "RMSE",
      "Coverage",
      "Interval width"
    )
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  )


#plot

hist_plot_df <- ts_train_val %>%
  select(id, counter, value) %>%
  rename(y = value)

test_plot_df <- ts_all %>%
  filter(counter > max(train_val_counters)) %>%
  group_by(id) %>%
  slice(1:H) %>%
  ungroup() %>%
  select(id, counter, value) %>%
  rename(y = value)



for (example_id in unique(ar1_benchmark_fc$id)[6:10]) {
  
  df_hist <- hist_plot_df %>% filter(id == example_id)
  df_test <- test_plot_df %>% filter(id == example_id)
  df_fc   <- ar1_benchmark_fc %>% filter(id == example_id)
  
  print(
    ggplot() +
      
      # --- 95% Prediction Interval ---
      geom_ribbon(
        data = df_fc,
        aes(
          x = counter,
          ymin = lower95,
          ymax = upper95,
          fill = "95% Prediction Interval"  
        ),
        alpha = 0.2
      ) +
      
      # --- Forecast mean ---
      geom_line(
        data = df_fc,
        aes(
          x = counter,
          y = forecast,
          color = "Mean forecast"
        ),
        linewidth = 1
      ) +
      
      # --- Test true values ---
      geom_point(
        data = df_test,
        aes(
          x = counter,
          y = y,
          color = "Observed values"
        )
      ) +
      
      geom_line(
        data = df_test,
        aes(
          x = counter,
          y = y,
          color = "Observed values"
        ),
        linetype = "dashed"
      ) +
      
      # --- Historical values ---
      geom_line(
        data = df_hist,
        aes(
          x = counter,
          y = y,
          color = "Historical data"
        ),
        linewidth = 1
      ) +
      
      scale_color_manual(values = c(
        "Mean forecast"   = "blue",
        "Observed values"       = "black",
        "Historical data" = "grey40"
      )) +
      scale_fill_manual(values = c(
        "95% Prediction Interval" = "red"
      )) +
      labs(
        title = paste("AR(1) — ID", example_id),
        x = "Time",
        y = "positive physical health behavior",
        color = ""
      ) +
      
      theme_minimal()
  )
}


#---------------------------------------------------------------------------------------
### residual plot AR(1)
#----------------------------------------------------------------------------------------
ggplot(ar1_benchmark_fc, aes(x = true, y = forecast)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_wrap(~ id, scales = "free") +
  labs(
    x = "Observed",
    y = "Predicted",
    title = "Observed vs Predict per ID"
  ) +
  theme_minimal()


ar1_benchmark_fc <- ar1_benchmark_fc %>%
  mutate(resid = true - forecast)

ggplot(ar1_benchmark_fc, aes(x = true, y = resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Observed",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot AR(1)"
  ) +
  theme_minimal()




ggplot(ar1_benchmark_fc, aes(x = true, y = resid)) +
  
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
    x = "Observed value",
    y = "Residual (Observed – Predicted)",
    title = "Residual Plot AR(1)",
    subtitle = "Systematic over- and underestimation"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

