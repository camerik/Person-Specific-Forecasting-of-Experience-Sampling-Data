check_ar1_residuals <- function(
    ts_train_val,
    target_item, 
    value_col = "value",
    id_col = "id",
    time_col = "counter",
    lag_max = 30,
    lb_lags = 10,
    make_plots = TRUE
) {
  # Filter item and keep needed cols
  df <- ts_train_val %>%
    dplyr::filter(.data$item == target_item) %>%
    dplyr::select(dplyr::all_of(c(id_col, time_col, "item", value_col))) %>% # per ID
    tsibble::as_tsibble(
      key   = dplyr::all_of(c(id_col, "item")),
      index = dplyr::all_of(time_col)
    )
  
  #Fit AR(1) per ID on train_val_set
  fits <- df %>%
    model(AR1 = ARIMA(.data[[value_col]] ~ pdq(1,0,0)))
  
  # Extract residuals
  #    augment() gives .resid aligned with the time index
  aug <- fits %>%
    fabletools::augment() %>%
    dplyr::rename(resid = .resid)
  
  # perform Ljung-Box test to check if residuals show autocorrelation
  safe_lb_p <- function(x, lag) {
    x <- stats::na.omit(x)
    if (length(x) < (lag + 2)) return(NA_real_)
    stats::Box.test(x, lag = lag, type = "Ljung-Box")$p.value
  }
  
  # Summary table per ID: n, residual SD, Ljung-Box p, max abs ACF
  # ACF computed on residual series per ID
  acf_tbl <- aug %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      n_resid = sum(!is.na(resid)),
      resid_sd = stats::sd(resid, na.rm = TRUE),
      lb_pvalue = safe_lb_p(resid, lb_lags),
      max_abs_acf = {
        rr <- stats::na.omit(resid)
       {
          ac <- stats::acf(rr, plot = FALSE, lag.max = lag_max)$acf[-1] # drop lag 0
          max(abs(ac), na.rm = TRUE)
        }
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lb_flag_autocorr = dplyr::if_else(!is.na(lb_pvalue) & lb_pvalue < 0.05, TRUE, FALSE)
    )
  
  # Visualization: ACF plots per ID using feasts::ACF
  acf_plot <- NULL
  if (isTRUE(make_plots)) {
    acf_plot <- aug %>%
      dplyr::filter(!is.na(resid)) %>%
      dplyr::group_by(.data[[id_col]]) %>%
      feasts::ACF(resid, lag_max = lag_max) #%>%
     #ggplot2::autoplot() +
     #ggplot2::facet_wrap(stats::as.formula(paste("~", id_col)), scales = "free_y") +
     #ggplot2::labs(
     #  title = paste0("ACF of AR(1) training residuals — item: ", item_name),
     #  x = "Lag",
     #  y = "ACF"
      #) +
     # ggplot2::theme_minimal()
  }
  
  list(
    fits = fits,
    residuals_long = aug,
    summary = acf_tbl,
    acf_plot = acf_plot
  )
}
