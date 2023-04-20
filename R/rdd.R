plot_density <- function(d, fn_full, dig = 3) {
  mccrary_p <- rdd::DCdensity(d$prev_dem_margin)
  jpeg(paste0(fn_full, "_mccrary_p_", round(mccrary_p, dig), ".jpeg"), width = 6, height = 6, units = "in", res = 300)
  rdd::DCdensity(d$prev_dem_margin)
  dev.off()
  
  NULL
}

plot_rdd <- function(d, fn_full, 
                     dig = 3, cols = config$analysis$colors) {
  late <- round(rdd::RDestimate(dem_margin ~ prev_dem_margin, d)$est["LATE"], dig)
  
  p <- d |>
    dplyr::mutate(prev_winner = ifelse(prev_dem_margin > 0, "Democrat", "Other")) |>
    ggplot(aes(x = prev_dem_margin, y = dem_margin, color = prev_winner, group = prev_winner)) +
    geom_point(size = 0.5, alpha = 0.25) +
    geom_smooth(size = 2, alpha = 0.75) +
    labs(x = "Democratic margin, t-1", 
         y = "Democratic margin, t",
         color = "Winner, t-1",
         caption = paste("LATE =", late)) +
    theme_minimal() +
    scale_color_manual(values = c("Democrat" = cols$dem, "Other" = cols$rep)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)
  
  ggsave(paste0(fn_full, "_late_", late, ".jpeg"), p)
  
  NULL
}

simple_rdd <- function(df,
                       subgroups = config$analysis$subgroups,
                       figs = config$paths$figures) {
  purrr::walk(
    subgroups,
    function(f) {
      d <- rlang::exec(f, df)
      plot_density(d, paste0(figs, f))
      plot_rdd(d, paste0(figs, f))
    }
  )
  
  NULL
}

covariate_rdd <- function(df) {
  d <- lag_any_candidates(df)
  
  rdd::RDestimate(dem_margin ~ prev_dem_margin | prcntWhiteAll + meanIncome + prcntUnemp, d)
}

backwards_rdd <- function(df, figs = config$paths$figures, dig = 3, cols = config$analysis$colors) {
  d <- lag_any_candidates(df)
  
  mccrary_p <- rdd::DCdensity(d$dem_margin)
  jpeg(paste0(figs, "backwards_rdd", "_mccrary_p_", round(mccrary_p, dig), ".jpeg"), width = 6, height = 6, units = "in", res = 300)
  rdd::DCdensity(d$dem_margin)
  dev.off()
  
  late <- round(rdd::RDestimate(prev_dem_margin ~ dem_margin, d)$est["LATE"], dig)
  
  p <- d |>
    dplyr::mutate(x_winner = ifelse(dem_margin > 0, "Democrat", "Other")) |>
    ggplot(aes(x = dem_margin, y = prev_dem_margin, color = x_winner, group = x_winner)) +
    geom_point(size = 0.5, alpha = 0.25) +
    geom_smooth(size = 2, alpha = 0.75) +
    labs(x = "Democratic margin, t", 
         y = "Democratic margin, t-1",
         color = "Winner, t",
         caption = paste("LATE =", late)) +
    theme_minimal() +
    scale_color_manual(values = c("Democrat" = cols$dem, "Other" = cols$rep)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)
  
  ggsave(paste0(figs, "backwards_rdd" , "_late_", late, ".jpeg"), p)
  
  NULL
}

logistic <- function(df, cols = config$analysis$colors) {
  d_lower <- lag_any_candidates(df) |>
    dplyr::mutate(dem_margin_bin = as.numeric(dem_margin > 0)) |>
    dplyr::filter(prev_dem_margin < 0) |>
    dplyr::select(prev_dem_margin, dem_margin_bin)
  
  d_upper <- lag_any_candidates(df) |>
    dplyr::mutate(dem_margin_bin = as.numeric(dem_margin > 0)) |>
    dplyr::filter(prev_dem_margin > 0) |>
    dplyr::select(prev_dem_margin, dem_margin_bin)
  
  fit_lower <- glm(dem_margin_bin ~ prev_dem_margin, 
                   data = d_lower, 
                   family = "binomial")
  
  fit_upper <- glm(dem_margin_bin ~ prev_dem_margin, 
                   data = d_upper, 
                   family = "binomial")
  
  eps <- 0.001
  (predict(fit_upper, newdata = data.frame(prev_dem_margin = -eps), type = "response") - 
    predict(fit_lower, newdata = data.frame(prev_dem_margin = eps), type = "response")) |>
    print()
  
  d_lower |>
    dplyr::mutate(p = predict(fit_lower, newdata = d_lower, type = "response"),
                  Winner = "Other") |>
    dplyr::bind_rows(
      d_upper |>
        dplyr::mutate(p = predict(fit_upper, newdata = d_upper, type = "response"),
                      Winner = "Democrat")
    ) |>
    ggplot(aes(x = prev_dem_margin, y = p, color = Winner)) +
    geom_point() +
    scale_color_manual(values = c("Democrat" = cols$dem, "Other" = cols$rep)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    geom_vline(xintercept = 0) +
    theme_minimal() +
    labs(x = "Democratic margin, t-1",
         y = "Probability of Democratic victory, t",
         color = "Winner, t-1")
}

logistic_backwards <- function(df, cols = config$analysis$colors) {
  d_lower <- lag_any_candidates(df) |>
    dplyr::mutate(prev_dem_margin_bin = as.numeric(prev_dem_margin > 0)) |>
    dplyr::filter(dem_margin < 0) |>
    dplyr::select(dem_margin, prev_dem_margin_bin)
  
  d_upper <- lag_any_candidates(df) |>
    dplyr::mutate(prev_dem_margin_bin = as.numeric(prev_dem_margin > 0)) |>
    dplyr::filter(dem_margin > 0) |>
    dplyr::select(dem_margin, prev_dem_margin_bin)
  
  fit_lower <- glm(prev_dem_margin_bin ~ dem_margin, 
                   data = d_lower, 
                   family = "binomial")
  
  fit_upper <- glm(prev_dem_margin_bin ~ dem_margin, 
                   data = d_upper, 
                   family = "binomial")
  
  eps <- 0.001
  (predict(fit_upper, newdata = data.frame(dem_margin = -eps), type = "response") - 
      predict(fit_lower, newdata = data.frame(dem_margin = eps), type = "response")) |>
    print()
  
  d_lower |>
    dplyr::mutate(p = predict(fit_lower, newdata = d_lower, type = "response"),
                  Winner = "Other") |>
    dplyr::bind_rows(
      d_upper |>
        dplyr::mutate(p = predict(fit_upper, newdata = d_upper, type = "response"),
                      Winner = "Democrat")
    ) |>
    ggplot(aes(x = dem_margin, y = p, color = Winner)) +
    geom_point() +
    scale_color_manual(values = c("Democrat" = cols$dem, "Other" = cols$rep)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    geom_vline(xintercept = 0) +
    theme_minimal() +
    labs(x = "Democratic margin, t",
         y = "Probability of Democratic victory, t-1",
         color = "Winner, t")
}


