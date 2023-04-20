plot_density <- function(d, fn_full, dig = 3) {
  mccrary_p <- rdd::DCdensity(d$prev_dem_margin)
  jpeg(paste0(fn_full, "_mccrary_p_", round(mccrary_p, dig), ".jpeg"), width = 350, height = 350)
  rdd::DCdensity(d$prev_dem_margin)
  dev.off()
  
  NULL
}

plot_rdd <- function(d, fn_full, 
                     dig = 3, cols = config$analysis$colors) {
  late <- round(rdd::RDestimate(dem_margin ~ prev_dem_margin, d)$est["LATE"], dig)
  
  p <- d |>
    dplyr::mutate(prev_winner = ifelse(prev_dem_margin > 0, "Democrat", "Other")) |>
    ggplot(d, aes(x = prev_dem_margin, y = dem_margin, color = prev_winner, group = prev_winner)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Previous Democratic margin", 
         y = "Democratic margin",
         color = "Previous winner",
         caption = paste0("LATE =", late)) +
    theme_minimal()
  
  ggsave(paste0(fn_full), "_late_", late, ".jpeg")
  
  NULL
}

simple_rdd <- function(df,
                       lags = config$analysis$lag_fns,
                       figs = config$paths$figures) {
  purrr::walk(
    lags,
    function(f) {
      d <- rlang::exec(f, df)
      plot_density(d, paste0(figs, f))
      # plot_rdd(d, paste0(figs, f))
    }
  )
  
  return()
}
