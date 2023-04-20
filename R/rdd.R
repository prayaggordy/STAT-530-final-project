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
    ggplot(aes(x = prev_dem_margin, y = dem_margin, color = prev_winner, group = prev_winner)) +
    geom_point(size = 0.5, alpha = 0.25) +
    geom_smooth(size = 2, alpha = 0.75) +
    labs(x = "Previous Democratic margin", 
         y = "Democratic margin",
         color = "Previous winner",
         caption = paste("LATE =", late)) +
    theme_minimal() +
    scale_color_manual(values = c("Democrat" = cols$dem, "Other" = cols$rep)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)
  
  ggsave(paste0(fn_full, "_late_", late, ".jpeg"), p)
  
  jpeg(paste0(fn_full, "_late_", late, "_BASE.jpeg"), width = 1000, height = 1000)
  plot(rdd::RDestimate(dem_margin ~ prev_dem_margin, d))
  dev.off()
  
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
