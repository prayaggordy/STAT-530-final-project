test_covars <- function(df) {
  d <- lag_any_candidates(DF) |>
    dplyr::select(year, full_district, prev_dem_margin, white = prcntWhiteAll, income = meanIncome, unemp = prcntUnemp) |>
    tidyr::pivot_longer(cols = c(white, income, unemp),
                        names_to = "demo") |>
    dplyr::filter(!is.na(value))
  
  d |>
    dplyr::group_by(demo) |>
    tidyr::nest(data = c(year, full_district, prev_dem_margin, value)) |>
    dplyr::mutate(rdd = purrr::map(
      data,
      function(x) {
        est <- rdd::RDestimate(value ~ prev_dem_margin, x)$est[1]
        se <- rdd::RDestimate(value ~ prev_dem_margin, x)$se[1]
        bw <- rdd::RDestimate(value ~ prev_dem_margin, x)$bw[1]
        data.frame(est = est, se = se, bw = bw)
      }
    )) |>
    tidyr::unnest(rdd)
  
  purrr::map_dfr(
    seq(0.01, 0.15, by = 0.01),
    function(bw) {
      d |>
        dplyr::filter(prev_dem_margin >= -bw,
                      prev_dem_margin <= bw) |>
        dplyr::group_by(demo, winner) |>
        dplyr::summarize(value = mean(value),
                         district_year_obs = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(bandwidth = bw)
    }
  )
}

decade_rdd <- function(df) {
  lag_any_candidates(df) |>
    dplyr::mutate(decade = floor(year/10)*10) |>
    dplyr::group_by(decade) |>
    tidyr::nest() |>
    dplyr::mutate(
      res = 
        purrr::map(
          data,
          function(x) {
            mccrary <- rdd::DCdensity(x$prev_dem_margin, plot = F)
            est <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$est[1]
            se <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$se[1]
            bw <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$bw[1]
            
            data.frame(mccrary = mccrary, est = est, se = se, bw = bw)
          }
        )
    ) |>
    tidyr::unnest(res) |>
    dplyr::select(Decade = decade, LATE = est)
}

region_rdd <- function(df,
                       url = "https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv") {
  readr::read_csv(url) |>
    dplyr::inner_join(lag_any_candidates(df), by = c("State Code" = "state_po")) |>
    dplyr::group_by(Division) |>
    tidyr::nest() |>
    dplyr::mutate(
      res =
        purrr::map(
          data,
          function(x) {
            mccrary <- rdd::DCdensity(x$prev_dem_margin, plot = F)
            est <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$est[1]
            se <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$se[1]
            bw <- rdd::RDestimate(dem_margin ~ prev_dem_margin, x)$bw[1]

            data.frame(mccrary = mccrary, est = est, se = se, bw = bw)
          }
        )
    ) |>
    tidyr::unnest(res) |>
    dplyr::select(Division, LATE = est)
}
