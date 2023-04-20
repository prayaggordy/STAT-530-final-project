remove_redistricting <- function(df,
                                 remainder = config$data$elections$remove_remainder) {
  df |>
    dplyr::filter(year %% 10 != remainder)
}

lag_simple <- function(df,
                       demos = DF_DEMOS) {
  d <- df |>
    remove_redistricting()
  
  d |>
    dplyr::select(year, full_district, dem_margin) |>
    dplyr::inner_join(
      d |>
        dplyr::mutate(next_election = year + 2) |>
        dplyr::select(next_election, full_district, prev_dem_margin = dem_margin),
      by = c("year" = "next_election", "full_district")
    ) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}


