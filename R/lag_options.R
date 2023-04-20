lag_simple <- function(df,
                       demos = DF_DEMOS) {
  df |>
    dplyr::select(year, full_district, dem_margin) |>
    dplyr::inner_join(
      df |>
        dplyr::mutate(next_election = year + 2) |>
        dplyr::select(next_election, full_district, prev_dem_margin = dem_margin),
      by = c("year" = "next_election", "full_district")
    ) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_same_dem <- function(df,
                         demos = DF_DEMOS) {
  
}
