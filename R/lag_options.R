lu_repeated_candidate <- function(df, party) {
  rlang::arg_match(party, c("democrat", "highest_other"))
  party_col <- paste("candidate", party, sep = "_")
  
  df |>
    dplyr::group_by(full_district) |>
    dplyr::filter(dplyr::if_all(.cols = dplyr::all_of(party_col),
                                .fns = ~ . == dplyr::lag(.))) |>
    dplyr::ungroup() |>
    dplyr::select(full_district, year)
}

lag_simple <- function(df,
                       demos = DF_DEMOS) {
  df |>
    dplyr::select(year, full_district, dem_margin) |>
    dplyr::inner_join(
      df |>
        dplyr::mutate(next_election = year + 2) |>
        dplyr::select(next_election, full_district, prev_dem_margin = dem_margin),
      by = c("year" = "next_election", "full_district")
    )
}

lag_any_candidates <- function(df,
                               demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_same_dem <- function(df,
                         demos = DF_DEMOS) {
  concurrent_dems <- lu_candidates(df) |>
    dplyr::group_by(full_district)
    
}
