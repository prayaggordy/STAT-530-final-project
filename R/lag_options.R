lu_repeated_candidate <- function(df, dem) {
  party_col <- paste("candidate", ifelse(dem, "democrat", "highest_other"), sep = "_")
  
  df |>
    dplyr::group_by(full_district) |>
    dplyr::filter(dplyr::if_all(.cols = dplyr::all_of(party_col),
                                .fns = ~ . == dplyr::lag(.))) |>
    dplyr::ungroup() |>
    dplyr::select(full_district, year)
}

lag_simple <- function(df) {
  df |>
    dplyr::inner_join(
      df |>
        dplyr::mutate(next_election = year + 2) |>
        dplyr::select(next_election, full_district, 
                      prev_dem_margin = dem_margin, 
                      prev_candidate_democrat = candidate_democrat, 
                      prev_candidate_highest_other = candidate_highest_other),
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
  lag_simple(df) |>
    dplyr::filter(candidate_democrat == prev_candidate_democrat) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_diff_dem <- function(df,
                         demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_democrat != prev_candidate_democrat) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_same_dem_same_rep <- function(df,
                                  demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_democrat == prev_candidate_democrat,
                  candidate_highest_other == prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_same_dem_diff_rep <- function(df,
                                  demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_democrat == prev_candidate_democrat,
                  candidate_highest_other != prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_same_rep <- function(df,
                         demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_highest_other == prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_diff_rep <- function(df,
                         demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_highest_other != prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_diff_dem_same_rep <- function(df,
                                  demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_democrat != prev_candidate_democrat,
                  candidate_highest_other == prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}

lag_diff_dem_diff_rep <- function(df,
                                  demos = DF_DEMOS) {
  lag_simple(df) |>
    dplyr::filter(candidate_democrat != prev_candidate_democrat,
                  candidate_highest_other != prev_candidate_highest_other) |>
    dplyr::left_join(demos, by = c("year", "full_district"))
}
