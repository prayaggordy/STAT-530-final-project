read_elections <- function(fn, raw) {
  readr::read_csv(paste0(raw, fn))
}

apply_filters <- function(df,
                          filters = config$data$elections$filters,
                          min_percent = config$data$elections$min_percent,
                          democrats = paste0("^",
                                             config$data$elections$democrat,
                                             "$")) {
  df |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(filters), ~ !dplyr::coalesce(., FALSE)),
                  stage == "GEN",
                  totalvotes > 1) |>
    dplyr::group_by(year, state, district) |>
    dplyr::filter(any(stringr::str_detect(party, paste(democrats, collapse = "|")))) |>
    dplyr::ungroup()
}

calculate_dem_margin <- function(df,
                                 democrats = paste0("^",
                                                    config$data$elections$democrat,
                                                    "$")) {
  df |>
    dplyr::mutate(full_district = paste(state_po, district),
                  share = candidatevotes/totalvotes,
                  party = dplyr::coalesce(party, candidate),
                  democrat = ifelse(stringr::str_detect(party, paste(democrats, collapse = "|")),
                                    "democrat",
                                    "highest_other")) |>
    dplyr::group_by(year, full_district, democrat) |>
    dplyr::summarize(share = max(share)) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, full_district) |>
    dplyr::filter(dplyr::n() == 2) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = democrat, values_from = share) |>
    dplyr::mutate(dplyr::across(c(democrat, highest_other), ~ tidyr::replace_na(., 0)),
                  dem_margin = democrat - highest_other)
}

get_elections <- function(fn = config$data$elections$fn,
                          raw = config$paths$raw,
                          proc = config$paths$proc) {
  df_raw <- read_elections(fn = fn, raw = raw)
  
  df <- df_raw |>
    apply_filters() |>
    calculate_dem_margin() |>
    dplyr::left_join(
      df_raw |>
        dplyr::mutate(full_district = paste(state_po, district)) |>
        dplyr::group_by(year, full_district) |>
        dplyr::summarize(totalvotes = max(totalvotes)),
      by = c("year", "full_district")
    )
  
  readr::write_csv(df, paste0(proc, fn))
  
  df
}
