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
    dplyr::select(-c(dplyr::all_of(filters), stage)) |>
    dplyr::group_by(year, state, district) |>
    dplyr::filter(any(stringr::str_detect(party, paste(democrats, collapse = "|"))),
                  length(unique(totalvotes)) == 1) |>
    dplyr::ungroup()
}

calculate_dem_margin <- function(df,
                                 democrats = paste0("^",
                                                    config$data$elections$democrat,
                                                    "$")) {
  df |>
    dplyr::mutate(full_district = paste(state_po, district),
                  share = candidatevotes/totalvotes,
                  democrat = ifelse(stringr::str_detect(party, paste(democrats, collapse = "|")),
                                    "democrat",
                                    "highest_other") |>
                    dplyr::coalesce("highest_other")) |>
    dplyr::group_by(year, full_district, democrat) |>
    dplyr::slice_max(share) |>
    dplyr::ungroup() |>
    dplyr::group_by(year, full_district) |>
    dplyr::filter(dplyr::n() == 2) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = democrat, values_from = c(share, party, candidate, candidatevotes, writein)) |>
    dplyr::mutate(dem_margin = share_democrat - share_highest_other)
}

get_elections <- function(fn = config$data$elections$fn,
                          raw = config$paths$raw,
                          proc = config$paths$proc) {
  df <- read_elections(fn = fn, raw = raw) |>
    apply_filters() |>
    calculate_dem_margin()
  
  readr::write_csv(df, paste0(proc, fn))
  
  df
}
