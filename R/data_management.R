read_elections <- function(fn, raw) {
  readr::read_csv(paste0(raw, fn))
}

apply_filters <- function(df) {
  df |>
    dplyr::filter(!special, !runoff, !unofficial, !fusion_ticket)
}

calculate_dem_margin <- function(df,
                                 democrats = config$data$elections$democrat) {
  df |>
    dplyr::mutate(full_district = paste(state_po, district),
                  share = candidatevotes/totalvotes,
                  democrat = ifelse(stringr::str_detect(party, paste(democrats, collapse = "|")),
                                    "democrat",
                                    "highest_other") |>
                    dplyr::coalesce("highest_other")) |>
    dplyr::group_by(year, full_district, democrat) |>
    dplyr::summarize(share = max(share)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = democrat, values_from = share) |>
    dplyr::mutate(dem_margin = democrat - highest_other)
}

get_elections <- function(fn = config$data$elections$fn,
                          raw = config$paths$raw,
                          proc = config$paths$proc) {
  read_elections(fn = fn, raw = raw) |>
    apply_filters() |>
    calculate_dem_margin()
}
