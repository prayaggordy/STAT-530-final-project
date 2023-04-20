read_demos <- function(fn, raw) {
  readr::read_csv(paste0(raw, fn))
}

clean_district <- function(df,
                           first_year = config$data$demographics$first_year) {
  df |>
    dplyr::mutate(state_po = stringr::str_sub(stateDist, end = 2),
                  district = stringr::str_sub(stateDist, start = 4) |>
                    stringr::str_pad(3, "left", "0"),
                  full_district = paste(state_po, district),
                  year = first_year + 2*(congNum - min(congNum, na.rm = T))) |>
    dplyr::select(-c(state_po, district, stateDist)) |>
    dplyr::relocate(full_district, year, .before = 1)
}

filter_demos <- function(df) {
  df |>
    dplyr::filter(!is.na(full_district), !is.na(year)) |>
    dplyr::group_by(full_district, year) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}

get_demos <- function(fn = config$data$demographics$fn,
                      raw = config$paths$raw,
                      proc = config$paths$proc) {
  read_demos(fn = fn, raw = raw) |>
    clean_district() |>
    filter_demos()
}
