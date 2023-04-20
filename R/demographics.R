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

test_demos <- function(df) {
  d <- lag_any_candidates(df) |>
    dplyr::select(year, full_district, prev_dem_margin, white = prcntWhiteAll, educ = prcntBA, unemp = prcntUnemp) |>
    tidyr::pivot_longer(cols = c(white, educ, unemp),
                        names_to = "demo") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(winner = ifelse(prev_dem_margin > 0, "dem", "other"))
  
  vals <- purrr::map_dfr(
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
  
  lines <- ggplot(vals, aes(x = bandwidth, y = value, color = winner)) +
    geom_line() + 
    facet_wrap(~ demo, ncol = 1, scales = "free_y")
  
  ggplot(d, aes(x = prev_dem_margin, y = value, color = winner)) +
    geom_point() +
    facet_wrap(~ demo, ncol = 1, scales = "free_y")
}
