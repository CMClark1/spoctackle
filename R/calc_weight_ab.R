#' Calculate Fish Weights from Survey As and Bs
#'
#' @param data A dataframe containing columns for species code, sex, survey season, and length in the format that they are used in the survey database (SPEC,FSEX,SEASON,FLEN). To run for As and Bs, by quarter, it also needs to have a numeric "MONTH" column.
#' @param ab_source Source of the ab values: "survey" (Noble and Clark 2017) or "other" which will default to quarterly As and Bs as used in Catch.
#'
#' @return "data" input file with additional columns for calculated weight (weight_ab) and weight units
#' @export
#'

calc_weight_ab <- function(data = NULL, ab_source = NULL) {
  if (stringr::str_to_lower(ab_source) == "survey") {
    ab_file <- spoctackle::survey_ab
    data$SEASON <- base::tolower(data$SEASON)
    sex_stratified <- data |>
      janitor::clean_names() |>
      dplyr::rename("code" = "spec", "sex" = "fsex") |>
      dplyr::filter(code %in% c(14, 220, 2550, 2511, 2513, 2521, 2523, 2526, 2527, 2532)) |>
      dplyr::left_join(ab_file, by = c("sex", "code", "season")) |>
      dplyr::mutate(
        weight_ab = length_weight_a * (flen^length_weight_b),
        sex_stratified_ab = TRUE
      ) |>
      dplyr::select(-n, -length_weight_a, -length_weight_b, -max_length, -length_units, -r, -common_name) |>
      dplyr::rename("spec" = "code", "fsex" = "sex")
    sex_unstratified <- data |>
      janitor::clean_names() |>
      dplyr::rename("code" = "spec", "sex" = "fsex") |>
      dplyr::filter(!code %in% c(14, 220, 2550, 2511, 2513, 2521, 2523, 2526, 2527, 2532)) |>
      dplyr::left_join(ab_file, by = c("code", "season")) |>
      dplyr::mutate(
        weight_ab = ifelse(is.na(fwt), length_weight_a * (flen^length_weight_b), fwt),
        sex_stratified_ab = FALSE
      ) |>
      dplyr::select(-n, -length_weight_a, -length_weight_b, -max_length, -length_units, -r, -common_name, -sex.y) %>%
      dplyr::rename("spec" = "code", "fsex" = "sex.x")
    all_weights <- dplyr::full_join(sex_stratified, sex_unstratified)
    base::print(all_weights)
  } else if (stringr::str_to_lower(ab_source) == "4xcod") {
    ab_file <- spoctackle::commercial_ab
    cod4X <- data |>
      janitor::clean_names() |>
      dplyr::filter(spec == 10) %>%
      dplyr::mutate(
        month = lubridate::month(as.Date(sdate)),
        quarter = dplyr::case_when(
          month %in% 1:3 ~ 1,
          month %in% 4:6 ~ 2,
          month %in% 7:9 ~ 3,
          month %in% 10:12 ~ 4
        )
      ) %>%
      dplyr::left_join(ab_file, by = c("quarter")) |>
      dplyr::mutate(
        weight_ab = ifelse(is.na(fwt), length_weight_a * (flen^length_weight_b), fwt),
        sex_stratified_ab = FALSE
      ) |>
      dplyr::select(-stock, -sex, -length_weight_a, -length_weight_b, -code)
    base::print(cod4X)
  } else if (!ab_source %in% c("survey", "4Xcod")) {
    print("A/B source not available. See documentation for function calc_weight_ab.")
  }
}
