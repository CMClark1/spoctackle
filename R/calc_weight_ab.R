#' Calculate Fish Weights from Survey As and Bs
#'
#' @param data A dataframe containing columns for species code, sex, survey season, and length in the format that they are used in the survey database (SPEC,FSEX,SEASON,FLEN)
#' @param ab_file A dataframe containing the survey a and b values
#'
#' @return "data" input file with additional columns for calculated weight (weight_ab) and weight units
#' @export
#'

calc_weight_ab <- function(data = NULL, ab_file = spoctackle::survey_ab) {
  data$SEASON <- base::tolower(data$SEASON)
  sex_stratified <- data |>
    janitor::clean_names() |>
    dplyr::rename("code" = "spec", "sex" = "fsex") |>
    dplyr::filter(code %in% c(14, 220, 2550)) |>
    dplyr::left_join(ab_file, by = c("sex", "code", "season")) |>
    dplyr::mutate(
      weight_ab = length_weight_a * (flen^length_weight_b),
      sex_stratified_ab = TRUE
    ) |>
    dplyr::select(-n, -length_weight_a, -length_weight_b, -max_length, -length_units, -r, -common_name) |>
    dplyr::rename("spec"="code","fsex"="sex")
  sex_unstratified <- data |>
    janitor::clean_names() |>
    dplyr::rename("code" = "spec", "sex" = "fsex") |>
    dplyr::filter(!code %in% c(14, 220, 2550)) |>
    dplyr::left_join(ab_file, by = c("code", "season")) |>
    dplyr::mutate(
      weight_ab = length_weight_a * (flen^length_weight_b),
      sex_stratified_ab = FALSE
    ) |>
    dplyr::select(-n, -length_weight_a, -length_weight_b, -max_length, -length_units, -r, -common_name, -sex.y)%>%
    dplyr::rename("spec"="code","fsex"="sex.x")
  all_weights <- dplyr::full_join(sex_stratified, sex_unstratified)
  base::print(all_weights)
}
