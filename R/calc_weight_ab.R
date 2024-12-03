#' Calculate Fish Weights from Survey As and Bs
#'
#' @param data A dataframe containing columns for species code, sex, survey season, and length
#' @param ab_file A dataframe containing the survey a and b values
#' @param sex Sex code as used on the surveys: 0 (unspecified), 1 (male), or 2 (female)
#' @param code Species code as used on the surveys: 10 (cod), 11 (haddock), 14 (silver hake), 16 (pollock)
#' @param season Survey season: "summer" or "winter"
#' @param flen Fish length, in the same units as the ab_file.
#'
#' @return "data" input file with additional columns for calculated weight (weight_ab) and weight units
#' @export
#'

calc_weight_ab <- function(data=NULL, ab_file=spoctackle::survey_ab, sex="FSEX",code="SPEC",season="SEASON", flen="FLEN"){
  data$SEASON <- tolower(data$SEASON)
  sex_stratified <- data |>
    janitor::clean_names() |>
    dplyr::rename("code"="spec","sex"="fsex") |>
    dplyr::filter(code%in%c(14,220,2550)) |>
    dplyr::left_join(ab_file, by=c("sex","code","season")) |>
    dplyr::mutate(weight_ab=length_weight_a*(flen^length_weight_b),
                  sex_stratified_ab=TRUE) |>
    dplyr::select(-n,-length_weight_a,-length_weight_b,-max_length,-length_units,-r, -common_name)
  sex_unstratified <- data |>
    janitor::clean_names() |>
    dplyr::rename("code"="spec","sex"="fsex") |>
    dplyr::filter(!code%in%c(14,220,2550)) |>
    dplyr::left_join(ab_file, by=c("code","season")) |>
    dplyr::mutate(weight_ab=length_weight_a*(flen^length_weight_b),
                  sex_stratified_ab=TRUE) |>
    dplyr::select(-n,-length_weight_a,-length_weight_b,-max_length,-length_units,-r, -common_name, -sex.x, -sex.y)
  all_weights <- dplyr::full_join(sex_stratified, sex_unstratified)
  base::print(all_weights)


}
