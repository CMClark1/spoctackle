calc_weight_ab <- function(data=NULL, ab_file=spoctacklebox::survey_ab, sex="sex",code="code",season="season"){
  sex_stratified <- data |>
    dplyr::filter(code%in%c(14,220,2550)) |>
    dplyr::left_join(ab_file, by=c(sex, code, season)) |>
    dplyr::mutate(weight_ab=length_weight_a*(flen^length_weight_b),
                  sex_stratified_ab=TRUE) |>
    dplyr::select(-n,-length_weight_a,-length_weight_b,-max_length,-length_units,-r)
  sex_unstratified <- data |>
    dplyr::filter(!code%in%c(14,220,2550)) |>
    dplyr::left_join(ab_file, by=c(code, season)) |>
    dplyr::mutate(weight_ab=length_weight_a*(flen^length_weight_b),
                  sex_stratified_ab=FALSE) |>
    dplyr::select(-n,-length_weight_a,-length_weight_b,-max_length,-length_units,-r, -sex.y)%>%
    dplyr::rename(sex=sex.x)
  all_weights <- dplyr::full_join(sex_stratified, sex_unstratified)
  base::print(all_weights)
}
