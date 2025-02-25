#' Tidy Groundfish Conversion Factors from the DFO Spring and Summer Surveys
#'
#' @param login Your username for the appropriate Oracle schema.
#' @param pw Your password for the appropriate Oracle schema.
#' @param dsn The name of the schema.
#' @param species_list The species codes you want to include. They must require length-based abundance conversion. You can include one or multiple concatenated.
#' @param season The season of the survey.
#' @param localdir A file path for a local directory. Temporarily, you need the file result_conversion.RData in that directory if you want to do spring conversions.
#'
#' @return Dataframe of species, length, from vessel, to vessel (vessel equivalent) and conversion factor.
#' @export
#'
#'

tidy_conversions <- function(login = NULL, pw = NULL, dsn = NULL, species_list = 10, season = "SUMMER", localdir = NULL) {
  if (stringr::str_to_upper(season) == stringr::str_to_upper("summer")) {
    channel <- ROracle::dbConnect(DBI::dbDriver("Oracle"), username = login, password = pw, dsn)

    gsconversions <- ROracle::dbGetQuery(channel, paste("select * from groundfish.gsconversions")) |> janitor::clean_names()

    conversion <- gsconversions |>
      dplyr::filter(spec %in% species_list & season == season)

    conversion_nedtel <- conversion |>
      dplyr::filter(from_vessel == "NED_TEM" & to_vessel == "TEL_VEN") |>
      dplyr::select(spec, flen, cf_value) %>%
      dplyr::rename("cf_value_ned_to_tel" = "cf_value")

    conversion_telcar <- conversion |>
      dplyr::filter(from_vessel == "TEL_VEN" & to_vessel == "CAR_CAB") |>
      dplyr::select(spec, flen, cf_value) %>%
      dplyr::rename("cf_value_tel_to_car" = "cf_value")

    conversion_atcham <- base::data.frame(spec = c(11, 40, 41, 42, 43), cf_value_atcham_to_ned = c(1.2, 0.8, 0.8, 0.8, 0.8))

    # Needler equivalent

    # ATC/Hammond to Needler

    atcham_needler <- conversion_nedtel |>
      dplyr::bind_rows(conversion_nedtel) |>
      dplyr::mutate(vesel = base::rep(c("A", "H"), each = base::nrow(conversion_nedtel))) |>
      dplyr::select(-cf_value_ned_to_tel) |>
      dplyr::left_join(conversion_atcham, relationship = "many-to-many")

    # Teleost/Venturer to Needler
    teleost_needler <- conversion_nedtel |>
      dplyr::bind_rows(conversion_nedtel) |>
      dplyr::mutate(
        cf_value_tel_to_ned = 1 / cf_value_ned_to_tel,
        vesel = base::rep(c("S", "V"), each = base::nrow(conversion_nedtel))
      ) |>
      dplyr::select(-cf_value_ned_to_tel)

    # Cartier to Needler
    cartier_needler <- conversion_telcar |>
      dplyr::left_join(conversion_nedtel) |>
      dplyr::bind_rows(conversion_telcar |> dplyr::left_join(conversion_nedtel)) |>
      dplyr::mutate(
        cf_value_car_to_tel = 1 / cf_value_tel_to_car,
        cf_value_tel_to_ned = 1 / cf_value_ned_to_tel,
        vesel = base::rep(c("J", "B"), each = base::nrow(conversion_telcar))
      ) |>
      dplyr::select(-cf_value_tel_to_car, -cf_value_ned_to_tel)

    joined_needler <- plyr::join_all(base::list(atcham_needler, teleost_needler, cartier_needler), by = c("vesel", "spec", "flen"), type = "full") %>%
      base::replace(base::is.na(.), 1) |>
      dplyr::mutate(
        conversion = 1 / (cf_value_atcham_to_ned * cf_value_tel_to_ned * cf_value_car_to_tel),
        vesseleq = "needler"
      )

    # Teleost equivalent

    # ATC/Hammond to Needler to Teleost

    atcham_teleost <- conversion_nedtel |>
      dplyr::bind_rows(conversion_nedtel) |>
      dplyr::mutate(vesel = base::rep(c("A", "H"), each = base::nrow(conversion_nedtel))) |>
      dplyr::left_join(conversion_atcham, relationship = "many-to-many")

    # Needler/Templeman to Teleost
    needler_teleost <- conversion_nedtel |>
      dplyr::bind_rows(conversion_nedtel) |>
      dplyr::mutate(vesel = base::rep(c("N", "T"), each = base::nrow(conversion_nedtel)))

    # Cartier/Cabot to Teleost/Venturer
    cartier_teleost <- conversion_telcar |>
      dplyr::bind_rows(conversion_telcar) |>
      dplyr::mutate(
        cf_value_car_to_tel = 1 / cf_value_tel_to_car,
        vesel = base::rep(c("J", "B"), each = base::nrow(conversion_telcar))
      ) |>
      dplyr::select(-cf_value_tel_to_car)

    joined_teleost <- plyr::join_all(list(atcham_teleost, needler_teleost, cartier_teleost), by = c("vesel", "spec", "flen"), type = "full") %>%
      base::replace(is.na(.), 1) |>
      dplyr::mutate(
        conversion = 1 / (cf_value_atcham_to_ned * cf_value_ned_to_tel * cf_value_car_to_tel),
        vesseleq = "teleost"
      )

    # Cartier equivalent

    # ATC/Hammond to Needler to Teleost to Cartier

    atcham_cartier <- conversion_nedtel |>
      dplyr::left_join(conversion_telcar) |>
      dplyr::bind_rows(conversion_nedtel |>
        dplyr::left_join(conversion_telcar)) |>
      dplyr::mutate(vesel = base::rep(c("A", "H"), each = base::nrow(conversion_nedtel))) |>
      dplyr::left_join(conversion_atcham, relationship = "many-to-many")

    # Needler/Templeman to Teleost to Cartier
    needler_cartier <- conversion_nedtel |>
      dplyr::left_join(conversion_telcar) |>
      dplyr::bind_rows(conversion_nedtel |>
        dplyr::left_join(conversion_telcar)) |>
      dplyr::mutate(vesel = base::rep(c("N", "T"), each = base::nrow(conversion_nedtel)))

    # Teleost/Venturer to Cartier
    teleost_cartier <- conversion_telcar |>
      dplyr::bind_rows(conversion_telcar) |>
      dplyr::mutate(vesel = base::rep(c("S", "V"), each = base::nrow(conversion_telcar)))

    joined_cartier <- plyr::join_all(list(atcham_cartier, needler_cartier, teleost_cartier), by = c("vesel", "spec", "flen"), type = "full") %>%
      base::replace(is.na(.), 1) |>
      dplyr::mutate(
        conversion = 1 / (cf_value_atcham_to_ned * cf_value_ned_to_tel * cf_value_tel_to_car),
        vesseleq = "cartier"
      )

    joined_all <- base::rbind(
      joined_needler |> dplyr::select(spec, flen, vesel, conversion, vesseleq),
      joined_teleost |> dplyr::select(spec, flen, vesel, conversion, vesseleq),
      joined_cartier |> dplyr::select(spec, flen, vesel, conversion, vesseleq)
    )

    base::return(joined_all)
  } else if (stringr::str_to_upper(season) %in% stringr::str_to_upper("spring", "winter")) {
    # Add spring conversions for cod and haddock - temporary until these are in the survey database

    base::load(base::paste(localdir, "//result_conversion.RData", sep = ""))

    result_format <- result_conversion |>
      dplyr::select(spec, lenseq, est_rho) |>
      dplyr::rename("flen" = lenseq, "cf_value" = est_rho)

    maxmins <- base::rbind(
      base::data.frame(spec = c(base::rep(10, 12), base::rep(11, 18)), flen = c(1:12, 1:18)) |> dplyr::mutate(cf_value = dplyr::case_when(spec == 10 ~ 0.220355888258648, spec == 11 ~ 0.250406234496239)),
      base::data.frame(spec = c(base::rep(10, 169 - 93), base::rep(11, 90 - 62)), flen = c(94:169, 63:90)) |> dplyr::mutate(cf_value = case_when(spec == 10 ~ 1.284894, spec == 11 ~ 1.695796))
    )

    spring_conversions <- base::rbind(result_format, maxmins) |> dplyr::mutate(from_vessel = "TEL_VEN", to_vessel = "CAR_CAB", season = "SPRING", cf_model_type = "LDM", cf_metric = "ABUNDANCE", cf_unit = "CM")

    conversion <- spring_conversions |>
      dplyr::filter(spec %in% species_list & season == season)

    conversion_telcar <- conversion |>
      dplyr::filter(from_vessel == "TEL_VEN" & to_vessel == "CAR_CAB") |>
      dplyr::select(spec, flen, cf_value) %>%
      dplyr::rename("cf_value_tel_to_car" = "cf_value")

    # Needler/Templeman/Teleost/Venturer to Cartier/Cabot

    teleost_cartier <- conversion_telcar |>
      dplyr::bind_rows(conversion_telcar, conversion_telcar, conversion_telcar) |>
      dplyr::mutate(vesel = base::rep(c("N", "T", "S", "V"), each = base::nrow(conversion_telcar))) |>
      dplyr::mutate(
        conversion = 1 / (cf_value_tel_to_car),
        vesseleq = "cartier"
      )

    # Cartier/Cabot to Needler/Templeman/Teleost/Venturer

    cartier_teleost <- conversion_telcar |>
      dplyr::bind_rows(conversion_telcar, conversion_telcar, conversion_telcar) |>
      dplyr::mutate(
        cf_value_car_to_tel = 1 / cf_value_tel_to_car,
        vesel = base::rep(c("N", "T", "J", "B"), each = base::nrow(conversion_telcar))
      ) |>
      dplyr::select(-cf_value_tel_to_car) |>
      dplyr::mutate(
        conversion = 1 / (cf_value_car_to_tel),
        vesseleq = "teleost"
      )

    # Join em up

    joined_all <- base::rbind(teleost_cartier |> dplyr::select(spec, flen, vesel, conversion, vesseleq), cartier_teleost |> dplyr::select(spec, flen, vesel, conversion, vesseleq))

    base::return(joined_all)
  } else {
    base::return(print("Season not summer, spring, or winter."))
  }
}
