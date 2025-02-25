#' Apply Groundfish Conversion Factors from the DFO Spring and Summer Surveys
#'
#' @param oracle.username Your username for the appropriate Oracle schema.
#' @param oracle.password Your password for the appropriate Oracle schema.
#' @param oracle.dsn The name of the schema.
#' @param species The species codes you want to include. They must require length-based abundance conversion. You can include one or multiple concatenated. Default is cod 10.
#' @param season_select The season of the survey. Default is summer.
#' @param stratum The selected stratum. Default is 4X.
#' @param year_start The first year required. Default is 1970.
#' @param year_end The final year required. Default is 2024.
#' @param equivalent The vessel equivalent. You can select any vessel, but, for example: "needler","teleost","cartier". Default is "cartier"
#' @param ab The source of the ab values that are applied. See the function calc_weight_ab. Options include "survey" and "4Xcod" currently. Default is "survey"
#' @param localdir A file path for a local directory. Temporarily, you need the file result_conversion.RData in that directory if you want to do spring conversions.
#' @param summary The type of summary needed. The options are abundance and biomass per length group ("lf"), set level ("set_level"), strata level ("strata level"), or totals for the selected area ("total"). Default is "total"
#' @param type The type of sets to include. Default is 1, valid primary sets.
#'
#' @return Dataframe of species, length, from vessel, to vessel (vessel equivalent) and conversion factor.
#' @export
#'
#'

survey_convert <- function(summary = "total", species = 10, season_select = "summer", stratum = c(470:478, 480:485, 490:495), year_start = 1970, year_end = 2024, type = 1, equivalent = "cartier", ab = "survey", localdir = NULL, oracle.username = NULL, oracle.password = NULL, oracle.dsn = NULL) {

  # This pulls in the tidied conversion factors for the required species, season, and vessel equivalent
  joined <- spoctackle::tidy_conversions(login = oracle.username, pw = oracle.password, dsn = oracle.dsn, species_list = species, season = season_select, localdir = "C:\\LocalDataDump\\GROUNDFISH\\") |>
    dplyr::filter(vesseleq == stringr::str_to_lower(equivalent))

  # This checks that you have the necessary data present in your localdir

  file_list <- fs::dir_ls(localdir)

  required_files <- c("GROUNDFISH.GSCAT.RData", "GROUNDFISH.GSINF.RData", "GROUNDFISH.GSSPECIES.RData", "GROUNDFISH.GSDET.RData", "GROUNDFISH.GSMISSIONS.RData", "GROUNDFISH.GSSTRATUM.RData")

  if (spoctackle::check_files_in_folder(localdir, required_files) == FALSE) {
    library(Mar.datawrangling)
    Mar.datawrangling::get_data(
      db = "rv",
      usepkg = "roracle",
      force.extract = FALSE,
      data.dir = file.path(localdir),
      fn.oracle.username = oracle.username,
      fn.oracle.password = oracle.password,
      fn.oracle.dsn = oracle.dsn,
      env = .GlobalEnv,
      quiet = FALSE
    )
  } else if (spoctackle::check_files_in_folder(localdir, required_files) == TRUE) {
    file_list <- fs::dir_ls(localdir)
    base::lapply(file_list, load, .GlobalEnv)
  } else if (!spoctackle::check_files_in_folder(localdir, required_files) %in% c(TRUE, FALSE)) {
    print("Unable to check if required files are present.")
  }


  # Format and clean the local data
  detail_combined <- GSDET |>
    dplyr::left_join(GSINF, by = c("MISSION", "SETNO"), relationship = "many-to-many") |>
    dplyr::left_join(GSCAT, by = c("MISSION", "SETNO", "SPEC"), relationship = "many-to-many") |>
    dplyr::left_join(GSMISSIONS, by = "MISSION", relationship = "many-to-many") |>
    # The next line adds a width column based on gear type
    dplyr::mutate(
      width = base::ifelse(GEAR == 3, 10.97 / 1000, base::ifelse(GEAR == 9, 12.49 / 1000, base::ifelse(GEAR == 15, 13 / 1000, NA))),
      # The next lines clean up the sampwgt column and calculate a ratio between sampwgt and totwgt
      SAMPWGT = dplyr::case_when(
        SAMPWGT == 0 | is.na(SAMPWGT) ~ TOTWGT,
        SAMPWGT != 0 & !is.na(SAMPWGT) ~ SAMPWGT
      ),
      RATIO = ifelse(is.nan(TOTWGT / SAMPWGT), 1, TOTWGT / SAMPWGT)
    ) |>
    # The next line filters based on the species, season, strata, and years required
    dplyr::filter(SPEC %in% base::as.numeric(species) & SEASON %in% stringr::str_to_upper(season_select) & STRAT %in% stratum & YEAR >= year_start & YEAR <= year_end & TYPE %in% type) |>
    # The next lines clean up FSEX
    dplyr::mutate(
      FSEX = dplyr::case_when(
        !FSEX %in% c(1:3) ~ 0,
        FSEX %in% c(1:2) ~ FSEX,
        FSEX == 3 ~ 2
      ),
      # The next lines clean up FLEN for herring and grenadier
      FLEN2 = dplyr::case_when(
        SPEC == 410 & YEAR < 2019 & !MISSION == "NED2010027" ~ ((FLEN - 1.770067) / 4.453725) * 10,
        SPEC == 410 & MISSION == "NED2010027" & !SETNO %in% c(219, 222, 223, 229, 230, 233, 236) ~ ((FLEN - 1.770067) / 4.453725) * 10,
        SPEC == 410 & MISSION == "NED2010027" & SETNO %in% c(219, 222, 223, 229, 230, 233, 236) ~ FLEN * 10,
        SPEC == 410 & MISSION == "NED2019102" & SETNO %in% c(13, 15, 132, 133, 150) ~ FLEN * 10,
        SPEC == 410 & MISSION == "NED2019030" & SETNO %in% c(132, 133, 150) ~ FLEN * 10,
        SPEC == 60 & YEAR < 2016 ~ FLEN * 10,
        SPEC == 60 & MISSION %in% c("TEL2016002", "TEL2016003") ~ FLEN * 10
      ),
      FLEN = base::ifelse(!is.na(FLEN2), FLEN2, FLEN)
    ) |>
    dplyr::select(-FLEN2)

  detail_combined2 <- spoctackle::calc_weight_ab(detail_combined, ab_source = ab)

  # Length Frequency -- this first part is standardized to 1 km tow distance

  per_lengthgroup <- detail_combined2 |>
    dplyr::left_join(joined |> dplyr::select(spec, flen, vesel, conversion), by = c("spec", "flen", "vesel")) %>%
    dplyr::mutate(
      conversion = base::ifelse(is.na(conversion), 1, conversion), # cleans up conversions by using 1 when NA
      clen_conv = ((clen * conversion) * ratio), # applies conversion, totwgt/sampwgt ratio
      clen_unconv = (clen * ratio), # applies only ratio for unconverted
      cwt_conv = clen_conv * weight_ab, # these are the weights/biomass
      cwt_unconv = clen_unconv * weight_ab
    ) |>
    dplyr::group_by(year, season, mission, strat, setno, dist, width, flen, conversion) |>
    # This summarises it by length group
    dplyr::summarise(
      clen_conv = sum(clen_conv) / (dist * 1.852),
      clen_unconv = sum(clen_unconv) / (dist * 1.852),
      cwt_conv = sum(cwt_conv) / (dist * 1.852),
      cwt_unconv = sum(cwt_unconv) / (dist * 1.852)
    )

  # Tow level - standardized to 1 km tow distance

  per_tow <- per_lengthgroup |>
    dplyr::mutate(id = paste(mission, setno, sep = "-")) |>
    dplyr::group_by(year, season, strat, id, dist, width) |>
    dplyr::summarise(
      number_conv = sum(clen_conv), # sum to tow level and standardize to km2
      number_unconv = sum(clen_unconv),
      weight_conv = sum(cwt_conv),
      weight_unconv = sum(cwt_unconv)
    ) |>
    dplyr::distinct()

  # Strata level - standardizes to km2 using tow distance and with

  per_stratum <- detail_combined2 |>
    dplyr::left_join(joined |> dplyr::select(spec, flen, vesel, conversion), by = c("spec", "flen", "vesel")) %>%
    dplyr::mutate(
      conversion = base::ifelse(is.na(conversion), 1, conversion), # cleans up conversions by using 1 when NA
      clen_conv = ((clen * conversion) * ratio), # applies conversion, totwgt/sampwgt ratio
      clen_unconv = (clen * ratio), # applies only ratio for unconverted
      cwt_conv = clen_conv * weight_ab, # these are the weights/biomass
      cwt_unconv = clen_unconv * weight_ab
    ) |>
    dplyr::group_by(year, season, mission, strat, setno, dist, width, flen, conversion) |>
    # This summarises it by length group
    dplyr::summarise(
      clen_conv = sum(clen_conv),
      clen_unconv = sum(clen_unconv),
      cwt_conv = sum(cwt_conv),
      cwt_unconv = sum(cwt_unconv)
    ) |>
    dplyr::mutate(id = paste(mission, setno, sep = "-")) |>
    dplyr::group_by(year, season, strat, id) |>
    dplyr::summarise(
      number_conv = sum(clen_conv) / (dist * 1.852 * width), # sum to tow level and standardize to km2
      number_unconv = sum(clen_unconv) / (dist * 1.852 * width),
      weight_conv = sum(cwt_conv) / (dist * 1.852 * width),
      weight_unconv = sum(cwt_unconv) / (dist * 1.852 * width)
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(GSINF |>
                       dplyr::left_join(GSMISSIONS, relationship = "many-to-many") |>
                       dplyr::filter(SEASON %in% stringr::str_to_upper(season_select) & TYPE %in% type) |>
                       dplyr::group_by(YEAR, SEASON, STRAT) |>
                       dplyr::summarise(n_sets = length(SETNO)) |>
                       janitor::clean_names()) |>
    dplyr::group_by(year, season, strat) |> # switched to year/season in case of different missions in same year
    dplyr::summarise(
      number_km2_conv = sum(number_conv) / mean(n_sets), # This is taking the sum of the sets per stratum and dividing by the total number of sets in that stratum
      number_km2_unconv = sum(number_unconv) / mean(n_sets),
      weight_km2_conv = sum(weight_conv) / mean(n_sets),
      weight_km2_unconv = sum(weight_unconv) / mean(n_sets)
    ) |>
    dplyr::left_join(GSSTRATUM |> dplyr::select(STRAT, AREA) |> dplyr::rename("STRAT_AREA" = AREA) |> janitor::clean_names()) |>
    dplyr::mutate(strat_area = strat_area * 3.429904) |> # convert strata area to km2
    dplyr::mutate(
      number_strata_conv = number_km2_conv * strat_area,
      number_strata_unconv = number_km2_unconv * strat_area,
      weight_strata_conv = (weight_km2_conv / 1000) * strat_area,
      weight_strata_unconv = (weight_km2_unconv / 1000) * strat_area
    ) # convert to kg

  if (summary == "lf") {
    print("Converted and unconverted numbers and weight per length group, standardized to 1km tow distance.")
    return(per_lengthgroup)
  } else if (summary == "set_level") {
    print("Converted and unconverted numbers and weight per set (also known as a tow), standardized to 1km tow distance.")
    return(per_tow)
  } else if (summary == "strata_level") {
    print("Converted and unconverted numbers and weight per stratum, calculated by standardizing to 1km2.")
    return(per_stratum |> dplyr::select(year, season, strat, number_strata_conv, number_strata_unconv, weight_strata_conv, weight_strata_unconv))
  } else if (summary == "total") {
    print("Total abundance and biomass for all selected strata.")
    return(per_stratum |> dplyr::group_by(year, season) |> dplyr::summarise(abundance_conv = sum(number_strata_conv), abundance_unconv = sum(number_strata_unconv), biomass_conv = sum(weight_strata_conv), biomass_unconv = sum(weight_strata_unconv)))
  } else {
    print("Requested summary not available. See documentation for survey_convert.")
  }
}
