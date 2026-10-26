#' Recreate Age Length Key tab from STRANAL in long format
#'
#' @param cxn  A valid Oracle connection object. 
#' @param species The species codes you want to include. You can include one or multiple concatenated. Default is cod 10.
#' @param season_select The season of the survey. Default is summer.
#' @param stratum The selected stratum. Default is 4X.
#' @param year_start The first year required. Default is 1970.
#' @param year_end The final year required. Default is 2024.
#' @param localdir A file path for a local directory where Mar.utils::get_data_tables stores copies of data tables. Temporarily, you need the file result_conversion.RData in that directory if you want to do spring conversions.
#' @param type_select The type of sets to include. Default is 1, valid primary sets.
#'
#' @return Dataframe of species, length, from vessel, to vessel (vessel equivalent) and conversion factor.
#' @export
#'
#'

agelengthkey <- function(species = 10, 
                         season_select = "summer", 
                         stratum = c(470:478, 480:485, 490:495), 
                         year_start = 1970, 
                         year_end = as.integer(format(Sys.Date(), "%Y")), 
                         type_select = 1, 
                         localdir = "C:\\DFO-MPO\\PESDData\\", 
                         cxn = NULL,
                         login = NULL,
                         computer = NULL){

  # This checks that you have the necessary data present in your localdir
  
  required_files <- c("GROUNDFISH.GSDET.RData", "GROUNDFISH.GSMISSIONS.RData", "GROUNDFISH.GSINF.RData")
  
  files_present <- spoctackle::check_files_in_folder(
    localdir,
    required_files
  )
  
  if (isFALSE(files_present)) {
    
    library(Mar.utils)
    
    Mar.utils::get_data_tables(
      schema = "GROUNDFISH",
      tables = c("GSDET","GSMISSIONS","GSINF"),
      cxn = cxn,
      data.dir = localdir)
    
  } else if (isTRUE(files_present)) {
    
    library(Mar.utils)
    
    for (i in required_files) {
      Mar.utils::load_encrypted(
        file.path(localdir, i),
        extract_user = login,
        extract_computer = computer
      )
    }
    
  } else {
    print("Unable to check if required files are present.")
  }

  #Join the dataframes and do all the filtering based on user input
data_filtered <-  GSDET |> 
    dplyr::left_join(GSMISSIONS, by = "MISSION") |> 
    dplyr::left_join(GSINF |> dplyr::select(MISSION,SETNO,TYPE,STRAT)) |> 
    janitor::clean_names() |> 
    dplyr::filter(stringr::str_to_lower(season)%in%season_select &
                    strat %in% stratum &
                    year>=year_start & year<=year_end & 
                    type %in% type_select &
                    spec %in% species &
                    !is.na(age))

#Bin in 3cm length bins, group and count by length and age
alk_long <- data_filtered |> 
  dplyr::mutate(flen_bin = round((flen - 1) / 3) * 3 + 1) |> 
  dplyr::group_by(spec, year,season,flen_bin,age) |> 
    dplyr::count() |> 
  dplyr::group_by(spec,year,season,flen_bin) |> 
  dplyr::mutate(p_age=n/sum(n))

return(alk_long)
  
}