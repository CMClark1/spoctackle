#' Check Files in Folder
#'
#' @param folder_path A filepath to the folder where the files should be
#' @param file_names The files that should be in the folder
#'
#' @return TRUE (no files missing) or FALSE (missing files)
#' @export
#'

check_files_in_folder <- function(folder_path, file_names) {

    missing_files <- c()

     for (file in file_names) {

        full_path <- paste(folder_path, file, sep = "/")

        if (!file.exists(full_path)) {

             missing_files <- c(missing_files, file)

         }

     }



    if (length(missing_files) > 0) {

         message("Missing files: ", paste(missing_files, collapse = ", "))

         return(FALSE)

     } else {

         message("All required files found in folder")

        return(TRUE)

     }

 }
