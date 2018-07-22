#' Read the Castor CRF exported file for formatting
#'
#' @param fn CRF_export.xlsx or other Castor project MS Excel filename
#'
#' @return
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_xlsx
#' @export
#'
#' @examples
castor_read_data <-
  function(
    fn                      = "CRF_export.xlsx"
  ) {

  if(!file.exists(fn)) {
    stop(paste0("File does not exist: ", fn))
  }

  # library(readxl)
  fn_sheet_names <- readxl::excel_sheets(fn)

  dat <- list()
  for (i_sheet in seq_along(fn_sheet_names)) {
    dat[[ fn_sheet_names[i_sheet] ]] <- readxl::read_xlsx(fn, sheet = fn_sheet_names[i_sheet])
  }

  return(dat)
}
