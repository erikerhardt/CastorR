#' Read and format Castor CRF export data.
#'
#' @param fn CRF_export.xlsx or other Castor project MS Excel filename
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
castor_format_R <-
  function(
    fn                      = "CRF_export.xlsx"
  ) {

  # library(dplyr)
  dat <-
    castor_read_data(fn = fn) %>%
    castor_collect_factor_groups() %>%
    castor_assign_variable_labels()

  return(dat)
}
