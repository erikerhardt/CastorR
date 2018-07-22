#' Collect the Castor factor groups for variable sets
#'
#' @param dat
#'
#' @return
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr one_of
#' @export
#'
#' @examples
castor_collect_factor_groups <-
  function(
    dat = dat
  ) {

  dat$factor_group_labels_levels <- list()

  factor_groups <- unique(dat$`Field options`$`Option group name`)

  for (i_group in seq_along(factor_groups)) {
    dat$factor_group_labels_levels[[ factor_groups[i_group] ]] <-
      dat$`Field options` %>%
      dplyr::filter(`Option group name` == factor_groups[i_group]) %>%
      dplyr::select(dplyr::one_of(c("Option name", "Option value")))
  }

  return(dat)
}
