#' Assign variable labels and variable classes
#'
#' @param dat
#'
#' @return
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr one_of
#' @importFrom tibble tibble
#' @importFrom lubridate dmy
#' @importFrom lubridate dmy_hm
#' @importFrom lubridate dmy_hms
#' @importFrom labelled var_label
#' @export
#'
#' @examples
castor_assign_variable_labels <-
  function(
    dat = dat
  ) {

  # colums of interest
  var_names_for_labels <-
    c("Variable name", "Field type", "Field label", "Optiongroup name")

  ####################
  ## Label variables

  ##########
  ## Data in `Study results` has labels in `Study variable list`

  # library(dplyr)
  # keep only columns with Variable names and label information
  `Study variable list` <-
    dat$`Study variable list` %>%
    dplyr::filter(!is.na(`Variable name`)) %>%
    dplyr::select(dplyr::one_of(var_names_for_labels))

  ## only keep variables that exist in data (in case there is ever a mismatch)
  #`Study variable list` <-
  #  `Study variable list`[(`Study variable list`$`Variable name` %in% names(dat$`Study results`)), ]

  # library(labelled)
  ## assign variable labels
  for (i_var_name in seq_along(`Study variable list`$`Variable name`)) {
    labelled::var_label(
      dat$`Study results`[,`Study variable list`$`Variable name`[i_var_name]]
      ) <-
      `Study variable list`$`Field label`[i_var_name]

      #labelled::var_label(dat$`Study results`[,`Study variable list`$`Variable name`[i_var_name]])
      # labelled::var_label(dat$`Study results`)
  }

  ##########
  ## Data in other data sheets (between`Study results` and `Study variable list`)
  ##   has labels in `Report variable list`

  # keep only columns with "Variable name" and "Field label"
  `Report variable list` <-
    dat$`Report variable list` %>%
    dplyr::filter(!is.na(`Variable name`)) %>%
    dplyr::select(dplyr::one_of(var_names_for_labels))

  # Data sheets
  # Study results sheets
  names_sheets_study_data       <- c("Study results", "Study variable list")

  # Other fixed names are "Report variable list" and "Field options"
  # this variable is only used to help choose the "other" sheets
  names_sheets_report_variables <- c("Report variable list", "Field options")

  sheet_master_data <-
    (names(dat) %in% names_sheets_study_data) %>%
    which()
  # all other sheets
  sheet_numbers_others <-
    which( !(names(dat) %in% c(names_sheets_study_data, names_sheets_report_variables)))


  for (i_sheet in sheet_numbers_others) {
    # find column numbers for variables with matching labels
    vars_to_label <-
      which(names(dat[[i_sheet]]) %in% `Report variable list`$`Variable name`)

    if (length(vars_to_label) == 0) {
      next
    }

    for (i_var in vars_to_label) {

      var_name_to_match <-
        names(dat[[i_sheet]][, i_var])
      var_label_to_match <-
        subset(`Report variable list`, `Variable name` == var_name_to_match)$`Field label`

      labelled::var_label(
        dat[[i_sheet]][, i_var]
        ) <-
        var_label_to_match

        # labelled::var_label(dat[[i_sheet]])
    }
  }


  ####################
  ## assign factor levels and labels

  ##########
  ## Data in `Study results` has labels in `Study variable list`

  study_variable_list_factors <-
    `Study variable list` %>%
    dplyr::filter(!is.na(`Optiongroup name`))


  ## assign variable labels
  for (i_var_name in seq_along(study_variable_list_factors$`Variable name`)) {

    #study_variable_list_factors$`Optiongroup name`[i_var_name]

    current_varname <-
      study_variable_list_factors$`Variable name`[i_var_name]
    current_optiongroup <-
      dat$factor_group_labels_levels[[study_variable_list_factors$`Optiongroup name`[i_var_name]]]



    #dat$`Study results`[,current_varname]

    dat$`Study results`[,current_varname] <-
      factor(
        x       = as.numeric(unlist(dat$`Study results`[,current_varname], use.names=FALSE))
      , levels  = as.numeric(current_optiongroup$`Option value`)
      , labels  = current_optiongroup$`Option name`
      , ordered = TRUE
      )

    #dat$`Study results`[,current_varname]
    #str(dat$`Study results`[,current_varname])

  }


  ##########
  ## Data in other data sheets (between`Study results` and `Study variable list`)
  ##   has labels in `Report variable list`

  report_variable_list_factors <-
    `Report variable list` %>%
    dplyr::filter(!is.na(`Optiongroup name`))


  for (i_sheet in sheet_numbers_others) {
    # find column numbers for variables with matching labels
    vars_to_label <-
      which(names(dat[[i_sheet]]) %in% report_variable_list_factors$`Variable name`)

    if (length(vars_to_label) == 0) {
      next
    }

    for (i_var in vars_to_label) {

      current_varname <-
        names(dat[[i_sheet]][, i_var])

      sub_report_variable_list_factors <-
        report_variable_list_factors %>%
        dplyr::filter(`Variable name` == !!current_varname)

      current_optiongroup <-
        dat$factor_group_labels_levels[[sub_report_variable_list_factors$`Optiongroup name`]]

      dat[[i_sheet]][, i_var] <-
        factor(
          x       = as.numeric(unlist(dat[[i_sheet]][, i_var], use.names=FALSE))
        , levels  = as.numeric(current_optiongroup$`Option value`)
        , labels  = current_optiongroup$`Option name`
        , ordered = TRUE
        )

      #dat[[i_sheet]][, i_var]
      #str(dat[[i_sheet]][, i_var])

    }
  }

  #str(dat[[1]])
  #str(dat[[2]])



  ####################
  ## data classes for date, datetime, and numeric

  # field types for which to change data class
  field_type_list <-
    c(
      "date"
    , "datetime"
    , "calculation"
    , "numeric"
    )

  ##########
  ## Data in `Study results` has labels in `Study variable list`


  study_variable_list_class <-
    `Study variable list` %>%
    dplyr::filter(`Field type` %in% field_type_list)

  # library(tibble)
  # Add the "Record Creation Date" column
  study_variable_list_class <-
    rbind(
      study_variable_list_class
    , tibble::tibble(
        `Variable name`    = "Record Creation Date"
      , `Field type`       = "datetime_ss"
      , `Field label`      = NA
      , `Optiongroup name` = NA
      )
    )

  ## assign variable labels
  for (i_var_name in seq_along(study_variable_list_class$`Variable name`)) {

    #study_variable_list_class$`Field type`[i_var_name]

    current_varname <-
      study_variable_list_class$`Variable name`[i_var_name]

    ## XXX DEBUG
    if (FALSE) {
      cat("Study results:", i_var_name, current_varname, "\n")
    }


    #dat$`Study results`[,current_varname]

    # library(lubridate)
    # date
    if (study_variable_list_class$`Field type`[i_var_name] == "date") {
      dat$`Study results`[,current_varname] <-
        lubridate::dmy(unlist(dat$`Study results`[,current_varname], use.names=FALSE))
    }

    # datetime
    if (study_variable_list_class$`Field type`[i_var_name] == "datetime") {
      dat$`Study results`[,current_varname] <-
        lubridate::dmy_hm(unlist(dat$`Study results`[,current_varname], use.names=FALSE))
    }

    # datetime_ss (special for seconds)
    if (study_variable_list_class$`Field type`[i_var_name] == "datetime_ss") {
      dat$`Study results`[,current_varname] <-
        lubridate::dmy_hms(unlist(dat$`Study results`[,current_varname], use.names=FALSE))
    }

    # numeric
    if (study_variable_list_class$`Field type`[i_var_name] == "numeric") {
      dat$`Study results`[,current_varname] <-
        as.numeric(unlist(dat$`Study results`[,current_varname], use.names=FALSE))
    }

    # calculation
    if (study_variable_list_class$`Field type`[i_var_name] == "calculation") {

      ## type.convert() will change it to numeric if it can, otherwise it keeps it as character
      dat$`Study results`[,current_varname] <-
        type.convert(unlist(dat$`Study results`[,current_varname], use.names=FALSE), as.is=TRUE)
    }

    #dat$`Study results`[,current_varname]
    #str(dat$`Study results`[,current_varname])

  }


  ##########
  ## Data in other data sheets (between`Study results` and `Study variable list`)
  ##   has labels in `Report variable list`

  report_variable_list_class <-
    `Report variable list` %>%
    dplyr::filter(`Field type` %in% field_type_list)

  # Add the "Record Creation Date" column
  report_variable_list_class <-
    rbind(
      report_variable_list_class
    , tibble::tibble(
        `Variable name`    = "Report Creation Date"
      , `Field type`       = "datetime_ss"
      , `Field label`      = NA
      , `Optiongroup name` = NA
      )
    )


  for (i_sheet in sheet_numbers_others) {
    # find column numbers for variables with matching labels
    vars_to_class <-
      which(names(dat[[i_sheet]]) %in% report_variable_list_class$`Variable name`)

    if (length(vars_to_class) == 0) {
      next
    }

    for (i_var in vars_to_class) {

      current_varname <-
        names(dat[[i_sheet]][, i_var])

      sub_report_variable_list_class <-
        report_variable_list_class %>%
        dplyr::filter(`Variable name` == !!current_varname)

      ## XXX DEBUG
      if (FALSE) {
        cat(i_sheet, i_var, current_varname, "\n")
      }


      #dat[[i_sheet]][, i_var]

      # date
      if (sub_report_variable_list_class$`Field type` == "date") {
        dat[[i_sheet]][, i_var] <-
          lubridate::dmy(unlist(dat[[i_sheet]][, i_var], use.names=FALSE))
      }

      # datetime
      if (sub_report_variable_list_class$`Field type` == "datetime") {
        dat[[i_sheet]][, i_var] <-
          lubridate::dmy_hm(unlist(dat[[i_sheet]][, i_var], use.names=FALSE))
      }

      # datetime_ss (special for seconds)
      if (sub_report_variable_list_class$`Field type` == "datetime_ss") {
        dat[[i_sheet]][, i_var] <-
          lubridate::dmy_hms(unlist(dat[[i_sheet]][, i_var], use.names=FALSE))
      }

      # numeric
      if (sub_report_variable_list_class$`Field type` == "numeric") {
        dat[[i_sheet]][, i_var] <-
          as.numeric(unlist(dat[[i_sheet]][, i_var], use.names=FALSE))
      }

      # calculation
      if (sub_report_variable_list_class$`Field type` == "calculation") {

        ## type.convert() will change it to numeric if it can, otherwise it keeps it as character
        dat[[i_sheet]][, i_var] <-
          type.convert(unlist(dat[[i_sheet]][, i_var], use.names=FALSE), as.is=TRUE)
      }

      #dat[[i_sheet]][, i_var]
      #str(dat[[i_sheet]][, i_var])

    }
  }

  return(dat)
}
