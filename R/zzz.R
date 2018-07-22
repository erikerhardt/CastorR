## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  start_message <- c( "CastorR:  Formatting Castor files\n\n"
                      , "  by Dr. Erik Barry Erhardt <erik@StatAcumen.com>\n\n"
                    )
  packageStartupMessage(start_message)
  invisible()
}


#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return
#'
#' @examples
#' getOption("CastorR.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.CastorR <- list(
    #CastorR.path = "~/R-dev",
    CastorR.install.args  = "",
    CastorR.name          = "Erik Barry Erhardt",
    CastorR.desc.author   = "Erik Erhardt <erik@StatAcumen.com> [aut, cre]",
    CastorR.desc.license  = "Proprietary",
    CastorR.desc.suggests = NULL,
    CastorR.desc          = list()
  )
  toset <- !(names(op.CastorR) %in% names(op))
  if (any(toset)) options(op.CastorR[toset])

  invisible()
}
