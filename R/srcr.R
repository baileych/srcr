#' srcr: Simplify connections to database sources
#'
#' Connecting to databases requires boilerplate code to specify
#' connection parameters and to set up sessions properly with the DBMS.
#' This package provides a simple tool to fill two purposes: abstracting
#' connection details, including secret credentials, out of your source
#' code and managing configuration for frequently-used database connections
#' in a persistent and flexible way, while minimizing requirements on the
#' runtime environment.
#'
#' @docType package
#' @name srcr
#'
#' @import dplyr
#' @import DBI
#' @export srcr
#' @export find_config_files
#'
#' @keywords internal
"_PACKAGE"
NULL
