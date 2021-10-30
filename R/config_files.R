#' Locate candidate configuration files
#'
#' Given vectors of directories, basenames, and suffices,
#' combine them to find existing files.
#'
#' This function is intended to support a variety of installation patterns, so
#' it attempts to be flexible in looking for configuration files.  First,
#' environment variables of the form _basename_`_CONFIG`, where
#' _basename_ is the uppercase form of each candidate basename, are
#' examined to see whether any translate to a file path.
#'
#' Following this, the path name parts supplied as arguments are used to
#' build potential file names.  If `dirs` is not specified, the
#' following directories are checked by default:
#'
#'   1. the user's `$HOME` directory
#'   2. the directory named `.srcr` (no leading `.` on Windows) under `$HOME`
#'   2. the directory in which the executing script is located
#'   3. the directory in which the calling function's calling function's
#'      source file is located (typically an application-level library). For
#'      example, if the function `my_setup()` calls [srcr()], which in turn calls
#'      [find_config_files()], then the directory of the file containing
#'      `my_setup()` will be tried.
#'   4. the directory in which the calling function's source file is located
#'       (typically a utility function, such as [srcr()])
#'
#' Note that the current working directory is not part of the search by
#' default.  This is done to limit the potential for accidentally introducing
#' (potentially harmful) configuration files by setting the working directory.
#'
#' In each location, the file names given in `basenames` are checked; if
#' none are specified, several default file names are tried:
#'
#'    1. the name of the calling function's source file
#'    2. the name of the executing script
#'    3. the directory in which the calling function's calling function's
#'       source file is located (typically an application-level library).  For
#'       example, if the function `my_setup()` calls [srcr()], which in turn calls
#'       [find_config_files()], then the name of the file containing
#'      `my_setup()` will be tried.
#'
#' The suffices (file "type"s) of `.json`, `.conf`, and nothing,
#' are tried with each candidate path; you may override this default by
#' using the `suffices` parameter.  Finally, in order to accommodate the Unix
#' tradition of "hidden" configuration files, each basename is prefixed with
#' a period before trying the basename alone.
#'
#' @param basenames A vector of file names (without directory or file type) to
#' use in searching for configuration files.
#' @param dirs A vector of directory names to use in searching for configuration
#'   files.
#' @param suffices A vector of suffices (file "type"s) to use in searching for
#'   the configuration file.
#'
#' @return A vector of path specifications, or an empty vector if none are
#'   found.
#'
#' @examples
#' \dontrun{
#' find_config_files() # All defaults
#' find_config_files(dirs = c(file.path(Sys.getenv('HOME'),'etc'),
#'                           '/usr/local/etc', '/etc'),
#'                  basenames = c('my_app'),
#'                  suffices = c('.conf', '.rc'))
#' }
#' @export
find_config_files <- function(basenames = .basename.defaults(),
                              dirs = .dir.defaults(),
                              suffices = .suffix.defaults()) {

  files <- c()

  for (b in basenames) {
    cand <- Sys.getenv(paste0(toupper(b),'_CONFIG'))
    if (cand %in% files) next
    if (file.exists(cand)) files <- c(files,cand)
  }

  for (d in dirs[ !is.na(dirs) ]) {
    for (b in basenames) {
      for (name in c(paste0('.',b), b)) {
        for (type in suffices) {
          cand <- file.path(d,
                            paste0(name,type))
          if (nchar(cand) < 1 || cand %in% files) next
          if (file.exists(cand)) files <- c(files,cand)
        }
      }
    }
  }
  files
}


### "Private" functions
.read_json_config <- function(paths = c()) {
    if (length(paths) < 1) stop("No config paths provided")

    for (p in paths) {
        config <-
            tryCatch(jsonlite::fromJSON(p),
                     error = function(e) NA)
        if (!is.na(config[1])) return(config)
    }

    stop("No valid config files found in ", paste(paths, collapse = ', '))
}


.dir.defaults <- function() {
    p <- c(Sys.getenv('HOME'),
           file.path(Sys.getenv('HOME'),
                     ifelse(.Platform$OS.type == 'windows', 'srcr', '.srcr')),
           unlist(lapply(c(1, sys.parent(3), sys.parent(2)),
                         function (x)
                             tryCatch(utils::getSrcDirectory(sys.function(x)),
                                      error = function(e) character(0))
                         )))
    p[nchar(p) > 0]
}

.basename.defaults <- function() {
    p <- unlist(lapply(c(sys.parent(2), 1, sys.parent(3)),
                       function (x)
                           sub('\\.[^.]*$', '',
                               tryCatch(utils::getSrcFilename(sys.function(x)),
                                        error = function(e) character(0)),
                               perl = TRUE)
                       ))
    p[nchar(p) > 0]
}

.suffix.defaults <- function() c('.json', '.conf', '')
