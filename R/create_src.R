#' Connect to database using config file
#'
#' Set up a or DBI or legacy dplyr database connection using information from a
#' JSON configuration file, and return the connection.
#'
#' The configuration file must provide all of the information necessary to set
#' up the DBI connection or dplyr src.  Given the variety of ways a data source
#' can be specified, the JSON must be a hash containing at least two elements:
#'
#' * The `src_name` key points to a string containing name of a DBI driver
#'   method (e.g. `SQLite`), as one might pass to [DBI::dbDriver()], or an old-style
#'   dplyr function that sets up the data source (e.g.  [dplyr::src_postgres()].
#'   If the value associated with `src_name` begins with 'src_', it is taken as the
#'   latter, otherwise it is taken as the former.  In this case, an attempt will
#'   be made to load the appropriate DBI-compliant database library (e.g. RSQLite
#'   for the above example) if it hasn't already been loaded.
#' * The `src_args` key points to a nested hash, whose keys are the arguments
#'   to that function, and whose values are the argument values.
#'
#' To locate the necessary configuration file, you can use all of the arguments
#' taken by [find_config_files()], but remember that the contents of the file
#' must be JSON, regardless of the file's name.  Alternatively, if `paths` is
#' present, only the specified paths are checked. The first file that exists, is
#' readable, and evaluates as legal JSON is used as the source of configuration
#' data.
#'
#' If your deployment strategy does not make use of configuration files (e.g. you
#' access configuration data via a web service or similar API), you may also
#' pass a list containing the configuration data directly via the `config`
#' parameter.  In this case, no configuration files are used.
#'
#' Once the connection is established, the `post_connect_sql` and
#' `post_connect_fun` elements of the configuration data can be used to perform
#' additional processing to set session characteristics, roles, etc.  However,
#' because this entails the configuration file providing code that you won't see
#' prior to runtime, you need to opt in to these features.  You can make this
#' choice globally by setting the `srcr.allow_post_connect` option via
#' [base::options()].
#'
#' @inheritParams find_config_files
#' @param paths A vector of full path names for the configuration file.  If
#'   present, only these paths are checked; [find_config_files()] is not called.
#' @param config A list containing the configuration data, to be used instead of
#'   reading a configuration file, should you wish to skip that step.
#' @param allow_post_connect A vector specifying what session setup you will
#'   permit after the connection is established.  If any element of the vector
#'   is `sql`, then the post_connect_sql section of the configuration file is
#'   executed.  Similarly, if any element is `fun`, then the post_connect_fun
#'   section of the config file is executed (after post_connect_sql, if both are
#'   present and allowed).
#'
#' @return A database connection.  The specific class of the object is determined
#'   by the `src_name` in the configuration data.
#'
#' @examples
#' \dontrun{
#' # Search all the (filename-based) defaults
#' srcr()
#'
#' # "The usual"
#' srcr('myproj_prod')
#'
#' # Look around
#' srcr(dirs = c(Sys.getenv('PROJ_CONF_DIR'), 'var/lib', getwd()),
#'      basenames = c('myproj', Sys.getenv('PROJ_NAME')) )
#'
#' # No defaults
#' srcr(paths = c('/path/to/known/config.json'))
#' srcr(config =
#'        list(src_name = 'Postgres',
#'             src_args = list(host = 'my.host', dbname = 'my_db', user = 'me'),
#'             post_connect_sql = 'set role project_role;'))
#' }
#' @export
srcr <- function(basenames = NA, dirs = NA, suffices = NA,
                 paths = NA, config = NA,
                 allow_post_connect =
                   getOption('srcr.allow_post_connect', c())) {

    if (all(is.na(config))) {
        if (all(is.na(paths))) {
            args <- mget(c('dirs','basenames', 'suffices'))
            args <- args[ !is.na(args) ]
            paths <- do.call(find_config_files, args)
            if (length(paths) < 1)
                stop('No config files found for ',
                     paste(vapply(names(args),
                                  function (x) paste(x, '=',
                                                     paste(args[[x]],
                                                           collapse = ', ')),
                                  FUN.VALUE = character(1)),
                           collapse = '; '))
        }
        config <- .read_json_config(paths)
    }

  stopifnot(exprs = {
    ! is.na(config)
    is.character(config$src_name)
    is.list(config$src_args)
  })

  if (! grepl('^src_', config$src_name)) {
        drv <- tryCatch(DBI::dbDriver(config$src_name), error = function(e) NULL)
        if (is.null(drv)) {
            lib <- tryCatch(library(config$src_name,
                                    character.only = TRUE),
                            error = function(e) NULL)
            if (is.null(lib))
                library(paste0('R', config$src_name), character.only = TRUE)
            drv <- DBI::dbDriver(config$src_name)
        }
        config$src_name <- function(...) DBI::dbConnect(drv,...)
    }

    db <- do.call(config$src_name, config$src_args)

    if (any(allow_post_connect == 'sql') &&
        exists('post_connect_sql', where = config)) {
        con <- if (inherits(db, 'src_dbi')) db$con else db
        lapply(config$post_connect_sql, function(x) DBI::dbExecute(con, x))
    }
    if (any(allow_post_connect == 'fun') &&
        exists('post_connect_fun', where = config)) {
        pc <- eval(parse(text = paste(config$post_connect_fun,
                                      sep = "\n", collapse = "\n")))
        db <- do.call(pc, list(db))
    }
    db
}
