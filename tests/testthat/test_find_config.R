# Testing identification of config files


here <- normalizePath(utils::getSrcDirectory(function (x) x))
conf.json <- sub('\\.[^.]*$', '.json', utils::getSrcFilename(function (x) x),
                 perl = TRUE)
conf.conf <- sub('.json', '.conf', conf.json)
base <- sub('.json', '', conf.json)

finder <- function (...) find_config_files(...)

env.test <- function () {
    env.name <- paste0(toupper(base),'_CONFIG')
    file.name <- c(file.path(here, 'config_etc', 'find_by_env.rc'))
    names(file.name) <- env.name
    old <- Sys.getenv(env.name)
    do.call(Sys.setenv, as.list(file.name))
    rslt <- finder()
    if (nchar(old) > 0) {
        file.name <- old
        names(file.name) <- env.name
        do.call(Sys.setenv, as.list(file.name))
    }
    else Sys.unsetenv(env.name)
    rslt
}

# Do this dynamically as (at least) some Windows systems don't like
# leading-dot filenames
hidden.test <- function() {
    test.path <- file.path(here, paste0('.', base, '_hidden.json'))
    created <- try(cat('{ "suffix" : "json"}\n', file = test.path))
    if (! is.null(created)) return(test.path)
    # Need to canonize here before removing file else normalizePath() chokes
    rslt <- normalizePath(finder(basenames = c(paste0(base, '_hidden'))))
    file.remove(test.path)
    rslt
}


if (length(collisions <- grep('TBL_ARGOS_CONFIG|TEST_FIND_CONFIG_CONFIG',
                              names(Sys.getenv()))) > 0)
    warning("Possible interfering environment variables detected:\n\t",
            paste(names(Sys.getenv())[collisions], sep = "\n\t"),"\n",
            "These may cause multiple test failures!\n",
            call. = FALSE)

def.conf <- finder()
alt.conf <- finder(suffices = c('.conf', '.json'))
alt.dir <- finder(dirs = c('config_etc'))

test_that('Correct config file is found',{
    expect_equal(normalizePath(def.conf)[1],
                 file.path(here,conf.json))
    expect_equal(normalizePath(alt.conf)[1],
                 file.path(here,conf.conf))
    expect_equal(hidden.test()[1],
                 file.path(here, paste0('.', base, '_hidden.json')))
    expect_equal(normalizePath(alt.dir)[1],
                 file.path(here, 'config_etc', base))
    expect_equal(normalizePath(env.test())[1],
                 file.path(here, 'config_etc', 'find_by_env.rc'))
}
          )
