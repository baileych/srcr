# Testing of internal defaults for config file path parts

source(file.path('config_etc','defaults_fun.R'))

twostep.dir <- function () bounce.dir()
twostep.basename <- function () bounce.basename()
twostep.suffix <- function () bounce.suffix()

here <- normalizePath(utils::getSrcDirectory(twostep.dir))
there <- normalizePath(utils::getSrcDirectory(bounce.dir))

this <- sub('\\.[^.]*$', '', utils::getSrcFilename(twostep.basename),
            perl = TRUE)
that <- sub('\\.[^.]*$', '', utils::getSrcFilename(bounce.basename),
            perl = TRUE)

test_that('Config file defaults are correct',{
    expect_equal(normalizePath(twostep.dir()),
                 c(Sys.getenv('HOME'),
                   here, there))
    expect_equal(twostep.basename(),
                 c(that, this))
    expect_equal(Argos:::.suffix.defaults(),
                 c('.json', '.conf', ''))
})
