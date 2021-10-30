# Test whether the contents of a config file can be read and parsed correctly
library(jsonlite)

config.path <- (function () find_config_files(dirs = c('config_etc')))()
config.content <- fromJSON(config.path)

test_that('Config file contents (JSON) can be parsed', {
    expect_equal(srcr:::.read_json_config(config.path), config.content)
    expect_named(config.content, c('src_name', 'src_args'),
                 ignore.order = TRUE)
})

test_that('Missing config causes error', {
    expect_error(srcr:::.read_json_config('does/not/exist'),
                 'No valid config files found.*')
})

