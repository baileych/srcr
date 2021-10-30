# Test that srcr() can set up a simple SQLite src

has_sqlite <- function () {
    if (! require(RSQLite, quietly = TRUE))
        skip("RSQLite needed to test src instantiation")
}

get.src <- function() srcr(dirs = c('config_etc'))
get.con <- function() srcr(basenames = c('test_src_dbi'),
                           dirs = c('config_etc'))

test.data <- data.frame(ids = c(1, 2, 3), strs = c('a', 'b', 'c'))

test_that('Proper dplyr src is set up by srcr()', {
    has_sqlite()
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_is((mysrc <- get.src()), 'src_sql')
    expect_is((test.table <- copy_to(mysrc, test.data, name = 'test_table')),
              'tbl')
    expect_equal(collect(tally(test.table))$n, 3)
})

test_that('Proper DBI connection is set up by srcr()', {
    has_sqlite()
    expect_is((mysrc <- get.con()), 'SQLiteConnection')
    expect_is((test.table <- copy_to(mysrc, test.data, name = 'test_table')),
              'tbl')
    expect_equal(tbl(mysrc,'test_table'), test.table)
    if (any(class(mysrc) == 'SQLiteConnection')) DBI::dbDisconnect(mysrc)
})
