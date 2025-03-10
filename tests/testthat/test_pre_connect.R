# Test that srcr() can handle post-connect params

has_sqlite <- function () {
  if (! require(RSQLite, quietly = TRUE))
    skip("RSQLite needed to test src instantiation")
}

get.con.yes <- function(tag) {
  con <- srcr(basenames = paste0('test_src_', tag),
              dirs = c('config_etc'),
              allow_config_code = c('dummy', 'sql', 'fun'))
  withr::defer_parent(DBI::dbDisconnect(con))
  con
}
get.con.no <- function(tag) {
  con <- srcr(basenames = paste0('test_src_', tag),
            dirs = c('config_etc'))
  withr::defer_parent(DBI::dbDisconnect(con))
  con
}

test.data <- data.frame(id = c(1L, 2L, 3L), str = c('a', 'b', 'test_'),
                        stringsAsFactors = FALSE)
pre.data <- data.frame(id = c(1L, 2L, 3L), tag = c('a', 'b', 'pre_'),
                        stringsAsFactors = FALSE)
test_that('Pre-connect function executes if parameter permits', {
  has_sqlite()
  expect_is((mysrc <- get.con.yes('prenodb')), 'SQLiteConnection')
  expect_is((pre.table <- tbl(mysrc,'pre_table')), 'tbl')
  expect_equal(as.data.frame(collect(pre.table)), pre.data)
})

test_that('Pre-connect function executes if option permits', {
  has_sqlite()
  oldopt = getOption('srcr.allow_config_code')
  on.exit(options('srcr.allow_config_code' = oldopt))
  options('srcr.allow_config_code' = c('dummy', 'fun'))
  expect_is((mysrc <- get.con.no('prenodb')), 'SQLiteConnection')
  expect_is((pre.table <- tbl(mysrc,'pre_table')), 'tbl')
  expect_equal(as.data.frame(collect(pre.table)), pre.data)
})

test_that('Pre-connect function does not execute unless permitted', {
  has_sqlite()
  expect_is((mysrc <- get.con.no('predb')), 'SQLiteConnection')
  expect_error((pre.table <- tbl(mysrc,'pre_table')), 'no such table')
})

test_that('Pre-connect function can replace connection', {
  has_sqlite()
  expect_is((mysrc <- get.con.yes('predb')), 'SQLiteConnection')
  expect_is((pre.table <- tbl(mysrc,'pre_table')), 'tbl')
  expect_is((test.table <- tbl(mysrc,'test_table')), 'tbl')
  expect_equal(as.data.frame(collect(pre.table)), pre.data)
  expect_equal(as.data.frame(collect(test.table)), test.data)
})

