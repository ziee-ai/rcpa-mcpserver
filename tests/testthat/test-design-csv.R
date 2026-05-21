test_that("parse_design_csv parses comma-separated input", {
  tmp <- write_design_csv(c("s1", "s2", "s3", "s4"),
                          groups = c("A", "A", "B", "B"))
  withr::defer(unlink(tmp))
  out <- parse_design_csv(tmp)
  expect_length(out, 4L)
  expect_equal(out[[1L]]$sample, "s1")
  expect_equal(out[[1L]]$group,  "A")
  expect_null(out[[1L]]$pair)
})

test_that("parse_design_csv parses tab-separated input", {
  tmp <- tempfile(fileext = ".tsv")
  withr::defer(unlink(tmp))
  writeLines(c("sample\tgroup",
               "s1\tA",
               "s2\tB"), tmp)
  out <- parse_design_csv(tmp)
  expect_length(out, 2L)
  expect_equal(out[[2L]]$group, "B")
})

test_that("parse_design_csv carries pair column as integer", {
  tmp <- write_design_csv(paste0("s", 1:4),
                          groups = c("A", "B", "A", "B"),
                          pairs = c(1L, 1L, 2L, 2L))
  withr::defer(unlink(tmp))
  out <- parse_design_csv(tmp)
  expect_equal(out[[1L]]$pair, 1L)
  expect_equal(out[[3L]]$pair, 2L)
})

test_that("parse_design_csv rejects non-integer pair values", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(sample = c("s1", "s2"),
                   group  = c("A", "B"),
                   pair   = c("one", "two"))
  utils::write.csv(df, tmp, row.names = FALSE)
  expect_error(parse_design_csv(tmp), "must be an integer")
})

test_that("parse_design_csv errors on missing columns", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(name = c("s1"), kind = c("A"))
  utils::write.csv(df, tmp, row.names = FALSE)
  expect_error(parse_design_csv(tmp), "missing required columns")
})

test_that("parse_design_csv errors on empty file", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  writeLines("sample,group", tmp)
  expect_error(parse_design_csv(tmp), "empty")
})

test_that("parse_design_csv trims whitespace in sample and group", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  writeLines(c("sample,group",
               "  s1 ,  A ",
               "s2,B"), tmp)
  out <- parse_design_csv(tmp)
  expect_equal(out[[1L]]$sample, "s1")
  expect_equal(out[[1L]]$group,  "A")
})
