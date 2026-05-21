test_that("make_job_script writes exactly 3 lines + params.json", {
  out <- withr::local_tempdir()
  res <- make_job_script(out, "demo", "de_analysis",
                         params = list(a = 1L, b = "two", c = NULL))
  expect_true(file.exists(res$script_path))
  expect_true(file.exists(res$params_path))
  lines <- readLines(res$script_path)
  expect_length(lines, 3L)
  expect_match(lines[[1L]], "^library\\(jsonlite\\)$")
  expect_match(lines[[2L]], "params <- jsonlite::fromJSON")
  expect_match(lines[[3L]], "^source\\(")
  params <- jsonlite::fromJSON(res$params_path, simplifyVector = FALSE)
  expect_equal(params$a, 1L)
  expect_equal(params$b, "two")
})

test_that("make_job_script creates the out_dir if missing", {
  base <- withr::local_tempdir()
  out <- file.path(base, "nested", "deep", "dir")
  expect_false(dir.exists(out))
  res <- make_job_script(out, "demo", "de_analysis", params = list())
  expect_true(dir.exists(out))
  expect_true(file.exists(res$script_path))
})

test_that("make_job_script errors when template missing", {
  out <- withr::local_tempdir()
  expect_error(make_job_script(out, "demo", "no_such_template",
                               params = list()),
               "not found")
})

test_that("make_run_dir produces unique directories", {
  base <- withr::local_tempdir()
  d1 <- make_run_dir(base)
  d2 <- make_run_dir(base)
  expect_true(dir.exists(d1$dir))
  expect_true(dir.exists(d2$dir))
  expect_false(identical(d1$run_id, d2$run_id))
})
