test_that("is_safe_uri accepts http and https", {
  expect_true(is_safe_uri("http://example.com/data.csv"))
  expect_true(is_safe_uri("https://example.com/data.csv"))
})

test_that("is_safe_uri rejects file://, data:, and ftp", {
  expect_false(is_safe_uri("file:///etc/passwd"))
  expect_false(is_safe_uri("data:text/csv;base64,abc"))
  expect_false(is_safe_uri("ftp://ftp.example.com/x"))
})

test_that("is_safe_uri rejects malformed inputs", {
  expect_false(is_safe_uri(NULL))
  expect_false(is_safe_uri(c("http://a", "http://b")))
  expect_false(is_safe_uri(42))
})

test_that("fetch_to_tempfile rejects unsafe URIs with RCPA_ALLOW_LOCAL_URIS off", {
  withr::with_envvar(c("RCPA_ALLOW_LOCAL_URIS" = "FALSE"), {
    expect_error(fetch_to_tempfile("file:///etc/passwd"), "Refusing")
    expect_error(fetch_to_tempfile("data:text/csv,abc"),  "Refusing")
  })
})

test_that("fetch_to_tempfile accepts file:// when allow flag is set", {
  src <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", src)
  withr::defer(unlink(src))
  withr::with_envvar(c("RCPA_ALLOW_LOCAL_URIS" = "TRUE"), {
    out <- fetch_to_tempfile(paste0("file://", src))
    withr::defer(unlink(out))
    expect_true(file.exists(out))
    expect_match(paste(readLines(out), collapse = "\n"), "a,b")
  })
})

test_that("rewrite_local_uri is a no-op when RCPA_CODER_HOST is unset", {
  withr::with_envvar(c("RCPA_CODER_HOST" = ""), {
    expect_identical(rewrite_local_uri("http://127.0.0.1:9005/x.csv"),
                     "http://127.0.0.1:9005/x.csv")
  })
})

test_that("rewrite_local_uri rewrites localhost when RCPA_CODER_HOST is set", {
  withr::with_envvar(c("RCPA_CODER_HOST" = "main--ziee--khoi.example.com"), {
    out <- rewrite_local_uri("http://127.0.0.1:9005/results/r1/x.csv")
    expect_identical(
      out,
      "https://9005--main--ziee--khoi.example.com/results/r1/x.csv")
  })
})
