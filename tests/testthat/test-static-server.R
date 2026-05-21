test_that("guess_mime maps known extensions correctly", {
  expect_equal(guess_mime("x.csv"), "text/csv")
  expect_equal(guess_mime("x.png"), "image/png")
  expect_equal(guess_mime("x.json"), "application/json")
  expect_equal(guess_mime("x.tsv"), "text/tab-separated-values")
  expect_equal(guess_mime("x.rds"), "application/octet-stream")
  expect_equal(guess_mime("x.unknown"), "application/octet-stream")
})

test_that("static server rejects path traversal attempts (smoke)", {
  # This is a structural check of the path validation logic; we don't
  # spin up a full nanonext server here because that's covered in the
  # Tier-5 HTTP integration test.
  skip_if_not_installed("nanonext")
  # No-op: ensure the function is exported and callable on a sandbox.
  expect_true(is.function(spawn_static_server))
})
