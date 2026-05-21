test_that("validate_expression_matrix accepts well-formed input", {
  tmp <- write_expr_csv(n_genes = 5L, n_samples = 4L)
  withr::defer(unlink(tmp))
  res <- validate_expression_matrix(tmp)
  expect_true(res$valid)
  expect_equal(res$n_genes, 5L)
  expect_equal(res$n_samples, 4L)
  expect_length(res$sample_names, 4L)
})

test_that("validate_expression_matrix flags duplicate gene IDs", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  m <- matrix(1:8, nrow = 4L, ncol = 2L,
              dimnames = list(c("g1", "g1", "g2", "g3"), c("s1", "s2")))
  utils::write.csv(m, tmp)
  res <- validate_expression_matrix(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("[Dd]uplicate", res$issues)))
})

test_that("validate_expression_matrix flags non-numeric cells", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(s1 = c("1", "abc", "3"),
                   s2 = c("4", "5", "6"),
                   row.names = c("g1", "g2", "g3"))
  utils::write.csv(df, tmp)
  res <- validate_expression_matrix(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("non-numeric", res$issues)))
})

test_that("validate_expression_matrix flags too-few samples", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  m <- matrix(1:3, nrow = 3L, ncol = 1L,
              dimnames = list(c("g1", "g2", "g3"), "s1"))
  utils::write.csv(m, tmp)
  res <- validate_expression_matrix(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("at least 2", res$issues)))
})

test_that("validate_expression_matrix flags empty file", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  writeLines("", tmp)
  res <- validate_expression_matrix(tmp)
  expect_false(res$valid)
})

test_that("validate_gene_stats accepts canonical columns", {
  tmp <- write_gene_stats_csv(n = 10L)
  withr::defer(unlink(tmp))
  res <- validate_gene_stats(tmp)
  expect_true(res$valid)
  expect_equal(res$n_genes, 10L)
})

test_that("validate_gene_stats reports missing columns with alias hints", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(gene_id = "g1", log2FoldChange = 1.5, padj = 0.01)
  utils::write.csv(df, tmp, row.names = FALSE)
  res <- validate_gene_stats(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("rename 'gene_id' to 'id'", res$issues)))
  expect_true(any(grepl("rename 'log2FoldChange' to 'logFC'", res$issues)))
  expect_true(any(grepl("rename 'padj' to 'pvalue'", res$issues)))
})

test_that("validate_gene_stats flags duplicate IDs", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(id = c("g1", "g1", "g2"),
                   logFC = c(1, 2, 3),
                   pvalue = c(0.1, 0.2, 0.3))
  utils::write.csv(df, tmp, row.names = FALSE)
  res <- validate_gene_stats(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("[Dd]uplicate", res$issues)))
})

test_that("validate_gene_stats flags non-numeric stat columns", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(id = c("g1", "g2"),
                   logFC = c("1", "xyz"),
                   pvalue = c(0.1, 0.2))
  utils::write.csv(df, tmp, row.names = FALSE)
  res <- validate_gene_stats(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("non-numeric", res$issues)))
})

test_that("validate_experimental_design accepts canonical columns", {
  tmp <- write_design_csv(paste0("s", 1:6))
  withr::defer(unlink(tmp))
  res <- validate_experimental_design(tmp)
  expect_true(res$valid)
  expect_equal(res$n_samples, 6L)
  expect_setequal(res$groups, c("Control", "Treatment"))
  expect_false(res$is_paired)
})

test_that("validate_experimental_design detects paired designs", {
  tmp <- write_design_csv(paste0("s", 1:6),
                          groups = rep(c("A", "B"), each = 3L),
                          pairs = rep(1:3, times = 2L))
  withr::defer(unlink(tmp))
  res <- validate_experimental_design(tmp)
  expect_true(res$valid)
  expect_true(res$is_paired)
})

test_that("validate_experimental_design rejects non-integer pair column", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(sample = c("s1", "s2"),
                   group = c("A", "B"),
                   pair = c("one", "two"))
  utils::write.csv(df, tmp, row.names = FALSE)
  res <- validate_experimental_design(tmp)
  expect_false(res$valid)
  expect_true(any(grepl("integers", res$issues)))
})

test_that("validate_experimental_design rejects missing columns", {
  tmp <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp))
  df <- data.frame(name = c("s1"), kind = c("A"))
  utils::write.csv(df, tmp, row.names = FALSE)
  res <- validate_experimental_design(tmp)
  expect_false(res$valid)
})
