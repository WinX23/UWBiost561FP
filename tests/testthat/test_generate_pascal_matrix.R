context("Testing generate_pascal_matrix")

test_that("generate_tri_pascal_matrix works", {
  set.seed(10)
  matrix <- generate_tri_pascal_matrix(7, "upper")

  expect_equal(unique(diag(matrix)), 1)
  expect_true(is.matrix(matrix))
  expect_equal(matrix[3,6], dim(combn(5,2))[2])
})

test_that("generate_symm_pascal_matrix works", {
  set.seed(10)
  matrix <- generate_symm_pascal_matrix(7)

  expect_true(is.matrix(matrix))
  expect_equal(matrix, t(matrix))
})

test_that("generate_symm_pascal_matrix ouput is correct", {
  set.seed(10)
  matrix <- generate_symm_pascal_matrix(7)
  matrix_upper <- generate_tri_pascal_matrix(7, "upper")
  matrix_lower <- generate_tri_pascal_matrix(7, "lower")

  expect_true(isSymmetric(matrix))
  expect_equal(matrix, matrix_lower %*% matrix_upper)
})
