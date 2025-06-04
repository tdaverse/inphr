# Test basic functionality with mock data
test_two_sample_test_basic <- function() {
  # Mock persistence sets
  ps1 <- trefoils1[1:5] # Assuming trefoils1 is a persistence set
  ps2 <- trefoils2[1:5] # Assuming trefoils2 is a persistence set

  # Mock parse_inputs function
  parse_inputs <- function(x, y, dimension, p, ncores) {
    list(
      D = matrix(runif(100), 10, 10),
      sample_sizes = c(5, 5)
    )
  }

  # Mock null_spec function
  null_spec <- function(...) NULL

  result <- two_sample_test(ps1, ps2, B = 10L)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
}

# Test parameter validation
test_two_sample_test_params <- function() {
  ps1 <- trefoils1[1:5] # Assuming trefoils1 is a persistence set
  ps2 <- trefoils2[1:5] # Assuming trefoils2 is a persistence set

  expect_error(two_sample_test(ps1, ps2, dimension = -1L))
  expect_error(two_sample_test(ps1, ps2, p = 0L))
  expect_error(two_sample_test(ps1, ps2, B = 0L))
  expect_error(two_sample_test(ps1, ps2, npc = "invalid"))
}

# Test with distance matrix input
test_two_sample_test_dist <- function() {
  D <- as.dist(matrix(runif(100), 10, 10))
  sample_sizes <- c(5L, 5L)

  result <- two_sample_test(D, sample_sizes, B = 10L)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
}

# Test verbose output
test_two_sample_test_verbose <- function() {
  ps1 <- trefoils1[1:5] # Assuming trefoils1 is a persistence set
  ps2 <- trefoils2[1:5] # Assuming trefoils2 is a persistence set

  expect_silent(two_sample_test(ps1, ps2, B = 10L, verbose = FALSE))
}

# Test different npc methods
test_two_sample_test_npc <- function() {
  ps1 <- trefoils1[1:5] # Assuming trefoils1 is a persistence set
  ps2 <- trefoils2[1:5] # Assuming trefoils2 is a persistence set

  result_tippett <- two_sample_test(ps1, ps2, B = 10L, npc = "tippett")
  result_fisher <- two_sample_test(ps1, ps2, B = 10L, npc = "fisher")

  expect_true(is.numeric(result_tippett))
  expect_true(is.numeric(result_fisher))
}

# Run all tests
test_two_sample_test <- function() {
  test_two_sample_test_basic()
  test_two_sample_test_params()
  test_two_sample_test_dist()
  test_two_sample_test_verbose()
  test_two_sample_test_npc()
}

test_two_sample_test()
