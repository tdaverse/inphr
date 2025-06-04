# Test parse_inputs() function
test_two_sample_test_inputs <- function() {
  ps1 <- 0
  ps2 <- 0
  expect_error(two_sample_test(ps1, ps2))
  ps1 <- trefoils1[1:5]
  expect_error(two_sample_test(ps1, ps2))
  ps2 <- trefoils2[1:5]
  result <- two_sample_test(ps1, ps2, B = 10L)
  expect_true(is.numeric(result))

  ps1 <- c(trefoils1[1:5], trefoils2[1:5])
  ps2 <- c(5L, 5L) # Sample sizes
  result <- two_sample_test(ps1, ps2, B = 10L)
  expect_true(is.numeric(result))

  ps1 <- phutil::wasserstein_pairwise_distances(
    ps1,
    dimension = 0L,
    p = 2L,
    ncores = 1L
  )
  ps2 <- 0
  expect_error(two_sample_test(ps1, ps2))
  ps2 <- c(5L, 5L) # Sample sizes
  result <- two_sample_test(ps1, ps2, B = 10L)
  expect_true(is.numeric(result))
}

# Test basic functionality
test_two_sample_test_basic <- function() {
  ps1 <- trefoils1[1:5]
  ps2 <- trefoils2[1:5]

  result <- two_sample_test(ps1, ps2, B = 10L)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
}

# Test parameter validation
test_two_sample_test_params <- function() {
  ps1 <- trefoils1[1:5]
  ps2 <- trefoils2[1:5]

  expect_error(two_sample_test(ps1, ps2, dimension = -1L))
  expect_error(two_sample_test(ps1, ps2, p = 0L))
  expect_error(two_sample_test(ps1, ps2, B = 0L))
  expect_error(two_sample_test(ps1, ps2, npc = "invalid"))
}

# Test with distance matrix input
test_two_sample_test_dist <- function() {
  D <- phutil::wasserstein_pairwise_distances(
    c(trefoils1[1:5], trefoils2[1:5]),
    dimension = 0L,
    p = 2L,
    ncores = 1L
  )
  sample_sizes <- c(5L, 5L)

  result <- two_sample_test(D, sample_sizes, B = 10L)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
}

# Test verbose output
test_two_sample_test_verbose <- function() {
  ps1 <- trefoils1[1:5]
  ps2 <- trefoils2[1:5]

  expect_silent(two_sample_test(ps1, ps2, B = 10L, verbose = FALSE))
  expect_message(
    two_sample_test(ps1, ps2, B = 10L, verbose = TRUE),
    "Parsing inputs..."
  )
  expect_message(
    two_sample_test(ps1, ps2, B = 10L, verbose = TRUE),
    "Setting up the plausibility function..."
  )
  expect_message(
    two_sample_test(ps1, ps2, B = 10L, verbose = TRUE),
    "Calculating the p-value..."
  )
}

# Test different npc methods
test_two_sample_test_npc <- function() {
  ps1 <- trefoils1[1:5]
  ps2 <- trefoils2[1:5]

  result_tippett <- two_sample_test(ps1, ps2, B = 10L, npc = "tippett")
  result_fisher <- two_sample_test(ps1, ps2, B = 10L, npc = "fisher")

  expect_true(is.numeric(result_tippett))
  expect_true(is.numeric(result_fisher))
}

test_two_sample_test_bottleneck <- function() {
  ps1 <- trefoils1[1:5]
  ps2 <- trefoils2[1:5]

  result <- two_sample_test(ps1, ps2, p = Inf, B = 10L)
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
}

# Run all tests
test_two_sample_test <- function() {
  test_two_sample_test_inputs()
  test_two_sample_test_basic()
  test_two_sample_test_params()
  test_two_sample_test_dist()
  test_two_sample_test_verbose()
  test_two_sample_test_npc()
  test_two_sample_test_bottleneck()
}

test_two_sample_test()
