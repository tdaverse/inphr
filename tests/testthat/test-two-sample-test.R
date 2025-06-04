test_that("two_sample_test works with persistence sets", {
  testthat::skip_if_not_installed("phutil")
  testthat::skip_if_not_installed("flipr")

  # Mock the parse_inputs function to avoid actual computation
  mockD <- matrix(1:16, 4, 4)
  class(mockD) <- "dist"
  mock_result <- list(D = mockD, sample_sizes = c(2, 2))

  testthat::local_mocked_bindings(
    parse_inputs = function(...) mock_result,
    null_spec = function(y, parameters) y
  )

  # Create a mock PlausibilityFunction class
  MockPF <- R6::R6Class(
    "PlausibilityFunction",
    public = list(
      alternative = NULL,
      nperms = NULL,
      aggregator = NULL,
      initialize = function(...) {
      },
      get_value = function(...) 0.042
    )
  )

  # Mock the flipr package
  testthat::local_mocked_bindings(
    PlausibilityFunction = MockPF,
    stat_t_ip = function() {
    },
    stat_f_ip = function() {
    },
    .package = "flipr"
  )

  # Test with default parameters
  result <- two_sample_test(
    x = list(),
    y = list(),
    B = 100L
  )
  expect_equal(result, 0.042)
})

test_that("two_sample_test handles verbose parameter correctly", {
  testthat::skip_if_not_installed("phutil")
  testthat::skip_if_not_installed("flipr")

  # Mock the parse_inputs function to avoid actual computation
  mockD <- matrix(1:16, 4, 4)
  class(mockD) <- "dist"
  mock_result <- list(D = mockD, sample_sizes = c(2, 2))

  testthat::local_mocked_bindings(
    parse_inputs = function(...) mock_result,
    null_spec = function(y, parameters) y
  )

  # Create a mock PlausibilityFunction class
  MockPF <- R6::R6Class(
    "PlausibilityFunction",
    public = list(
      alternative = NULL,
      nperms = NULL,
      aggregator = NULL,
      initialize = function(...) {
      },
      get_value = function(...) 0.042
    )
  )

  # Mock the flipr package
  testthat::local_mocked_bindings(
    PlausibilityFunction = MockPF,
    stat_t_ip = function() {
    },
    stat_f_ip = function() {
    },
    .package = "flipr"
  )

  # Test with verbose = TRUE
  expect_snapshot(
    two_sample_test(
      x = list(),
      y = list(),
      B = 100L,
      verbose = TRUE
    )
  )
})

test_that("two_sample_test sets correct parameters for plausibility function", {
  testthat::skip_if_not_installed("phutil")
  testthat::skip_if_not_installed("flipr")

  # Mock the parse_inputs function
  mockD <- matrix(1:16, 4, 4)
  class(mockD) <- "dist"
  mock_result <- list(D = mockD, sample_sizes = c(2, 2))

  testthat::local_mocked_bindings(
    parse_inputs = function(...) mock_result,
    null_spec = function(y, parameters) y
  )

  # Create a testable mock PlausibilityFunction class that records parameters
  params_set <- list()
  MockPF <- R6::R6Class(
    "PlausibilityFunction",
    public = list(
      alternative = NULL,
      nperms = NULL,
      aggregator = NULL,
      initialize = function(...) {
        params_set <<- list(...)
      },
      get_value = function(...) 0.042
    )
  )

  # Mock the flipr package
  testthat::local_mocked_bindings(
    PlausibilityFunction = MockPF,
    stat_t_ip = function() {
    },
    stat_f_ip = function() {
    },
    .package = "flipr"
  )

  # Test with custom B and npc parameters
  custom_B <- 500L
  custom_npc <- "fisher"

  result <- two_sample_test(
    x = list(),
    y = list(),
    B = custom_B,
    npc = custom_npc
  )

  # Verify correct parameters were set
  expect_equal(params_set[[4]], mockD) # D parameter
  expect_equal(params_set[[5]], 2) # sample_sizes[1]

  # Check the PlausibilityFunction properties were set correctly
  expect_equal(MockPF$new()$alternative, "right_tail")
  expect_equal(MockPF$new()$nperms, custom_B)
  expect_equal(MockPF$new()$aggregator, custom_npc)
})
