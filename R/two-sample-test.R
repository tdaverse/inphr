#' Two-Sample Test for Persistence Homology Data
#'
#' This function performs a two-sample test for persistence homology data using
#' the theory of permutation hypothesis testing. The input data can take on
#' various forms:
#' - A persistence set, which is a collection of persistence diagrams.
#' - A distance matrix, which is a pairwise distance matrix between persistence
#' diagrams.
#' - One of the PH vectorizations available in the
#' [{TDAvec}](https://cran.r-project.org/package=TDAvec) package.
#'
#' @param x An object of class `persistence_set` typically produced by
#'   [`phutil::as_persistence_set()`] or of class `dist` typically produced by
#'   [`phutil::bottleneck_pairwise_distances()`] or
#'   [`phutil::wasserstein_pairwise_distances()`]. If `x` is a persistence set,
#'   then `y` must be either a vector of two integers (sample sizes) or another
#'   persistence set. If `x` is a distance matrix, then `y` must be a vector of
#'   two integers (sample sizes).
#' @param y An object of class `persistence_set` typically produced by
#'   [`phutil::as_persistence_set()`] or a vector of two integers. If `x` is a
#'   persistence set, then `y` must be either a vector of two integers (sample
#'   sizes) or another persistence set. If `x` is a distance matrix, then `y`
#'   must be a vector of two integers (sample sizes).
#' @param dimension An integer value specifying the homology dimension to use.
#'   Defaults to `0L`, which corresponds to the 0-dimensional homology.
#' @param p An integer value specifying the p-norm to use for the Wasserstein
#'   distance. Defaults to `2L`, which corresponds to the Euclidean distance. If
#'   `p` is set to `Inf`, then the Bottleneck distance is used.
#' @param ncores An integer value specifying the number of cores to use when
#'   computing the pairwise distance matrix between all combined persistence
#'   diagrams. Defaults to `1L`, which means that the computation is done
#'   sequentially.
#' @param B An integer value specifying the number of permutations to use for
#'   the permutation hypothesis test. Defaults to `1000L`.
#' @param stat_functions A list of functions that compute test statistics to be
#'   used for solving the inference problem. These functions must take two
#'   arguments: first, an object of class `dist` representing a distance matrix
#'   and second, an integer vector specifying the indices of the data points
#'   belonging to the first sample. Defaults to
#'   `list(flipr::stat_t_ip, flipr::stat_f_ip`)` which are distance-based
#'   statistics equivalent to Student's and Fisher's statistics respectively.
#' @param npc A string specifying the non-parametric combination method to use.
#'   Choices are either `"tippett"` (default) or `"fisher"`. The former
#'   corresponds to the Tippet's method, while the latter corresponds to
#'   Fisher's method.
#' @param seed An integer value specifying the seed for random number
#'   generation. Defaults to `NULL` which uses current time.
#' @param verbose A boolean value indicating whether to print some information
#'   about the progress of the computation. Defaults to `FALSE`.
#' @param keep_null_distribution A boolean specifying whether the empirical
#'   permutation null distribution should be returned as well. Defaults to
#'   `FALSE`.
#' @param keep_permutations A boolean specifying whether the list of sampled
#'   permutations used to compute the empirical permutation null distribution
#'   should be returned as well. Defaults to `FALSE`.
#'
#' @returns A numeric value storing the p-value from the two-sample test where
#'   the null hypothesis is that the two samples come from the same
#'   distribution. If one of `keep_null_distribution` or `keep_permutations` is
#'   set to `TRUE`, then the output will be a list containing the p-value and
#'   the null distribution (if `keep_null_distribution` is set to `TRUE`) and
#'   the list of sampled permutations (if `keep_permutations` is set to `TRUE`).
#'
#' @export
#' @examples
#' two_sample_test(trefoils1[1:5], trefoils2[1:5], B = 100L)
#' two_sample_test(trefoils1[1:5], archspirals[1:5], B = 100L)
two_sample_test <- function(
  x,
  y,
  dimension = 0L,
  p = 2L,
  ncores = 1L,
  B = 1000L,
  stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
  npc = "tippett",
  seed = NULL,
  verbose = FALSE,
  keep_null_distribution = FALSE,
  keep_permutations = FALSE
) {
  if (verbose) {
    cli::cli_alert_info("Parsing inputs...")
  }
  l <- parse_inputs(
    x = x,
    y = y,
    dimension = dimension,
    p = p,
    ncores = ncores
  )
  D <- l$D
  sample_sizes <- l$sample_sizes
  if (verbose) {
    cli::cli_alert_info("Setting up the plausibility function...")
  }
  # We could use alternative statistics for PH vectorizations
  pf <- flipr::PlausibilityFunction$new(
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = list(mean = 1, sd = 2),
    D,
    sample_sizes[1],
    seed = if (is.null(seed)) Sys.time() else seed
  )
  pf$alternative <- "right_tail"
  pf$nperms <- B
  pf$aggregator <- npc
  if (verbose) {
    cli::cli_alert_info("Calculating the p-value...")
  }
  pf$get_value(
    parameters = c(0, 1),
    keep_null_distribution = keep_null_distribution,
    keep_permutations = keep_permutations
  )
}
