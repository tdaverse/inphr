#' Two-sample test for functional representations of persistence homology data
#'
#' This function performs a two-sample test for persistence homology data using
#' the theory of permutation hypothesis testing to test the null hypothesis that
#' the two samples come from the same distribution. The input data must be objects
#' of class `persistence_set` typically produced by [`phutil::as_persistence_set()`].
#'
#' @param x An object of class `persistence_set` typically produced by
#'   [`phutil::as_persistence_set()`] specifying the first sample.
#' @param y An object of class `persistence_set` typically produced by
#'   [`phutil::as_persistence_set()`] specifying the second sample.
#' @param scale_size An integer value specifying the number of scale values to
#'   use for the functional representation. Defaults to `100L`.
#' @param representation A string specifying the functional representation to
#'   use. Choices are `"betti"`, `"euler"`, `"life"`, `"silhouette"`, and
#'   `"entropy"`. Defaults to `"betti"`.
#' @param nknots An integer value specifying the number of knots to use for the
#'   B-spline representation. Defaults to `scale_size`.
#' @inheritParams two_sample_diagram_test
#' @inheritParams fdatest::ITP2bspline
#'
#' @returns A length-4 list containing the following objects:
#'
#' - `xfd`: A numeric matrix of shape \eqn{n_1 \times p} storing the
#'   representation of the first sample on a uniform grid.
#' - `yfd`: A numeric matrix of shape \eqn{n_2 \times p} storing the
#'   representation of the second sample on a uniform grid.
#' - `scale_seq`: A numeric vector of shape \eqn{p} storing the scale sequence
#'   used for the functional representation.
#' - `iwt`: An object of class `ITP2` which is a list containing at least the
#'   following components:
#'
#'   - `basis`: A string indicating the basis used for the first phase of the algorithm. In this case, equals to `"B-spline"`.
#'   - `test`: A string indicating the type of test performed. In this case, equals to `"2pop"`.
#'   - `mu`: The difference between the mean of the first and second populations under the null hypothesis (as entered by the user).
#'   - `paired`: A boolean value indicating whether the two samples are paired or not (as entered by the user).
#'   - `coeff`: A numeric matrix of shape \eqn{n \times p} of the \eqn{p} coefficients of the B-spline basis expansion, with \eqn{n = n_1 + n_2}. Rows are associated to units and columns to the basis index. The first \eqn{n_1} rows report the coefficients of the first population units and the following \eqn{n_2} rows report the coefficients of the second population units.
#'   - `pval`: A numeric vector of shape \eqn{p} storing the **uncorrected** p-values for each coefficient of the B-spline basis expansion.
#'   - `pval.matrix`: A numeric matrix of shape \eqn{p \times p} of the p-values of the multivariate tests. The element \eqn{(i, j)} of the `pval.matrix` matrix contains the p-value of the joint NPC test of the components \eqn{(j, j+1, \dots, j+(p-i))}.
#'   - `corrected.pval`: A numeric vector of shape \eqn{p} storing the **corrected** p-values for each coefficient of the B-spline basis expansion.
#'   - `labels`: A character vector of shape \eqn{n} storing the membership of each unit to the first or second population.
#'   - `data.eval`: A numeric matrix of shape \eqn{n \times p} storing the evaluation of the functional data on a uniform grid.
#'   - `heatmap.matrix`: A numeric matrix storing the p-values. Used only for plots.
#'
#' @references Pini, A., & Vantini, S. (2017). Interval-wise testing for functional data. Journal of Nonparametric Statistics, 29(2), 407-424.
#'
#' @export
#' @examples
#' out <- two_sample_functional_test(trefoils1, archspirals, B = 100L, scale_size = 50L)
#' plot(out$iwt, xrange = range(out$scale_seq))
#' matplot(
#'   out$scale_seq[-1],
#'   t(rbind(out$xfd, out$yfd)),
#'   type = "l",
#'   col = c(rep(1, length(trefoils1)), rep(2, length(archspirals)))
#' )
two_sample_functional_test <- function(
  x,
  y,
  dimension = 0L,
  scale_size = 100L,
  representation = c("betti", "euler", "life", "silhouette", "entropy"),
  mu = 0,
  order = 2L,
  nknots = scale_size,
  B = 1000L,
  paired = FALSE
) {
  if (!inherits(x, "persistence_set")) {
    cli::cli_abort(
      "The first argument {.arg x} must be of class {.cls persistence_set}."
    )
  }

  if (!inherits(y, "persistence_set")) {
    cli::cli_abort(
      "The second argument {.arg y} must be of class {.cls persistence_set}."
    )
  }

  # Match arguments
  representation <- rlang::arg_match(representation)

  # Transform persistence objects into matrices
  x <- lapply(x, as.matrix)
  y <- lapply(y, as.matrix)

  # Compute scale sequence
  lims <- TDAvec::computeLimits(c(x, y), homDim = dimension)
  scale_seq <- seq(min(lims), max(lims), len = scale_size)

  # Compute requested functional representation of persistence
  switch(
    representation,
    betti = {
      xfd <- do.call(
        rbind,
        args = lapply(x, function(el) {
          TDAvec::computeBettiCurve(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
      yfd <- do.call(
        rbind,
        args = lapply(y, function(el) {
          TDAvec::computeBettiCurve(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
    },
    euler = {
      xfd <- do.call(
        rbind,
        args = lapply(x, function(el) {
          TDAvec::computeEulerCharacteristic(el, scaleSeq = scale_seq)
        })
      )
      yfd <- do.call(
        rbind,
        args = lapply(y, function(el) {
          TDAvec::computeEulerCharacteristic(el, scaleSeq = scale_seq)
        })
      )
    },
    life = {
      xfd <- do.call(
        rbind,
        args = lapply(x, function(el) {
          TDAvec::computeNormalizedLife(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
      yfd <- do.call(
        rbind,
        args = lapply(y, function(el) {
          TDAvec::computeNormalizedLife(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
    },
    silhouette = {
      xfd <- do.call(
        rbind,
        args = lapply(x, function(el) {
          TDAvec::computePersistenceSilhouette(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
      yfd <- do.call(
        rbind,
        args = lapply(y, function(el) {
          TDAvec::computePersistenceSilhouette(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
    },
    entropy = {
      xfd <- do.call(
        rbind,
        args = lapply(x, function(el) {
          TDAvec::computePersistentEntropy(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
      yfd <- do.call(
        rbind,
        args = lapply(y, function(el) {
          TDAvec::computePersistentEntropy(
            el,
            homDim = dimension,
            scaleSeq = scale_seq
          )
        })
      )
    }
  )

  test_output <- fdatest::ITP2bspline(xfd, yfd)
  list(xfd = xfd, yfd = yfd, scale_seq = scale_seq, iwt = test_output)
}
