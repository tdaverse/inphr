null_spec <- function(y, parameters) {
  return(y)
}

compute_distance_matrix <- function(x, dimension = 0L, p = 2L, ncores = 1L) {
  if (is.infinite(p)) {
    D <- phutil::bottleneck_pairwise_distances(
      x = x,
      validate = TRUE,
      dimension = dimension,
      ncores = ncores
    )
  } else {
    D <- phutil::wasserstein_pairwise_distances(
      x = x,
      validate = TRUE,
      dimension = dimension,
      ncores = ncores,
      p = p
    )
  }
  D
}

parse_inputs <- function(x, y, dimension = 0L, p = 2L, ncores = 1L) {
  if (is.list(x)) x <- phutil::as_persistence_set(x)
  if (is.list(y)) y <- phutil::as_persistence_set(y)

  if (inherits(x, "persistence_set")) {
    if (is.integer(y) && length(y) == 2L) {
      D <- compute_distance_matrix(
        x,
        dimension = dimension,
        p = p,
        ncores = ncores
      )
      sample_sizes <- y
    } else if (inherits(y, "persistence_set")) {
      sample_sizes <- c(length(x), length(y))
      x <- phutil::as_persistence_set(c(x, y))
      D <- compute_distance_matrix(
        x,
        dimension = dimension,
        p = p,
        ncores = ncores
      )
    } else {
      cli::cli_abort(
        "When the first argument {.arg x} is of class {.cls persistence_set}, the second argument {.arg y} must be either a vector of two integers (sample sizes) or another persistence set."
      )
    }
  } else if (inherits(x, "dist")) {
    if (is.integer(y) && length(y) == 2L) {
      D <- x
      sample_sizes <- y
    } else {
      cli::cli_abort(
        "When the first argument {.arg x} is of class {.cls dist}, the second argument {.arg y} must be a vector of two integers (sample sizes)."
      )
    }
  } else {
    # TO DO: add support for PH vectorizations
    cli::cli_abort(
      "The first argument {.arg x} must be either a persistence set or a distance matrix (of class {.cls dist})."
    )
  }
  list(D = D, sample_sizes = sample_sizes)
}
