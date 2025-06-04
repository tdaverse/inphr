sim1 <- tibble(
  dgms = c(trefoils1, trefoils2),
  id = as.factor(c(rep(1, n), rep(2, n)))
)
sim2 <- tibble(
  dgms = c(trefoils1, arch_spirals),
  id = as.factor(c(rep(1, n), rep(2, n)))
)

library(infer)

my_specify <- function(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  success = NULL
) {
  infer:::check_type(x, is.data.frame)
  x <- infer:::standardize_variable_types(x)
  response <- rlang::enquo(response)
  explanatory <- rlang::enquo(explanatory)
  x <- infer:::parse_variables(x, formula, response, explanatory)
  attr(x, "success") <- success
  attr(x, "generated") <- FALSE
  attr(x, "hypothesized") <- FALSE
  attr(x, "fitted") <- FALSE
  infer:::check_success_arg(x, success)
  x <- x %>%
    select(any_of(c(infer:::response_name(x), infer:::explanatory_name(x))))
  # is_complete <- stats::complete.cases(x)
  # if (!all(is_complete)) {
  #   x <- dplyr::filter(x, is_complete)
  #   cli_warn("Removed {sum(!is_complete)} rows containing missing values.")
  # }
  infer:::append_infer_class(x)
}

sim1 |>
  my_specify(response = dgms, explanatory = id) |>
  hypothesize(null = "independence") |>
  generate(reps = 10, type = "permute") |>
  calculate(stat = "diff in means", order = c("1", "2")) |>
  visualize()

two_sample_test(trefoils1, trefoils2, ncores = 8L)
two_sample_test(trefoils1, arch_spirals, ncores = 8L)
