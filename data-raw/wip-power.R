library(inphr)

R <- 100L
B <- 100L
alpha <- 0.05

Ns <- seq(5, 35, by = 5)
mirai::daemons(5)

withr::with_seed(1234, {
  cli::cli_h1("Power simulation for Archimedian spirals differing in variance")

  powers_variance <- purrr::map(Ns, \(N) {
    cli::cli_alert_info("Running power simulation for n = {N} ...")
    pvalues <- purrr::map(
      1:R,
      purrr::in_parallel(
        \(r) {
          archspirals1 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S1 <- tdaunif::sample_arch_spiral(
                n = 120L,
                arms = 2L,
                ar = 1,
                sd = 0.05
              )
              phutil::as_persistence(TDA::ripsDiag(
                S1,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          archspirals2 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S2 <- tdaunif::sample_arch_spiral(
                n = 120L,
                arms = 2L,
                ar = 1,
                sd = 0.1
              )
              phutil::as_persistence(TDA::ripsDiag(
                S2,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          list(
            mean_variance_tippett = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "tippett"
            ),
            mean_variance_fisher = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "fisher"
            ),
            mean_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip)
            ),
            variance_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_f_ip)
            )
          )
        },
        B = B,
        R = R,
        N = N
      )
    ) |>
      purrr::list_transpose() |>
      purrr::map_dbl(\(p) mean(p <= alpha))
  }) |>
    do.call(rbind, args = _)

  cli::cli_h1("Power simulation for Archimedian spirals differing in mean")

  powers_mean <- purrr::map(Ns, \(N) {
    cli::cli_alert_info("Running power simulation for n = {N} ...")
    pvalues <- purrr::map(
      1:R,
      purrr::in_parallel(
        \(r) {
          archspirals1 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S1 <- tdaunif::sample_arch_spiral(
                n = 120L,
                arms = 2L,
                ar = 1,
                sd = 0.05
              )
              phutil::as_persistence(TDA::ripsDiag(
                S1,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          archspirals2 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S2 <- tdaunif::sample_arch_spiral(
                n = 120L,
                arms = 2L,
                ar = 1.1,
                sd = 0.05
              )
              phutil::as_persistence(TDA::ripsDiag(
                S2,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          list(
            mean_variance_tippett = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "tippett"
            ),
            mean_variance_fisher = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "fisher"
            ),
            mean_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip)
            ),
            variance_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_f_ip)
            )
          )
        },
        B = B,
        R = R,
        N = N
      )
    ) |>
      purrr::list_transpose() |>
      purrr::map_dbl(\(p) mean(p <= alpha))
  }) |>
    do.call(rbind, args = _)
})

withr::with_seed(1234, {
  cli::cli_h1("Power simulation for Archimedian spirals differing in variance")

  powers_n <- purrr::map(Ns, \(N) {
    cli::cli_alert_info("Running power simulation for n = {N} ...")
    pvalues <- purrr::map(
      1:R,
      purrr::in_parallel(
        \(r) {
          archspirals1 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S1 <- tdaunif::sample_arch_spiral(
                n = 120L,
                arms = 2L,
                ar = 1,
                sd = 0.05
              )
              phutil::as_persistence(TDA::ripsDiag(
                S1,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          archspirals2 <- phutil::as_persistence_set(lapply(
            seq(N),
            function(i) {
              S2 <- tdaunif::sample_arch_spiral(
                n = 60L,
                arms = 2L,
                ar = 1,
                sd = 0.05
              )
              phutil::as_persistence(TDA::ripsDiag(
                S2,
                maxdimension = 2,
                maxscale = 6
              ))
            }
          ))

          list(
            mean_variance_tippett = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "tippett"
            ),
            mean_variance_fisher = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip, flipr::stat_f_ip),
              npc = "fisher"
            ),
            mean_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_t_ip)
            ),
            variance_only = inphr::two_sample_test(
              archspirals1,
              archspirals2,
              B = B,
              stat_functions = list(flipr::stat_f_ip)
            )
          )
        },
        B = B,
        R = R,
        N = N
      )
    ) |>
      purrr::list_transpose() |>
      purrr::map_dbl(\(p) mean(p <= alpha))
  }) |>
    do.call(rbind, args = _)
})

mirai::daemons(0)

inference <- colnames(powers_variance)

tibble::tibble(
  N = rep(Ns, times = 4L),
  inference = rep(inference, each = length(Ns)),
  power_variance = c(powers_variance),
  power_mean = c(powers_mean),
  power_n = c(powers_n)
) |>
  tidyr::pivot_longer(
    -c(N, inference),
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(variance|mean|n)"
  ) |>
  dplyr::mutate(
    type = factor(
      type,
      levels = c("variance", "mean", "n"),
      labels = c(
        "Differences in variance",
        "Differences in mean",
        "Differences in sample size"
      )
    ),
    inference = factor(
      inference,
      levels = c(
        "mean_variance_tippett",
        "mean_variance_fisher",
        "mean_only",
        "variance_only"
      ),
      labels = c("Tippett", "Fisher", "Mean only", "Variance only")
    )
  ) |>
  ggplot(aes(x = N, y = power, color = inference)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Sample size (n)",
    y = "Power",
    title = "Power of two-sample tests for Archimedian spirals"
  ) +
  facet_wrap(vars(type)) +
  theme_bw()

save(
  alpha,
  inference,
  Ns,
  powers_mean,
  powers_variance,
  powers_n,
  R,
  B,
  file = "data-raw/power.RData"
)
