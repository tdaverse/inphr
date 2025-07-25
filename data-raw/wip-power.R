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

mirai::daemons(0)
