library(inphr)

R <- 100L
B <- 100L
alpha <- 0.05

n_vec <- seq(5, 30, by = 5)
mirai::daemons(5)

withr::with_seed(1234, {
  power_vec <- purrr::map_dbl(n_vec, \(n) {
    cli::cli_alert_info("Running power simulation for n = {n} ...")
    pvalues <- purrr::map_dbl(
      1:R,
      purrr::in_parallel(
        \(r) {
          cli::cli_alert_success("Simulation run {r} of {R} ...")

          archspirals1 <- phutil::as_persistence_set(lapply(
            seq(n),
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
            seq(n),
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

          inphr::two_sample_test(archspirals1, archspirals2, B = B)
        },
        B = B,
        R = R,
        n = n
      )
    )
    mean(pvalues <= alpha)
  })
})

mirai::daemons(0)

plot(n_vec, power_vec, type = "b")
