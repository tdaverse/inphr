library(phutil)

n <- 24L

withr::with_seed(28415, {
  trefoils1 <- as_persistence_set(lapply(seq(n), function(i) {
    S1 <- tdaunif::sample_trefoil(n = 120L, sd = .05)
    as_persistence(TDA::ripsDiag(S1, maxdimension = 2, maxscale = 6))
  }))
})

withr::with_seed(28415, {
  trefoils2 <- as_persistence_set(lapply(seq(n), function(i) {
    S1 <- tdaunif::sample_trefoil(n = 120L, sd = .05)
    as_persistence(TDA::ripsDiag(S1, maxdimension = 2, maxscale = 6))
  }))
})

withr::with_seed(28415, {
  archspirals <- as_persistence_set(lapply(seq(n), function(i) {
    S2 <- cbind(tdaunif::sample_arch_spiral(n = 120L, arms = 2L), 0)
    S2 <- tdaunif::add_noise(S2, sd = .05)
    as_persistence(TDA::ripsDiag(S2, maxdimension = 2, maxscale = 6))
  }))
})

usethis::use_data(trefoils1, trefoils2, archspirals, overwrite = TRUE)
