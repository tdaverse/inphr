n <- 10

# Variance effect ---------------------------------------------------------

spl1 <- purrr::map(1:n, \(.x) {
  pts <- tdaunif::sample_arch_spiral(n = 120L, arms = 3L, ar = 1, sd = 0.05)
  TDA::ripsDiag(
    pts,
    maxdimension = 2,
    maxscale = 6
  )$diagram
})
spl2 <- purrr::map(1:n, \(.x) {
  pts <- tdaunif::sample_arch_spiral(n = 120L, arms = 3L, ar = 1, sd = 0.1)
  TDA::ripsDiag(
    pts,
    maxdimension = 2,
    maxscale = 6
  )$diagram
})

lims <- TDAvec::computeLimits(c(spl1, spl2), homDim = 0)
# x <- seq(min(lims), max(lims), len = 1e3)
x <- seq(0, 2.5, len = 1e3)

y1 <- purrr::map(spl1, \(D) {
  D |>
    as.matrix() |>
    TDAvec::computeBettiCurve(homDim = 0, scaleSeq = x)
}) |>
  do.call(cbind, args = _)

y2 <- purrr::map(spl2, \(D) {
  D |>
    as.matrix() |>
    TDAvec::computeBettiCurve(homDim = 0, scaleSeq = x)
}) |>
  do.call(cbind, args = _)

colnames(y1) <- paste0("arm1_", 1:n)
colnames(y2) <- paste0("arm2_", 1:n)
bind_cols(r = x[-1], y1, y2) |>
  tidyr::pivot_longer(-r) |>
  tidyr::separate(name, into = c("SampleId", "PointId")) |>
  ggplot(aes(
    x = r,
    y = value,
    color = SampleId,
    group = interaction(SampleId, PointId)
  )) +
  geom_line() +
  theme_bw()

# Mean effect -------------------------------------------------------------

spl1 <- purrr::map(1:n, \(.x) {
  pts <- tdaunif::sample_arch_spiral(n = 120L, arms = 3L, ar = 1, sd = 0.05)
  TDA::ripsDiag(
    pts,
    maxdimension = 2,
    maxscale = 6
  )$diagram
})
spl2 <- purrr::map(1:n, \(.x) {
  pts <- tdaunif::sample_arch_spiral(n = 120L, arms = 3L, ar = 1.1, sd = 0.05)
  TDA::ripsDiag(
    pts,
    maxdimension = 2,
    maxscale = 6
  )$diagram
})

lims <- TDAvec::computeLimits(c(spl1, spl2), homDim = 0)
# x <- seq(min(lims), max(lims), len = 1e4)
x <- seq(0, 2.5, len = 1e3)

# Betti curve
# Euler characteristic curve
# Normalized life curve
# Landscape
# Silhouette
# Entropy
y1 <- purrr::map(spl1, \(D) {
  D |>
    as.matrix() |>
    # TDAvec::computeBettiCurve(homDim = 0, scaleSeq = x)
    # TDAvec::computeEulerCharacteristic(scaleSeq = x)
    # TDAvec::computeNormalizedLife(homDim = 0, scaleSeq = x)
    # TDAvec::computePersistenceLandscape(homDim = 0, scaleSeq = x)
    # TDAvec::computePersistenceSilhouette(homDim = 0, scaleSeq = x)
    TDAvec::computePersistentEntropy(homDim = 0, scaleSeq = x)
}) |>
  do.call(cbind, args = _)

y2 <- purrr::map(spl2, \(D) {
  D |>
    as.matrix() |>
    # TDAvec::computeBettiCurve(homDim = 0, scaleSeq = x)
    # TDAvec::computeEulerCharacteristic(scaleSeq = x)
    # TDAvec::computeNormalizedLife(homDim = 0, scaleSeq = x)
    # TDAvec::computePersistenceLandscape(homDim = 0, scaleSeq = x)
    # TDAvec::computePersistenceSilhouette(homDim = 0, scaleSeq = x)
    TDAvec::computePersistentEntropy(homDim = 0, scaleSeq = x)
}) |>
  do.call(cbind, args = _)

colnames(y1) <- paste0("arm1_", 1:n)
colnames(y2) <- paste0("arm2_", 1:n)
bind_cols(r = x[-1], y1, y2) |>
  tidyr::pivot_longer(-r) |>
  tidyr::separate(name, into = c("SampleId", "PointId")) |>
  ggplot(aes(
    x = r,
    y = value,
    color = SampleId,
    group = interaction(SampleId, PointId)
  )) +
  geom_line() +
  theme_bw()
