#' Persistence diagrams from trefoil knot samples (first set)
#'
#' @description A set of 24 persistence diagrams computed from noisy samples of
#'   trefoil knots. Each sample consists of 120 points sampled from a trefoil
#'   knot with Gaussian noise (sd = 0.05) added. Vietoris-Rips persistence was
#'   computed up to dimension 2 with maximum scale 6 using [`TDA::ripsDiag()`].
#'   Generated with seed `28415`.
#'
#' @format An object of class `persistence_set` containing 24 objects of class
#'   [`phutil::persistence`].
"trefoils1"

#' Persistence diagrams from trefoil knot samples (second set)
#'
#' @description A set of 24 persistence diagrams computed from noisy samples of
#'   trefoil knots. Each sample consists of 120 points sampled from a trefoil
#'   knot with Gaussian noise (sd = 0.05) added. Vietoris-Rips persistence was
#'   computed up to dimension 2 with maximum scale 6 using [`TDA::ripsDiag()`].
#'   Generated with seed `28415`.
#'
#' @format An object of class `persistence_set` containing 24 objects of class
#'   [`phutil::persistence`].
"trefoils2"

#' Persistence diagrams from Archimedean spiral samples
#'
#' @description A set of 24 persistence diagrams computed from noisy samples of
#'   2-armed Archimedean spirals. Each sample consists of 120 points sampled
#'   from an Archimedean spiral, embedded in 3D with a zero z-coordinate, then
#'   Gaussian noise (sd = 0.05) added. Vietoris-Rips persistence was computed up
#'   to dimension 2 with maximum scale 6 using [`TDA::ripsDiag()`]. Generated
#'   with seed `28415`.
#'
#' @format An object of class `persistence_set` containing 24 objects of class
#'   [`phutil::persistence`].
"archspirals"
