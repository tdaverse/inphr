spl1 <- phutil::as_persistence_set(trefoils1[1:5])
spl2 <- phutil::as_persistence_set(trefoils2[1:5])

# Test wrong inputs x and y using tinytest framework
expect_error(two_sample_functional_test(1, spl2))
expect_error(two_sample_functional_test(spl1, 1))

# Test correct behavior with betti curve
out <- two_sample_functional_test(
  spl1,
  spl2,
  scale_size = 50L,
  B = 100L,
  representation = "betti"
)
expect_length(out, 4L)

# Test correct behavior with euler characteristic
out <- two_sample_functional_test(
  spl1,
  spl2,
  scale_size = 50L,
  B = 100L,
  representation = "euler"
)
expect_length(out, 4L)

# Test correct behavior with life curve
out <- two_sample_functional_test(
  spl1,
  spl2,
  scale_size = 50L,
  B = 100L,
  representation = "life"
)
expect_length(out, 4L)

# Test correct behavior with silhouette
out <- two_sample_functional_test(
  spl1,
  spl2,
  scale_size = 50L,
  B = 100L,
  representation = "silhouette"
)
expect_length(out, 4L)

# Test correct behavior with entropy
out <- two_sample_functional_test(
  spl1,
  spl2,
  scale_size = 50L,
  B = 100L,
  representation = "entropy"
)
expect_length(out, 4L)
