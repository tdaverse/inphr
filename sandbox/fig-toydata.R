library(inphr)
library(ggtda)

prox <- 1

# trefoil1

pd <- as_tibble(trefoils1[[1]])
pd$dimension <- factor(pd$dimension, levels = 0:2)

p_bc_trefoils1 <- ggplot(pd, aes(start = birth, end = death)) +
  geom_barcode(linewidth = 1, aes(color = dimension, linetype = dimension)) +
  labs(
    x = "Diameter",
    y = "Homological features",
    color = "Dimension",
    linetype = "Dimension",
    title = "Trefoil 1"
  ) +
  geom_vline(xintercept = prox, color = "darkgoldenrod", linetype = "dotted") +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_barcode()

max_prox <- max(pd$death)

p_pd_trefoils1 <- ggplot(pd) +
  coord_fixed() +
  stat_persistence(aes(
    start = birth,
    end = death,
    colour = dimension,
    shape = dimension
  )) +
  geom_abline(slope = 1) +
  labs(x = "Birth", y = "Death", color = "Dimension", shape = "Dimension") +
  lims(x = c(0, max_prox), y = c(0, max_prox)) +
  geom_fundamental_box(
    t = prox,
    fill = "darkgoldenrod",
    color = "transparent"
  ) +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_persist()

# trefoil2

pd <- as_tibble(trefoils2[[1]])
pd$dimension <- factor(pd$dimension, levels = 0:2)

p_bc_trefoils2 <- ggplot(pd, aes(start = birth, end = death)) +
  geom_barcode(linewidth = 1, aes(color = dimension, linetype = dimension)) +
  labs(
    x = "Diameter",
    y = "Homological features",
    color = "Dimension",
    linetype = "Dimension",
    title = "Trefoil 2"
  ) +
  geom_vline(xintercept = prox, color = "darkgoldenrod", linetype = "dotted") +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_barcode()

max_prox <- max(pd$death)

p_pd_trefoils2 <- ggplot(pd) +
  coord_fixed() +
  stat_persistence(aes(
    start = birth,
    end = death,
    colour = dimension,
    shape = dimension
  )) +
  geom_abline(slope = 1) +
  labs(x = "Birth", y = "Death", color = "Dimension", shape = "Dimension") +
  lims(x = c(0, max_prox), y = c(0, max_prox)) +
  geom_fundamental_box(
    t = prox,
    fill = "darkgoldenrod",
    color = "transparent"
  ) +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_persist()

# archspiral

pd <- as_tibble(archspirals[[1]])
pd$dimension <- factor(pd$dimension, levels = 0:2)

p_bc_archspiral <- ggplot(pd, aes(start = birth, end = death)) +
  geom_barcode(linewidth = 1, aes(color = dimension, linetype = dimension)) +
  labs(
    x = "Diameter",
    y = "Homological features",
    color = "Dimension",
    linetype = "Dimension",
    title = "Archspiral"
  ) +
  geom_vline(xintercept = prox, color = "darkgoldenrod", linetype = "dotted") +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_barcode()

max_prox <- max(pd$death)

p_pd_archspiral <- ggplot(pd) +
  coord_fixed() +
  stat_persistence(aes(
    start = birth,
    end = death,
    colour = dimension,
    shape = dimension
  )) +
  geom_abline(slope = 1) +
  labs(x = "Birth", y = "Death", color = "Dimension", shape = "Dimension") +
  lims(x = c(0, max_prox), y = c(0, max_prox)) +
  geom_fundamental_box(
    t = prox,
    fill = "darkgoldenrod",
    color = "transparent"
  ) +
  scale_color_discrete(drop = FALSE) +
  scale_linetype_discrete(drop = FALSE) +
  scale_shape_discrete(drop = FALSE) +
  theme_persist()

patchwork::wrap_plots(
  p_bc_trefoils1,
  p_pd_trefoils1,
  p_bc_trefoils2,
  p_pd_trefoils2,
  p_bc_archspiral,
  p_pd_archspiral,
  ncol = 2,
  guides = "collect"
)
