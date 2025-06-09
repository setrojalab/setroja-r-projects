# Load packages
library(ggplot2)
library(ggfittext)
library(ggforce)
library(showtext)
showtext_auto()
font_add_google("Roboto", "roboto")

# PRISMA Box Data
boxes <- data.frame(
  xmin = c(1, 4, 4, 1, 4, 2.5),
  xmax = c(3, 6, 6, 3, 6, 5.5),
  ymin = c(12, 10, 8, 8, 6, 4),
  ymax = c(13, 11, 9, 9, 7, 5),
  label = c(
    "Records identified from\nDatabases (n = 167)",
    "Records after duplicates removed\n(n = 150)",
    "Records excluded\n(n = 125)",
    "Reports sought for retrieval\n(n = 25)",
    "Reports excluded:\n- Not RCT (n = 10)\n- No GBS outcome (n = 6)\n- Not full-text (n = 4)",
    "Studies included in meta-analysis\n(n = 5)"
  )
)

# Arrow connections
arrows <- data.frame(
  x = c(2, 5, 5, 2, 5),
  y = c(12, 10, 8, 8, 6),
  xend = c(5, 5, 5, 5, 4),
  yend = c(10, 8, 6, 5, 5)
)

# Plot
ggplot() +
  geom_rect(data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#E8F6F3", color = "#117864", linewidth = 1) +
  geom_fit_text(data = boxes,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, label = label),
                grow = TRUE, reflow = TRUE, family = "roboto", size = 4) +
  geom_segment(data = arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "#196F3D", linewidth = 0.8) +
  theme_void() +
  labs(title = "PRISMA 2020 Flow Diagram: Probiotics for GBS in Pregnancy") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, family = "roboto")
  )
