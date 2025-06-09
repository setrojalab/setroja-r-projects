# ─────────────────────────────────────────────
# STEP 1: Load libraries
# ─────────────────────────────────────────────
# Install if not yet installed
# install.packages(c("tidyverse", "metafor", "ggthemes", "ggpubr", "readr"))

library(tidyverse)
library(metafor)
library(ggthemes)
library(ggpubr)
library(readr)

# ─────────────────────────────────────────────
# STEP 2: Load data
# ─────────────────────────────────────────────
gbs_data <- read_csv("GBS_Probiotics_MetaData.csv")

# ─────────────────────────────────────────────
# STEP 3: Compute effect sizes (log OR + var)
# ─────────────────────────────────────────────
escalc_data <- escalc(
  measure = "OR",
  ai = Events_Tx, n1i = Total_Tx,
  ci = Events_Ctrl, n2i = Total_Ctrl,
  data = gbs_data
)

# ─────────────────────────────────────────────
# STEP 4: Fit random-effects model (REML)
# ─────────────────────────────────────────────
res <- rma(yi, vi, data = escalc_data, method = "REML")

# Optional summary printout
summary(res)

# ─────────────────────────────────────────────
# STEP 5: Prepare for ggplot2 (back-transform)
# ─────────────────────────────────────────────
escalc_data$OR <- exp(res$yi)
escalc_data$OR_low <- exp(res$yi - 1.96 * sqrt(res$vi))
escalc_data$OR_high <- exp(res$yi + 1.96 * sqrt(res$vi))
escalc_data$Study <- factor(gbs_data$Study, levels = rev(gbs_data$Study))

# ─────────────────────────────────────────────
# STEP 6: Beautified Forest Plot (ggplot2)
# ─────────────────────────────────────────────
forest_plot <- ggplot(escalc_data, aes(x = OR, y = Study)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_x_log10(
    breaks = c(0.1, 0.5, 1, 2, 5, 10),
    labels = c("0.1", "0.5", "1", "2", "5", "10")
  ) +
  theme_pubr(base_size = 14, border = TRUE) +
  labs(
    title = "Forest Plot: Odds Ratios for Probiotics Preventing GBS Colonization",
    x = "Odds Ratio (log scale)", y = "Study"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Print
print(forest_plot)

# Save
ggsave("ForestPlot_GBS_OR.png", forest_plot, width = 12, height = 8, dpi = 300)

# ─────────────────────────────────────────────
# STEP 7: Funnel Plot for Publication Bias
# ─────────────────────────────────────────────
funnel_data <- data.frame(yi = res$yi, sei = sqrt(res$vi))

funnel_plot <- ggplot(funnel_data, aes(x = yi, y = 1/sei)) +
  geom_point(size = 3, alpha = 0.6, color = "#D55E00") +
  geom_vline(xintercept = res$b, linetype = "dashed", color = "blue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Funnel Plot: Publication Bias Assessment",
    x = "Log Odds Ratio", y = "Precision (1/SE)"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Print
print(funnel_plot)

# Save
ggsave("FunnelPlot_GBS.png", funnel_plot, width = 8, height = 6, dpi = 300)

# ─────────────────────────────────────────────
# STEP 8: Leave-One-Out Sensitivity Analysis
# ─────────────────────────────────────────────
leave1out_results <- leave1out(res)
print(leave1out_results)

# Optional: Convert leave1out to dataframe for plotting if needed
leave_df <- as.data.frame(leave1out_results)
write_csv(leave_df, "Leave1Out_GBS.csv")

# ─────────────────────────────────────────────
# STEP 9: Export Coefficients & Confidence Intervals
# ─────────────────────────────────────────────
result_summary <- data.frame(
  Study = gbs_data$Study,
  OR = escalc_data$OR,
  CI_lower = escalc_data$OR_low,
  CI_upper = escalc_data$OR_high
)

write_csv(result_summary, "ForestPlot_Data_GBS.csv")

# ─────────────────────────────────────────────
# STEP 10: Add Optional Notes for GRADE Table (Manual Integration)
# ─────────────────────────────────────────────
# You can manually integrate the below into RevMan, GRADEpro, or a manuscript table
# Example template row (not printed, just illustrative)

# GRADE_table <- data.frame(
#   Outcome = "Maternal GBS colonization",
#   Effect = "OR 0.45 (95% CI: 0.30–0.66)",
#   Studies = nrow(gbs_data),
#   Certainty = "Moderate ⊕⊕⊕◯",
#   Comments = "Reduced colonization in both oral and vaginal probiotic arms"
# )

# write_csv(GRADE_table, "GRADE_Summary_GBS.csv")

# ─────────────────────────────────────────────
# DONE! 🎉
# You now have a full statistical + visual pipeline for publication.
# ─────────────────────────────────────────────
