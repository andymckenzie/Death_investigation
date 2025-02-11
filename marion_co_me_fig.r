# Load required libraries
library(ggplot2)
library(tidyr)

# Read data
data <- read.csv("MarionCoME.csv")

# Create clean dataframe
clean_df <- data.frame(
  Year = as.numeric(data$X[2:6]),
  Total_Deaths = as.numeric(data$X.1[2:6]),
  Total_Cases = as.numeric(gsub(",", "", data$X.2[2:6])),
  ME_Cases = as.numeric(data$X.4[2:6])
)

# Reshape data to long format
data_long <- tidyr::pivot_longer(
  clean_df,
  cols = c(Total_Deaths, Total_Cases, ME_Cases),
  names_to = "metric",
  values_to = "count"
)

# Create plot
p <- ggplot(data_long, aes(x = factor(Year), y = count, color = metric, group = metric)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_text(aes(label = count), vjust = -0.5, hjust = 0.5, size = 4.5, show.legend = FALSE) +
  scale_color_manual(
    values = c("#E69F00", "#0072B2", "#009E73"),
    labels = c(
      "Total ME Cases Accepted",
      "Total Cases Reported to ME",
      "Total Deaths"
    )
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 9),
    axis.title.x = element_text(size = 10, margin = margin(t = 8), face = "bold"),
    axis.title.y = element_text(size = 10, margin = margin(r = 8), face = "bold"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    legend.key.size = unit(0.6, "cm"),
    plot.title = element_text(size = 11, margin = margin(b = 8), face = "bold", hjust = 0.5),
    panel.border = element_rect(linewidth = 0.3),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot"
  ) +
  labs(
    title = "Marion County Medical Examiner Case Distributions",
    x = "Year",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_y_continuous(limits = c(0, 4200), expand = c(0, 0))

# Save plot
ggsave("marion_county_plot.png",
  width = 6,
  height = 6,
  dpi = 600,
  bg = "white"
)
