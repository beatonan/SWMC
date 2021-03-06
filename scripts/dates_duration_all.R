# Imports
library(ggplot2)
library(ggstance)
library(readxl)
library(grid)
library(gridExtra)
library(scales)
library(dplyr)
library(lubridate)
library(here)


# -------------------------- SETUP ----------------------------- #
# Data source
filename <- here::here("import", "All_Rivers.xlsx")

library(readxl)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(
      filename,
      sheet = X,
      col_types = c(
        "text", "text", "date",
        "skip", "date", "skip",
        "numeric", "numeric"
      )
    ))
  names(x) <- sheets
  x
}

all_rivers <- read_excel_allsheets(filename)

riv_dat <- bind_rows(all_rivers, .id = "river")

# Cast date columns
riv_dat$Ice.Off <- ymd(riv_dat$Ice.Off)
riv_dat$Last.Ice <- ymd(riv_dat$Last.Ice)

# Create DOY column
riv_dat$LI.DOY <- yday(riv_dat$Last.Ice)
riv_dat$IO.DOY <- yday(riv_dat$Ice.Off)

# Order rivers
riv_dat$river <- factor(
  riv_dat$river,
  levels = c("Moose", "Albany", "Attawap", "Winisk", "Severn")
)

#rename Attawap as Attawapiskat for plotting
levels(riv_dat$river)[levels(riv_dat$river) == "Attawap"] <- "Attawapiskat"

# Order rivers reaches
# io_tdy$Location_sht  <- factor(io_tdy$Location_sht,levels=c("KNG", "FRK-HI", "HI-CHP", "CHP-FC", "FC-FB", "FB-BC", "AT", "NOCH", "SOCH", "BC"))

# Get list of reaches to factor
segs <- unique(riv_dat$seg)

# Get vector of median break up order for each reach
medians <- lapply(segs, function(x) {
  dat <- riv_dat[riv_dat$seg == x, ]
  med <- median(dat$LI.DOY, na.rm = T)
}) %>% unlist()

# Build data frame for factoring
seg.mets <- data.frame(
  segment = segs,
  medians = medians
)

# Order by reach for plotting
seg.mets <- seg.mets[order(-seg.mets$medians), ]

# Add factored segs to data
riv_dat$seg_factor <- factor(riv_dat$seg,
  levels = seg.mets$segment,
  labels = seg.mets$segment
)


# Plot Breakup Timing
dates <- ggplot(data = riv_dat) +
  # Add breakup beginning median markers (squares)
  stat_summary(
    mapping = aes(x = seg_factor, y = as.Date(LI.DOY, origin = "2017-01-01")),
    fun.ymin = function(z) {
      quantile(z, 0.25)
    },
    fun.ymax = function(z) {
      quantile(z, 0.75)
    },
    fun.y = median, shape = 0, col = "black", alpha = 0.5, size = 0.3
  ) +
  # Add breakup beginning median lines for reach river
  geom_hline(
    data = filter(riv_dat, river == "Moose"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "black", alpha = 0.5, size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Albany"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "black", alpha = 0.5, size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Attawapiskat"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "black", alpha = 0.5, size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Winisk"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "black", alpha = 0.5, size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Severn"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "black", alpha = 0.5, size = 0.3
  ) +
  # Add breakup end median markers (triangles)
  stat_summary(
    mapping = aes(x = seg_factor, y = as.Date(IO.DOY, origin = "2017-01-01")),
    fun.ymin = function(z) {
      quantile(z, 0.25)
    },
    fun.ymax = function(z) {
      quantile(z, 0.75)
    },
    fun.y = median,
    shape = 2,
    col = "red",
    linetype = "dashed",
    size = 0.3
  ) +
  # Add breakup end median lines for reach river
  geom_hline(
    data = filter(riv_dat, river == "Moose"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "red", linetype = "dashed", size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Albany"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "red", linetype = "dashed", size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Attawapiskat"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "red", linetype = "dashed", size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Winisk"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "red", linetype = "dashed", size = 0.3
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Severn"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    col = "red", linetype = "dashed", size = 0.3
  ) +
  facet_grid(factor(river) ~ ., scale = "free_y", space = "free_y") +
  # Dummy Geoms for legend (hacky but it works)
  geom_point(x = NA, y = NA, aes(shape = "MedianBUbeginning")) +
  geom_point(x = NA, y = NA, aes(shape = "MedianBUEnd")) +
  geom_hline(aes(yintercept = NA, col = "VarBUbeginning"), linetype = "solid") +
  geom_hline(aes(yintercept = NA, col = "VarBUEnd"), linetype = "dashed") +
  geom_vline(aes(xintercept = NA, linetype = "MedianPORBUbeginning"), col = "black", alpha = 0.5) +
  geom_vline(aes(xintercept = NA, linetype = "MedianPORBUEnd"), col = "black", alpha = 0.5) +
  # Scales
  scale_x_discrete(name = "River Reach") +
  scale_y_date(name = "Date") +
  scale_shape_manual(
    name = "",
    labels = c(
      "Median Beginning of Breakup",
      "Median End of Breakup"
    ),
    values = c(0, 2)
  ) +
  scale_linetype_manual(
    name = "",
    values = c("solid", "dashed"),
    labels = c(
      "Median Beginning of Breakup (All Reaches)",
      "Median End of Breakup (All Reaches)"
    )
  ) +
  scale_color_manual(
    name = "",
    values = c("black", "black"),
    labels = c(
      "Variability in Beginning of Breakup ",
      "Variability in End of Breakup"
    )
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.text.y = element_text(size = 8),
    strip.text.y = element_text(colour = "white"),
    legend.position = "bottom"
  ) +
  # Make legend multi row and make sure
  # linetype/color set properly
  guides(
    linetype = guide_legend(
      override.aes = list(color = c("black", "red")),
      nrow = 2
    ),
    shape = guide_legend(
      override.aes = list(color = c("black", "red")),
      nrow = 2
    ),
    col = guide_legend(
      nrow = 2,
      override.aes = list(
        linetype = c(1, 2),
        color = c("black", "red")
      )
    )
  ) +
  labs(tag = "a)")

# Breakup duration plot
l <- ggplot(data = riv_dat) +
  stat_summary(
    mapping = aes(
      x = seg_factor,
      y = Break.Length
    ),
    fun.ymin = function(z) {
      quantile(z, 0.25)
    },
    fun.ymax = function(z) {
      quantile(z, 0.75)
    },
    fun.y = median,
    shape = 1,
    size = 0.3
  ) +
  facet_grid(factor(river) ~ ., scale = "free_y", space = "free_y") +
  scale_x_discrete(name = "River Reach") +
  scale_y_continuous(name = "Duration(days)") +
  coord_flip() +
  # DUMMY GEOMS FOR LEGEND
  geom_hline(aes(yintercept = -Inf, col = "Range")) + 
  geom_point(aes(x = -Inf, y = Inf, shape = "Median")) + 
  # Scale color for legend
  scale_color_manual(
    name = "",
    labels = c("Range of Breakup Duration"),
    values = c("black")
  ) +
  scale_shape_manual(
    name = "",
    labels = c("Median Breakup Duration"),
    values = c(1)
  ) + 
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) +
  # Multi-row legend to match height of timing plot
  guides(
    col = guide_legend(
      nrow = 2
    ),
    shape = guide_legend(
      nrow = 2
    )
  ) +
  labs(tag = "b)")

# Plot both
date_duration_plot <- grid.arrange(dates, l, widths = c(3, 1), nrow = 1)

#save plot
ggsave(plot = date_duration_plot, 
       filename = here::here("plots", "Figure_6_bu_date_duration.tiff"), 
       height = 7, width = 16)
