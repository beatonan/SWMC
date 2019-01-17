# Imports
library(ggplot2)
library(readxl)
library(grid)
library(gridExtra)
library(scales)
library(dplyr)
library(lubridate)
library(here)
library(readxl)

# Data source
filename <- here::here("import", "All_Rivers.xlsx")

# Function to read all sheets from Excel workbook
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(
      filename,
      sheet = X,
      col_types = c(
        "text", "text", "date",
        "skip", "date", "skip", "numeric",
        "numeric"
      )
    ))
  names(x) <- sheets
  x
}

# Read all sheets
all_rivers <- read_excel_allsheets(filename)

# Join sheets into single data frame
riv_dat <- bind_rows(all_rivers, .id = "river")

# Cast date columns
riv_dat$Ice.Off <- ymd(riv_dat$Ice.Off)
riv_dat$Last.Ice <- ymd(riv_dat$Last.Ice)

# Create DOY column
riv_dat$LI.DOY <- yday(riv_dat$Last.Ice)
riv_dat$IO.DOY <- yday(riv_dat$Ice.Off)

# Create year column
riv_dat$year <- year(riv_dat$Last.Ice)

# Add severity column
riv_dat <- riv_dat %>%
  mutate(
    severe =
      case_when(
        river == "Moose" & year %in% c(2012, 2013, 2006) ~ 1,
        river == "Albany" & year %in% c(2014, 2012, 2008, 2006) ~ 1
      )
  )

# Set severity to 0 for NAs
riv_dat$severe[is.na(riv_dat$severe)] <- 0

# Remove rows with NA values in last ice column
riv_dat <- riv_dat %>%
  filter(!is.na(LI.DOY))

# Get min/max for axis scaling
min.DOY <- min(c(
  min(riv_dat$LI.DOY, na.rm = T),
  min(riv_dat$IO.DOY, na.rm = T)
),
na.rm = T
)

max.DOY <- max(c(
  max(riv_dat$LI.DOY, na.rm = T),
  max(riv_dat$IO.DOY, na.rm = T)
),
na.rm = T
)

# set Min and Max as Dates
min.DOY <- as.Date(min.DOY, origin = "2017-01-01")
max.DOY <- as.Date(max.DOY, origin = "2017-01-01")

# Factor rivers to get order correct
# for plotting
riv_dat$river <- factor(
  riv_dat$river,
  levels = c(
    "Moose", "Albany",
    "Attawap", "Winisk",
    "Severn"
  )
)

#rename Attawap as Attawapiskat for plotting
levels(riv_dat$river)[levels(riv_dat$river) == "Attawap"] <- "Attawapiskat"

# Common plot settings for easy changes
text_settings <- theme(
  strip.background = element_blank(),
  axis.text.y = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.x = element_blank(),
  strip.text.y = element_text(colour = "white")
)

# LI plot code:
begin <- ggplot(data = riv_dat) + stat_summary(
  mapping = aes(
    x = factor(year),
    y = as.Date(LI.DOY, origin = "2017-01-01"),
    shape = factor(severe),
    col = factor(severe)
  ),
  fun.ymin = function(z) {
    min(z)
  },
  fun.ymax = function(z) {
    max(z)
  },
  fun.y = median, size = 0.3
) +
  scale_shape_manual(values = c(0, 2)) +
  scale_color_manual(values = c("black", "black")) +
  facet_grid(. ~ factor(river)) +
  geom_hline(
    data = filter(riv_dat, river == "Moose"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Albany"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Attawapiskat"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Winisk"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Severn"),
    aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  scale_x_discrete(name = "Year") +
  scale_y_date(name = "", limits = c(min.DOY, max.DOY)) +
  coord_flip() +
  theme_bw() +
  text_settings +
  theme(
    legend.position = "none",
    # Remove x axis labels
    axis.text.x = element_blank()
  ) +
  labs(tag = "a)")

# IO plot code
end <- ggplot(data = riv_dat) + stat_summary(
  mapping = aes(
    x = factor(year),
    y = as.Date(IO.DOY, origin = "2017-01-01"),
    shape = factor(severe),
    col = factor(severe)
  ),
  show.legend = FALSE,
  fun.ymin = function(z) {
    min(z)
  },
  fun.ymax = function(z) {
    max(z)
  },
  fun.y = median, size = 0.3
) +
  facet_grid(. ~ factor(river)) +
  geom_hline(
    data = filter(riv_dat, river == "Moose"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Albany"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Attawapiskat"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Winisk"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  geom_hline(
    data = filter(riv_dat, river == "Severn"),
    aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
    linetype = "dotted"
  ) +
  scale_x_discrete(name = "Year") +
  scale_y_date(
    name = "",
    limits = c(min.DOY, max.DOY)
  ) +
  # DUMMY GEOMS FOR LEGEND
  geom_vline(aes(xintercept = Inf, linetype = "MedianBU")) +
  geom_point(x = NA, y = NA, aes(alpha = "severe")) +
  geom_hline(aes(yintercept = Inf, alpha = "notsevere")) +
  scale_shape_manual(
    values = c(0, 2),
    name = "",
    labels = c("Median Breakup Date and Range for Years without High Water", 
               "Median Breakup Date and Range for Years with High Water")
  ) +
  scale_color_manual(
    name = "",
    values = c("black", "black"),
    labels = c("Median Breakup Date and Range for Years without High Water", 
               "Median Breakup Date and Range for Years with High Water")
  ) +
  scale_linetype_manual(
    name = "",
    values = c("dotted"),
    labels = c("Median Breakup Date (2000-2017)")
  ) +
  scale_alpha_manual(
    name = "",
    values = c(1, 1),
    labels = c("Median Breakup Date and Range for Years without High Water", 
               "Median Breakup Date and Range for Years with High Water")
  ) +
  coord_flip() +
  theme_bw() +
  text_settings +
  theme(
    legend.position = "bottom",
    # Remove facet labels
    strip.text = element_blank()
  ) +
  guides(
    alpha = guide_legend(
      override.aes = 
        list(
          col = c("black", "black"),
          shape = c(0, 2)
        )
      )
    ) +
  labs(tag = "b)")

ggsave(plot = begin, filename = here::here("plots", "test.png"), height = 6.5, width = 12)


#arrange for final plot
final_plot <- grid.arrange(begin, end)

#save plot
ggsave(plot = final_plot, 
       filename = here::here("plots", "bu_date_by_year_legend.tiff"), 
       height = 6.5, width = 12,
       dpi = "retina")
