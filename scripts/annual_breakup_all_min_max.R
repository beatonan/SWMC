# Imports
library(ggplot2)
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
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = c("text", "text", "date", 
                                                                                           "skip", "date", "skip", "numeric", 
                                                                                           "numeric")))
  names(x) <- sheets
  x
}

all_rivers <- read_excel_allsheets(filename)

riv_dat <- bind_rows(all_rivers, .id = "river")

# Cast date columns 
riv_dat$Ice.Off <- ymd(riv_dat$Ice.Off)
riv_dat$Last.Ice <- ymd(riv_dat$Last.Ice)

# Create DOY column
riv_dat$LI.DOY <- as.numeric(strftime(riv_dat$Last.Ice, '%j'))
riv_dat$IO.DOY <- as.numeric(strftime(riv_dat$Ice.Off, '%j'))

# Create year column
riv_dat$year <- as.numeric(strftime(riv_dat$Last.Ice, '%Y'))

#Add binary breakup severity:

test <- riv_dat %>%
  filter(river == "Moose" & year %in% c("2012", "2013", "2006") |
           river == "Albany" & year %in% c("2014", "2012","2008", "2006")) %>%
  mutate(severe = "1")

riv_dat <- left_join(riv_dat, test)

riv_dat$severe[is.na(riv_dat$severe)] <- 0

#remove NA values

riv_dat <- riv_dat %>%
  filter(!is.na(LI.DOY))

# Get min/max for axis scaling
min.DOY <- min(c(min(riv_dat$LI.DOY, na.rm = T), 
                 min(riv_dat$IO.DOY, na.rm = T)),
               na.rm = T)

max.DOY <- max(c(max(riv_dat$LI.DOY, na.rm = T), 
                 max(riv_dat$IO.DOY, na.rm = T)),
               na.rm = T)

#set min and max as dates

min.DOY <- as.Date(min.DOY, origin = "2017-01-01")
max.DOY <- as.Date(max.DOY, origin = "2017-01-01")

#order rivers

riv_dat$river  <- factor(riv_dat$river,levels = c("Moose","Albany","Attawap","Winisk", "Severn"))

# Plot

#LI plot code:

begin <- ggplot(data = riv_dat) + stat_summary(
  mapping = aes(x = factor(year), y = as.Date(LI.DOY, origin = "2017-01-01"), shape = severe, col = factor(severe)),
  fun.ymin = function(z) { min(z) },
  fun.ymax = function(z) { max(z) },
  fun.y = median, size = 0.3) +
  scale_shape_manual(values = c(0,2)) +
  scale_color_manual(values = c("black", "red")) +
  facet_grid(.~factor(river)) +
  geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  scale_x_discrete(name = "Year") + 
  scale_y_date(name = "", limits = c(min.DOY, max.DOY)) +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(), 
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.title.x = element_blank(),
        strip.text.y = element_text(colour = "white"),
        legend.position = "none")


#IO plot code

end <- ggplot(data = riv_dat) + stat_summary(
  mapping = aes(x = factor(year), y = as.Date(IO.DOY, origin = "2017-01-01"), shape = severe, col = factor(severe)),
  fun.ymin = function(z) { min(z) },
  fun.ymax = function(z) { max(z) },
  fun.y = median, size = 0.3) +
  scale_shape_manual(values = c(0,2)) +
  scale_color_manual(values = c("black", "red")) +
  facet_grid(.~factor(river)) +
  geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))), 
             linetype = "dotted") +
  scale_x_discrete(name = "Year") + 
  scale_y_date(name = "", limits = c(min.DOY, max.DOY)) +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        strip.text.y = element_text(colour = "white"),
        legend.position = "none")

final_plot <- grid.arrange(begin, end)


