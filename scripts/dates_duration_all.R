#install.packages("ggstance")

# Imports

library(ggplot2)
library(ggstance)
library(readxl)
library(grid)
library(gridExtra)
library(scales)
library(dplyr)
library(lubridate)

# -------------------------- SETUP ----------------------------- #
# Data source
filename <- "C:/Users/beatonan/Documents/SWMC/Projects/Far North Hydrology and Remote Sensing Project/R Files/Ryans Code/All_Rivers.xlsx"

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

#order rivers

riv_dat$river  <- factor(riv_dat$river,levels = c("Moose","Albany","Attawap","Winisk", "Severn"))

#Order rivers reaches

#io_tdy$Location_sht  <- factor(io_tdy$Location_sht,levels=c("KNG", "FRK-HI", "HI-CHP", "CHP-FC", "FC-FB", "FB-BC", "AT", "NOCH", "SOCH", "BC"))

# Get list of reaches to factor
segs <- unique(riv_dat$seg)

# Get vector of median break up order for each reach
medians <- lapply(segs, function(x){
  dat <- riv_dat[riv_dat$seg == x, ]
  med <- median(dat$LI.DOY, na.rm = T)
}) %>% unlist()

# Build data frame for factoring
seg.mets <- data.frame(segment = segs,
                       medians = medians)

# Order
seg.mets <- seg.mets[order(-seg.mets$medians), ]

# Add factored segs to data
riv_dat$seg_factor <- factor(riv_dat$seg, 
                               levels = seg.mets$segment,
                               labels = seg.mets$segment)

#beginning and end of breakup dates on same figure


dates <- ggplot(data = riv_dat) + stat_summary(
  mapping = aes(x = seg_factor, y = as.Date(LI.DOY, origin = "2017-01-01")),
  fun.ymin = function(z) { quantile(z,0.25) },
  fun.ymax = function(z) { quantile(z,0.75) },
  fun.y = median, shape = 0, col = "black", alpha = 0.5, size = 0.3) +
  
  geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "black", alpha = 0.5, size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "black", alpha = 0.5, size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "black", alpha = 0.5, size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "black", alpha = 0.5, size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "black", alpha = 0.5, size = 0.3) +
  stat_summary(
    mapping = aes(x = seg_factor, y = as.Date(IO.DOY, origin = "2017-01-01")),
    fun.ymin = function(z) { quantile(z,0.25) },
    fun.ymax = function(z) { quantile(z,0.75) },
    fun.y = median, shape = 2, col = "red", linetype = "dashed", size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "red", linetype = "dashed", size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "red", linetype = "dashed", size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "red", linetype = "dashed", size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "red", linetype = "dashed", size = 0.3) +
  geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
             col = "red", linetype = "dashed", size = 0.3) +
  facet_grid(factor(river) ~., scale = "free_y", space = "free_y") +
  scale_x_discrete(name = "River Reach") + 
  scale_y_date(name = "Date") +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(), 
        axis.text.y = element_text(size = 5),
        strip.text.y = element_text(colour = "white"))

#try shape = 17 and shape = 15 for solid symbols


#breakup length plot for grid.arrange

l <- ggplot(data = riv_dat) + 
  stat_summary(
  mapping = aes(x = seg_factor, y = Break.Length),
  fun.ymin = function(z) { quantile(z,0.25) },
  fun.ymax = function(z) { quantile(z,0.75) },
  fun.y = median, shape = 1, col = "black", size = 0.3) +
  facet_grid(factor(river) ~., scale = "free_y",space = "free_y") +
  scale_x_discrete(name = "River Reach") + 
  scale_y_continuous(name = "Duration(days)") +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


grid.arrange(dates, l, widths = c(3,1), nrow = 1)  

 

# scale_y_date(labels = date_format('%b %d')) +
# scale_x_discrete(name = "River reach") +
# 
# ## Theme settings
# theme(axis.text.x = element_blank(),
#       axis.text.y = element_text(size = 5),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       axis.ticks.x = element_blank(),
#       axis.ticks.y = element_line(size = 0.1),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.background = element_blank(),
#       axis.line = element_line(colour = "black", size = 0.1),
#       legend.position = 'none',
#       plot.title = element_text(size = 6,  hjust = 0.5))
# 
# 
#   
#   # geom_jitter(col = 'black', alpha = 0.4, size = 0.3) + 
#   # scale_y_date(labels = date_format('%b %d')) + 
#   # scale_x_discrete(name = "River reach") + 
#   # ## Use custom pallette
#   # scale_fill_manual(values = plot.palette) + 
#   # # Plot title
#   # ggtitle("Ice Off Date by Reach From 2000-2017") + 
#   # ## Theme settings
#   # theme(axis.text.x = element_blank(),
#   #       axis.text.y = element_text(size = 5),
#   #       axis.title.x = element_blank(),
#   #       axis.title.y = element_blank(),
#   #       axis.ticks.x = element_blank(),
#   #       axis.ticks.y = element_line(size = 0.1),
#   #       panel.grid.major = element_blank(), 
#   #       panel.grid.minor = element_blank(),
#   #       panel.background = element_blank(), 
#   #       axis.line = element_line(colour = "black", size = 0.1),
#   #       legend.position = 'none',
#   #       plot.title = element_text(size = 6,  hjust = 0.5)) 
# 
# ## Additional Plots:
# 
# 
# #plot by mean and std
# 
# ggplot(riv_dat, aes(x = factor(seg),
#                     y = as.Date(IO.DOY, origin = "2017-01-01"))) +
#   stat_summary(fun.data = "mean_sdl", shape = 18, color = "red") +
#   coord_flip()+ 
#   facet_grid(factor(river)~., scale="free_y",space="free_y")
# 
# #plot by median and min and max
# 
# ggplot(riv_dat, aes(x = factor(seg),
#                     y = as.Date(IO.DOY, origin = "2017-01-01"))) +
#   stat_summary(fun.y = median, fun.ymin = min, fun.ymax = max, colour = "red", shape = 18) +
#   coord_flip()+ 
#   facet_grid(factor(river)~., scale="free_y",space="free_y")

#stand alone breakup length plot 

# l <- ggplot(data = riv_dat) + stat_summary(
#   mapping = aes(x = seg_factor, y = Break.Length),
#   fun.ymin = function(z) { quantile(z,0.25) },
#   fun.ymax = function(z) { quantile(z,0.75) },
#   fun.y = median, shape = 1, col = "black") +
#   facet_grid(factor(river)~., scale="free_y",space="free_y") +
#   scale_x_discrete(name = "River Reach") + 
#   scale_y_continuous(name = "Duration(days)") +
#   coord_flip()+
#   theme_bw()+
#   theme(strip.background =element_rect(fill="white"), 
#         axis.text.y = element_text(size = 5))

# geom_pointrange(mapping=aes(x = seg_factor, y = mean(as.Date(LI.DOY, origin = "2017-01-01")), 
#                             ymin=function(z) { quantile(z,0.25) }, 
#                             ymax=function(z) { quantile(z,0.75)))+


#-------------Attempt to add legends---------------#

#reformat to tidy data:


# ggplot(data = riv_dat) +
#   stat_summary(
#     mapping = aes(x = seg_factor, y = as.Date(LI.DOY, origin = "2017-01-01"), shape = "Beginning of Breakup"),
#     fun.y = median, col = grey, size = 1, geom = "point") +
# 
#   geom_linerange(
#     mapping = aes(x = seg_factor, y = as.Date(LI.DOY, origin = "2017-01-01"), linetype = "Beginning of Breakup Range"),
#     ymin = function(z) { quantile(z,0.25) },
#     ymax = function(z) { quantile(z,0.75) },
#     na.rm = T,
#     col = "grey") +
# 
#   geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "black", alpha = 0.5, size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "black", alpha = 0.5, size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "black", alpha = 0.5, size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "black", alpha = 0.5, size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(LI.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "black", alpha = 0.5, size = 0.3)+
# 
#   stat_summary(
#     mapping = aes(x = seg_factor, y = as.Date(IO.DOY, origin = "2017-01-01"), linetype = "End of Breakup Range"),
#     fun.ymin = function(z) { quantile(z,0.25) },
#     fun.ymax = function(z) { quantile(z,0.75) },
#     geom = "line") +
#   
#   stat_summary(
#     mapping = aes(x = seg_factor, y = as.Date(IO.DOY, origin = "2017-01-01"), shape = "End of Breakup Range"),
#     fun.ymin = function(z) { quantile(z,0.25) },
#     fun.ymax = function(z) { quantile(z,0.75) },
#     fun.y = median, geom = "point") +
# 
#   geom_hline(data = filter(riv_dat, river == "Moose"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "red", linetype = "dashed", size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Albany"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "red", linetype = "dashed", size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Attawap"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "red", linetype = "dashed", size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Winisk"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "red", linetype = "dashed", size = 0.3)+
#   geom_hline(data = filter(riv_dat, river == "Severn"), aes(yintercept = as.numeric(as.Date((median(IO.DOY, na.rm = T)), origin = "2017-01-01"))),
#              col = "red", linetype = "dashed", size = 0.3)+
# 
#   facet_grid(factor(river)~., scale="free_y",space="free_y") +
#   scale_x_discrete(name = "River Reach") +
#   scale_y_date(name = "Date") +
#   coord_flip()+
#   theme_bw()+
#   theme(strip.background =element_blank(),
#         axis.text.y = element_text(size = 5),
#         strip.text.y = element_text(colour = "white"))+
#   scale_linetype_manual("", values = c("Beginning of Breakup Range" = 1, "End of Breakup Range" = 0))+
#   scale_shape_manual("", values = c("Beginning of Breakup" = 1, "End of Breakup" = 0))
# 
# stat_summary(data = survey, aes(x = date, y = depth), shape = 16, size = 0.2,
#              fun.y = mean,
#              fun.ymin = min,
#              fun.ymax = max,
#              na.rm = T)+
#   
#   
#   #breakup length plot for grid.arrange
#   
# ggplot(data = riv_dat, aes(x = seg_factor, y = Break.Length)) + 
#   stat_summary(
#     fun.ymin = function(z) { quantile(z,0.25) },
#     fun.ymax = function(z) { quantile(z,0.75) },
#     fun.y = median, shape = 1, col = "black", size = 0.3, show.legend = TRUE) +
#   facet_grid(factor(river)~., scale="free_y",space="free_y") +
#   scale_x_discrete(name = "River Reach") + 
#   scale_y_continuous(name = "Duration(days)") +
#   coord_flip()+
#   theme_bw()+
#   theme(strip.background =element_rect(fill="white"), 
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank())

