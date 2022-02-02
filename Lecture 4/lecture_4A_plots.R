# Biol 6692: Lecture 4, part 1
# Example of ggplot formatting, ggarrange panels, export

library(dplyr)
library(ggplot2)

# Set working drive to script folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Reynolds Creek data
rc_data <- read.csv("RCrk_data.csv", as.is=T)

# Fix first column name
colnames(rc_data)[1] <- "ID"

# Make a plot theme
plot_theme <- theme_minimal() +
                #GOOGLE: "R ggplot how to move axis title label away from plot"
                theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + # Move the axis title away from the plot
                theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
                #GOOGLE: "R ggplot remove grid lines"
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                #GOOGEL: "R ggplot increase text size"
                theme(axis.text = element_text(size = 14)) + 
                theme(axis.title = element_text(size = 16)) +
                theme(plot.title = element_text(size = 20)) +
                theme(plot.subtitle = element_text(size = 16))


# Make a plot
MAST_by_ELEV <- ggplot(rc_data, aes(x=ELEV_DEM, y=pMAST, color=pMAST)) + geom_point(size=4, alpha=0.8, pch=16) +
  scale_colour_gradient(
    low = "grey40",
    high = "grey90",
    na.value = "grey50") +
  xlab("Elevation (m)") +
  ylab("Mean annual soil temeprature (°C)") +
  labs(title = "Reynolds Creek CZO", 
       subtitle = "Soil temperature by elevation\n(0-30 cm soil depth)",
       color=expression(atop(bold("Temperature"),"(°C)"))) +
  plot_theme

MAST_by_ELEV # Using the variable here to quickly execute plot


# Create plot 2
GEP_by_ELEV <- ggplot(rc_data, aes(x=ELEV_DEM, y=pGEP_MSAVI2, color=pGEP_MSAVI2)) + geom_point(pch=16, size=4, alpha=0.8) +
  scale_colour_gradient(
    low = "dark green",
    high = "red",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  xlab("Elevation (m)") +
  ylab(expression(paste("Gross ecosystem productivity (gC ",~m^-2," ",yr^-1,")"))) + 
  labs(title = "Reynolds Creek CZO", 
       subtitle = "Gross ecosystem productivity\nby elevation",
       color=expression(atop(bold("Productivity"),paste("(gC ",m^-2," ",yr^-1,")")))) +
  plot_theme

GEP_by_ELEV # Using the variable here to quickly execute plot

### Make a panel plot
library(ggpubr)

panel_plot <- ggarrange(MAST_by_ELEV, GEP_by_ELEV,
          ncol = 2,
          nrow = 1,
          widths = 1,
          heights = 1,
          common.legend = FALSE,
          legend = 'bottom')

### Save plot (hi-res)
png(file="my_panel_plot.png", width=6000, height=3000, units="px", res=600)
panel_plot
dev.off()




