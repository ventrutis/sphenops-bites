require(ggplot2)
require(dplyr)

#set working directory

setwd("")

#load and prepare data
source("SCRIPT/LOAD.R")

### BOXPLOT ##############

source("SCRIPT/BOXPLOT.R")
# 
fish_box(
  pdf_path = "PLOT/Patch_Temperature.pdf",
  single_run = "patch",
  data = bites,
  x_val = "temperature",
  y_val = "nbites",
  z_val = "cut_entrance",
  col_val = c("cyan","red"), #patch: terrain.colors, temp: c("cyan","red"), gender: c("pink","blue")
  legend_position = "topright",
  ylim = c(0,80) #y_val="nbites", c(0,80)
)

### TESTS ##############

#step-wise regression
step_mod=lm(nbites ~ temperature * sex * entrance * patch, data=bites)
summary(step(step_mod))

#patch ANCOVA
p_ancova = aov(nbites ~ patch * time, data = bites)
summary(p_ancova)

#temp ANOVA
temp_anova = aov(nbites ~ patch * temperature, data = bites)
summary(temp_anova)

#tukeyHSD

sex_temp_anova = aov(nbites ~ sex * temperature, data = bites)
tukey_result= TukeyHSD(sex_temp_anova)


### INTERACTION PLOT #############

ggplot(bites, aes(x = temperature, 
                  y = nbites, 
                  group = sex, 
                  color = sex)) +
  
  # Adding lines with group colors
  stat_summary(fun = "mean", geom = "line", size = 1.2) +  # Colored lines
  
  # Adding error bars with group colors
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(color = sex), size = 1, width = 0.3) +
  
  # Custom colors for lines
  scale_color_manual(values = c("#FF69B4", "#FFB6C1"), name = "Sex") + # Change legend title to "Sex"
  theme_classic() +
  labs(x = "Temperature (°C)", y = "N° of bites for each feeding episode") +
  
  # Customizing the legend to show only colored points
  guides(color = guide_legend(override.aes = list(
    shape = 21,     # Circle shape with fill
    size = 5,       # Larger points in the legend
    fill = c("#FF69B4", "#FFB6C1"),  # Fill colors for each group
    stroke = 0.5    # Border thickness for the points
  )))
