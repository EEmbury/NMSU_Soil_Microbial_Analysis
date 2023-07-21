
my_data <- read.csv("CO2_adjusted_w_start.csv")


# CO2_measure_for_R is values without adjusting for start CO2
# CO2_raw.csv is values in jar without adjusting for biomass and not including start C02
# CO2_adjust_w_start is values adjusted for biomass and start CO2
# CO2_site is values of empty jar


library(ggplot2)
library(ggpubr)

my_data$Vegetation <- factor(my_data$Vegetation , 
                             levels=c("Grass", "Mesquite Grass", "Mesquite")) # Change order of vegetation in plot

p = ggplot(my_data, aes(x=Vegetation, y=Respiration, fill = Vegetation))  + #plot CO2 by vegetation type
  geom_boxplot(lwd=.8)  + # add box plot
  theme_bw()+ # remove grey background
  facet_wrap(~Month)+ #group by month
  ylab("CO2 g-1 of biomass h-1")+ # change y axis label
  ggtitle ("CO2 Measurements Across Seasons and Vegetation")+
  scale_fill_manual(values=c("Mesquite" ="#c04d27", "Grass" = "#edba1d", "Mesquite Grass" = "#47c0c4"))+ #add custom colors#change title
  theme( text = element_text(size = 20), legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle=35, vjust = 0.95, hjust =0.95))

  

p 


mycomparisons <- list( c("Grass", "Mesquite Grass"),  c("Mesquite", "Mesquite Grass"), c("Grass", "Mesquite")) #groupings for ANOVA

q <- p + stat_compare_means(comparisons = mycomparisons, hide.ns = T, label = "p.signif")


q




library(svglite)
ggsave(file="CO2_mar_may.svg", plot=q, width=9, height=12) # export plot

library(dplyr)
stats <- group_by(my_data, Vegetation) %>%
  summarise(
    count = n(),
    mean = mean(Respiration, na.rm = TRUE),
    sd = sd(Respiration, na.rm = TRUE)
  )

stats


aov <- aov(Respiration ~ Vegetation*Month, my_data)  
summary(aov)
TukeyHSD(aov)

#Jar CO2 not adjusted

#Mesquite Grass:March-Grass:January          -117.75737 0.0000000
#Mesquite:March-Grass:January                -112.15788 0.0000000
#Grass:May-Grass:January                     -139.85524 0.0000000
#Mesquite Grass:March-Mesquite Grass:January  -45.64306 0.0002964
#Mesquite:March-Mesquite Grass:January        -40.04357 0.0006014
#Grass:May-Mesquite Grass:January             -67.74093 0.0000157
#Mesquite Grass:March-Mesquite:January        -71.62179 0.0000092
#Mesquite:March-Mesquite:January              -66.02231 0.0000199
#Grass:May-Mesquite:January                   -93.71966 0.0000004
#Mesquite Grass:March-Grass:March             -62.43121 0.0000325
#Mesquite:March-Grass:March                   -56.83173 0.0000688
#Grass:May-Grass:March                        -84.52909 0.0000015
#Mesquite Grass:May-Mesquite Grass:March      248.96907 0.0000640
#Mesquite:May-Mesquite Grass:March            220.89029 0.0009072
#Mesquite Grass:May-Mesquite:March            243.36959 0.0001312
#Mesquite:May-Mesquite:March                  215.29081 0.0017878
#Mesquite Grass:May-Grass:May                 271.06694 0.0000034
#Mesquite:May-Grass:May                       242.98816 0.0000527


