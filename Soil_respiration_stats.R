
my_data <- read.csv("CO2_measure_for_R.csv")


library(ggplot2)
library(ggpubr)

p = ggplot(my_data, aes(x=Vegetation, y=Respiration))  + 
  geom_boxplot(lwd=.8)  + theme_bw()+ facet_wrap(~Month)



my_data$Vegetation <- factor(my_data$Vegetation , levels=c("Grass", "Mesquite Grass", "Mesquite")) # Change order

p 

q = p + stat_compare_means(comparisons = mycomparisons, size = 5)+
  stat_compare_means(method = "anova", label.y = 35, size = 6) 

mycomparisons <- list( c("Grass", "Mesquite Grass"),  c("Mesquite", "Mesquite Grass"), c("Grass", "Mesquite"))

d = q + geom_point(aes(color=Month), size = 3)  +
scale_color_manual(values=c("March" ="#ee9b00", "January" = "#94d2bd"))  + 
  theme(legend.position = "none", text = element_text(size = 20)) 


d




library(svglite)
ggsave(file="CO2_mar_jan.svg", plot=d, width=14, height=8)

library(dplyr)
stats <- group_by(my_data, Vegetation) %>%
  summarise(
    count = n(),
    mean = mean(Respiration, na.rm = TRUE),
    sd = sd(Respiration, na.rm = TRUE)
  )

stats


#only January

aov <- aov(Respiration ~ Vegetation, my_data)  
summary(aov)
TukeyHSD(aov)


#only March