PLFA <- read.csv("Oct_Mar_PLFA.csv")

library(ggplot2)
library(ggpubr)
library(tidyverse)

################# Bacteria ##########################
#graph broken up by veg

#graph of total bacterial % of total biomass
p = ggplot(PLFA, aes(y=Total.Bacteria....of.Tot..Biomass, 
                    (x=factor(Field.ID, level=c('Grass', 'Mesquite-Grass', 'Mesquite'))) ))  + #change order of x-axis
  geom_boxplot(lwd=.8)  + #change boxplot size
  theme_bw() + #remove grey background
  facet_wrap(~Month) + #group by vegetation type
  xlab("Vegetation") + # change x axis label
  ylab("% of Bacteria in Total Biomass")+ # change y axis label
  ggtitle ("% of Bacteria in Total Biomass Across Vegetation Types and Sampleing Periods")+ #change title
  theme(text = element_text(size = 20))   # change text size

p

mycomparisons_veg <- list(c("Grass", "Mesquite-Grass"), c("Mesquite", "Mesquite-Grass"), c("Grass", "Mesquite")) #groupings for ANOVA


d = p + stat_compare_means(comparisons = mycomparisons_veg, size = 5)+
  stat_compare_means(method = "anova", label.y = 7, size = 6) # add anova values to plot


b = d + geom_point(aes(color=Field.ID), size = 3)  + #change color of points
  scale_color_manual(values=c("Grass" ="#ee9b00", "Mesquite" = "#94d2bd"))  + #add custom colors
  theme(legend.position = "none", text = element_text(size = 20)) # change sizes

b

ggsave(file="bact_biomass_anova_month.svg", plot=b, width=16, height=9)

# graph broken up by month

#graph of total bacterial % of total biomass
p = ggplot(PLFA, aes( y=Total.Bacteria....of.Tot..Biomass,
                      x=factor(Month, level=c('October', 'January', 'March'))))  + 
  geom_boxplot(lwd=.8)  + #change boxplot size
  theme_bw() + #remove grey background
  facet_wrap(~Field.ID) + #group by vegetation type
  xlab("Month") + # change x axis label
  ylab("% of Bacteria in Total Biomass")+ # change y axis label
  ggtitle ("% of Bacteria in Total Biomass Across Vegetation Types and Sampleing Periods") #change title

p


mycomparisons <- list( c("January", "October"), c("January", "March"), c("March", "October")) #groupings for ANOVA

d = p + stat_compare_means(comparisons = mycomparisons, size = 5)+
  stat_compare_means(method = "anova",  # add anova values to plot
                     label.y = 7, size = 6) # move up and down, change size


b = d + geom_point(aes(color=Field.ID), size = 3)  + #change color of points
  scale_color_manual(values=c("Mesquite" ="#ee9b00", "Grass" = "#94d2bd"))  + #add custom colors
  theme(legend.position = "none", text = element_text(size = 20)) # change sizes

b

library(svglite)
ggsave(file="bact_biomass_anova_veg.svg", plot=b, width=16, height=9) # export plot



####################### fungi ######################

# graph broken up by veg

#graph of total fungi % of total biomass
q = ggplot(PLFA, aes(y=Total.Fungi....of.Tot..Biomass,
                     x=factor(Month, level=c('October', 'January', 'March')))) + 
  geom_boxplot(lwd=.8)  + #change boxplot size
  theme_bw() + #remove grey backgroun
  facet_wrap(~Field.ID) + #group by vegetation type
  xlab("Month") + # change x axis label
  ylab("% of Fungi in Total Biomass")+ # change y axis label
  ggtitle ("% of Fungi in Total Biomass Across Vegetation Types and Sampleing Periods") #change title


q


mycomparisons <- list( c("January", "October"), c("January", "March"), c("March", "October")) #groupings for ANOVA

d = q + stat_compare_means(comparisons = mycomparisons, size = 5)+
  stat_compare_means(method = "anova", label.y = -1.5, size = 6) # add anova values to plot


b = d + geom_point(aes(color=Field.ID), size = 3)  + #change color of points
  scale_color_manual(values=c("Mesquite" ="#ffd166", "Grass" = "#06d6a0", "Mesquite-Grass" = "#ef476f"))  + #add custom colors
  theme(legend.position = "none", text = element_text(size = 20)) # change sizes

b

ggsave(file="fun_biomass_anova_veg.svg", plot=b, width=16, height=9)





# plot broken by month


q = ggplot(PLFA, aes(x=Field.ID, y=Total.Fungi....of.Tot..Biomass))  + 
  geom_boxplot(lwd=.8)  + #change boxplot size
  theme_bw() + #remove grey backgroun
  facet_wrap(~Month) + #group by vegetation type
  xlab("Vegetation") + # change x axis label
  ylab("% of Fungi in Total Biomass")+ # change y axis label
  ggtitle ("% of Fungi in Total Biomass Across Vegetation Types and Sampleing Periods") #change title


q


mycomparisons_veg <- list(c("Grass", "Mesquite"), c("Mesquite", "Mesquite-Grass"), c("Grass", "Mesquite-Grass")) #groupings for ANOVA

d = q + stat_compare_means(comparisons = mycomparisons_veg, size = 5)+
  stat_compare_means(method = "anova", label.y = -1.5, size = 6) # add anova values to plot


b = d + geom_point(aes(color=Month), size = 3)  + #change color of points
  scale_color_manual(values=c("January" ="#ffd166", "March" = "#06d6a0", "October" = "#ef476f"))  + #add custom colors
  theme(legend.position = "none", text = element_text(size = 20)) # change sizes

b

ggsave(file="fun_biomass_anova_month.svg", plot=b, width=16, height=9)







## ANOVA of fungal % of biomass by month and vegetation type

aov.fungi <- aov(Total.Fungi....of.Tot..Biomass ~ Field.ID * Month, PLFA)  
summary(aov.fungi)
TukeyHSD(aov.fungi)


############## bacteria and fungi together ###############


PLFA %>% 
  rename( # here we rename the columns so things look nice in the graph later
    Fungi = Total.Fungi....of.Tot..Biomass,
    Bacteria = Total.Bacteria....of.Tot..Biomass
  ) %>% 
  pivot_longer( # then we collapse the columns for each side of the brain into a single column,with a second column holding size values
    cols = c("Fungi", "Bacteria"),
    names_to = "Type",
    values_to = "Biomass" )%>% # then we plot and give it a title
      ggplot(
        aes(x = Month, y = Biomass, fill = Type )) + 
      geom_bar(position="dodge", stat="identity") +
  facet_wrap(~Field.ID)+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
)
    


#### claculate stats for error bars
library(Rmisc)
PLFA_stat_fungi <- summarySE(PLFA, measurevar="Total.Fungi....of.Tot..Biomass", groupvars=c("Field.ID","Month"))
PLFA_stat_bacteria <- summarySE(PLFA, measurevar="Total.Bacteria....of.Tot..Biomass", groupvars=c("Field.ID","Month"))

PLFA_stat_bac_fun <- merge(PLFA_stat_bacteria, PLFA_stat_fungi, by= c("Field.ID", "Month", "N"))

##### results #####
                #Df Sum Sq Mean Sq F value   Pr(>F)    
#Field.ID        2  158.0   79.01   7.046 0.001604 ** 
#Month           2   14.0    7.02   0.626 0.537808    
#Field.ID:Month  4  274.9   68.74   6.130 0.000265 ***
  #Residuals      72  807.3   11.21                     


## ANOVA of bacteria % of biomass by month and vegetation type

aov.bac <- aov(Total.Bacteria....of.Tot..Biomass ~ Field.ID * Month, PLFA)  
summary(aov.bac)
TukeyHSD(aov.bac)


### results
                #Df Sum Sq Mean Sq F value  Pr(>F)   
#Field.ID        2    448  223.93   4.082 0.02093 * 
#Month           2    116   57.95   1.056 0.35306   
#Field.ID:Month  4   1084  271.07   4.941 0.00141 **
  #Residuals      72   3950   54.86     
