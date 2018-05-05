library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)

###bring in wellness data from a source
DF_wellness <- read.csv("wellness_deidentified.csv")
DF_wellness$newdate <- as.Date(DF_wellness$newdate, "%Y-%m-%d")
DF_wellness_daily <- DF_wellness %>% filter(newdate >= max(DF_wellness$newdate))
DF_wellness_daily$id <- ordered(DF_wellness_daily$id, levels = rev(sort(unique(DF_wellness_daily$id))))
###I create a scaled wellness score for each player and thats what I used as a visual


Daily <- ggplot(DF_wellness_daily, aes(reorder(id, id), Well_S, fill=Well_S))+
  geom_bar(stat="identity")+theme(axis.text.x= element_text(angle=90, size=12))+
  scale_fill_gradient2(name="Wellness", low='red', mid= "yellow", high='green', midpoint=0)+
  scale_x_discrete(name="")+ylab("Scaled Wellness < 0 is below their average")+coord_flip()+
  geom_hline(VB, yintercept = -1, colour = "yellow", lty=2, size=1)+
  geom_hline(VB, yintercept = -2.5, colour = "red", lty=3, size=2)+ggtitle(DF_wellness_daily$newdate)



Weekly <- ggplot(data=subset(DF_wellness, newdate >= max(DF_wellness$newdate) - days(7)), aes(newdate, Well_S, colour=Well_S))+
  geom_point(size=2)+geom_line()+scale_colour_gradient(low="red", high="green")+
  geom_hline(yintercept= -2, colour="red", lty=2)+geom_hline(yintercept= 0, lty=3)+
  facet_grid(id~., switch = "y", scale="free_y")+
  theme(axis.title=element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks =
          element_blank(),
        plot.title = element_text(size = rel(1), colour = 'grey10'),
        panel.background = element_rect(fill = 'white'),
        strip.background = element_blank(),
        panel.spacing = unit(0.9, "null"),strip.text.y = element_text(angle = 180),
        plot.margin = unit(c(0.90,0.5,0.90,0.5), "cm"))+ggtitle("Weekly Wellness (black = average, red = very low)")
                                                                                                                                                                                                                                                                                                                     

grid.arrange(Daily, Weekly, ncol=2)