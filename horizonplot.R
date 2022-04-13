
install.packages("ggHoriPlot")

library(tidyverse)
library(data.table)
library(ggHoriPlot) 
library(ggthemes)
library(readxl)
library(lubridate)
library(zoo)
library(gganimate)
library(transformr)
library(gifski)


#Dados mensais precipitação 

df_udi <- read_excel("D:/Rafael/Trabalhos/Clima/Doutorado-UFRGS/Colegas/Gabriel_Hofmann/df_udi3.xlsx")

df_udi$Data <- as.Date(with(df_udi, paste(Year2, Month, Day, sep="-")), "%Y-%m-%d")
df_udi$Month2 <- factor(df_udi$Month2, levels=unique(df_udi$Month2))

df_udi <- as_tibble(df_udi)

#horizonplot

cutpoints <- df_udi  %>% 
  mutate(
    outlier = between(
      prec, 
      quantile(prec, 0.25, na.rm=T)-
        1.5*IQR(prec, na.rm=T),
      quantile(prec, 0.75, na.rm=T)+
        1.5*IQR(prec, na.rm=T))) %>% 
  filter(outlier)

ori <- sum(range(cutpoints$prec))/2
sca <- seq(range(cutpoints$prec)[1], 
           range(cutpoints$prec)[2], 
           length.out = 7)[-4]

ori <- round(ori, 1) # The origin


sca <- round(sca, 1) # The horizon scale cutpoints

sca <- c("0", "5", "10", "15", "30", "50")
sca <- as.numeric(sca)

df_udi %>% ggplot() +
  geom_horizon(aes(Data, 
                   prec,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'YlGnBu', reverse = T) +
  facet_grid(Year~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  labs(fill='mm/day')+
  ggtitle('Monthly precipitation in Uberlândia, Brazil', 
          'from 1979 to 2020')

#Tileplot


df_udi %>%  
  ggplot(aes(Month2, Year, fill = prec))+
  geom_tile(color= NA)+
  labs(fill = 'Prec. (mm)', y='',
       x='', title= 'Monthly Precipitation in Uberlândia, Brazil', caption="Silva, R.C., 2021",
       subtitle ='ERA5 Precipitation Data')+
  theme_minimal()+
  scale_y_continuous(labels= seq(1950,2021,1), breaks = seq(1950,2021,1))+
  scale_fill_distiller(palette = 'YlGnBu', direction=1,
                       limits = c(min(0), max(650)),
                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600))+
  theme(panel.grid= element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=6),
        text = element_text (size=10),
        legend.key = element_rect (size=10))

#Animação

df_udi2 <- df_udi
df_udi2 <-  group_by(df_udi2, Year) 
df_udi2 <-  summarize(df_udi2, prec2= sum(prec))
df_udi2 <- filter(df_udi2, Year > 1989)
df_udi2$prec2 <- round(df_udi2$prec2, digits=0)


p<- ggplot(df_udi2,aes(Year, prec2))+
  geom_line(colour="Blue")+
  ylim(500,2000)+
  geom_segment(aes(xend=2021,yend=prec2), linetype=2,colour='Blue')+
  geom_point(size=2, colour='Blue')+
  geom_text(aes(x=2021, label=prec2,hjust=0))+
  transition_reveal(Year)+
  coord_cartesian(clip='off')+
  labs(title="Annual Precipitation in Uberlândia, Brazil (1990-2021)",
       y="Precipitation (mm)",
       caption="Source: ERA5 Precipitation. Author: Rafael Silva (2021)")+
  theme_minimal()+
  theme(plot.margin=margin(5.5,40,5.5,5.5))
p
#Aqui um exemplo de exportação com configurações personalizadas de fps
anim<-animate(p, fps=6)
anim
anim_save("D:/Rafael/Trabalhos/Clima/Laboratório Clima/Era5Prec_udi_50-21.gif")

#12 month mean

media<- frollsum(df_udi$prec, 12)
df_udi2<- cbind(df_udi, media_20)
df_udi2$Data <- as.Date(with(df_udi2, paste(Year, Month, Day, sep="-")), "%Y-%m-%d")

df_udi2 <- filter(df_udi2, Year>2000)


ggplot(df_udi2,aes(Data, media_20))+geom_col(colour="Blue")+
  labs(title="20 month mean Precipitation in Uberlândia, Brazil (1952-2021)",
       y="Precipitation (mm)",
       caption="Source: ERA5 Precipitation. Author: Rafael Silva (2021)")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
