library(repr) # To resize plots in Jupyter
options(repr.plot.width = 16, repr.plot.height = 9)
library(ggplot2)
library(grid) # For multiplots
library(gridExtra) # For multiplots
library(ggthemes)
library(dplyr)
remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(countrycode)
library(scales)


tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

# 1
forest_change <- tuesdata$forest

forest_change <- forest_change[forest_change$entity != "World",]

forest_change %>%
  group_by(entity) %>% 
  summarise(net_forest_conversion = sum(net_forest_conversion), years = n()) %>%
  filter(years == 4) -> forest_grouped
forest_grouped %>% filter(dense_rank(net_forest_conversion) <= 20 | dense_rank(desc(net_forest_conversion)) <= 20) -> forest_gr_f

forest_gr_f$continent <- countrycode(sourcevar = forest_gr_f$entity, origin = "country.name", destination = "continent")

forest_gr_f %>% ggplot(aes(x = reorder(entity, -net_forest_conversion,sum), y = net_forest_conversion, fill = continent, label = entity, color = continent))+
  geom_col(width = 0.9, color = "black") +
  geom_text(angle = 90, hjust = -0.6 * sign(forest_gr_f$net_forest_conversion ) + 0.5)+
  scale_y_continuous(expand = c(0.15,0.15), labels = comma)+
  xlab("Country") +
  ylab("Forest conversion [ha]")+
  guides(color = FALSE,fill=guide_legend(title = 'Continent:'))+
  ggtitle("Forest conversion 1990-2015")+
  theme_light()+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())
  


  
# 2
brazil <- tuesdata$brazil_loss
brazil_longer <- tidyr::pivot_longer(brazil, cols = commercial_crops : small_scale_clearing, names_to = 'cause', values_to = 'forest_loss')
my_colors <- c('yellow', 'red', 'blue', 'black', 'lightgreen', 'lightblue', 'brown', 'gray', 'green', 'lightyellow', 'orange')
brazil_longer %>% ggplot(aes(x = year, y = forest_loss, fill = reorder(cause, forest_loss), label = cause)) +
  geom_stream(type = 'proportional', bw = 0.8)+
  ylab("Forest loss [ha]") +
  xlab("Year")+
  guides(fill=guide_legend(title = 'Cause:'))+
  ggtitle("Brazil forest loss causes 2001-2013")+
  theme_light()+
  scale_y_continuous(expand = c(0.02,0.02), labels = comma)+
  scale_x_continuous(expand = c(0.02,0.02))

brazil_nop <- subset(brazil, select =  -c(pasture))
brazil_longer <- tidyr::pivot_longer(brazil_nop, cols = commercial_crops : small_scale_clearing, names_to = 'cause', values_to = 'forest_loss')
brazil_longer %>% ggplot(aes(x = year, y = forest_loss, fill = cause, label = cause)) +
  geom_stream(type = 'ridge', bw = 0.8)+
  ylab("Forest loss [ha]") +
  xlab("Year")+
  guides(fill=guide_legend(title = 'Cause:'))+
  ggtitle("Brazil forest loss causes 2001-2013 (excluding pasture)")+
  theme_light()+
  scale_y_continuous(expand = c(0.02,0.02), labels = comma)+
  scale_x_continuous(expand = c(0.02,0.02))

# 3
forest_percent %>%
  filter(year == 1992 | year == 2020) %>%
  tidyr::pivot_wider(names_from = year, names_prefix = "year_", values_from = forest_area)-> f_wider

f_wider$continent <- countrycode(sourcevar = f_wider$entity, origin = "country.name", destination = "continent")

f_wider %>% filter(entity %in% c('Africa', 'Asia', 
                                 'Europe', 
                                 'Oceania', 
                                 'Americas')) -> f_continents
f_wider %>% filter(!is.na(continent)) -> f_wider
f_wider$diff <- f_wider$year_2020 - f_wider$year_1992

f_wider = merge(f_wider, f_continents, by = "year", by.x = "continent", by.y = "entity")
f_wider %>% rename(year_1992 = year_1992.x, year_2020 = year_2020.x, cont_1992 = year_1992.y, cont_2015 = year_2020.y) -> f_wider
f_wider$count_share_2015 = f_wider$year_2020 / f_wider$cont_2015
f_wider
plot_more5 = f_wider %>% ggplot(aes(x = year_1992, y = year_2020, fill = continent, label = entity, size = count_share_2015)) +
  geom_point(alpha = 0.5, shape=21, color="black") +
  geom_point(shape=21, color="black", size = 0.1)+
  scale_size(range = c(.1, 24))+
  geom_text(vjust = 3, data = subset(f_wider, year_2020 > 5), size = 5)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Share in global forest coverage in 1992") +
  ylab("Share in global forest coverage in 2020")+
  guides(fill=guide_legend(title = 'Continent:', override.aes = list(size=10)), size = guide_legend(title = "Share in continent forest coverage in 2020:"))+
  ggtitle("28 years of reforestation & deforestation (1992-2020)")+
  theme_light()+
  scale_y_continuous(limits = c(0,21),labels = function(x) paste0(x, '%'))+
  scale_x_continuous(limits = c(0,21),labels = function(x) paste0(x, '%'))

subplot_less5 = subset(f_wider,year_2020 <= 5) %>% ggplot(aes(x = year_1992, y = year_2020, fill = continent, label = entity,  size = count_share_2015)) +
  geom_point(alpha = 0.6, shape=21, color="black") +
  geom_point( shape=21, color="black", size = 0.1) +
  scale_size(range = c(.1, 24))+
  geom_text(vjust = 1.8 ,data = subset(f_wider,year_2020 <= 5 & (dense_rank(diff) <= 4 | dense_rank(desc(diff)) <= 5 )), size = 5)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("") +
  ylab("")+
  guides(fill = FALSE, size = FALSE) +
  theme_light()+
  scale_y_continuous(limits = c(0,2),labels = function(x) paste0(x, '%'))+
  scale_x_continuous(limits = c(0,2),labels = function(x) paste0(x, '%'))


plot_more5 + annotation_custom(ggplotGrob(subplot_less5), xmin = 10, xmax = 21, ymin = -0.5, ymax = 10)
  

  
  
# 4
forest_p <- forest_percent
forest_p$continent <- countrycode(sourcevar = forest_p$entity, origin = "country.name", destination = "continent")
forest_p %>% filter(!is.na(continent)) -> f_4
| entity %in% c('Africa', 'Asia', 
                                  'Europe', 
                                  'Oceania', 
                                  'South America', 
                                  'Northern America')) -> f_4

f_4 %>% ggplot(aes(x = year, y = forest_area, group = entity, color = continent))+
  geom_line()

