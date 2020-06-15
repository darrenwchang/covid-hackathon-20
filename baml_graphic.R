library(tidyquant)
library(tidyverse)
library(ggthemes)
library(viridis)

setwd('C:\\Users\\darre\\Documents\\_econ\\covid-hackathon-20')

baml <- tq_get('BAMLH0A0HYM2',
    get = 'economic.data',
    from = '2019-06-14') 

g1 <- ggplot(baml, aes(x = date, y = price)) +
    geom_line(color = 'blue') + 
    labs(title = 'ICE BofA US High Yield Index Option-Adjusted Spread',
        caption = 'Source: FRED\nTeam 165') +
    theme_fivethirtyeight() + 
    scale_x_date(date_labels = "%Y-%m-%d") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))

ggsave("baml.png", plot = g1, device = png())
