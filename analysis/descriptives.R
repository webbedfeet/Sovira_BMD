library(pacman)
p_load(char=c('tidyverse','here','fs','broom', 'readxl', 'janitor',
              'ggrepel'))

dat <- read_excel(here('data','raw','bmd_longitudinal_nonames.xlsx')) %>% 
  clean_names()
ggplot(dat, aes(x = bmd_bagheri, y = total_synd, color = level))+
   geom_point() + 
  geom_smooth(se=F) +
  facet_wrap(~level)

