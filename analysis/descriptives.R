#' ---
#' title: Describing the data
#' author: Abhijit 
#' date: "`r format(Sys.Date(), '%B %d, %Y')`"
#' output: 
#'   html_document:
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#'     theme: journal
#'     highlight: zenburn
#'     fig_height: 4
#'     fig_width: 6
#'     fig_caption: true
#'
#' ---
#' 
# Preamble ----------------------------------------------------------------
#+ preamble, include = FALSE
#
library(pacman)
p_load(char=c('tidyverse','here','fs','broom', 'readxl', 'janitor',
              'ggrepel', 'DT', 'hrbrthemes', 'knitr',
              'kableExtra'))
options(knitr.kable.NA = '')
knitr::opts_chunk$set(warning=F, error=F, fig.path = '../graphics/',
                      dev = c('png','pdf'), cache=T)
dat <- read_excel(here('data','raw','bmd_longitudinal_nonames.xlsx')) %>% 
  clean_names() %>% 
  mutate(male = factor(ifelse(male == 0, 'Female','Male')),
         race= fct_relevel(as.factor(race), 
                           'white','black','asian'),
         level = fct_relevel(as.factor(level),
                             'T11','T12'),
         bridging = factor(ifelse(bridging==0, 'No','Yes')),
         year_labeled = paste('Year', year))



dat2 <- dat %>% filter(level != 'L4', year <= 2)
theme_set(ggthemes::theme_clean())

#' # Data

# Data --------------------------------------------------------------------
#'
#' In this document we're limiting the data to the first 2 years and omitting
#' the L4 vertebra since there are very few data in these groups
#' 
#+ data

DT::datatable(dat)
#' # Univariate
#' 
#' ## Distribution of age
#+ age
ggplot(dat2 %>% filter(year==0), aes(x = age_year_one)) + 
  geom_histogram(bins=50) +
  scale_y_continuous(breaks = seq(0,10,by=2))+
  labs(x = 'Age', y = 'Frequency')

#' ## Distribution of gender
#+ gender
ggplot(dat2 %>% filter(year == 0), aes(x = male))+
  geom_bar(aes(y = ..prop.., group=1)) + 
  scale_y_percent('', limits = c(0,1))+
  labs(x = 'Gender')

#' ## Syndesmophyte score by year
#+ tss
ggplot(dat2, aes(x = total_synd))+geom_histogram(bins=30)+
  facet_grid(year_labeled~level) + 
  labs(x = 'Total syndesmophyte score',
       y = 'Frequency')

#' ##  BMD (Bagheri) by year
#+ bmd_bagheri
ggplot(dat2, aes(x = bmd_bagheri))+
  geom_histogram(bins = 15)+
  facet_grid(year_labeled~level)+
  labs(x = 'BMD (Bagheri)',
       y = 'Frequency') 

#' ## BMD (Tan) by year
#'
#+ bmd_tan
ggplot(dat2, aes(x = bmd_sovira))+
  geom_histogram(bins = 15)+
  facet_grid(year_labeled~level)+
  labs(x = 'BMD (Bagheri)',
       y = 'Frequency') 


#' # Bridging transitions
#' The following plot looks at transitions in bridging status across 
#' years. **This graph covers all the vertebrae.** 
#' 
#' The way to read this plot is to look at the "barbells". The dark
#' dots represent when `bridging = 1` for that year, and the light dots 
#' represent when `bridging = 0` for that year. The cases where bridging
#' is 0 for all years is not shown. The "Set Size" graph shows, for each 
#' year, the number of observations where `bridging = 1`. 
#' The "Intersection Size" shows the frequency of each of the combinations of
#' bridging status across years
#' 
#' This graph shows that there is only two observations (an observation is data from a vertebra for a person over the 3 years) where bridging transitioned from 0 to 1 between years; once between years 1 and 2 and once between years 0 and 1. So, by and large, bridging status does not change over the years for any person for any vertebra.
#+ transitions, fig.cap = '**Figure:** Frequency of bridging transitions'
library(UpSetR)
bl <- dat2 %>% select(pat_id, level, bridging, year_labeled) %>% 
  mutate(bridging = ifelse(bridging=='Yes',1,0)) %>% 
  spread(year_labeled, bridging) %>% 
  filter(complete.cases(.))
upset(as.data.frame(bl))

#' 
#' Even for cases where data from a year is missing, there is no 
#' evidence of bridging transitions
#' 
dat2 %>% select(pat_id, level, bridging, year_labeled) %>% 
  mutate(bridging = ifelse(bridging=='Yes',1,0)) %>% 
  spread(year_labeled, bridging) %>% 
  filter(!complete.cases(.)) %>% 
  knitr::kable(col.names = c('Patient','Vertebra', paste('Year', 0:2)),
               align = 'c') %>% 
  kable_styling(full_width = F)


#' # Marginal relationship between BMD and total syndesmophyte score
#' 
#' This plot is marginal in the sense that it includes data for all persons for all years. 
#+ bmd_tss, eval = FALSE
ggplot(dat2, aes(x = bmd_bagheri, y = total_synd, color = level))+
   geom_point() + 
  geom_smooth(se=F) +
  facet_wrap(~level) + 
  labs(x = 'BMD (Bagheri)',
       y = 'Total syndesmophyte score', 
       color = 'Vertebra')

ggplot(dat2, aes(x = level, y = bmd_bagheri))+
  geom_violin()+
  facet_grid(year_labeled~.)+coord_flip()
ggplot(dat, aes(x = year, y = bmd_bagheri, group = pat_id)) + 
  geom_line() + 
  facet_wrap(~level)

dat %>% filter(level != 'L4', year != 4) %>% 
  ggplot(aes(x = year, y = bmd_bagheri))+
  geom_line(aes(group = pat_id), alpha = 0.3) + 
  geom_smooth(aes(color = as.factor(male)), se=F, show.legend=T, size = 2)+
  scale_x_continuous(breaks = 0:2)+
  facet_wrap(~level) +
  # facet_grid(level~.)+
  labs(x = 'Year', y = bquote('BMD (gm/'~cm^2~')'), color = 'Gender')+
  theme_classic() + 
  theme(legend.position = 'bottom')

dat2 %>% 
  ggplot(aes(x = year, y = total_synd))+
  geom_line(aes(group = pat_id), alpha = 0.3) + 
  geom_smooth(aes(color = as.factor(male)), se=F, show.legend=T, size = 1.5)+
  scale_x_continuous(breaks = 0:2)+
  facet_wrap(~level) +
  # facet_grid(level~.)+
  labs(x = 'Year', y = 'Total syndesmophyte score', color = 'Gender')+
  theme_classic() + 
  theme(legend.position = 'bottom')

ggplot(dat %>% filter(level != 'L4', year <=2), 
       aes(x = level, fill = bridging))+
  geom_bar(position = 'fill') + 
  facet_grid(factor(year)~.)+
  scale_fill_manual(values = c('No' = 'skyblue', 'Yes' = 'orange')) + 
  labs(x = 'Vertebra', y = 'Proportion', fill='Bridging') + 
  ggthemes::theme_clean() + 
  theme(legend.position = 'bottom')+
  labs(title = "Proportion of vertebra with bridging", 
       subtitle = 'By year')  

ggplot(dat %>% filter(level != 'L4', year <= 2) %>% 
         mutate(high_bridge = ifelse(total_synd > 108, 'Yes','No')),
       aes(x = level, fill = high_bridge))+
  geom_bar(position='fill') + 
  scale_fill_manual(values = c('No'='skyblue', 'Yes' = 'orange')) + 
  facet_grid(factor(year)~.) + 
  labs(x = 'Vertebra', y = 'Proportion', fill = ' > 75% bridged') + 
  ggthemes::theme_clean() + 
  theme(legend.position = 'bottom')+
  labs(title = 'Proportion with greater than 75% bridging', 
       subtitle = 'By year')

ggpot(dat2, aes())