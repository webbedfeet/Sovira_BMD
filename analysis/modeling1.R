#' ---
#' title: Modeling as repeated measures
#' author: Abhijit
#' output: html_document
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



# Converting to longitudinal data for time-based correlations -------------

Dt <- dat2 %>% group_by(year)
Dt %>% group_keys()
Dt2 <- Dt %>% group_split()
names(Dt2) <- Dt %>% group_keys() %>% pull(year)
for(i in 1:3){
  Dt2[[i]] <- Dt2[[i]] %>% 
    select(-year, -year_labeled) %>% 
    rename_at(vars(bmd_sovira:total_synd),
              ~paste(., paste0('y',i-1), sep='_'))
}
dat_long <- Reduce(left_join, Dt2)


# Descriptives ------------------------------------------------------------

dat_long %>% select(contains('bagheri')) %>% cor(use = 'pair')
dat_long %>% select(contains('synd')) %>% cor(use='pair')
dat_long %>% select(contains('bagheri')) %>% cbind(select(dat_long, contains('synd'))) %>% 
  cor(use='pair', method = 'spearman')
# Graphs ------------------------------------------------------------------

ggplot(dat_long, aes(x = bmd_bagheri_y0, y = bmd_bagheri_y1))+geom_point() +
  geom_smooth(se=F) + geom_abline()
              
ggplot(dat_long, aes(x = bmd_bagheri_y0, y = bmd_bagheri_y2))+geom_point() +
  geom_smooth(se=F) + geom_abline()

ggplot(dat_long, aes(x = total_synd_y0, y = total_synd_y1))+geom_point() +
  geom_smooth(se=F) + geom_abline()

ggplot(dat_long, aes(x = total_synd_y0, y = total_synd_y2))+geom_point() +
  geom_smooth(se=F) + geom_abline()

ggplot(dat_long, aes(x = bmd_bagheri_y0, y = total_synd_y2))+geom_point()+
  geom_smooth(se=F)

ggplot(dat_long, aes(x = bmd_bagheri_y0, y = extent_bridged_y1)) + geom_point()+
  geom_smooth(se=F)

ggplot(dat_long, aes(x = extent_bridged_y1, y = total_synd_y1))+geom_point() + 
  geom_smooth(se=F) + geom_abline()
