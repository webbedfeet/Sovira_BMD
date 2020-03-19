#' ---
#' title: Modeling as repeated measures
#' author: Abhijit
#' output: html_document
#' ---
#' 
#+ preamble, include = FALSE
# Preamble ----------------------------------------------------------------
#
library(pacman)
p_load(char=c('tidyverse','here','fs','broom', 'readxl', 'janitor',
              'ggrepel', 'DT', 'hrbrthemes', 'knitr',
              'kableExtra'))
options(knitr.kable.NA = '')
knitr::opts_chunk$set(echo=F,warning=F, error=F, fig.path = '../graphics/',
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

#' The structural model
#' 
#' The basic idea behind this project is that there is a feedback 
#' loop between both BMD and syndesmophyte levels (TSS) at year zero with both 
#' metrics in later times. Here I'm using the Bagheri version of BMD. 
#' 
library(DiagrammeR)
grViz("
     digraph model_flow {
  
  graph [overlap=true, fontsize=10, layout=dot, rankdir=LR]
  node [shape=box,fontname=Helvetica]
  A [label='BMD yr0'];B [label='TSS yr0']
  C [label='BMD yr1'];D [label='TSS yr1']
  E [label='BMD yr2'];F [label='TSS yr2']
  
   A->C A->D
   B->C B->D
   C->E C->F
   D->E D->F
} ")

#' There can of course also be cross-dependencies between the two at 
#' the same time point. 
#' To this end, we can look at patterns between data at different 
#' timepoints. 
#' 
# Descriptives ------------------------------------------------------------
#' Serial and cross correlations
#' 
#' We first look at the Spearman correlations between the data at 
#' different time points: 
# dat_long %>% select(contains('bagheri')) %>% cor(use = 'pair') %>% tidy()
# dat_long %>% select(contains('synd')) %>% cor(use='pair') %>% tidy()
dat_long %>% select(contains('bagheri')) %>% cbind(select(dat_long, contains('synd'))) %>% 
  cor(use='pair', method = 'spearman') %>% round(digits=2)

#' We find that each metric shows strong autocorrelation over time, 
#' while they have a negative correlation between them at the same time point. This can also be seen by the following figures:
#'
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
