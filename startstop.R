library(tidyverse)
library(lubridate)
library(data.table)
library(testthat)
library(glue)

folder <- 
  ifelse(Sys.info()['sysname'] == 'Darwin'
        , '/Users/ryandickerson/Documents/GitHub/IFM_Study'
        , '/home/john/Documents/IFM')

plot.dur <- function(df){
  library(RColorBrewer)  
  library(cowplot)
  cols <- brewer.pal(3, 'Dark2')
  to.plot <-   
    df[, .(Date = as.Date(dt.time[.N]),
           dur  = as.numeric(difftime(dt.time[.N], dt.time[1], unit = 'hour')))
       , .(g = (action =='stop') %>% {cumsum(.) - .})] %>%
      .[dur > 0.01] %>% 
      .[, .(dur = sum(dur)), Date] %>% 
      .[data.table(Date = seq.Date(Date[1], Date[.N], 'day'), dur = 0)
        , on = 'Date'
        , .(Date, dur = coalesce(dur, i.dur))] %>% 
      .[, cum.dur := cumsum(dur)] %>%
      .[, mean.dur := cum.dur/seq(.N)] 
  
  cumdur <- 
    to.plot %>% 
      ggplot(aes(x = Date)) +
        geom_line(aes(y = cum.dur), color = cols[1], size = 1.1) +
        theme_gray()
  
  daily <- 
    to.plot %>% 
    ggplot(aes(x = Date)) +
      geom_point(aes(y = dur), color = cols[2], size = 2) +
      geom_line(aes(y = mean.dur), color = cols[3], size = 1.1) +
      theme_gray()
  
  plot_grid(cumdur, daily, nrow = 2)
}

startstop <- function(chapter, section, action){
  file.loc <- glue('{folder}/study_record.rds')
  old <- read_rds(file.loc)
  print(old)
  expect_false(
    any(old[, action == shift(action, fill = 'stop')]),
    'Repeated Rows')
  new <- 
    data.table(
      dt.time = Sys.time()
      , chapter = chapter
      , section = section
      , action = action) %>% 
      .[, time := format(dt.time, '%I:%M %p')]
    
  df <- rbind(old, new)
  print(df)
  overwrite <- readline('overwrite? [y/n]:') %>% 
    toupper %>% 
    substr(1, 1) %>%
    `==`('Y')
  if(overwrite) write_rds(df, file.loc)
  if(df[.N, action] == 'stop')
    print(df[, round(diff(dt.time[.N - 1:0]), 1)])
}

Start <- function(chapter, section)
 startstop(chapter = chapter, section = section, action = 'start')

Stop <- function(chapter, section)
 startstop(chapter = chapter, section = section, action = 'stop')

df <- 
startstop(
  chapter = readline('Chapter:') %>% as.numeric
  , section = readline('Section:') %>% as.numeric
  , action = 
      readline('Start or Stop?:') %>% 
        tolower %>% 
        {if(!. %in% c('start', 'stop')) 
           stop(paste0('wtf does "', ., '" mean?')) 
         else .}
)

plot.dur(read_rds(glue('{folder}/study_record.rds')))
