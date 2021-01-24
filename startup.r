## let's scan all the cover pages for each series of interest

rm(list=ls())
options(scipen=100,stringsAsFactors=F, warn = 2)
SQLPATH='c:/temp/sqlscript/'
MISCPATH='c:/research/misc/'

library(dplyr)
source('c:/research/general_funct.r')
options(dplyr.print_max = 1e9, dplyr.width=Inf)

AHKPATH = 'c:/research/utils/autohotkeys/'
source(paste0(AHKPATH, 'ahkfunct.r'))
USERPATH = 'c:/git/seriespace/'
HTMLPATH = 'd:/miscracingdata/html/seriespacecomparison/'
thiscomputer = Sys.info()[['nodename']]
TEMPPATH='c:/temp/'

source(paste0(USERPATH, 'series-pace-funct.r'))
