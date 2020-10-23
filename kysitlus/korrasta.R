#? Created on 2020-10-21 19:04:04 by Jüri Lillemets with R version 4.0.3 (2020-10-10).
#? This script ...

# Set options
#options(device = 'X11')

# Set working directory
setwd('/home/jrl/work/maakasutus/kysitlus')

# Load packages
library('magrittr');library('plotly');library('htmlwidgets');library('openxlsx')


# Read and save (2020-10-23 08:41:07) ----------

## Read and tidy data
Alg <- read.csv('vastused.csv', check.names = F)
Alg <- Alg[, !grepl('Mineraalväetiste', names(Alg))]
write.csv(Alg, 'vastused.csv', row.names = F)

## Set elements and topics
Tulbad <- lapply(c('Skaala.1', 'Skaala.2'), function(x) grep(x, names(Alg)))
names(Tulbad) <- c('mõju', 'määramatus')
Nimed <- read.csv('vastused.csv', head = F) %>% .[1, Tulbad[[1]]]
Mõjurid <- sub('.*\\[(.*)\\]\\[.*', '\\1', Nimed) %>% 
  #lapply(strwrap, 50) %>% sapply(paste,  collapse = '\n')
  substr(1, 40) %>% paste0(ifelse(nchar(.) == 40, '...', ''))
Teemad <- sub('^.*?\\s(.*)\\s\\[.*$', '\\1', Nimed)

Alg[, unlist(Tulbad)] %<>% lapply(function(x) substring(x, 2) %>% as.numeric %>% `-`(3))

## Create and save a .xlsx workbook
Töövihik <- cbind(
  data.frame(Teemad, Mõjurid), 
  data.frame(Alg[Tulbad$mõju] %>% t, 
             Alg[Tulbad$mõju] %>% colMeans(na.rm = T), 
             Alg[Tulbad$määramatus] %>% t, 
             Alg[Tulbad$määramatus] %>% colMeans(na.rm = T)))
names(Töövihik) <- c('Teema', 'Mõjur', Alg$Perenimi, 'Keskmine mõju', Alg$Perenimi, 'Keskmine määramatus')
write.xlsx(Töövihik, 'hinnangud.xlsx', row.names = F)

## Save .RData
save.image('vastused.Rda')