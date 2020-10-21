library(tidyverse)
library(magrittr)

# load dados
load('dados/processados/da_incos.rda')

# filter down sample identifiers
sample <- select(da_incos, id, rowid, file_json)

# rearrange data to load movs
files <- sample %>%
  arrange(file_json, rowid) %>%
  group_by(file_json) %>%
  distinct(rowid) %>%
  summarize(rows = list(rowid))

files[200, 'rows'] %>%  unlist()
