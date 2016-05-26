library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

file_list = list.files(path = "csvs/.", pattern = ".csv", all.files = TRUE, full.names = FALSE)
df_list = list()

setwd("csvs")
year = 2007
for (file in file_list) {
  #assign(paste0("mls", year), read_csv(file) %>% mutate(Year = year))
  df_list[[file]] = assign(paste0("mls", year), read_csv(file) %>% mutate(Year = year))
  year = year + 1
}
setwd("C:/Users/bhart/Documents/R_Projects/mls-salaries")


mls = bind_rows(df_list)
write_rds(mls, "mls.rds")
