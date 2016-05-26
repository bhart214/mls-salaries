# Data Exploration

library(readr)
library(ggplot2)
library(plotly)
library(ggvis)
library(ggplotly)

mls = read_rds("mls_clean.rds")


mls %>% select(club, Year, position, base_salary) %>% 
  group_by(club, Year) %>% 
  summarise(med_base = median(base_salary)) %>% 
  arrange(club, med_base) %>% 
  ggvis(~Year, ~med_base) %>% 
  layer_lines(stroke = ~club) 


table(mls$club, mls$Year)

ggplot(data = mls, aes(x = (base_salary))) + geom_histogram() + facet_wrap(~Year)

mls %>% filter(Year == 2016) %>% 
  ggplot(aes(x = log(base_salary))) + geom_histogram() + facet_wrap(~club)

ggplot(data = mls, aes(x = reorder(club, base_salary, FUN = median), y = (base_salary), color=position)) + geom_boxplot()



plot = mls %>% select(Year, club, base_salary) %>% group_by(Year, club) %>% summarize(med_sal = median(base_salary)) %>% 
  ggplot(aes(x = Year, y = med_sal, group = club)) + geom_line(aes(color = club))
ggplotly(plot)

mls %>% filter(club == "TFC", Year == 2012)


ggplot(data = mls, aes(x = club, y = log(base_salary))) + geom_boxplot()


ggplot(data = mls, aes(club, position)) + 
  geom_tile(aes(fill = log(base_salary)), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ Year)



# Questions
# Which teams pay the most? least?
# which positions make the most money?
# which teams have the most variable salaries? least variable?
# how have salaries changed over time?
# what if we only look at players making less than $1 million?