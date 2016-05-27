# Data Exploration

library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(ggvis)
library(ggplotly)

mls = read_rds("mls_clean.rds")

# Total base salaries by club by year
mls %>% filter(Year %in% c(2008:2016)) %>% group_by(club, Year) %>% summarize(total = sum(base_salary)) %>% 
  ggplot(aes(x = club, y = total, fill = club)) + geom_bar(stat = "identity") + facet_wrap(~Year)

# Total base salaries by club in 2010
mls %>% filter(Year == 2010) %>% group_by(Year, club) %>% summarize(total = sum(guaranteed_compensation)) %>% arrange(total)

# Median base salary by club each year
mls %>% select(club, Year, position, base_salary) %>% 
  group_by(club, Year) %>% 
  summarise(med_base = median(base_salary)) %>% 
  arrange(club, med_base) %>% 
  ggvis(~Year, ~med_base) %>% 
  layer_lines(stroke = ~club) 

# Histogram of log(base_salary) by year
ggplot(data = mls, aes(x = log(base_salary))) + geom_histogram() + facet_wrap(~Year)

# Histogram of log(base_salary) by club in 2016
mls %>% filter(Year == 2016) %>% 
  ggplot(aes(x = log(base_salary))) + geom_histogram() + facet_wrap(~club)



# Salary By Position
mls$position <- factor(mls$position, levels = c("GK","D", "M", "F"), ordered = TRUE)

ggplot(data = mls, aes(x = reorder(club, base_salary, FUN = median), y = (base_salary), color=position)) + geom_boxplot()

ggplot(data = mls, aes(x = position, y = log(base_salary), color = position)) + 
  geom_boxplot() + geom_point(position = "jitter", alpha = 0.15) + 
  facet_grid(.~Year) +
  labs(x = "Position", y = "log(Base Salary)") + ggtitle("MLS Salaries by Position (2007-2016)") +
  theme(panel.background = element_rect(fill = "grey97"), legend.title=element_blank())


# ggplotly interactive of median salary for each club by year
plot = mls %>% select(Year, club, base_salary) %>% group_by(Year, club) %>% summarize(med_sal = median(base_salary)) %>% 
  ggplot(aes(x = Year, y = med_sal, group = club)) + geom_line(aes(color = club))
ggplotly(plot)


# Boxplot of salary for each club (all years)
ggplot(data = mls, aes(x = club, y = log(base_salary))) + geom_boxplot()

# Heatmaps of salary by position by club
ggplot(data = mls, aes(club, position)) + 
  geom_tile(aes(fill = log(base_salary)), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")

ggplot(data = mls, aes(club, position)) + 
  geom_tile(aes(fill = log(base_salary)), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ Year)

# Top 50 guaranteed compensations 2007-2016
mls %>% top_n(n = 50, guaranteed_compensation) %>% arrange(desc(guaranteed_compensation)) %>% print(n=50)


# stats: median, mean, range, sd
# numeric vars: base salary, guaranteed salary
# categorical variables: club, position, year

# Questions
# Which teams pay the most? least?
# which positions make the most money?
# which teams have the most variable salaries? least variable?
# how have salaries changed over time?
# what if we only look at players making less than $1 million?
# which players had the greatest changes in salary?
