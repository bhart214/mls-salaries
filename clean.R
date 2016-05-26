# Investigate and Clean Data

library(readr)


mls = read_rds("mls.rds")

str(mls)
summary(mls)

# Change Year to factor variable
mls$Year = as.factor(mls$Year)

# Create new variable for difference between guaranteed_compensation and base_salary
mls = mls %>% mutate(diff = guaranteed_compensation - base_salary)

summary(mls)

# Missing Data
mls[complete.cases(mls) == FALSE,]

# Club Names
unique(mls$club)
mls[mls == "SEA I"] = "SEA"
mls[mls == "TFC I"] = "TFC"
mls[mls == "DAL I"] = "DAL"
mls[mls == "CLB I"] = "CLB"
mls[mls == "PHI J"] = "PHI"
mls[mls == "DC J"] = "DC"
mls[mls == "NE J"] = "NE"
mls[mls == "SEA J"] = "SEA"
mls[mls == "SJ J"] = "SJ"
mls[mls == "CHV J"] = "CHV"
mls[mls == "KC J"] = "KC"
mls[mls == "CHI J"] = "CHI"
mls[mls == "DAL J"] = "DAL"
mls[mls == "RSL J"] = "RSL"
mls[mls == "LA J"] = "LA"
mls[mls == "TFC J"] = "TFC"
mls[mls == "COL J"] = "COL"
mls[mls == "NY"] = "NYRB"
mls[mls == "Pool"] = "POOL"

# Filter out NA, None, and POOL from club column
mls = mls %>% filter(!grepl("NA|None|POOL", club))

# Positions
unique(mls$position)
mls$position[mls$position == "MF"] = "M-F"
mls$position[mls$position == "M-D"] = "D-M"
mls$position[mls$position == "F-M"] = "M-F"
mls$position[mls$position == "M/F"] = "M-F"
mls$position[mls$position == "M/D"] = "D-M"
mls$position[mls$position == "D/M"] = "D-M"
mls$position[mls$position == "D/F"] = "D-F"
mls$position[mls$position == "F/M"] = "M-F"
mls$position[mls$position == "F-D"] = "D-F"


write_rds(mls, "mls_clean.rds")

