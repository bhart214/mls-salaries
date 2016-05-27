# Investigate and Clean Data

library(readr)


mls = read_rds("mls.rds")

str(mls)
summary(mls)

# Create new variable for difference between guaranteed_compensation and base_salary
mls = mls %>% mutate(diff = guaranteed_compensation - base_salary)

summary(mls)

# Missing Data - 35 records with missing values, but only one missing salary.
mls[complete.cases(mls) == FALSE,]
mls = mls[is.na(mls$base_salary) == FALSE, ]


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
# This is definitely an oversimplification, but I will convert all positions to GK, D, M, or F and
# will not allow any combinations.  The first position listed is the default.
unique(mls$position)
mls$position[mls$position == "MF" | mls$position == "M-D" | mls$position == "M-F" | mls$position == "M/F" | mls$position == "M/D"] = "M"
mls$position[mls$position == "D/M" | mls$position == "D-F" | mls$position == "D/F" | mls$position == "D-M"] = "D"
mls$position[mls$position == "F/M" | mls$position == "F-D" | mls$position == "F-M"] = "F"
mls = mls[is.na(mls$position) == FALSE, ]



# Remove records where there were not full rosters.
# For example, ATL only has 3 players on the payroll.  This could skew the stats.
table(mls$club, mls$Year)

df_list = list()
for (i in 2007:2016) {
  mls_sub = subset(mls, Year == i)
  tab = table(mls_sub$club)
  mls_sub = mls_sub[mls_sub$club %in% names(tab)[tab > 20],]
  df_list[[i]] = mls_sub
}
mls = bind_rows(df_list)

table(mls$club, mls$Year)

write_rds(mls, "mls_clean.rds")

