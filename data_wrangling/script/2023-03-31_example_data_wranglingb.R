##******************************
## Example data wrangling flow
##
## Karl Cottenie
##
## 2023-03-31
##
##******************************

##_ Packages used ----------
library(stats)
library(tidyverse)
library(viridis)
# + scale_color/fill_viridis(discrete = T/F)
theme_set(theme_light())

# Startup ends here

##_ Comment codes ------
# Coding explanations (#, often after the code, but not exclusively)
# Code organization (## XXXXX -----)
# Justification for a section of code ## XXX
# Dead end analyses because it did not work, or not pursuing this line of inquiry (but leave it in as a trace of it, to potentially solve this issue, or avoid making the same mistake in the future # (>_<) 
# Solutions/results/interpretations (#==> XXX)
# Reference to manuscript pieces, figures, results, tables, ... # (*_*)
# TODO items #TODO
# names for data frames (df_name), for lists (ls_name), for vectors (vc_name) (Thanks Jacqueline May)

##_ Data input -------

# data was generated this way
# write_csv(palmerpenguins::penguins, "../data/pengguins.csv") 

df_penguins = read_csv("../data/pengguins.csv")
df_penguins

# found their sizes on Wikipedio
df_size = read_csv("../data/size_raw.csv")
df_size

# Cleaning up column names
df_penguins = df_penguins %>% janitor::clean_names()
df_size = df_size %>% janitor::clean_names()

##_ Data Description, basic iEDA------
df_penguins %>% skimr::skim()

# Data validity - missing values at the data set level
df_penguins %>% 
  na.exclude() %>% 
  skimr::skim()

df_penguins %>% 
  filter(!complete.cases(.)) %>% 
  View()

# Univariate data validity - 0s, outliers, invalid values
df_penguins %>% dataMaid::visualize()
dev.off()

df_penguins %>% dataMaid::check()

# Bivariate data validity
df_penguins %>% GGally::ggpairs() 

# Predictive Power Score
df_penguins %>% ppsr::score_df()
df_penguins %>% ppsr::visualize_pps()

##_ Data exploration ---------

##__ Visualization ------

df_penguins %>% 
  # example of piping
  ggplot(aes(x = species, y = bill_length_mm)) + 
  # second type of piping, the + in ggplot statements
  geom_boxplot() +
  facet_grid(. ~ sex)

##__ Transformation ------

# example of filteR on Rows

df_penguins %>% 
  filter(island == "Biscoe")

df_penguins %>% 
  filter(island == "Biscoe", sex == "female")

df_penguins %>% 
  slice_sample(n = 10)

# example of seleCt on Columns

df_penguins %>% 
  select(island, sex, bill_length_mm)

df_penguins %>% 
  select(island:body_mass_g)

df_penguins %>% 
  select(contains("_mm"))

df_penguins %>% 
  select(starts_with("bill"))

# example of mutate

df_penguins %>% 
  mutate(bill_volume_mm_2 = bill_depth_mm * bill_length_mm,
         rel_bill_volume = bill_volume_mm_2/body_mass_g)

# example of summarize and group_by

df_penguins %>% 
  summarise(average_bill_length = mean(bill_length_mm, na.rm = T))

df_penguins %>% 
  group_by(sex, island) %>% 
  summarise(average_bill_length = mean(bill_length_mm, na.rm = T))
  # compare this to the figure created in line 65

df_penguins %>% 
  select(sex, island, bill_length_mm) %>% 
  group_by(sex, island) %>% 
  mutate(average_bill_length = mean(bill_length_mm, na.rm = T))

# string them all together

df_penguins %>% 
  select(species, island, bill_length_mm) %>% 
  filter(!is.na(bill_length_mm)) %>% 
  group_by(species, island) %>% 
  summarise(average_bill = mean(bill_length_mm)) %>% 
  ggplot(aes(x = species, y = average_bill)) +
  geom_col() +
  facet_wrap(~ island)

## __ Saving end result of a pipe ------

# if you want those new columns to be saved in the original df 
df_penguins = df_penguins %>% 
  # think wisely before overwriting the original df!!!!!!!!!!!!!!!!!!!!!!!
  mutate(bill_volume_mm_2 = bill_depth_mm * bill_length_mm,
         rel_bill_volume = bill_volume_mm_2/body_mass_g)

##__ Relational joins ---------

df_penguins %>% 
  left_join(df_size) %>% 
  View()

##_ Iteration -------

# __ Type 1: plotting -----

df_penguins %>% 
  ggplot(aes(y = bill_length_mm)) + 
  geom_boxplot() +
  facet_grid(island ~ sex)

# __ Type 2: group_by -------

df_penguins %>% 
  summarise(average_bill_length = mean(bill_length_mm, na.rm = T))

df_penguins %>% 
  select(sex, island, bill_length_mm) %>%
  group_by(sex, island) %>% 
  summarise(average_bill_length = mean(bill_length_mm, na.rm = T))

## __ New type: group_by, nest -------

df_penguins %>% 
  group_by(sex, island) %>% 
  nest() %>% # what, tibbles within tibbles! Amazing
  pull(data) # to quickly see each element

## ___ Option 1: work with lists -------

df_penguins %>% 
  group_by(sex, island) %>% 
  nest() %>% 
  pull(data) %>% # this becomes similar to split
  map(~ mutate(., average_bill = mean(bill_length_mm, na.rm = T))) 

# remember for piping that

df_penguins %>% 
  mutate(average_bill_length = mean(bill_length_mm, na.rm = T))

# is actually
mutate(df_penguins, average_bill_length = mean(bill_length_mm, na.rm = T))
# and thus replace the name of the df with the . inside the mapping function

# more complicated nest and map 
df_penguins %>% 
  group_by(sex, island) %>% 
  nest() %>% 
  pull(data) %>% 
  map(~ lm(bill_length_mm ~ body_mass_g, data = .)) %>% 
  map(summary)

##___ Option 2: work within the nested tibble ---------
df_penguins %>% 
  group_by(sex, island) %>% 
  nest() %>% 
  mutate(model = map(data,
                     ~ lm(bill_length_mm ~ body_mass_g, data = .))) %>% 
  # pull(data) %>% .[[1]] # to see the individual groups' data
  mutate(r_square = map_dbl(model, ~ summary(.)$r.squared))

# list vs nested tibble
# list = easy to code and check
# nested tibble = easy to work w/ corresponding columns, often identifiers
# general strategy: first pull, make code work, then convert to nested tibble


