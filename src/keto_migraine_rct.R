# Test Ketorolac Data Import and Analysis
library(tidyverse)
library(tidyr)
library(synthpop)

df_s <- read_csv("data/keto_data.csv",show_col_types = FALSE)

# df_f <- df %>%
#   select(where(~ any(!is.na(.))))


# Calculate the percentage of missing values for each variable
missing_percentage <- colMeans(is.na(df_s)) * 100

# Find the columns where the missing percentage is greater than 95%
columns_to_delete <- names(df_s)[missing_percentage > 95]

# Remove the columns from the dataframe
df_f <- df_s %>%
  select(-one_of(columns_to_delete)) 

df_f <- df_f %>%
  select(id,group,site,age,sex,weight,migdur,pedmidascalc,migmeds,migmeds_1,
         pmeds_num,pmed_1,pmed_2,pmed_3,anymeds_ed,edmed_1,
         mhistnum,mhcode_1,mhcode_2,
         bspainvas,t30painvas,t60painvas,t90painvas,t120painvas,
         rxmed1,rxmed2,akathisia_prob,
         haimproved,recurrence,fupheadsev,fupnaus,fupedret,fupsatis,fupanyaes,fupmeds)

write_csv(df_f,"data/keto_data.csv")
df_locf <- df_f %>%
  rowwise() %>%
  mutate(across(bspainvas:t120painvas, ~ ifelse(is.na(.), 
        last(na.omit(c_across(bspainvas:t90painvas))), .)))


  