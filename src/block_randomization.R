library(blockrand)
library(tidyverse)

#--- specify all strata variables
redcap_data_access_group <- c(9147 : 9149)
sex <- c(1:2)
age_group <- c(1:2)
blk_sizes = c(6,8)
n = 30 # Minimum number to randomize per strata
arm = c("A","B")

#--- create strata

strata <-expand_grid(sex,age_group,redcap_data_access_group)
stratum <- unite(strata,"stratum_id",sep = "")
strata <- bind_cols(strata,stratum)
stratum_id <- strata$stratum_id

rm(stratum)

# Function to lapply to blockrand each stratum by stratum_id
rand_stratum <- function(stratum_id, n, levels, blk_sizes) {
  
  blk_sizes <- blk_sizes / 2
  
  dB <- tibble(blockrand(
    n = n, 
    num.levels = length(levels), 
    levels = levels,
    id.prefix = stratum_id, 
    block.prefix  = stratum_id,
    stratum = stratum_id,
    block.sizes = blk_sizes)
  )
  
  db <- select(dB,stratum,treatment) %>%
    rename(stratum_id=stratum) %>%
    rename(arm=treatment)
}

rl <- bind_rows(lapply(stratum_id, function(s) rand_stratum(s, n, arm,blk_sizes)))

rl <- left_join(rl,strata,by="stratum_id")

rl <- select(rl,arm,sex,age_group,redcap_data_access_group)
