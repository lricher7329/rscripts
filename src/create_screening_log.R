library(consort)
library(tidyverse)
set.seed(1001)
N <- 645

sid <- 1:N
randomized <- rep("0", N)
randomized[sample(1:N,54)] <- "1"

exc1 <- rep(NA, N)
exc1[randomized=="0"] <- sample(c("Not meeting inclusion criteria", "Declined to participate",
                                  "Other reasons"), 591, replace = TRUE, prob = c(0.9, 0.06, 0.04))

exc2 <- rep(NA, N)



# randomized[is.na(exc1)] <- "1"


df <- tibble(sid,randomized,exc1)

  
# df <- df %>%
#   mutate(randomized_yn = ifelse(!is.na(randomized), "1", "0"))


df_r <- df %>%
  filter(randomized=="1") %>%
  mutate(id=row_number())



out <- consort_plot(data = df,
                    orders = c(sid = "Assessed for eligibility",
                               exc1    = "Excluded",
                               randomized    = "Randomized"),
                    side_box = c("exc1"),
                    labels = c("1" = "Screening", "2" = "Randomization"))

plot(out)

