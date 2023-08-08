# Prepare dataset
#| echo: false


set.seed(1001)
N <- 645

sid <- 1:N
included <- rep("0", N)
included[sample(1:N,54)] <- "1"

arm<- rep(NA, N)
arm[randomized=="1"] <- sample(c("keto/metoclo","plac/metoclo"),54,replace = TRUE)

exc1 <- exc1 <- rep(NA, N)
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

df_j <- left_join(df_r,data,by="id")
df <- left_join(df,df_j,by="sid") 
df <- df %>%
  select(-c(exc1.y,randomized.y)) %>%
  rename("exc1"="exc1.x",
         "randomized"="randomized.x")

rm(data,df_j,df_r)

df_r <- df %>%
  filter(randomized=="1")

write_csv(df,"data/keto_dataset.csv")