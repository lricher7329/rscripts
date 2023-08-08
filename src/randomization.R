
library(tidyverse)
library(randomizr)
library(estimatr)
library(ri2)
library(blockTools)
library(foreign)
library(psych)
library(lubridate)
library(ggplot2)


options(scipen=100, digits=12) # Removes exponential digits from the output
options(max.print = 500000) # allows the output to produces 500,000 lines





# Loading CSV file ####

D<-read.csv("data/Blocked Randomization_Data.csv", # Make sure you appropriately change the location
            header = T, # Tells r the rist row is the variable names
            sep = ",") # Tells r that the seperating character in the dataset is a ,

names(D) # Check if the variable names are correct
describe(D) # quick descriptives; * identifies string variables
view(D) # allows the user to view the data to make sure everything looks appropriate in the dataset

## Creating tables for Key contstructs

table(D$agecat, useNA = "always")
table(D$gender, useNA = "always")
table(D$White_nonWhite, useNA = "always")
table(D$risklevel, useNA = "always")

D$NW[D$White_nonWhite == 1]<-"White"
D$NW[D$White_nonWhite == 0]<-"Non White"
table(D$NW)

## Randomly Selecting "Consented" Participants ####

set.seed(1992)
D<-sample_n(D, replace=F, size=42)


# Block randomization Example 1: 7 strata of 6 participants ####
## Key Variable Matrix ####

numeric_mat_c <- model.matrix( ~ as.factor(NW)+ # Race: Listed as factor because it is a string variable
                                 as.factor(gender)+ # Risk Level: Listed as factor because it is a string variable
                                 as.factor(risklevel)+ # CrimeType: Listed as factor because it is a string variable
                                 as.factor(agecat), # Age is the only numeric variable in the dataframe
                               data = D)[,-1] # D = our dataframe # - 1 removes the intercept variable from the formula


### NOTE: we don't want to block on all variables in the dataframe because it could bias our randomization process


## BlockTools also requres an id variable ####
names(D) # Lists the names of all of the variables in dataframe D


df_forBT_c <- data.frame(offenderid = D$offenderid, numeric_mat_c) # Incorporates the ID number (oid) into a new 
# dataframe with the matrix previously specified



## Specifying the number of strata based on sample size (7 Strata) ####
n_strata_c <- floor(42 / 7) 

## Conducting the sampling: strata of 6 ####
out_c <- block(df_forBT_c, 
               n.tr = n_strata_c, 
               id.vars = "offenderid", 
               block.vars = names(df_forBT_c)[-1])


## Randomly Selecting 3 cases from each strata of 6 ####

my_match_c <-
  D %>%
  mutate(
    # Extact the block_ids
    strata_id_t = createBlockIDs(out_c, df_forBT_c, id.var = "offenderid"),
    # Conduct actual random assignment with block_ra()
    S_stratified_t = strata_rs(strata = strata_id_t, n = 3)
  )

## Exploring how well the blocking procedure worked for each variable ####


### Race

with(my_match_c, table(NW, strata_id_t))

ggplot(aes(x =NW, fill=as.factor(strata_id_t)), data = my_match_c)+
  geom_histogram(stat="count",position="dodge")+scale_y_continuous(breaks=c(0:20))+
  theme(line = element_line(colour = "black", size = 1), axis.line = element_line(colour = "black"), panel.border= element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text=element_text(size=24, color = "Black"), text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

### Risk Level

with(my_match_c, table(risklevel, strata_id_t))

my_match_c$risklevel <- factor(my_match_c$risklevel, levels=c("Low","Moderate","High", "Very High"))


ggplot(aes(x =risklevel, fill=as.factor(strata_id_t)), data = my_match_c)+
  geom_histogram(stat="count",position="dodge")+scale_y_continuous(breaks=c(0:8))+
  theme(line = element_line(colour = "black", size = 1), axis.line = element_line(colour = "black"), panel.border= element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text=element_text(size=24, color = "Black"), text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

### Age Category

with(my_match_c, table(agecat, strata_id_t))

ggplot(aes(x =as.factor(agecat), fill=as.factor(strata_id_t)), data = my_match_c)+
  geom_histogram(stat = "count", position="dodge")+scale_y_continuous(breaks=c(0:7))+scale_x_discrete(breaks=c(20, 30, 40, 50, 60))+
  theme(line = element_line(colour = "black", size = 1), axis.line = element_line(colour = "black"), panel.border= element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text=element_text(size=24, color = "Black"), text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

### Gender

with(my_match_c, table(gender, strata_id_t))

ggplot(aes(x =gender, fill=as.factor(strata_id_t)), data = my_match_c)+
  geom_histogram(stat = "count", position="dodge")+scale_y_continuous(breaks=c(0:20))+
  theme(line = element_line(colour = "black", size = 1), axis.line = element_line(colour = "black"), panel.border= element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text=element_text(size=24, color = "Black"), text=element_text(size=24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

