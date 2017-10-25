library(tidyverse)

## pull all data together into one df
pig_D1 <- read.csv("file_path/SalmonellaResults1998.csv")
pig_D2 <- read.csv("file_path/SalmonellaResults1999.csv")
pig_D3 <- read.csv("file_path/SalmonellaResults2000.csv")
pig_D4 <- read.csv("file_path/SalmonellaResults2001.csv")
pig_D5 <- read.csv("file_path/SalmonellaResults2002.csv")
pig_D6 <- read.csv("file_path/SalmonellaResults2003.csv")
pig_D7 <- read.csv("file_path/SalmonellaResults2004.csv")
pig_D8 <- read.csv("file_path/SalmonellaResults2005.csv")
pig_D9 <- read.csv("file_path/SalmonellaResults2006.csv")
pig_D10 <- read.csv("file_path/SalmonellaResults2007.csv")

# why the f do 9 and 10 have 2 fewer columns
# need to remove 'SampleDesc' and 'Batch' from the others - eyeroll
dim(pig_D10)

d1 <- select(pig_D1, -SampleDesc, -Batch)
d2 <- select(pig_D2, -SampleDesc, -Batch)
d3 <- select(pig_D3, -SampleDesc, -Batch)
d4 <- select(pig_D4, -SampleDesc, -Batch)
d5 <- select(pig_D5, -SampleDesc, -Batch)
d6 <- select(pig_D6, -SampleDesc, -Batch)
d7 <- select(pig_D7, -SampleDesc, -Batch)
d8 <- select(pig_D8, -SampleDesc, -Batch)

all_results <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, pig_D9, pig_D10)

## take only the 'no salmonella results' ####

no_sal <- droplevels(filter(all_results, TestName == "No Salmonella isolated"))
table(no_sal$TestName) # 126070

## remove multiples of case ID #### 
names(no_sal)
cases <- select(no_sal, CaseId)

case_dups <- duplicated(cases)
table(case_dups) # 114696 true, 11374 FALSE

rem_dups <- cbind(no_sal, case_dups)
names(rem_dups)
no_sal_sing <- filter(rem_dups, case_dups == "FALSE")

#######################
### clean for pigs ####
#######################

table(no_sal_sing$Species)

# remove replicates of year and Owner - this doesn't apply to broilers!! ####
owner_dups <- select(no_sal_sing, Yr, Owner)

oy_dup <- duplicated(owner_dups)
table(oy_dup)

oyd <- cbind(no_sal_sing, oy_dup)
oyc <- filter(oyd, oy_dup == "FALSE")

names(oyc)
oycc <- oyc[ , 1:20]

# remove replicated of year and OwnerAddress ####
add_dups <- select(oycc, Yr, OwnerAddress)

ayd <- duplicated(add_dups)
table(ayd)

adyd <- cbind(oycc, ayd)
ad_nop <- filter(adyd, ayd == "FALSE")

