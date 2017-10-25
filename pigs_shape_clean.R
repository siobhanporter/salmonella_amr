library(tidyverse)

## compile data ####

y93_98 <- read.csv("C:/Users/afbi-porters/Documents/salmonella_pigs/raw_data/01_01_93_09_08_98.csv")
y98_07 <- read.csv("C:/Users/afbi-porters/Documents/salmonella_pigs/raw_data/10_08_98_31_12_07.csv")
y08_14 <- read.csv("C:/Users/afbi-porters/Documents/salmonella_pigs/raw_data/01_01_08_31_12_14.csv")
y15_17 <- read.csv("C:/Users/afbi-porters/Documents/salmonella_pigs/raw_data/01_01_15_16_06_17.csv")

pigs_sal <- rbind(y93_98, y98_07, y08_14, y15_17)

#write.csv(pigs_sal, "C:/Users/afbi-porters/Documents/salmonella_pigs/raw_data/pig_raw_all.csv")

## remove records that aren't 'isolates' but keep those w/AMR testing ####
table(pigs_sal$Isolate)

# which do not have isolate?
not_iso <- droplevels(filter(pigs_sal, Isolate != "Isolate"))

# Do any of these have AMR testing?
nots <- data.frame(table(not_iso$AntiMic))

# extract these and keep in data set
amr_T <- filter(not_iso, AntiMic %in% c("Ac", "Sp", "AcApSp", "AcSp", "ApSp", "FyS", "Sensitive")) 

# Add isolate to isolate column...
amr_T$Isolate <- "Isolate"

# remove non-isolates from pou_sal, then add amr_t (first)
iso <- droplevels(filter(pigs_sal, Isolate == "Isolate"))

all_iso <- rbind(amr_T, iso) 


## Are all records from NI? ####
table(all_iso$Vet)

ni_recs <- all_iso %>% filter(!Vet %in% c("Monaghan Vet Labs (ROI)", "Kiernan Milling (ROI)", "Sparrow D (ROI)", "Oldcastle Labs (ROI) Kavanagh NT", "Mills Vet (D Brady - ROI)"))

table(ni_recs$Vet)

## change salmonella to same nomen ####
table(ni_recs$Salmonella)

ni_recs$Salmonella <- as.character(ni_recs$Salmonella)

ni_recs$Salmonella[ni_recs$Salmonella == "S.Cholerae suis"] <- "S.Choleraesuis"
ni_recs$Salmonella[ni_recs$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium"
ni_recs$Salmonella[ni_recs$Salmonella == "S.cholerae suis"] <- "S.Choleraesuis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.virchow"] <- "S.Virchow"
ni_recs$Salmonella[ni_recs$Salmonella == "S.umbilo"] <- "S.Umbilo"
ni_recs$Salmonella[ni_recs$Salmonella == "S.typhimurium"] <- "S.Typhimurium"
ni_recs$Salmonella[ni_recs$Salmonella == "S.stanley"] <- "S.Stanley"
ni_recs$Salmonella[ni_recs$Salmonella == "S.panama"] <- "S.Panama"
ni_recs$Salmonella[ni_recs$Salmonella == "S.newport"] <- "S.Newport"
ni_recs$Salmonella[ni_recs$Salmonella == "S.newington"] <- "S.Newington"
ni_recs$Salmonella[ni_recs$Salmonella == "S.montevideo"] <- "S.Montevideo"
ni_recs$Salmonella[ni_recs$Salmonella == "S.mbandaka"] <- "S.Mbandaka"
ni_recs$Salmonella[ni_recs$Salmonella == "S.london"] <- "S.London"
ni_recs$Salmonella[ni_recs$Salmonella == "S.kentucky"] <- "S.Kentucky"
ni_recs$Salmonella[ni_recs$Salmonella == "S.kedougou"] <- "S.Kedougou"
ni_recs$Salmonella[ni_recs$Salmonella == "S.idikan"] <- "S.Idikan"
ni_recs$Salmonella[ni_recs$Salmonella == "S.infantis"] <- "S.Infantis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.grampian"] <- "S.Grampian"
ni_recs$Salmonella[ni_recs$Salmonella == "S.gold-coast"] <- "S.Gold-coast"
ni_recs$Salmonella[ni_recs$Salmonella == "S.give"] <- "S.Give"
ni_recs$Salmonella[ni_recs$Salmonella == "S.enteritidis"] <- "S.Enteritidis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.dublin"] <- "S.Dublin"
ni_recs$Salmonella[ni_recs$Salmonella == "S.derby"] <- "S.Derby"
ni_recs$Salmonella[ni_recs$Salmonella == "S.bredeney"] <- "S.Bredeney"
ni_recs$Salmonella[ni_recs$Salmonella == "S.brandenburg"] <- "S.Brandenburg"
ni_recs$Salmonella[ni_recs$Salmonella == "S.anatum"] <- "S.Anatum"
ni_recs$Salmonella[ni_recs$Salmonella == "S.agona"] <- "S.Agona"
ni_recs$Salmonella[ni_recs$Salmonella == "S.agama"] <- "S.Agama"

table(ni_recs$Salmonella)


## add ID column to observations ####
ni_recs$id <- seq.int(nrow(ni_recs))


## function to include all replicated cases ####
allDup <- function (value) {
  duplicated(value) | duplicated(value, fromLast = TRUE)
} 

## update those later determined as monophasic ####

mono <- read.csv("C:/Users/afbi-porters/Documents/salmonella_pigs/pigs_monophasic.csv")

# cases in original data later confirmed as MST
mps <- semi_join(ni_recs, mono, by = "CaseId")
table(mps$Salmonella)

# isolate these...
mps_all <- filter(mps, Salmonella %in% c("Monophasic S.Typhimurium", "S.Spp", "S.Typhimurium"))

table(mps_all$Salmonella)

# change these all to mst
mps_all$Salmonella <- "Monophasic S.Typhimurium"
table(mps_all$Salmonella)

# add to mst cases with other types of salmonella and the rest of the dataset
mps_rest <- filter(mps, !Salmonella %in% c("Monophasic S.Typhimurium", "S.Spp", "S.Typhimurium"))
table(mps_rest$Salmonella)

pigs_rest <- anti_join(ni_recs, mono, by = "CaseId")
table(pigs_rest$Salmonella)

all_sal <- rbind(pigs_rest, mps_all, mps_rest)
table(all_sal$Salmonella)

## remove non-typed salmonella ####

# data = 'all_sal'
table(all_sal$Salmonella)

sal_name <- droplevels(filter(all_sal, Salmonella != "S.Unnamed"))
sal_name1 <- droplevels(filter(sal_name, Salmonella != "S.unnamed"))
sal_name2 <- droplevels(filter(sal_name1, Salmonella != "S.Spp"))
sal_name3 <- droplevels(filter(sal_name2, Salmonella != "S.spp"))

sal_por <- sal_name3


## are there observations with same SampleID and Salmonella? - yep! ####

samps <- select(sal_por, SampleId, Salmonella)
samp_dups <- data.frame(duplicated(samps))

table(samp_dups$duplicated.samps.)

#remove these
id_dups <- cbind(sal_por, samp_dups)

dups_rem <- filter(id_dups, duplicated.samps. == 'FALSE')
table(dups_rem$duplicated.samps.)

dim(dups_rem)
sal_por <- dups_rem[ , 1:92]

str(sal_por)

## remove records with same CaseID, Tag and Salmonella ####

# remove cases w/ same CaseId, strain and Tag
tag_id <- select(sal_por, "CaseId", "Tag", "Salmonella")

tag_id_dup <- duplicated(tag_id)
table(tag_id_dup)

tag_id_dup2 <- cbind(sal_por, tag_id_dup)
table(tag_id_dup2$tag_id_dup)
str(tag_id_dup2)

pigs_clean <- droplevels(filter(tag_id_dup2, tag_id_dup == "FALSE"))

# remove false column
pigs_clean <- pigs_clean[ , 1:92]
str(pigs_clean)

## sort out records with '#' ####

# why do so many of the isolates have an animal id of '#'?? 109 records
hash <- filter(pigs_clean, AnimalId == "#")

# isolate all cases with a '#' observation # semi_join gives '#' obs and same CaseIds 
hash_recs <- semi_join(pigs_clean, hash, by = "CaseId") 

## there are '#' cases with multiple salmonella but with only one sensitivity test ('#')
test_hash <- select(hash_recs, Owner, CaseId, Salmonella)

# '#' records are fisrt, so they are assigned 'FALSE' replicates
dup_hash <- duplicated(test_hash) 
table(dup_hash)

hash_dups <- cbind(hash_recs, dup_hash)
hash_sing <- filter(hash_dups, dup_hash == "TRUE") 

# remove TRUE replicates
hash_clean <- anti_join(pigs_clean, hash_sing, by = "id") 
pig_dt <- hash_clean

## remove records from same farm at same time ####
names(pig_dt)

case_sal <- select(pig_dt, CaseId, Salmonella)

case_sal_dup <- duplicated(case_sal)
table(case_sal_dup) # 134 TRUEs

# check these records
case_dup <- cbind(pig_dt, case_sal_dup)
names(case_dup)
trues <- filter(case_dup, case_sal_dup == "TRUE")

pos_reps <- semi_join(pig_dt, trues, by = 'CaseId')

pigs_no_reps <- filter(case_dup, case_sal_dup == 'FALSE')

str(pigs_no_reps)
pig_data <- pigs_no_reps[ , 1:92]

## check for replicates from the same farm in a short time period ####
farm_check <- select(pig_data, Year, Month, CaseId, Owner, OwnerAddress, Salmonella, Tag, id)

reps <- data.frame(c(265, 1194, 1167, 1168, 1169, 1170, 1171, 1172, 1173, 1174, 1175, 1187, 1188, 1190, 1191, 1192, 1193, 164, 533, 463, 496, 497, 513, 542, 543, 21, 1092, 837, 692, 721, 698, 699, 570, 600, 661, 700, 809, 794, 892, 18, 733, 734, 778, 645, 670, 639, 641, 821, 516, 422, 631, 652, 584, 615, 632, 651, 828, 454, 461, 487, 492, 562, 634, 455, 462, 479, 488, 493, 635, 441, 433, 442, 551, 588, 783, 38, 49, 8, 6, 650, 614, 861, 875, 1155, 1162, 1179, 1099, 1100, 1123, 1125, 1139, 1104, 1057, 1197, 1137, 1120, 1138, 1116, 1117, 1118, 1119, 75, 1108, 1091, 1107, 52, 107, 1128, 1129, 764, 745, 559, 770, 567, 642, 659, 795, 741, 565, 739, 781, 715, 575, 720, 525, 738, 772, 743, 561, 653, 430, 792, 793, 785, 587, 674, 803, 671, 714, 1141, 1074, 1105, 1081, 1076, 1078, 1085, 1086, 1130, 1106, 1255, 1273))

colnames(reps) <- 'id'

pig_farm_check <- anti_join(pig_data, reps, by = "id")

## remove roi records ####

cavan <- pig_farm_check %>%
  filter(grepl("cavan", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

donegal <- pig_farm_check %>%
  filter(grepl("donegal", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

meath <- pig_farm_check %>%
  filter(grepl("meath", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

tipperary <- pig_farm_check %>%
  filter(grepl("tipperary", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

mayo <- pig_farm_check %>%
  filter(grepl("mayo", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

cork <- pig_farm_check %>%
  filter(grepl("cork", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

wexford <- pig_farm_check %>%
  filter(grepl("wexford", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

longford <- pig_farm_check %>%
  filter(grepl("longford", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

monaghan <- pig_farm_check %>%
  filter(grepl("monaghan", OwnerAddress, ignore.case = TRUE)) %>%
  droplevels

roi <- rbind(cavan, donegal, meath, tipperary, mayo, cork, wexford, longford, monaghan)

north_recs <- anti_join(pig_farm_check, roi, by = 'id')

## write out clean data ####
clean_pigs <- north_recs

write.csv(clean_pigs, "C:\\Users\\afbi-porters\\Documents\\salmonella_pigs\\pigs_clean.csv")
