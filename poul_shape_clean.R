library(tidyverse)

# compile data ####

y93_98 <- read.csv("file_path/01_01_93_09_08_98.csv")
y98_07 <- read.csv("file_path/10_08_98_31_12_07.csv")
y08_14 <- read.csv("file_path/01_01_08_31_12_14.csv")
y15_17 <- read.csv("file_path/01_01_15_13_06_17.csv")

names(y93_98)
names(y98_07)
names(y08_14)
names(y15_17)

poul_sal <- rbind(y93_98, y98_07, y08_14, y15_17)

#write.csv(poul_sal, "file_path/raw_all.csv")

## remove records that aren't 'isolates' but keep those w/AMR testing ####
table(poul_sal$Isolate)

# which do not have isolate?
not_iso <- droplevels(filter(poul_sal, Isolate != "Isolate"))

nots <- data.frame(table(not_iso$AntiMic))

# Do any of these have AMR testing?
amr_T <- filter(not_iso, AntiMic %in% c("Ac", "AcSp", "Ap", "ApSp", "Fy", "Sp", "Sensitive")) 

# Add isolate to isolate column...
amr_T$Isolate <- "Isolate"

# remove non-isolates from pou_sal, then add amr_t (first)
iso <- droplevels(filter(poul_sal, Isolate == "Isolate"))

all_iso <- rbind(amr_T, iso) 

#! will have to remove duplicate CaseId and Salmonella...

## remove records from Monaghan ####
table(all_iso$Vet)
ni_recs <- filter(all_iso, Vet != "*vet_name*")

## unify Salmonella names ####
table(ni_recs$Salmonella)
str(ni_recs)

ni_recs$Salmonella <- as.character(ni_recs$Salmonella)

ni_recs$Salmonella[ni_recs$Salmonella == "S.dublin"] <- "S.Dublin"
ni_recs$Salmonella[ni_recs$Salmonella == "S.agona"] <- "S.Agona"
ni_recs$Salmonella[ni_recs$Salmonella == "S.anatum"] <- "S.Anatum"
ni_recs$Salmonella[ni_recs$Salmonella == "s.binza"] <- "S.Binza"
ni_recs$Salmonella[ni_recs$Salmonella == "S.blockley"] <- "S.Blockley"
ni_recs$Salmonella[ni_recs$Salmonella == "S.braenderup"] <- "S.Braenderup"
ni_recs$Salmonella[ni_recs$Salmonella == "S.brandenburg"] <- "S.Brandenburg"
ni_recs$Salmonella[ni_recs$Salmonella == "S.bredeney"] <- "S.Bredeney"
ni_recs$Salmonella[ni_recs$Salmonella == "S.chandans"] <- "S.Chandans"
ni_recs$Salmonella[ni_recs$Salmonella == "S.colorado"] <- "S.Colorado"
ni_recs$Salmonella[ni_recs$Salmonella == "S.derby"] <- "S.Derby"
ni_recs$Salmonella[ni_recs$Salmonella == "S.enteritidis"] <- "S.enteritidis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.essen"] <- "S.Essen"
ni_recs$Salmonella[ni_recs$Salmonella == "S.give"] <- "S.Give"
ni_recs$Salmonella[ni_recs$Salmonella == "S.hadar"] <- "S.Hadar"
ni_recs$Salmonella[ni_recs$Salmonella == "S.halmstad"] <- "S.Halmstad"
ni_recs$Salmonella[ni_recs$Salmonella == "S.heidelberg"] <- "S.eidelberg"
ni_recs$Salmonella[ni_recs$Salmonella == "S.indiana"] <- "S.Indiana"
ni_recs$Salmonella[ni_recs$Salmonella == "S.infantis"] <- "S.Infantis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.java"] <- "S.Java"
ni_recs$Salmonella[ni_recs$Salmonella == "S.kaapstad"] <- "S.Kaapstad"
ni_recs$Salmonella[ni_recs$Salmonella == "S.kentucky"] <- "S.Kentucky"
ni_recs$Salmonella[ni_recs$Salmonella == "S.kilwa"] <- "S.Kilwa"
ni_recs$Salmonella[ni_recs$Salmonella == "S.larochelle"] <- "S.Larochelle"
ni_recs$Salmonella[ni_recs$Salmonella == "S.livingstone"] <- "S.Livingstone"
ni_recs$Salmonella[ni_recs$Salmonella == "S.london"] <- "S.London"
ni_recs$Salmonella[ni_recs$Salmonella == "S.makiso"] <- "S.Makiso"
ni_recs$Salmonella[ni_recs$Salmonella == "S.manhattan"] <- "S.Manhattan"
ni_recs$Salmonella[ni_recs$Salmonella == "S.mbandaka"] <- "S.Mbandaka"
ni_recs$Salmonella[ni_recs$Salmonella == "S.montevideo"] <- "S.Montevideo"
ni_recs$Salmonella[ni_recs$Salmonella == "S.muenchen"] <- "S.Muenchen"
ni_recs$Salmonella[ni_recs$Salmonella == "S.new-haw"] <- "S.New-haw"
ni_recs$Salmonella[ni_recs$Salmonella == "S.newington"] <- "S.Newington"
ni_recs$Salmonella[ni_recs$Salmonella == "S.newport"] <- "S.Newport"
ni_recs$Salmonella[ni_recs$Salmonella == "S.ohio"] <- "S.Ohio"
ni_recs$Salmonella[ni_recs$Salmonella == "S.oranienburg"] <- "S.Oranienburg"
ni_recs$Salmonella[ni_recs$Salmonella == "S.orion"] <- "S.Orion"
ni_recs$Salmonella[ni_recs$Salmonella == "S.saint-paul"] <- "S.Saint-paul"
ni_recs$Salmonella[ni_recs$Salmonella == "S.san-diego"] <- "S.San-diego"
ni_recs$Salmonella[ni_recs$Salmonella == "S.sarajane"] <- "S.Sarajane"
ni_recs$Salmonella[ni_recs$Salmonella == "S.schwarzengrund"] <- "S.Schwarzengrund"
ni_recs$Salmonella[ni_recs$Salmonella == "S.senftenberg"] <- "S.Senftenberg"
ni_recs$Salmonella[ni_recs$Salmonella == "S.sofia"] <- "S.Sofia"
ni_recs$Salmonella[ni_recs$Salmonella == "S.tennessee"] <- "S.Tennessee"
ni_recs$Salmonella[ni_recs$Salmonella == "S.thompson"] <- "S.Thompson"
ni_recs$Salmonella[ni_recs$Salmonella == "S.typhimurium"] <- "S.Typhimurium"
ni_recs$Salmonella[ni_recs$Salmonella == "S.virchow"] <- "S.Virchow"
ni_recs$Salmonella[ni_recs$Salmonella == "S.aberdeen"] <- "S.Aberdeen"
ni_recs$Salmonella[ni_recs$Salmonella == "S.arizonae"] <- "S.Arizonae"
ni_recs$Salmonella[ni_recs$Salmonella == "S.gold-coast"] <- "S.Goldcoast"
ni_recs$Salmonella[ni_recs$Salmonella == "S.Goldcoast"] <- "S.Gold-coast"
ni_recs$Salmonella[ni_recs$Salmonella == "S.vancouver"] <- "S.Vancouver"
ni_recs$Salmonella[ni_recs$Salmonella == "S.urbana"] <- "S.Urbana"
ni_recs$Salmonella[ni_recs$Salmonella == "S.singapore"] <- "S.Singapore"
ni_recs$Salmonella[ni_recs$Salmonella == "S.ruiru"] <- "S.Ruiru"
ni_recs$Salmonella[ni_recs$Salmonella == "S.riggel"] <- "S.Riggel"
ni_recs$Salmonella[ni_recs$Salmonella == "S.poona"] <- "S.Poona"
ni_recs$Salmonella[ni_recs$Salmonella == "S.manila"] <- "S.Manila"
ni_recs$Salmonella[ni_recs$Salmonella == "S.lomita"] <- "S.Lomita"
ni_recs$Salmonella[ni_recs$Salmonella == "S.lamberhurst"] <- "S.Lamberhurst"
ni_recs$Salmonella[ni_recs$Salmonella == "S.lexington"] <- "S.Lexington"
ni_recs$Salmonella[ni_recs$Salmonella == "S.bareilly"] <- "S.Bareilly"
ni_recs$Salmonella[ni_recs$Salmonella == "S.bareilly"] <- "S.Bareilly"
ni_recs$Salmonella[ni_recs$Salmonella == "S.enteritidis"] <- "S.Enteritidis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.enteritidis"] <- "S.Enteritidis"
ni_recs$Salmonella[ni_recs$Salmonella == "S.binza"] <- "S.Binza"
ni_recs$Salmonella[ni_recs$Salmonella == "S.eidelberg"] <- "S.Eidelberg"


## add ID column to observations ####
ni_recs$ID <- seq.int(nrow(ni_recs))


## function to include all replicated cases ####
allDup <- function (value) {
  duplicated(value) | duplicated(value, fromLast = TRUE)
} 

## post-hoc Id of monophasis Sal Typh ####

# CaseId where MST has been determined after testing
mst <- data.frame(c("2010-003204", "2010-003908", "2011-000622", "2011-017092", "2011-019010", "2012-017637", "2013-022849", "2013-025663", "2014-009222", "2014-011757", "2014-023518", "2015-002277", "2015-002645", "2015-013175", "2015-019473", "2015-024014", "2016-008152", "2016-020838", "2016-022706", "2016-022792", "2016-023385", "2016-024545"))

colnames(mst) <- "CaseId"

# ni_recs with matching CaseId to the above
mono <- semi_join(ni_recs, mst, by = "CaseId")

# duplicate CaseIds
dup_mono <- mono[allDup(mono$CaseId), ]

# Remove non-typhus from these...
dmono <- dup_mono %>% filter(!Salmonella %in% c("S.Mbandaka", "S.Gold-coast", "S.Takoradi", "S.Kottbus"))

# Look for those with the same sample id to remove duplicates
mp_cases <- select(dmono, SampleId)
mp_cases_dups <- duplicated(mp_cases)

iddups <- cbind(dmono, mp_cases_dups)

# remove duplicates
id_mono_clean <- filter(iddups, mp_cases_dups == "FALSE")

# add these to rest of mono data
# remove T/F column
dim(id_mono_clean)
monos <- id_mono_clean[ , 1:92]

# add data that wasn't part of mono duplicate stuff
typs <- rbind(monos, mono)

typhs_samps <- select(typs, SampleId)

typs_dups <- duplicated(typhs_samps)

ty_du <- cbind(typs, typs_dups)
typh_clean <- filter(ty_du,  typs_dups == "FALSE")

typh_mono <- typh_clean[ , 1:92]

# Change Salmonella to mST

typh_mono$Salmonella <- "Monophasic Salmonella Typhimurium"

# add to rest of data set - but what about same cases that had other salmonella??

# rest of data set
not_mono <- anti_join(ni_recs, mst, by = "CaseId")

# matching caseId for non-typhus strains
non_typh <- filter(mono, Salmonella %in% c("S.Mbandaka", "S.Gold-coast", "S.Takoradi", "S.Kottbus"))

# rbind with typhs
sal_names <- rbind(typh_mono, non_typh, not_mono)


## remove non-typed Samlmonella ####
table(sal_names$Salmonella)

sal_name <- sal_names %>% filter(!Salmonella %in% c("S.Spp (final)", "S.Spp", "S.spp", "S.Unnamed", "S.spp (unnamed)", "S.unnamed", "SALMANI", "SALMENS"))

## Is there replication of SampleId and Salmonella? - NO ####
sampleids <- sal_name

samps <- select(sampleids, SampleId, Salmonella)
samp_dups <- data.frame(duplicated(samps))

table(samp_dups$duplicated.samps.)


## Remove replication of CaseId, Tag and Salmonella ####
# cases with same CaseId
dup_caseid <- sampleids[allDup(sampleids$CaseId), ]

# remove cases w/ same CaseId, strain and Tag
tag_id <- select(dup_caseid, "CaseId", "Salmonella", "Tag")
tag_id_dup <- duplicated(tag_id)
table(tag_id_dup)

tag_id_dup2 <- cbind(dup_caseid, tag_id_dup)
table(tag_id_dup2$tag_id_dup)

tag_id_sal_t <- filter(tag_id_dup2, tag_id_dup == "TRUE")

## remove trues and corresponding obs from sal_data
tag_id_sal_clean <- anti_join(sampleids, tag_id_sal_t, by = "ID") 

poul_data <- tag_id_sal_clean
str(poul_data)


## Sort out 'hash' records ####

## Records with '#' don't have tags but are replicates of others. Always listed first, so can use 'duplicated'
hash <- filter(poul_data, AnimalId == "#")

# isolate all cases with a '#' observation # semi_join gives '#' obs and same CaseIds 
hash_recs <- semi_join(poul_data, hash, by = "CaseId") 

# hashs seem to be the sensitivity training, so try to keep?
# Id those with same CaseId and Salmonella
test_hash <- select(hash_recs, CaseId, Salmonella)

# '#' records are fisrt, so they are assigned 'FALSE' replicates
dup_hash <- duplicated(test_hash) 
table(dup_hash)

hash_dups <- cbind(hash_recs, dup_hash)
hash_sing <- filter(hash_dups, dup_hash == "TRUE") 

# remove TRUE replicates
hash_clean <- anti_join(poul_data, hash_sing, by = "ID") 

poul_dt <- hash_clean

## Remove Salmonella Enteritidis vaccine strains ####
# post 2007, layers with phage type 4 (LC, LG, LP)

pd_2007 <- filter(poul_dt, Year > 2006)
pd_layers <- filter(pd_2007, Flock %in% c("LB", "LG", "LC", "LP"))
pd_phage <- filter(pd_layers, Phage %in% c("4", "PT 4b", "PT 4"))

# remove from dataset
vf_poul <- anti_join(poul_dt, pd_phage, by = "ID")


## Remove broilers resampled in < 8 weeks ####

# BC and BR are broilers
broil <- filter(vf_poul, Flock == "BC")
o_broil <- filter(vf_poul, Flock == "BR")

broilers <- rbind(broil, o_broil)

# just eyeball the data?
broil_reps <- select(broilers, Year, Month, CaseId, Owner, Salmonella, Tag, ID, OwnerAddress)

# replicate testing and testing in < 8 week intervals00

id_reps <- data.frame(c(2796, 3886, 2562, 2271, 2476, 2776, 2792, 2621, 2622, 2657, 2540, 392, 2583, 2733, 2619, 1411, 1449, 3125, 2698, 2587, 2267, 1960, 1928, 3304, 728, 874, 875, 876, 1956, 1964, 3419, 3309, 3346, 1412, 3360, 137, 4672, 5077, 4725, 4835, 4791, 4819, 4787, 4788, 4803, 4577, 4576, 5097, 5540, 5558, 2611, 3302, 3484, 1975, 2786, 2781, 2794, 4082, 3900, 3872, 3969, 3970, 3972, 3973, 3964, 4023, 4084, 4144, 3890, 3891, 3968, 4187, 4062, 4063, 3849, 4365, 4330, 3917, 4238, 4239, 3873, 4228, 3946, 4206, 3676, 4015, 4016, 3867, 4983, 5522, 5498, 4763, 5189, 4868, 5446, 4372, 13, 14, 15, 16, 17, 4379, 4380, 4382, 4383, 4385, 4387, 18, 4404, 4408, 4411, 4412, 111, 4590, 132, 4657, 4683, 4710, 162, 164, 4809, 4810, 4816, 4828)) 

colnames(id_reps) <- "ID"

broils1 <- anti_join(vf_poul, id_reps, by = "ID")


## Remove broilers same farm, same time, different tags ####
id_farm <- data.frame(c(1958, 3478, 2657, 3487, 1432, 4013, 4014, 2635, 2630, 1027, 2239, 2279, 2293, 2295, 2312, 1962, 2894, 2217, 4158, 4182, 3878, 4096, 4421, 4422, 3985, 3, 4, 4341, 158, 170, 169, 4837, 168, 4853, 104, 2817, 3835, 4214, 3955, 3906, 3860, 3959, 4100, 4028, 4113, 4172, 3913, 4323, 3966, 4274, 4259, 4091, 4205, 3822, 4018, 3943, 3944, 3841, 4056, 4057, 4058, 4939, 4920, 4802, 4954, 5104, 5103, 5521))

colnames(id_farm) <- "ID"

# check that those removed before weren't samples with AMR testing - all seem to be tested, so it's grand
testing <- semi_join(broils1, id_farm, by = "ID")
testing2 <- semi_join(vf_poul, testing, by = "CaseId")

# check SampleId first (for those added from monophasic dataset)

case_reps <- anti_join(broils1, id_farm, by = "ID")


## same for not broilers ####

others <- anti_join(vf_poul, broilers, by = "SampleId")

oth_reps <- select(others, Year, Month, CaseId, Owner, Salmonella, Tag, ID, OwnerAddress)

rem <- data.frame(c(3325, 1196, 3386, 1376, 2358, 820, 1315, 959, 412, 2837, 4698, 4792, 2134, 2138, 2149, 2150, 2172, 2179, 314, 944, 1023, 270, 362, 355, 1022, 2390, 1604, 2933, 643, 1014, 1180, 1272, 1173, 2887, 3755, 1246, 722, 2951, 3111, 3532, 3536, 1094, 1606, 644, 1213, 1714, 915, 1688, 3181, 966, 616, 1786, 1672, 1664, 1666, 1537, 1770, 1170, 1175, 1351, 1361, 1362, 1576, 1621, 1679, 2830, 2872, 2892, 2907, 872, 1408, 302, 299, 2747, 3721, 775, 3516, 249, 311, 3518, 3579, 3594, 3606, 655, 847, 3401, 3745, 2964, 3024, 945, 3164, 3621, 3388, 1149, 378, 3067, 4488, 2785, 2815, 3382, 1018, 1019, 1539, 3203, 789, 363, 379, 1518, 1228, 1229, 919, 1358, 1359, 3512, 650, 1281, 1305, 3387, 1233, 1089, 2669, 1648, 2921, 3514, 2800, 211, 212, 70, 4541, 4368, 4384, 4475, 4511, 4512, 4484, 3743, 3744, 3781, 4400, 4445, 4449, 4193, 8, 4361, 4248, 4255, 3784, 3786, 3785, 3787, 4075, 3726, 4454, 4395, 4043, 3626, 3623, 3651, 3683, 3597, 4298, 4443, 4446, 4447, 4557, 4562, 93, 4563, 4565, 4527, 4566, 4567, 4526, 5523, 4872, 4875, 4964, 160, 161, 4617, 4781, 4782, 4783, 4784, 4812, 4572, 4708, 4638, 109, 4583, 4588, 135, 4662, 4507, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 4661, 4505, 4922, 4923, 4924, 4656, 4639, 4679, 4546, 71, 72, 73, 74, 75, 76, 77, 78, 79, 4547, 4119, 4125, 4925, 4931, 4930, 4941, 4691, 4926, 4934, 4940, 4687, 4620, 4857, 4712, 4716, 4534, 147, 148, 4726, 4736, 5245, 5244, 5243, 5363, 5370, 5391, 5100, 5111, 1212, 1270, 1534, 1360, 978, 679, 983, 3771, 4280, 4455, 4441, 3600, 4208, 4209, 4210, 4174, 4465, 4538, 4970, 4730, 4756, 4827, 4539, 4906, 4914, 4690, 4693, 4665, 5390))

colnames(rem) <- "ID"

no_reps <- anti_join(case_reps, rem, by = "ID")

## remove vaccine records ####
poultry_no_reps <- droplevels(filter(no_reps, Salmonella != "S.Enteritidis vaccine strain"))

## remove 93/94 records ####
poultry <- filter(poultry_no_reps, Year > 1994)

## write out data ####

write.csv(poultry, "file_path.csv")
