library(tidyverse)
library(grid)
library(car)
library(ROCR)
library(data.table)
library(popbio)
library(plyr)

pigs <- read.csv("C:\\Users\\afbi-porters\\Documents\\salmonella_pigs\\pigs_clean.csv")

## make bounded year variable ####
range(pigs$Year)
pigs <- mutate(pigs, year_bound = Year - 1992)
range(pigs$year_bound)

# remove 2017 - incomplete data
pigs2 <- filter(pigs, Year < 2017)
range(pigs2$Year)

# Counts of numbers of all antimicrobial resistance for each serovar
ses_t <- droplevels(dplyr::select(pigs2, Ampicillin:SulphaTrim))
ses_t$resist <- rowSums(ses_t == "R", na.rm = TRUE)
ses_t$sensit <- rowSums(ses_t == "S", na.rm = TRUE)
ses_t$inter <- rowSums(ses_t == "I", na.rm = TRUE)

ses_t$tested <- rowSums(ses_t[ , 25:27])

ast_tests <- ses_t[ , 25:28]

sensitivity <- cbind(pigs2, ast_tests)

# overall has there been a pattern in resistance found in samples over time?

str(sensitivity)

ggplot(sensitivity, aes(Year, resist)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point()


# not good enough
m1 <- lm(resist ~ year_bound, data = sensitivity)
summary(m1)
plot(m1)

hist(Year ~ resist, data = sensitivity)

# ast R results into numerical data
# all R's as 1, I and S as 0 and leave NA's as NA's
drugs <- droplevels(dplyr::select(pigs2, Ampicillin:SulphaTrim))
str(drugs)

# change df name for converstion 
conv <- drugs

conv[] <- lapply(conv, gsub, pattern = "R", replacement = 1, fixed = TRUE)
conv[] <- lapply(conv, gsub, pattern = "S", replacement = 0, fixed = TRUE)
conv[] <- lapply(conv, gsub, pattern = "I", replacement = 0, fixed = TRUE)
conv[] <- lapply(conv, gsub, pattern = "X", replacement = 0, fixed = TRUE)

# asign data numeric
dr_in <- colwise(as.numeric)(conv)
str(dr_in)

# so many NAs - might be a problem later?
summary(dr_in)

# add to rest of data
names(pigs2)
rest <- pigs2[ , 1:43]
rest_yb <- pigs2[ , 94]
str(rest)

pigs_d <- cbind(rest, rest_yb, dr_in)
str(pigs_d) 
range(pigs_d$Year)

# have incidence of AMR changed over time for any of these serovars? Individually and grouped into families (how are they grouped?)

# for all results
hist(sensitivity$resist)

m1 <- glm(resist ~ year_bound, family = "poisson", data = sensitivity)
summary(m1)
plot(m1) # this is balls... 


# testing for each drug separately ####
ggplot(pigs_d, aes(Year, Ampicillin)) +
  geom_smooth(method = 'lm') 

mod1 <- glm(Ampicillin ~ rest_yb, family = binomial(link = 'logit'), data = pigs_d)
summary(mod1)
plot(mod1)
anova(mod1, test = "Chisq")

exp(0.04855)

exp(confint(mod1))

# plot fitted values - think this is just the same as the geom_smooth(method = lm)? 
years <- data.frame(rest_yb = seq(from = 1, to = 24, by = 1))
pred <- predict(mod1, newdata = years, type = "response")
plot(pigs_d$rest_yb, pigs_d$Ampicillin)
lines(years$rest_yb, pred)

# AUC
p <- predict(mod1, type = 'response')

summary(pigs_d$Ampicillin)

# need to remove observations with NA for Ampicillin
pigs2 <- pigs_d[!is.na(pigs_d$Ampicillin), ]

pr <- prediction(p, pigs2$Ampicillin) 
pred_roc <- performance(pr, 'tpr', 'fpr')

am_auc <- performance(pr, measure = "auc")
auc_am <- am_auc@y.values[[1]]

plot(pred_roc)

# try logi.hist.plot
logi.hist.plot(pigs_d$Year, pigs_d$SulphaTrim, boxp=FALSE,type="hist",col="gray", mainlabel = "SulphaTrim")

######################################################################################
# do test for model best fit
######################################################################################
# separate into different drugs classes 
# could plot all lines for each class on one plot?
str(pigs_d)

# how many rows will be disscounted with NAs removed? some only have <10 observations... maybe need to keep separate, or exclude these drugs??

# aminoglycosides - spectinomycin included as related to aminoglycosides
ami <- select(pigs_d, Year, Salmonella, Gentamycin, Streptomycin, Kanamycin, Neomycin, Spectinomycin, Apramycin, Framycetin)

# beta lactims (maybe split into further classes - e.g. cephalosporins?)
beta <- select(pigs_d, Year, Salmonella, Penicillin, Ampicillin, Amoxycillin, Cefotaxime, Cefoperazone, Ceftazidime, AmoxyClavu)

# Chloramphenicol
chlor <- select(pigs_d, Year, Salmonella, Chloramphenicol)

# Diaminopyrimidines (Trimethoprim)
trim <- select(pigs_d, Year, Salmonella, Trimethoprim)

# fluroquinolones
flor <- select(pigs_d, Year, Salmonella, Enrofloxacin, Ciprofloxacin, Nalidixicacid)

# sulphonamides - why are these all grouped together?
sulph <- select(pigs_d, Year, Salmonella, Sulphonamides)

# Tetracycline
tet <- select(pigs_d, Year, Salmonella, Tetracycline, Oxytetracycline)

# SulphaTrim - a mix of sulfadoxine (a sulphidamide) and trimethoprim (a Diaminopyrimidine)
sutr <- select(pigs_d,  Year, Salmonella, SulphaTrim)


## a focus on those that are critically important ####
