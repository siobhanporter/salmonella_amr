library(tidyverse)

pigs <- read.csv("C:\\Users\\afbi-porters\\Documents\\salmonella_pigs\\pigs_clean.csv")
poultry <- read.csv("C:/Users/afbi-porters/Documents/sallmonella_poultry/poultry_clean.csv")

## relative proportional frequency of  top serovars ####
pig_rate <- droplevels(filter(pigs, Salmonella %in% c("S.Typhimurium", "S.Derby", "Monophasic S.Typhimurium", "S.Give", "S.Choleraesuis")))

poul_rate <- droplevels(filter(poultry, Salmonella %in% c("S.Typhimurium", "Monophasic Salmonella Typhimurium", "S.Enteritidis", "S.Tennessee", "S.Muenster")))

table(poul_rate$Salmonella)
table(pig_rate$Salmonella)

pig_rate$Salmonella <- as.character(pig_rate$Salmonella)
pig_rate$Salmonella[pig_rate$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 377"
pig_rate$Salmonella[pig_rate$Salmonella == "S.Derby"] <- "S.Derby, n = 109"
pig_rate$Salmonella[pig_rate$Salmonella == "Monophasic S.Typhimurium"] <- "Monophasic S.Typhimurium, n = 59"
pig_rate$Salmonella[pig_rate$Salmonella == "S.Give"] <- "S.Give, n = 16"
pig_rate$Salmonella[pig_rate$Salmonella == "S.Choleraesuis"] <- "S.Choleraesuis, n = 15"

poul_rate$Salmonella <- as.character(poul_rate$Salmonella)
poul_rate$Salmonella[poul_rate$Salmonella == "S.Enteritidis"] <- "S.Enteritidis, n = 501"
poul_rate$Salmonella[poul_rate$Salmonella == "S.Tennessee"] <- "S.Tennessee, n = 229"
poul_rate$Salmonella[poul_rate$Salmonella == "S.Muenster"] <- "S.Muenster, n = 143"
poul_rate$Salmonella[poul_rate$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 117"
poul_rate$Salmonella[poul_rate$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium, n = 34"

pig_year_sum <- pig_rate %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

poul_year_sum <- poul_rate %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# reorder to have st and mst together
pig_year_sum$Salmonella <- factor(pig_year_sum$Salmonella, levels = c("S.Choleraesuis, n = 15", "S.Give, n = 16","S.Derby, n = 109", "S.Typhimurium, n = 377", "Monophasic S.Typhimurium, n = 59"), ordered = TRUE)

poul_year_sum$Salmonella <- factor(poul_year_sum$Salmonella, levels = c("S.Muenster, n = 143", "S.Enteritidis, n = 501", "S.Tennessee, n = 229", "S.Typhimurium, n = 117", "Monophasic S.Typhimurium, n = 34"), ordered = TRUE)

ggplot(pig_year_sum, aes(Year, freq, fill = Salmonella)) +
  theme_bw() +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(1993, 2017, 4)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size=15, face = "bold", colour = "black"))

ggplot(poul_year_sum, aes(Year, freq, fill = Salmonella)) +
  theme_bw() +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(1993, 2017, 4)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size=15, face = "bold", colour = "black"))


## levels of amr in pigs and poultry ####
poul_resist <- read.csv("C:/Users/afbi-porters/Documents/Salmonella_gen/resistance_testing/poul_resist.csv")
pig_resist <- read.csv("C:/Users/afbi-porters/Documents/Salmonella_gen/resistance_testing/pig_resist.csv")

# poultry
ggplot(poul_resist, aes(Year, resist)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, colour = "red") +
  labs(y = "Number of resistance in samples") +
  scale_x_continuous(breaks = seq(1993, 2017, 4)) +
  ggtitle("Levels of antimicrobial resistance in \n Salmonella isolated from poultry") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=22))

#pigs
ggplot(pig_resist, aes(Year, resist)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, colour = "red") +
  labs(y = "Number of resistance in samples") +
  scale_x_continuous(breaks = seq(1993, 2017, 4)) +
  ggtitle("Levels of antimicrobial resistance in \n Salmonella isolated from pigs") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=22))


## Number of animicrobials top serovars are resistant to ####
# select most common serovars for each group - 5?

sort(table(poul_resist$Salmonella))
# Enteriditis, tennessee, muenster, typhi and mst 

sort(table(pig_resist$Salmonella))
#typh, mst, derby, give, choleraesuis

# select common serovars
pig_sers <- droplevels(filter(pig_resist, Salmonella %in% c("S.Typhimurium", "S.Derby", "Monophasic S.Typhimurium", "S.Give", "S.Choleraesuis")))

poul_sers <- filter(poul_resist, Salmonella %in% c("S.Typhimurium", "Monophasic Salmonella Typhimurium", "S.Enteritidis", "S.Tennessee", "S.Muenster"))

# rename with 'n='
pig_sers$Salmonella <- as.character(pig_sers$Salmonella)
pig_sers$Salmonella[pig_sers$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 317"
pig_sers$Salmonella[pig_sers$Salmonella == "S.Derby"] <- "S.Derby, n = 85"
pig_sers$Salmonella[pig_sers$Salmonella == "Monophasic S.Typhimurium"] <- "Monophasic S.Typhimurium, n = 38"
pig_sers$Salmonella[pig_sers$Salmonella == "S.Give"] <- "S.Give, n = 13"
pig_sers$Salmonella[pig_sers$Salmonella == "S.Choleraesuis"] <- "S.Choleraesuis, n = 10"

poul_sers$Salmonella <- as.character(poul_sers$Salmonella)
poul_sers$Salmonella[poul_sers$Salmonella == "S.Enteritidis"] <- "S.Enteritidis, n = 501"
poul_sers$Salmonella[poul_sers$Salmonella == "S.Tennessee"] <- "S.Tennessee, n = 229"
poul_sers$Salmonella[poul_sers$Salmonella == "S.Muenster"] <- "S.Muenster, n = 143"
poul_sers$Salmonella[poul_sers$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 117"
poul_sers$Salmonella[poul_sers$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium, n = 34"

# resist as factor
pig_sers$resist <- as.factor(pig_sers$resist)
poul_sers$resist <- as.factor(poul_sers$resist)

# order in terms of levels of resistance
pig_sers$Salmonella <- factor(pig_sers$Salmonella, levels = c("S.Derby, n = 85", "S.Choleraesuis, n = 10", "S.Give, n = 13", "S.Typhimurium, n = 317", "Monophasic S.Typhimurium, n = 38"), ordered = TRUE)

poul_sers$Salmonella <- factor(poul_sers$Salmonella, levels = c("S.Muenster, n = 143", "S.Enteritidis, n = 501", "S.Tennessee, n = 229", "Monophasic S.Typhimurium, n = 34", "S.Typhimurium, n = 117"), ordered = TRUE)

# plot
ggplot(pig_sers, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size=15, face = "bold", colour = "black")) +
  theme(legend.position='none') 

ggplot(poul_sers, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(text = element_text(size=15, face = "bold", colour = "black")) +
  theme(legend.position='none')


####################
## old code 
library(tidyverse)

## has there been an increase in amr? ####

table(clean_pigs$Salmonella)

# Count of number of resistant/sensitive/inter accross each observation
ses_t <- droplevels(dplyr::select(clean_pigs, Ampicillin:SulphaTrim))
ses_t$resist <- rowSums(ses_t == "R", na.rm = TRUE)
ses_t$sensit <- rowSums(ses_t == "S", na.rm = TRUE)
ses_t$inter <- rowSums(ses_t == "I", na.rm = TRUE)

ses_t$tested <- rowSums(ses_t[ , 25:27])

ast_tests <- ses_t[ , 25:28]

clean_pigs <- cbind(clean_pigs, ast_tests)

table(clean_pigs$Salmonella)

# remove columns with no tests
table(clean_pigs$tested)
str(clean_pigs)
sens_test <- filter(clean_pigs, tested > 0)
table(sens_test$tested)
table(sens_test$Salmonella)

## write these data out for joint analysis with pigs
str(sens_test)

# only need year, and last four columns
pig_resist <- dplyr::select(sens_test, Year, Salmonella, resist, sensit, inter, tested)

table(pig_resist$tested)

write.csv(pig_resist, "C:/Users/afbi-porters/Documents/Salmonella_gen/resistance_testing/pig_resist.csv")

## comparing resistance over time between pigs and poultry ####
poul_resist <- read.csv("C:/Users/afbi-porters/Documents/Salmonella_gen/resistance_testing/poul_resist.csv")

poul_resist$Salmonella <- as.character(poul_resist$Salmonella)
poul_resist$Salmonella[poul_resist$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium"

table(poul_resist$Salmonella)

str(pig_resist)
str(poul_resist)

poul_resist <- poul_resist[ , 2:7]

poul_resist$spp <- "poultry"
pig_resist$spp <- "pigs"

resist_all <- rbind(poul_resist, pig_resist)
str(resist_all)

# no poul data until 1995 - remove these years
res_all <- filter(resist_all, Year > 1994)

res_all$Year <- as.factor(res_all$Year)
res_all$spp <- as.factor(res_all$spp)

## plot these 
ggplot(res_all, aes(x = Year, y = resist, fill = spp)) +
  theme_bw() +
  geom_boxplot() +
  annotate("rect", xmin = 13, xmax = 25, ymin = 0, ymax = 12.5, fill = "blue", alpha = 0.2)


# bin into two year intervals?

## plot number of antimicrobials each are resistant to ####
str(res_all)

table(res_all$Salmonella)

table(pig_resist$Salmonella)

# do this for priority serovars
priority_poul <- droplevels(filter(res_all, Salmonella %in% c("S.Enteritidis", "S.Muenster", "S.Tennessee", "S.Typhimurium", "Monophasic S.Typhimurium")))

priority_pigs <- droplevels(filter(res_all, Salmonella %in% c("S.Give", "S.Derby", "S.Typhimurium", "Monophasic S.Typhimurium")))

str(priority)
priority$Year <- as.ordered(priority$Year)
priority$Salmonella <- as.character(priority$Salmonella)
priority$resist <- as.factor(priority$resist)

table(priority$Year)

# separate by species and NCP
poul_pre <- filter(priority_poul, spp == "poultry" & Year < 2007)
poul_post <- filter(priority_poul, spp == "poultry" & Year > 2006)
pigs_pre <- filter(priority_pigs, spp == "pigs" & Year < 2007)
pigs_post <- filter(priority_pigs, spp == "pigs" & Year > 2006)

# poul pre plots
table(poul_pre$Salmonella)

priority_poul$Salmonella[priority_poul$Salmonella == "S.Enteritidis"] <- "S.Enteritidis, n = 464"
priority_poul$Salmonella[priority_poul$Salmonella == "S.Muenster"] <- "S.Muenster, n = 2"
priority_poul$Salmonella[priority_poul$Salmonella == "S.Tennessee"] <- "S.Tennessee, n = 129"
priority_poul$Salmonella[priority_poul$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 80"

str(poul_pre)


# reorder in order of sensitivity
priority_poul$Salmonella <- factor(priority_poul$Salmonella, levels = c("S.Muenster, n = 2", "S.Enteritidis, n = 464", "S.Tennessee, n = 129", "S.Typhimurium, n = 80"))

priority_poul$resist <- as.factor(priority_poul$resist)

ggplot(priority_poul, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="No. antimicrobials"))


## poul post
table(poul_post$Salmonella)

poul_post$Salmonella[poul_post$Salmonella == "S.Enteritidis"] <- "S.Enteritidis, n = 37"
poul_post$Salmonella[poul_post$Salmonella == "S.Muenster"] <- "S.Muenster, n = 141"
poul_post$Salmonella[poul_post$Salmonella == "S.Tennessee"] <- "S.Tennessee, n = 100"
poul_post$Salmonella[poul_post$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 37"
poul_post$Salmonella[poul_post$Salmonella == "Monophasic S.Typhimurium"] <- "Monophasic S.Typhimurium, n = 34"

poul_post$resist <- as.factor(poul_post$resist)

ggplot(poul_post, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="No. antimicrobials"))

poul_post$Salmonella <- factor(poul_post$Salmonella, levels = c("S.Muenster, n = 141", "S.Enteritidis, n = 37", "Monophasic S.Typhimurium, n = 34", "S.Typhimurium, n = 37", "S.Tennessee, n = 100"))

# pigs all

pigs_all <- rbind(pigs_post, pigs_pre)

table(pigs_all$Salmonella)

pigs_all$Salmonella[pigs_all$Salmonella == "S.Give"] <- "S.Give, n = 13"
pigs_all$Salmonella[pigs_all$Salmonella == "S.Derby"] <- "S.Derby, n = 83"
pigs_all$Salmonella[pigs_all$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 316"
pigs_all$Salmonella[pigs_all$Salmonella == "Monophasic S.Typhimurium"] <- "Monophasic S.Typhimurium, n = 38"

pigs_all$resist <- as.factor(pigs_all$resist)

pigs_all$Salmonella <- factor(pigs_all$Salmonella, levels = c("S.Derby, n = 83", "S.Give, n = 13", "S.Typhimurium, n = 316", "Monophasic S.Typhimurium, n = 38"))

ggplot(pigs_clean, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="No. antimicrobials"))

# proportional frequencies
sal_year_sum <- poul_pre %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(sal_year_sum, aes(Year, freq, fill = Salmonella)) +
  theme_bw() +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  ggtitle("Proportional relative frequency poultry pre_ncp")

## plot resistance over time
names(clean_pigs)

plot(clean_pigs$Year, clean_pigs$resist)
abline(lm(clean_pigs$resist ~ sens_test$Year)) 

hist(clean_pigs$resist)


ggplot(poul_resist, aes(Year, resist)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, colour = "red") +
  labs(y = "Number of resistance in samples") +
  scale_x_continuous(breaks = seq(1993, 2017, 4)) +
  ggtitle("Levels of antimicrobial resistance in \n Salmonella isolated from poultry") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.title = element_text(size=22))


# model resistance over time
#var > mean so use neg binom 
var(clean_pigs$resist)
mean(clean_pigs$resist)

library(MASS)

mod1 <- glm.nb(resist ~ Year, data = poul_resist)
summary(mod1)
plot(mod1)

# goddness of fit for this mod = 0.94 (needs to be >0.05)
1 - pchisq(summary(mod1)$deviance, 
           summary(mod1)$df.residual
)

# has there been an increse in mrd? ####

sens_test$resist <- as.numeric(sens_test$resist)

mdr <- sens_test %>%
  mutate(mul_res = if_else(resist > 2, 1, 0))

str(mdr)
mdr$Year <- as.factor(mdr$Year)

# overall proportion of mdr
mdr_prop <- mdr %>%
  group_by(Year) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

plot(mdr_prop$Year, mdr_prop$freq)

ggplot(mdr_prop, aes(Year, freq)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, colour = "red") + # this isn't working, need multiple values for error measure
  labs(y = "Resistant samples")

# proportion mdr by serovar
mdr_year_sum <- mdr %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

## graph no microbials resistant by year?
sens_test$Year <- as.factor(sens_test$Year)
sens_test$resist <- as.numeric(sens_test$resist)
str(sens_test)

ggplot(sens_test, aes(Year, resist)) +
  theme_bw() +
  geom_boxplot() # this is a mess
