library(tidyverse)
library(grid)
library(gridExtra)
library(MASS)
library(pscl)

poultry <- read.csv("C:/Users/afbi-porters/Documents/sallmonella_poultry/poultry_clean.csv")

# change name of mST
poultry$Salmonella <- as.character(poultry$Salmonella)
poultry$Salmonella[poultry$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium"
poultry$Salmonella <- as.factor(poultry$Salmonella)



# Get rid of wierd ser names first
poul <- poultry %>%
  filter(!Salmonella %in% c("S.ParatyphiB var. Java", "S.Diarizonae O 61"))

## remove unknown flock types

table(poul$Flock)

# change flock names
# change all unknown/wierd ones to 'UN'
poul$Flock[poul$Flock == ""] <- "UN"
poul$Flock[poul$Flock == "LA"] <- "UN"
poul$Flock[poul$Flock == "BB"] <- "UN"
poul$Flock[poul$Flock == "DC"] <- "UN"

table(poul$Flock)

## remove unpecified flocks
pf <- droplevels(filter(poul, Flock != "UN"))

table(pf$Flock)

# summary of  records per flock per year
year_flock <- pf %>%
  group_by(Year, Flock) %>%
  summarise (n = n()) 

setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "isolates_flock_year.tiff", width = 10, height = 5, units = "in", res = 400)

ggplot(year_flock, aes(x = Year, y = n, fill = Flock)) +
  theme_minimal() +
  ylab("count") +
  xlab("") +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300))


dev.off()

## tables of ncp and non-ncp ocurance and relative proportional frequency ####
# calculate rel. prop. freq. of each serovar
sal_freq <- pf %>%
  group_by(Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# select NCP serovars
ncps <- sal_freq %>%
  filter(Salmonella %in% c("S.Enteritidis", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Hadar", "S.Infantis", "S.Virchow"))

# write out
write.csv(ncps, "C:/Users/afbi-porters/Documents/sallmonella_poultry/ncps.csv")

# for non-ncps
non_ncps <- sal_freq %>%
  filter(!Salmonella %in% c("S.Enteritidis", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Hadar", "S.Infantis", "S.Virchow"))

# write out
write.csv(non_ncps, "C:/Users/afbi-porters/Documents/sallmonella_poultry/non_ncps.csv")

## plot serovars with rel. prop. freq and numnber of isolates over time ####

# too few for S Hadar - when were these records?
filter(pf, Salmonella == "S.Hadar")

# calculate relative proportional frequency over time

# count all ST together
pf$Salmonella[pf$Salmonella == "Monophasic S.Typhimurium"] <- "S.Typhimurium"

sal_year_sum <- pf %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ser <- filter(sal_year_sum, Salmonella  == "S.Kentucky")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#0000FF") +
  geom_line(size = 0.75, colour = "#0000FF") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Kentucky") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=30)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#0000FF") +
  geom_line(size = 0.75, colour = "#0000FF") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  xlim(1995, 2017) +
  scale_x_continuous() +
  theme(axis.title.y = element_text(margin = margin(r=30)))

setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "S.Kentucky.tiff", width = 5.5, height = 5, units = "in", res = 400)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.off()

## number of isolates coming into the lab by year
iso_year <- poul %>%
  group_by(Year) %>%
  summarise (n = n()) 

setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "num_isolates.tiff", width = 10, height = 5, units = "in", res = 400)

ggplot(iso_year, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FE2E64") +
  geom_line(size = 0.75, colour = "#FE2E64") +
  ylab("count") +
  ggtitle("Isolates recorded") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=20)))  +
  scale_x_continuous()

dev.off()

## number of isolates per year per flock ####
# are the magority of SE in chickens broilers? 

se_freq <- pf %>%
  group_by(Salmonella, Year, Flock) %>%
  summarise (n = n()) 

se_all <- filter(se_freq, Salmonella == "S.Enteritidis")
se_bc <- filter(se_all, Flock == "BC")

# SE not from broilers
se_nbc <- filter(se_all, Flock != "BC")

se_nbc$F2 <- "non-broiler"
se_bc$F2 <- "broiler"

# join and plot 
se_flocks <- rbind(se_nbc, se_bc)

setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "broiler_SE.tiff", width = 10, height = 5, units = "in", res = 400)

ggplot(se_flocks, aes(x = Year, y = n, fill = F2)) +
  theme_minimal() +
  ylab("count") +
  xlab("") +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) + 
  theme(legend.title=element_blank())

dev.off()

# SE through all flock types
setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "flock_SE.tiff", width = 10, height = 5, units = "in", res = 400)

ggplot(se_all, aes(x = Year, y = n, fill = Flock)) +
  theme_minimal() +
  ylab("count") +
  xlab("") +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150)) + 
  theme(legend.title=element_blank())

dev.off()

##########################################################################################
# infantis
setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_poultry\\rpf_count_plots")
tiff(file = "S.Virchow.tiff", width = 5.5, height = 5, units = "in", res = 400)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.off()

ser <- filter(sal_year_sum, Salmonella  == "S.Infantis")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FFBF00") +
  geom_line(size = 0.75, colour = "#FFBF00") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Infantis") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=20)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FFBF00") +
  geom_line(size = 0.75, colour = "#FFBF00") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  xlim(1995, 2017) +
  scale_x_continuous() +
  theme(axis.title.y = element_text(margin = margin(r=20)))

#typhi
ser <- filter(sal_year_sum, Salmonella  == "S.Typhimurium")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#0000FF") +
  geom_line(size = 0.75, colour = "#0000FF") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Typhimurium") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=20)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#0000FF") +
  geom_line(size = 0.75, colour = "#0000FF") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  xlim(1995, 2017) +
  scale_x_continuous() +
  theme(axis.title.y = element_text(margin = margin(r=20)))
#Enteriditis
ser <- filter(sal_year_sum, Salmonella  == "S.Enteritidis")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FF0000") +
  geom_line(size = 0.75, colour = "#FF0000") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Enteritidis") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=20)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FF0000") +
  geom_line(size = 0.75, colour = "#FF0000") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  xlim(1995, 2017) +
  scale_x_continuous() +
  theme(axis.title.y = element_text(margin = margin(r=20)))

# virchow
ser <- filter(sal_year_sum, Salmonella  == "S.Virchow")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#DF3A01") +
  geom_line(size = 0.75, colour = "#DF3A01") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Virchow") +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=30)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#DF3A01") +
  geom_line(size = 0.75, colour = "#DF3A01") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  xlim(1995, 2017) +
  scale_x_continuous() +
  theme(axis.title.y = element_text(margin = margin(r=30)))
