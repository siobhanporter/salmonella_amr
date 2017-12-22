library(tidyverse)
library(grid)
library(gridExtra)
library(MASS)
library(pscl)

poultry <- read.csv("C:/Users/afbi-porters/Documents/sallmonella_poultry/poultry_clean.csv")

## how many of each serovar are there? Which need to be removed? ####
sal_sum <- data.frame(table(poultry$Salmonella))

sal_sum$Var1

# remove sals with wierd/wrong names 

sals1 <- filter(poultry, Salmonella != "S.ParatyphiB var. Java")
sals2 <- filter(sals1, Salmonella != "S.Diarizonae O 61")

serovars <- sals2

## divide data into groups of interest (general, priority, sensitivity, gallin, contamin) ####
priority <- droplevels(filter(serovars, Salmonella %in% c("S.Enteritidis", "S.Hadar", "S.Infantis", "S.Typhimurium", "Monophasic Salmonella Typhimurium", "S.Virchow")))

sensitivity <- droplevels(filter(serovars, Salmonella %in% c("S.Gold-coast", "S.Infantis", "S.Kentucky", "S.Newport", "S.Typhimurium", "Monophasic Salmonella Typhimurium")))

gallin <- droplevels(filter(serovars, Salmonella == "S.Gallinarum"))

contamin <- droplevels(filter(serovars, Salmonella %in% c("S.Dublin", "S.Kottbus")))

general <- droplevels(filter(serovars, !Salmonella %in% c("S.Enteritidis", "S.Hadar", "S.Infantis", "S.Typhimurium", "Monophasic Salmonella Typhimurium", "S.Virchow", "S.Gold-coast", "S.Kentucky", "S.Newport", "S.Gallinarum", "S.Dublin", "S.Kottbus")))

# check this


## separate general serovars ####
# what serovars are they?
table(general$Salmonella)

# separate individul serovars (ser1 - ser50)
ser1 <- filter(poultry, Salmonella == "S.Agona")
ser2 <- filter(poultry, Salmonella == "S.Altona")
ser3 <- filter(poultry, Salmonella == "S.Anatum")
ser4 <- filter(poultry, Salmonella == "S.Arizonae")
ser5 <- filter(poultry, Salmonella == "S.Bareilly") 
ser6 <- filter(poultry, Salmonella == "S.Binza")
ser7 <- filter(poultry, Salmonella == "S.Braenderup")
ser8 <- filter(poultry, Salmonella == "S.Brandenburg")
ser9 <- filter(poultry, Salmonella == "S.Bredeney")
ser10 <- filter(poultry, Salmonella == "S.Chandans")
ser11 <- filter(poultry, Salmonella == "S.Colorado")
ser12 <- filter(poultry, Salmonella == "S.Derby")
ser13 <- filter(poultry, Salmonella == "S.Eidelberg")
ser14 <- filter(poultry, Salmonella == "S.Give")
ser15 <- filter(poultry, Salmonella == "S.Indiana")
ser16 <- filter(poultry, Salmonella == "S.Java")
ser17 <- filter(poultry, Salmonella == "S.Kaapstad")
ser18 <- filter(poultry, Salmonella == "S.Kilwa")
ser19 <- filter(poultry, Salmonella == "S.Lamberhurst")
ser20 <- filter(poultry, Salmonella == "S.Larochelle")
ser21 <- filter(poultry, Salmonella == "S.Lexington")
ser22 <- filter(poultry, Salmonella == "S.Livingstone")
ser23 <- filter(poultry, Salmonella == "S.Llandoff")
ser24 <- filter(poultry, Salmonella == "S.Lomita")
ser25 <- filter(poultry, Salmonella == "S.London")
ser26 <- filter(poultry, Salmonella == "S.Makiso")
ser27 <- filter(poultry, Salmonella == "S.Manhattan")
ser28 <- filter(poultry, Salmonella == "S.Manila")
ser29 <- filter(poultry, Salmonella == "S.Mbandaka")
ser30 <- filter(poultry, Salmonella == "S.Montevideo")
ser31 <- filter(poultry, Salmonella == "S.Muenster")
ser32 <- filter(poultry, Salmonella == "S.Napoli")
ser33 <- filter(poultry, Salmonella == "S.New-haw")
ser34 <- filter(poultry, Salmonella == "S.Newington")
ser35 <- filter(poultry, Salmonella == "S.Nottingham")
ser36 <- filter(poultry, Salmonella == "S.Ohio")
ser37 <- filter(poultry, Salmonella == "S.Orion")
ser38 <- filter(poultry, Salmonella == "S.Poona")
ser39 <- filter(poultry, Salmonella == "S.Riggel")
ser40 <- filter(poultry, Salmonella == "S.Rissen")
ser41 <- filter(poultry, Salmonella == "S.Ruiru")
ser42 <- filter(poultry, Salmonella == "S.Saint-paul")
ser43 <- filter(poultry, Salmonella == "S.San-diego")
ser44 <- filter(poultry, Salmonella == "S.Sarajane")
ser45 <- filter(poultry, Salmonella == "S.Schwarzengrund")
ser46 <- filter(poultry, Salmonella == "S.Senftenberg")
ser47 <- filter(poultry, Salmonella == "S.Singapore")
ser48 <- filter(poultry, Salmonella == "S.Sofia")
ser49 <- filter(poultry, Salmonella == "S.Tennessee")
ser50 <- filter(poultry, Salmonella == "S.Thompson")





## plot general frequency and density ####
p1 <- ggplot(ser1, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0000") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Agona, n = 23 ") 

p2 <- ggplot(ser2, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Altona, n = 1") 

p3 <- ggplot(ser3, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0000FF") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Anatum, n = 11") 

p4 <- ggplot(ser4, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Arizonae, n = 1") 

p5 <- ggplot(ser5, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FFBF00") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Bareilly, n = 7") 

p6 <- ggplot(ser6, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#BCA9F5") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Binza, n = 5") 

p7 <- ggplot(ser7, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#DF3A01") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Braenderup, n = 8") 

p8 <- ggplot(ser8, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0B3B17") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Brandenburg, n = 14")

p9 <- ggplot(ser9, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#4C0B5F") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Bredeney, n = 13") 

p10 <- ggplot(ser10, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Chandans, n = 1") 

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)

p11 <- ggplot(ser11, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Colorado, n = 1") 

p12 <- ggplot(ser12, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#9AFE2E") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Derby, n = 10") 

p13 <- ggplot(ser, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#00FFFF") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Eidelberg, n = 17") 

p14 <- ggplot(ser, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FA5882") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Give, n = 7") 

p15 <- ggplot(ser15, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FACC2E") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Indiana, n = 24") 

p16 <- ggplot(ser16, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0080FF") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Java, n = 2") 

p17 <- ggplot(ser17, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Kaapstad, n = 1") 

p18 <- ggplot(ser18, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Kilwa, n = 1") 

p19 <- ggplot(ser19, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Lamberhurst, n = 1") 

p20 <- ggplot(ser20, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Larochelle, n = 1") 

grid.arrange(p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, ncol = 2)

p21 <- ggplot(ser21, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#8181F7") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Lexington, n = 2") 

p22 <- ggplot(ser22, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#088A29") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Livingstone, n = 9") 

p23 <- ggplot(ser23, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FE2E64") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Llandoff, n = 2") 

p24 <- ggplot(ser24, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  xlim(1993, 2017) + 
  geom_density(size = 0.7) +
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Lomita, n = 1")

p25 <- ggplot(ser25, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#81F781") + #new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.London, n = 18") 

p26 <- ggplot(ser26, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + 
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Makiso, n = 1") 

p27 <- ggplot(ser27, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + 
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Manhattan, n = 1") 

p28 <- ggplot(ser28, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Manila, n = 1") 

p29 <- ggplot(ser29, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#F7819F") + #new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Mbandaka, n = 971") 

p30 <- ggplot(ser30, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#F7BE81") + #new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Montevideo, n = 26") 

grid.arrange(p21, p22, p23, p24, p25, p26, p27, p28, p29, p30,  ncol = 2)

p31 <- ggplot(ser31, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0101DF") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Muenster, n = 146") 

p32 <- ggplot(ser32, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Napoli, n = 1")

p33 <- ggplot(ser33, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#800000") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.New-haw, n = 11") 

p34 <- ggplot(ser34, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0040") + # newcol
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Newington, n = 10") 

p35 <- ggplot(ser35, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + #new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Nottingham, n = 1") 

p36 <- ggplot(ser36, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF00FF") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Ohio, n = 3") 

p37 <- ggplot(ser37, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#086A87") + # new col
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Orion, n = 20") 

p38 <- ggplot(ser38, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#58ACFA") + #newcol
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Poona, n = 3") 

p39 <- ggplot(ser39, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Riggel, n = 1") 

p40 <- ggplot(ser40, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") + 
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Rissen, n = 1") 

grid.arrange(p31, p32, p33, p34, p35, p36, p37, p38, p39, p40,  ncol = 2)

p41 <- ggplot(ser41, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Ruiru, n = 1") 

p42 <- ggplot(ser42, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FACC2E") +# newcol
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Saint-paul, n = 2") 

p43 <- ggplot(ser43, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0101DF") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.San-diego, n = 3") 

p44 <- ggplot(ser44, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#086A87") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Sarajane, n = 3") 

p45 <- ggplot(ser45, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#8181F7") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Schwarzengrund, n = 22") 

p46 <- ggplot(ser46, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#088A29") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Senftenberg, n = 116") 

p47 <- ggplot(ser47, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "black") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Singapore, n = 1") 

p48 <- ggplot(ser48, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#81F781") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Sofia, n = 2")

p49 <- ggplot(ser49, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FE2E64") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Tennessee, n = 239") 

p50 <- ggplot(ser50, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#800000") +
  xlim(1993, 2017) + 
  geom_density(size = 0.7) +
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Thompson, n = 3")

grid.arrange(p41, p42, p43, p44, p45, p46, p47, p48, p49, p50,  ncol = 2)


## gallinarium summary ####
str(gallin)

# one of these is a replcate
gall <- gallin[1:4, ]

gall1 <- ggplot(gall, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0000") +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Gallinarum, n = 4")

## priority serovars ####

# relative frequency plots
# relative frequency of each of these in relation to all Salmonella observations

sal_year_sum <- serovars %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# filter priority serovars
priority_prop <- semi_join(sal_year_sum, priority, by = "Salmonella")

table(priority_prop$Salmonella)

# rename monophasic to be shorter
priority_prop$Salmonella <- as.character(priority_prop$Salmonella)
priority_prop$Salmonella[priority_prop$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium"

#separate year summary of priority serovars
prop1 <- filter(priority_prop, Salmonella == "S.Enteritidis")
prop2 <- filter(priority_prop, Salmonella == "S.Typhimurium")
prop3 <- filter(priority_prop, Salmonella == "Monophasic Salmonella Typhimurium")
prop4 <- filter(priority_prop, Salmonella == "S.Infantis")
prop5 <- filter(priority_prop, Salmonella == "S.Virchow")
prop6 <- filter(priority_prop, Salmonella == "S.Hadar")

#plot rel freq lines
ggplot(prop1, aes(Year, freq, colour = Salmonella)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(size = 0.75) +
  theme(legend.position = "none") +
  ggtitle("Proportional relative frequency of S.Enteritidis (1995-2017)")
  
#plot rel freq bars
ggplot(priority_prop, aes(Year, freq, fill = Salmonella)) +
  theme_bw() +
    geom_bar(stat="identity") +
    scale_fill_brewer(palette="Set1") +
    ggtitle("Proportional relative frequency of six priority serovars (1995-2017)")


## dublin and kottbus observations ####
dub <- filter(serovars, Salmonella == "S.Dublin")
kott <- filter(serovars, Salmonella == "S.Kottbus")

pt1 <- ggplot(dub, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "red") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Dublin, n = 42") 

pt2 <- ggplot(kott, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "blue") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Kottbus, n = 30") 

grid.arrange(pt1, pt2, ncol = 2)

## AST testing summary


## unspecified and LA chickens ####
table(poultry$Flock)

# filter un and la
unspec <- droplevels(filter(poultry, Flock %in% c("LA", "UN")))

# first look at Moy Park records...
moy_park <- unspec %>%
  filter(grepl("Moy", Owner, ignore.case = TRUE)) %>%
  droplevels

# where are the rest of observations from?
rest_un <- anti_join(unspec, moy_park, by = "ID")

moy_park2 <- rest_un %>%
  filter(grepl("Moy", Company, ignore.case = TRUE)) %>%
  droplevels

rest_un2 <- anti_join(rest_un, moy_park2, by = "ID")

moy_park3 <- rest_un2 %>%
  filter(grepl("Moy", Vet, ignore.case = TRUE)) %>%
  droplevels

# non of these are moy
rest_un3 <- anti_join(rest_un2, moy_park3, by = "ID")  

## summary of Moy park obs
moyp <- rbind(moy_park, moy_park2, moy_park3)

table(moyp$Flock)
range(moyp$Year)

## take out Anser labs records

anser <- rest_un3 %>%
  filter(grepl("anser", Vet, ignore.case = TRUE)) %>%
  droplevels

rest_un4 <- anti_join(rest_un3, anser, by = "ID")

anser2 <- rest_un4 %>%
  filter(grepl("anser", Owner, ignore.case = TRUE)) %>%
  droplevels

rest_un5 <- anti_join(rest_un4, anser2, by = "ID")

ansert <- rbind(anser, anser2)
table(ansert$Flock)
range(ansert$Year)

# afbi vsd
vsd <- rest_un5 %>%
  filter(grepl("vsd", Vet, ignore.case = TRUE)) %>%
  droplevels

table(vsd$Flock)
range(vsd$Year)

rest_un6 <- anti_join(rest_un5, vsd, by = "ID")

newf <- rest_un6 %>%
  filter(grepl("newforge", Vet, ignore.case = TRUE)) %>%
  droplevels

range(newf$Year)

rest_un7 <- anti_join(rest_un6, newf, by = "ID")

dvo <- rest_un7 %>%
  filter(grepl("dvo", Vet, ignore.case = TRUE)) %>%
  droplevels

range(dvo$Year)

rest_un8 <- anti_join(rest_un7, dvo, by = "ID")

omagh <- rest_un8 %>%
  filter(grepl("omagh", Vet, ignore.case = TRUE)) %>%
  droplevels

rest_un9 <- anti_join(rest_un8, omagh, by = "ID")

range(omagh$Year)

midant <- rest_un9 %>%
  filter(grepl("mid", Vet, ignore.case = TRUE)) %>%
  droplevels

range(midant$Year)

rest_un10 <- anti_join(rest_un9, midant, by = "ID")

park <- rest_un10 %>%
  filter(grepl("parkland", Vet, ignore.case = TRUE)) %>%
  droplevels

range(park$Year)

rest_un11 <- anti_join(rest_un10, park, by = "ID")

scull <- rest_un11 %>%
  filter(grepl("scull", Vet, ignore.case = TRUE)) %>%
  droplevels

rest_un12 <- anti_join(rest_un11, scull, by = "ID")

range(rest_un12$Year)
table(rest_un12$Flock)

# add organisation to data, then r bind and summarise by serovar
moyp$organisation <- "moy_park"
dvo$organisation <- "dvo"
ansert$organisation <- "anser"
newf$organisation <- "newforge"
vsd$organisation <- "vsd"
omagh$organisation <- "omagh"
park$organisation <- "parklands"
midant$organisation <- "midantrim"
rest_un12$organisation <- "misc"

orgs <- rbind(moyp, dvo, ansert, newf, vsd, omagh, park, midant, rest_un12)

sals <- data.frame(table(orgs$Salmonella))

serovar <- droplevels(filter(orgs, Salmonella == "S.Derby"))
table(serovar$organisation)


## count summary of ast serovars of interest ####

# count and density plots for these serovars

#separate serovars
ser1 <- filter(poultry, Salmonella == "S.Gold-coast")
ser2 <- filter(poultry, Salmonella == "S.Infantis")
ser3 <- filter(poultry, Salmonella == "S.Kentucky")
ser4 <- filter(poultry, Salmonella == "S.Newport")
ser5 <- filter(poultry, Salmonella == "S.Typhimurium") 
ser6 <- filter(poultry, Salmonella == "Monophasic Salmonella Typhimurium")

p1 <- ggplot(ser1, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0000") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Gold-coast, n = 46") 

p2 <- ggplot(ser2, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#58ACFA") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Infantis, n = 60") 

p3 <- ggplot(ser3, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#0000FF") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Kentucky, n = 121") 

p4 <- ggplot(ser4, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#086A87") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Newport, n = 8") 

p5 <- ggplot(ser5, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FFBF00") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S.Typhimurium, n = 154") 

p6 <- ggplot(ser6, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#BCA9F5") +
  geom_density(size = 0.7) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "Monophasic S.Typhimurium, n = 35") 

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)




## general summary of amr in sensitivity serovars ####

table(sensitivity$Salmonella)

# Count of number of resistant/sensitive/inter accross each observation
ses_t <- droplevels(dplyr::select(sensitivity, Ampicillin:SulphaTrim))
ses_t$resist <- rowSums(ses_t == "R", na.rm = TRUE)
ses_t$sensit <- rowSums(ses_t == "S", na.rm = TRUE)
ses_t$inter <- rowSums(ses_t == "I", na.rm = TRUE)

ses_t$tested <- rowSums(ses_t[ , 25:27])

ast_tests <- ses_t[ , 25:28]

sensitivity <- cbind(sensitivity, ast_tests)

# remove columns with no tests
table(sensitivity$tested)
sens_test <- filter(sensitivity, tested > 0)
table(sens_test$tested)


## write these data out for joint analysis with pigs
str(sens_test)

# only need year, and last four columns
table(sens_test$MultiResistant)
poul_resist <- dplyr::select(sens_test, Year, Salmonella, resist, sensit, inter, tested)

write.csv(poul_resist, "C:/Users/afbi-porters/Documents/Salmonella_gen/resistance_testing/poul_resist.csv")

## plot resistance over time
names(sensitivity)

plot(sensitivity$Year, sensitivity$resist)
abline(lm(sensitivity$resist ~ sens_test$Year)) 

hist(sensitivity$resist)

ggplot(sensitivity, aes(Year, resist)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, colour = "red") +
  labs(y = "Resistant samples")


# model resistance over time
#var > mean so use neg binom 
var(sensitivity$resist)
mean(sensitivity$resist)

mod1 <- glm.nb(resist ~ Year, data = sensitivity)
summary(mod1)
plot(mod1)

# goddness of fit for this mod = 0.94 (needs to be >0.05)
1 - pchisq(summary(mod1)$deviance, 
           summary(mod1)$df.residual
)

## plot number of antimicrobials each serovar is resistant to ####
# see EU AMR report for figures
# similar colours for susceptable (blue), resistant to 1/2 (orange) and then multi (red)

sens_test$resist <- as.factor(sens_test$resist)
sens_test$Salmonella <- as.character(sens_test$Salmonella)

table(sens_test$Salmonella)

#add 'n=' for plotting
sens_test$Salmonella[sens_test$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium, n = 34"
sens_test$Salmonella[sens_test$Salmonella == "S.Gold-coast"] <- "S.Gold-coast, n = 46"
sens_test$Salmonella[sens_test$Salmonella == "S.Infantis"] <- "S.Infantis, n = 26"
sens_test$Salmonella[sens_test$Salmonella == "S.Kentucky"] <- "S.Kentucky, n = 71"
sens_test$Salmonella[sens_test$Salmonella == "S.Newport"] <- "S.Newport, n = 4"
sens_test$Salmonella[sens_test$Salmonella == "S.Typhimurium"] <- "S.Typhimurium, n = 117"

# reorder in order of sensitivity
sens_test$Salmonella <- factor(sens_test$Salmonella, levels = c("S.Newport, n = 4", "S.Infantis, n = 26", "S.Kentucky, n = 71", "Monophasic S.Typhimurium, n = 34", "S.Gold-coast, n = 46", "S.Typhimurium, n = 117"))

ggplot(sens_test, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="No. antimicrobials"))

# plot line graph of no. multidrug resistant (3 + substances) over time
# create column of multi-drugs
# should this be a proportion of those tested?

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

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

summarySE(sens_test, measurevar = "resist", groupvars = c("Year"))

## plot by mean (± error) ####
### maybe trends for each serovar too?
#### some way of looking at cumulation of AMR?

  
#  geom_bar(position = position_fill(reverse = TRUE)) +
 #  guides(fill = guide_legend(reverse = TRUE)) +
  #theme(axis.title.x=element_blank()) +
  #theme(axis.title.y=element_blank()) +
  #guides(fill=guide_legend(title="No. antimicrobials"))

## break ####

## extras ####



# separate data for each serovar
ser1 <- filter(poultry, Salmonella == "S.Agona")
ser2 <- filter(poultry, Salmonella == "S.Bredeney")
ser3 <- filter(poultry, Salmonella == "S.Dublin")#lab contam
ser4 <- filter(poultry, Salmonella == "S.eidelberg")
ser5 <- filter(poultry, Salmonella == "S.Enteritidis") #priority
ser6 <- filter(poultry, Salmonella == "S.Hadar")#priority
ser7 <- filter(poultry, Salmonella == "S.Gallinarum")
ser8 <- filter(poultry, Salmonella == "S.Gold-coast")#amr
ser9 <- filter(poultry, Salmonella == "S.Indiana")
ser10 <- filter(poultry, Salmonella == "S.Infantis")#amr/priority
ser11 <- filter(poultry, Salmonella == "S.Kentucky")#amr
ser12 <- filter(poultry, Salmonella == "S.Kottbus")#lab contam
ser13 <- filter(poultry, Salmonella == "S.London")
ser14 <- filter(poultry, Salmonella == "S.Mbandaka")
ser15 <- filter(poultry, Salmonella == "S.Montevideo")
ser16 <- filter(poultry, Salmonella == "S.Muenster")
ser17 <- filter(poultry, Salmonella == "S.Newport")#amr
ser18 <- filter(poultry, Salmonella == "S.Orion")
ser19 <- filter(poultry, Salmonella == "S.Schwarzengrund")
ser20 <- filter(poultry, Salmonella == "S.Senftenberg")
ser21 <- filter(poultry, Salmonella == "S.Tennessee")
ser22 <- filter(poultry, Salmonella == "S.Typhimurium")#amr/priority
ser23 <- filter(poultry, Salmonella == "S.Virchow")#priority
ser24 <- filter(poultry, Salmonella == "Monophasic Salmonella Typhimurium")#amr/priority

# plot 
ggplot(ser1, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0000") +
  geom_density(size = 1.5) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 15, colour = "black")) +
  theme(axis.title = element_text(size = 15, colour = "black", face = "bold")) +
  labs(title = "S. Agona occurrence") +
  theme(plot.title = element_text(size = rel(3)))


# Maybe try to look at proportional changes in all first, then take out those with too little data?
# collapse into years as rows and serovars as columns? With SE. 

sal_year_sum <- poultry %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# check these...
y_1993 <- filter(sal_year_sum, Year == 1993)
sum(y_1993$freq) # = 1

# remove small proportions?? 
range(sal_year_sum$freq) #0.000685401 0.687500000

## too many serovars!!
#s_props <- filter(sal_year_sum, freq > 0.25) - this doesnt work

table(sal_sum$Freq) # remove serovars w < 100 observations

freq100 <- filter(sal_sum, Freq > 250)
colnames(freq100) <- c("Salmonella", "freq")
sal100 <- semi_join(sal_year_sum, freq100, by = "Salmonella")

table(sal100$Salmonella)

# line graphs of relFreq ~ time for 5 most freqently recorded serovars
ggplot(sal100, aes(Year, freq, colour = Salmonella)) +
  geom_point() +
  geom_line(size = 0.75) +
  facet_grid(. ~ Salmonella) +
  ggtitle("Proportional relative frequency of five most common serovars (1993-2017)")

# stacked bars of relFreq ~ time for 5 most freqently recorded serovars
ggplot(sal100, aes(Year, freq, fill = Salmonella)) + 
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set1") +
  ggtitle("Proportional relative frequency of five most common serovars (1993-2017)")



grid.arrange(p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24,  ncol = 2)

## plot serovars with flock type info ####
### flock stacks - change names

p1 <- ggplot(ser1, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Agona, n = 33 ") 

p2 <- ggplot(ser2, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Bredeney, n = 30 ") 

p3 <- ggplot(ser3, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Dublin, n = 50 ") 

#p4 <- ggplot(ser4, aes(Year, ..count..)) +
# theme_bw() +
#geom_histogram(bins = 24, binwidth = 0.6, fill = "#FF0040") +
#geom_density(size = 0.7) +
#xlim(1993, 2017) + 
#theme(axis.text = element_text(size = 8, colour = "black")) +
#theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
#labs(title = "S. eidelberg, n =  ") 

p5 <- ggplot(ser5, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Enteritidis, n = 1031 ") 

#p6 <- ggplot(ser6, aes(Year, ..count..)) +
# theme_bw() +
#geom_histogram(bins = 24, binwidth = 0.6, fill = "#BCA9F5") +
#geom_density(size = 0.7) +
#xlim(1993, 2017) + 
#theme(axis.text = element_text(size = 8, colour = "black")) +
#theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
#labs(title = "S. Enteritidis vaccine strain ") 

p7 <- ggplot(ser7, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Gallinarum, n = 5 ") 

p8 <- ggplot(ser8, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Gold-coast, n = 46 ")

p9 <- ggplot(ser9, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Indiana, n = 31 ") 

p10 <- ggplot(ser10, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Infantis, n = 197 ") 

p11 <- ggplot(ser11, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Kentucky, n = 197 ") 

p12 <- ggplot(ser12, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Kottbus, n = 30 ") 

p13 <- ggplot(ser13, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. London, n = 20 ") 

grid.arrange(p1, p2, p3, p5, p7, p8, p9, p10, p11, p12, p13, ncol=2)



p14 <- ggplot(ser14, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Mbandaka, n = 1090 ") 

p15 <- ggplot(ser15, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Montevideo, n = 26 ") 

p16 <- ggplot(ser16, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Muenster, n = 146 ") 

p17 <- ggplot(ser17, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Newport, n = 19 ") 

p18 <- ggplot(ser18, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Orion, n = 20 ") 

p19 <- ggplot(ser19, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Schwarzengrund, n = 26 ") 

p20 <- ggplot(ser20, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Senftenberg, n = 116 ") 

p21 <- ggplot(ser21, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Tennessee, 242 ") 

p22 <- ggplot(ser22, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Typhimurium, n = 225 ") 

p23 <- ggplot(ser23, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, aes(fill = Flock)) +
  xlim(1993, 2017) + 
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "S. Virchow, n = 98 ") 

p24 <- ggplot(ser24, aes(Year, ..count..)) +
  theme_bw() +
  geom_histogram(bins = 24, binwidth = 0.6, fill = "#800000") +
  xlim(1993, 2017) + 
  geom_density(size = 1.5) +
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(axis.title = element_text(size = 8, colour = "black", face = "bold")) +
  labs(title = "Monophasic Salmonella Typhimurium, n = 35 ") 

grid.arrange( p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, ncol = 2)



