library(tidyverse)

poultry <- read.csv("C:/Users/afbi-porters/Documents/sallmonella_poultry/poultry_clean.csv")

str(poultry)
table(poultry$Salmonella)

# change name of MST
poultry$Salmonella <- as.character(poultry$Salmonella)
poultry$Salmonella[poultry$Salmonella == "Monophasic Salmonella Typhimurium"] <- "Monophasic S.Typhimurium"
poultry$Salmonella <- as.factor(poultry$Salmonella)

## make bounded year variable ####
range(poultry$Year)
poultry <- mutate(poultry, year_bound = Year - 1994)
range(poultry$year_bound)

# remove 2017 - incomplete data
poultry <- filter(poultry, Year < 2017)
range(poultry$Year)

# remove serovars with fewer than 10 records ####
sers <- data.frame(table(poultry$Salmonella))

sers10 <- filter(sers, Freq > 9)
table(sers10$Freq)
colnames(sers10) <- c("Salmonella", "freq")
poul_10 <- droplevels(semi_join(poultry, sers10, by = "Salmonella"))
table(poul_10$Salmonella)


# relative prortional frequency of all of these vs all of the non NCPs ####

# calculate relative frequency of these serovars amoung all salmonella isolates...
sal_year_sum <- poultry %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# divide these into NCP and non-NCPs 
ncps <- sal_year_sum %>%
  filter(Salmonella %in% c("S.Enteritidis", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Hadar", "S.Infantis", "S.Virchow"))

ncps$ncp <- "NCP"

non_ncps <- sal_year_sum %>%
  filter(!Salmonella %in% c("S.Mbandaka", "S.Enteritidis", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Hadar", "S.Infantis", "S.Virchow"))

non_ncps$ncp <- "non_NCP"

sal_ncp <- rbind(ncps, non_ncps)

# summarise these by year to plot?
ggplot(sal_ncp, aes(Year, freq, colour = ncp)) +
  theme_bw() +
  geom_point(size = 2) +
  ggtitle(" (1995-2017)") +
  geom_smooth(method = 'loess')

m1 <- glm(freq ~ Year, family = 'binomial', data = ncps)
summary(m1)
plot(m1)

# relative proportional frequency of common serovars ####


## proportional changes in NCP serovars ####



# calculate relative frequency of these serovars amoung all salmonella isolates...
sal_year_sum <- poultry %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

pf_sal_sum <- semi_join(sal_year_sum, ncps, by = 'Salmonella')

ncps <- sal_year_sum %>%
  filter(Salmonella %in% c("S.Enteritidis", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Hadar", "S.Infantis", "S.Virchow"))

ser <- filter(ncps, Salmonella == "S.Virchow")

ggplot(ser, aes(Year, freq)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(size = 0.75) +
  ggtitle("Proportional relative frequency of S.Virchow (1995-2017)") + 
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.03)


nonncp_count <- data.frame(table(non_ncps$Salmonella))
nonncp_10 <- filter(nonncp_count, Freq > 9)
colnames(nonncp_10) <- c("Salmonella", "freq")
poul_10 <- droplevels(semi_join(sal_year_sum, nonncp_10, by = "Salmonella"))
table(poul_10$Salmonella)

ser <- filter(poul_10, Salmonella == "S.Tennessee")

ggplot(ser, aes(Year, freq)) +
  theme_bw() +
  geom_point(size = 2) +
  geom_line(size = 0.75) +
  ggtitle("Proportional relative frequency of S.Tennessee (1995-2017)") + 
  geom_rect(aes(xmin = 2004, xmax = 2007, ymin = -Inf, ymax = Inf),
            fill = "pink", alpha = 0.03) +
  geom_smooth(method = 'loess')
