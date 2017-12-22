library(tidyverse)
library(grid)

pigs <- read.csv("C:\\Users\\afbi-porters\\Documents\\salmonella_pigs\\pigs_clean.csv")

sers <- data.frame(table(pigs$Salmonella))
range(pigs$Year)

# relative frequecy of all serovars
rel_freq <- pigs %>%
  group_by(Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

str(pigs)

# plot top 10 serovars temporal changes ####

# relative frequency by year
rel_freq_year <- pigs %>%
  group_by(Year, Salmonella) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

ser <- filter(rel_freq_year, Salmonella  == "S.Typhimurium")

p1 <- ggplot(ser, aes(Year, freq)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FF0000") +
  geom_line(size = 0.75, colour = "#FF0000") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("relative frequency") +
  ggtitle("S.Typhimurium") +
  coord_cartesian(xlim = c(1993, 2017)) +
  theme(text = element_text(size=18, face = "bold")) +
  theme(axis.title.y = element_text(margin = margin(r=30)))

p2 <- ggplot(ser, aes(Year, n)) +
  theme_minimal() +
  geom_point(size = 2, colour = "#FF0000") +
  geom_line(size = 0.75, colour = "#FF0000") +
  ylab("count") +
  theme(text = element_text(size=18, face = "bold")) +
  coord_cartesian(xlim = c(1993, 2017)) +
  theme(axis.title.y = element_text(margin = margin(r=30)))

setwd("C:\\Users\\afbi-porters\\Documents\\a1_papers_reports\\NRL_NI_pigs_ast\\serovar_plots")
tiff(file = "S.Typhimurium.tiff", width = 5.5, height = 5, units = "in", res = 400)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.off()

# number of resistant 
# Count of number of resistant/sensitive/inter accross each observation
ses_t <- droplevels(dplyr::select(pigs, Ampicillin:SulphaTrim))
ses_t$resist <- rowSums(ses_t == "R", na.rm = TRUE)
ses_t$sensit <- rowSums(ses_t == "S", na.rm = TRUE)
ses_t$inter <- rowSums(ses_t == "I", na.rm = TRUE)

ses_t$tested <- rowSums(ses_t[ , 25:27])

ast_tests <- ses_t[ , 25:28]

clean_pigs <- cbind(pigs, ast_tests)

table(clean_pigs$Salmonella)

sens_test <- filter(clean_pigs, tested > 0)
table(sens_test$tested)

top_ten <- droplevels(filter(sens_test, Salmonella %in% c("S.Give", "S.Derby", "S.Typhimurium", "Monophasic S.Typhimurium", "S.Choleraesuis", "S.Gold-coast", "S.Dublin", "S.London", "S.Mbandaka", "S.Bredeney")))

top_ten$Salmonella <- factor(top_ten$Salmonella, levels = c("S.London", "S.Dublin", "S.Gold-coast","S.Bredeney", "S.Mbandaka",   "S.Derby","S.Choleraesuis", "S.Give", "Monophasic S.Typhimurium", "S.Typhimurium"))

top_ten$resist <- as.factor(top_ten$resist)

ggplot(top_ten, aes(Salmonella, fill = resist)) +
  theme_bw() +
  geom_bar(position = position_fill(reverse = TRUE)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill=guide_legend(title="No. antimicrobials"))
