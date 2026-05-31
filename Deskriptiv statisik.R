### -------------------------------------------------------------------------###
###                       SPECIALE I STATSKUNDSKAB                           ###
###                         Deskriptiv statistik                             ###
### -------------------------------------------------------------------------###

# Vi laver deskriptiv statistik på vores stikprøves baggrundsvariable


# Sætter working directory

setwd("C:/Users/fieto/OneDrive/Skole ting/Universitet/Speciale/R")

# Indlæser relevante pakker

library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)


# Indlæser vores behandlede data fra data behandling 

data_DS<- readRDS("conjoint_data.rds")


### ------------------------------ Kommune --------------------------------- ###
# Starter med at ændre vores sample fra absolutte tal til andele 

kommune_sample <- data_DS %>%
  select(participant_id, kommune) %>%
  distinct() %>%
  count(kommune) %>%
  mutate(pct_sample = n / sum(n) * 100) %>%
  filter(!is.na(kommune))

# Visualisering af fordelingen

ggplot(kommune_sample, aes(y = kommune, x = pct_sample)) + 
  geom_col(fill = "cadetblue") + 
  labs(title = "Fordeling af respondenter på kommune i stikprøven", 
       x = "Andele (%)", y = NULL) + 
  theme_minimal() 

# Vi ønsker at sammenligne med seneste Folketingsvalg, hvorfor vi henter data
# fra Danmarks Statistik

kommune_data <- read_excel("Kommune.xlsx")

# Klargør populationsdata ved først at fjerne regionerne

kommune_pop <- kommune_data  %>%
  filter(!kommune %in% c("Region Hovedstaden", "Region Midtjylland", 
                         "Region Nordjylland", "Region Sjælland", 
                         "Region Syddanmark", "Christiansø"))

# Omdanner herefter til andele 

kommune_pop <- kommune_pop %>%
  group_by(kommune) %>%
  summarise(n = sum(Antal, na.rm = TRUE)) %>%
  mutate(pct_pop = n / sum(n) * 100)


# Slår de to datasæt sammen 

compare_kommune <- kommune_sample %>%
  full_join(kommune_pop, by = c("kommune" = "kommune"))

# Gør klar til at plotte ved stable sample og population oven på hinanden 
# i stedet for ved siden af hinanden

combined_kommune <- compare_kommune %>%
  select(kommune, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct") %>%
  mutate(source = recode(source,
                         pct_sample = "Stikprøve",
                         pct_pop = "Population"))

# Laver plottet 

plot_by <- ggplot(combined_kommune, aes(x = kommune, y = pct, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue"),
                    labels = c("Population", "Stikprøve")) +
  labs(title = "Stikprøve vs. befolkning fordelt på kommune",
       y = "Procent (%)", x = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7),
    panel.grid.major.y = element_blank()) +
  coord_flip()

plot_by

# Gemmer det

ggsave("by_plot.png", plot = plot_by, width = 8, height = 12)



### ---------------------------- Bopæl-kategorier ----------------------------- ###

# Grundet hypotesen om bopæl ønsker vi at inddele kommuner i grupperinger
# De er taget fra Danmarks statistik

# Laver grupperne i stikprøven og får det i procent

by_sample <- data_DS %>%
  mutate(by_kategori = case_when(
    kommune %in% c("København", "Frederiksberg", "Ballerup", "Brøndby", "Dragør",
                   "Gentofte", "Gladsaxe", "Glostrup", "Herlev", "Albertslund",
                   "Hvidovre", "Høje-Taastrup", "Lyngby-Taarbæk", "Rødovre",
                   "Ishøj", "Tårnby", "Vallensbæk", "Furesø", "Allerød",
                   "Hørsholm", "Rudersdal", "Egedal", "Greve", "Solrød") ~ "Hovedstadskommuner",
    
    kommune %in% c("Odense", "Aarhus", "Aalborg") ~ "Storbykommuner",
    
    kommune %in% c("Helsingør", "Hillerød", "Køge", "Roskilde", "Slagelse",
                   "Næstved", "Esbjerg", "Fredericia", "Horsens", "Kolding",
                   "Vejle", "Herning", "Holstebro", "Randers", "Silkeborg",
                   "Viborg") ~ "Provinsbykommuner",
    
    kommune %in% c("Fredensborg", "Frederikssund", "Halsnæs", "Gribskov",
                   "Holbæk", "Faxe", "Ringsted", "Stevns", "Sorø", "Lejre",
                   "Middelfart", "Assens", "Faaborg-Midtfyn", "Kerteminde",
                   "Nyborg", "Nordfyns", "Vejen", "Syddjurs", "Favrskov",
                   "Odder", "Skanderborg", "Ikast-Brande", "Hedensted",
                   "Rebild") ~ "Oplandskommuner",
    
    kommune %in% c("Odsherred", "Kalundborg", "Lolland", "Guldborgsund",
                   "Vordingborg", "Bornholm", "Svendborg", "Langeland", "Ærø",
                   "Haderslev", "Billund", "Sønderborg", "Tønder", "Fanø",
                   "Varde", "Aabenraa", "Lemvig", "Struer", "Norddjurs",
                   "Samsø", "Ringkøbing-Skjern", "Morsø", "Skive", "Thisted",
                   "Brønderslev", "Frederikshavn", "Vesthimmerlands", "Læsø",
                   "Mariagerfjord", "Jammerbugt", "Hjørring") ~ "Landkommuner",
    
    TRUE ~ NA_character_
  )) %>%
  select(participant_id, by_kategori) %>%
  distinct() %>%
  filter(!is.na(by_kategori)) %>%
  count(by_kategori) %>%
  mutate(pct_sample = n / sum(n) * 100)

# Visualisering af fordelingen

ggplot(by_sample, aes(y = by_kategori, x = pct_sample)) + 
  geom_col(fill = "cadetblue") + 
  labs(title = "Fordeling af respondenter på kommune i stikprøven", 
       x = "Andele (%)", y = NULL) + 
  theme_minimal() 


# Gør det samme for populationsdata, da vi ønsker at sammenligne med populationen

by_pop <- kommune_data %>%
  mutate(by_kategori = case_when(
    kommune %in% c("København", "Frederiksberg", "Ballerup", "Brøndby", "Dragør",
                   "Gentofte", "Gladsaxe", "Glostrup", "Herlev", "Albertslund",
                   "Hvidovre", "Høje-Taastrup", "Lyngby-Taarbæk", "Rødovre",
                   "Ishøj", "Tårnby", "Vallensbæk", "Furesø", "Allerød",
                   "Hørsholm", "Rudersdal", "Egedal", "Greve", "Solrød") ~ "Hovedstadskommuner",
    
    kommune %in% c("Odense", "Aarhus", "Aalborg") ~ "Storbykommuner",
    
    kommune %in% c("Helsingør", "Hillerød", "Køge", "Roskilde", "Slagelse",
                   "Næstved", "Esbjerg", "Fredericia", "Horsens", "Kolding",
                   "Vejle", "Herning", "Holstebro", "Randers", "Silkeborg",
                   "Viborg") ~ "Provinsbykommuner",
    
    kommune %in% c("Fredensborg", "Frederikssund", "Halsnæs", "Gribskov",
                   "Holbæk", "Faxe", "Ringsted", "Stevns", "Sorø", "Lejre",
                   "Middelfart", "Assens", "Faaborg-Midtfyn", "Kerteminde",
                   "Nyborg", "Nordfyns", "Vejen", "Syddjurs", "Favrskov",
                   "Odder", "Skanderborg", "Ikast-Brande", "Hedensted",
                   "Rebild") ~ "Oplandskommuner",
    
    kommune %in% c("Odsherred", "Kalundborg", "Lolland", "Guldborgsund",
                   "Vordingborg", "Bornholm", "Svendborg", "Langeland", "Ærø",
                   "Haderslev", "Billund", "Sønderborg", "Tønder", "Fanø",
                   "Varde", "Aabenraa", "Lemvig", "Struer", "Norddjurs",
                   "Samsø", "Ringkøbing-Skjern", "Morsø", "Skive", "Thisted",
                   "Brønderslev", "Frederikshavn", "Vesthimmerlands", "Læsø",
                   "Mariagerfjord", "Jammerbugt", "Hjørring") ~ "Landkommuner",
    
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(by_kategori)) %>%
  group_by(by_kategori) %>%
  summarise(n = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_pop = n / sum(n) * 100)

# Slår de to sammen

compare_by <- by_sample %>%
  full_join(by_pop, by = "by_kategori")

# Gør klar til at plotte

combined_by <- compare_by %>%
  select(by_kategori, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct") %>%
  mutate(source = recode(
    source,
    pct_sample = "Stikprøve",
    pct_pop = "Population"))

# Sørger for rækkefølgen bliver korrekt ved at lave det til en faktor

combined_by$by_kategori <- factor(
  combined_by$by_kategori,
  levels = rev(c("Hovedstadskommuner",
             "Storbykommuner",
             "Provinsbykommuner",
             "Oplandskommuner",
             "Landkommuner")))

# Plotter det

plot_by_kategori <- ggplot(combined_by, aes(x = pct, y = by_kategori, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue")) +
  labs(x = "Procent (%)",
    y = NULL,
    fill = NULL) +
  theme_minimal()

plot_by_kategori

# Gemmer plottet

ggsave("by_kategori_plot.png", plot = plot_by_kategori, width = 8, height = 5)


### ------------------------------- Køn ------------------------------------ ###

ggplot(data_DS, aes(y = gender)) + 
  geom_bar(fill = "cadetblue") + 
  labs(title = "Fordeling af alder i stikprøven", 
       x = "Respondenter i stikprøven", y = NULL) + 
  theme_minimal()

# Gør stikprøvedata klar til sammenligningen ved at få det i procent

koen_sample <- data_DS %>%
  select(participant_id, gender) %>%
  distinct() %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(pct_sample = n / sum(n) * 100)

# Henter populationsdata

koen_data <- read_excel("Køn.xlsx")

# Gør populationsdata klar

koen_pop <- koen_data %>%
  filter(!is.na(Køn)) %>%
  group_by(Køn) %>%
  summarise(n = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_pop = n / sum(n) * 100)

# Standardiserer kategorierne så de matcher

koen_pop <- koen_pop %>%
  mutate(Køn = recode(Køn,
                       "Mænd" = "Mand",
                       "Kvinder" = "Kvinde"))

# Slår data sammen til et

compare_koen <- koen_sample %>%
  full_join(koen_pop, by = c("gender" = "Køn"))

# Gør klar til at plotte

combined_koen <- compare_koen %>%
  select(gender, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct"
  ) %>%
  mutate(source = recode(
    source,
    pct_sample = "Stikprøve",
    pct_pop = "Population"
  )) %>%
  filter(!is.na(gender), !is.na(pct))

# Plotter 
plot_køn <- ggplot(combined_koen, aes(x = pct, y = gender, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue")) +
  labs(y = NULL,
    x = "Procent (%)",
    fill = NULL
  ) +
  theme_minimal()

plot_køn

# Gemmer plottet 
ggsave("koen_plot.png", plot = plot_køn, width = 8, height = 5)


### ------------------------------- Alder ---------------------------------- ###
# Vi ønsker at lave kategorier inden for alder, da variablen består af alle
# indtastede aldre, og dermed spænder fra 18-86

# Starter derfor med at gruppere alder og fjerner de tre NA som er den 99 årige
# og de to 15 årige

alder_sample <- data_DS %>%
  mutate(alder_gruppe = case_when(
    alder >= 18 & alder <= 29 ~ "18-29",
    alder >= 30 & alder <= 44 ~ "30-44",
    alder >= 45 & alder <= 59 ~ "45-59",
    alder >= 60 ~ "60+",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(alder_gruppe))

# Andelen af respondenter i hver aldersgruppe

alder_sample <- alder_sample %>%
  select(participant_id, alder_gruppe) %>%
  distinct() %>%
  count(alder_gruppe) %>%
  mutate(pct_sample = n / sum(n) * 100)

# Visualisering af fordelingen

ggplot(alder_sample, aes(y = alder_gruppe, x = pct_sample)) + 
  geom_col(fill = "cadetblue") + 
  labs(title = "Fordeling af respondenter på alder i stikprøven", 
       x = "Respondenter i stikprøven", y = NULL) + 
  theme_minimal() 

# Vi ønsker igen at sammenligne med populationen, og anvender data fra
# Danmarks statistik

alder_data <- read_excel("Alder.xlsx")

# Laver samme gruppering som i stikprøven og sørger for det bliver i %

alder_data_grupperet <- alder_data %>%
  mutate(alder_gruppe = case_when(
    År >= 18 & År <= 29 ~ "18-29",
    År >= 30 & År <= 44 ~ "30-44",
    År >= 45 & År <= 59 ~ "45-59",
    År >= 60 ~ "60+",
    TRUE ~ NA_character_)) %>%
  group_by(alder_gruppe) %>%
  summarise(n = sum(Antal, na.rm = TRUE)) %>%
  mutate(pct_pop = n / sum(n) * 100)


# Slår de to datasæt sammen

compare_alder <- alder_sample %>%
  full_join(alder_data_grupperet, by = "alder_gruppe")


# Gør klar til at plotte ved stable sample og population oven på hinanden 
# i stedet for ved siden af hinanden.

combined_alder <- compare_alder %>%
  select(alder_gruppe, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct") %>%
  mutate(source = recode(
    source,
    pct_sample = "Stikprøve",
    pct_pop = "Population"))

# Og plotter det

plot_alder <- ggplot(combined_alder, aes(y = alder_gruppe, x = pct, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue")) +
  labs(y = NULL,
    x = "Procent (%)",
    fill = NULL) +
  theme_minimal()

plot_alder

# Gemmer plottet

ggsave("alder_plot.png", plot = plot_alder, width = 8, height = 5)

# Samler alder og køn i ét plot

combined_plot_KA <- (plot_køn + plot_alder) +
plot_layout(guides = "collect")

combined_plot_KA

ggsave("alder_køn.png",
       combined_plot_KA,
       width = 8,
       height = 5)


### --------------------------- Uddannelse --------------------------------- ###

# Vi ønsker at vide andelen af respondenter i hver aldersgruppe. Desuden ændres
# navnen, så de bliver kortere. 

uddannelse_sample <- data_DS %>%
  mutate(uddannelse = case_when(
    uddannelse == "Lang videregående uddannelse (5 år eller mere)" ~ "LVU",
    uddannelse == "Kort videregående uddannelse (under 3 år)" ~ "KVU",
    uddannelse == "Mellemlang videregående uddannelse (3-4 år)" ~ "MVU",
    uddannelse == "Erhvervsuddannelse og EUX" ~ "EUD & EUX",
    uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
    uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)" ~ "Folkeskole",
    TRUE ~ NA_character_)) %>%
  select(participant_id, uddannelse) %>%
  distinct() %>%
  filter(!is.na(uddannelse)) %>%
  count(uddannelse)%>%
  mutate(pct_sample = n / sum(n) * 100)

# Visualisering af fordelingen

ggplot(uddannelse_sample, aes(y = uddannelse, x = pct_sample)) + 
  geom_col(fill = "cadetblue") + 
  labs(title = "Fordeling af respondenter på uddannelse i stikprøven", 
       x = "Respondenter i stikprøven", y = NULL) + 
  theme_minimal() 

# Vi ønsker at sammenligne med populationen 

uddannelse <- read_excel("Uddannelse.xlsx")

# Sikre at navnen er de samme og får også populationsdata i andele

uddannelse_pop <- uddannelse %>%
  mutate(uddannelse = case_when(
    uddannelse %in% c("Lange videregående uddannelser, LVU") ~ "LVU",
    uddannelse %in% c("Mellemlange videregående uddannelser, MVU") ~ "MVU",
    uddannelse %in% c("Korte videregående uddannelser, KVU") ~ "KVU",
    uddannelse %in% c("Erhvervsfaglige uddannelser") ~ "EUD & EUX",
    uddannelse %in% c("Gymnasiale uddannelser") ~ "Gymnasium",
    uddannelse %in% c("Grundskole") ~ "Folkeskole",
    TRUE ~ NA_character_)) %>%
  group_by(uddannelse) %>%
  summarise(n = sum(Antal, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_pop = n / sum(n) * 100)

# Slår de to datasæt sammen

compare_uddannelse <- uddannelse_sample %>%
  full_join(uddannelse_pop, by = "uddannelse")

# Gør data klar til ggplot

combined_uddannelse <- compare_uddannelse %>%
  select(uddannelse, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct") %>%
  mutate(source = recode(
    source,
    pct_sample = "Stikprøve",
    pct_pop = "Population"))

# Sikrer rækkefølgen

combined_uddannelse$uddannelse <- factor(
  combined_uddannelse$uddannelse,
  levels = rev(c("Folkeskole",
                 "Gymnasium",
                 "EUD & EUX",
                 "KVU",
                 "MVU",
                 "LVU")))


# Laver plottet 

plot_uddannelse <- ggplot(combined_uddannelse, aes(x = pct, y = uddannelse, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue")) +
  labs(x = "Procent (%)",
    y = NULL,
    fill = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.title = element_text(size = 12, hjust = 0.5))


plot_uddannelse

# Gemmer plottet

ggsave("uddannelse_plot.png", plot = plot_uddannelse, width = 8, height = 5)


### ------------------------------ Parti ----------------------------------- ###
# Starter med at ændre vores sample fra absolutte tal til andele 
# Desuden fjernes "ved ikke" og "jeg har ikke stemmeret" svarene

parti_sample <- data_DS %>%
  select(participant_id, parti) %>%
  distinct() %>%
  count(parti) %>%
  mutate(pct_sample = n / sum(n) * 100) %>%
  filter(!is.na(parti))

# Visualisering af fordelingen

ggplot(parti_sample, aes(y = parti, x = pct_sample)) + 
  geom_col(fill = "cadetblue") + 
  labs(title = "Fordeling af respondenter på partivalg i stikprøven", 
       x = "Andele (%)", y = NULL) + 
  theme_minimal() 

# Vi ønsker at sammenligne med seneste Folketingsvalg, hvorfor vi henter data
# fra Danmarks Statistik

FT26 <- read_excel("FT26.xlsx")

# Klargør populationsdata, så det også bliver i andele 

population <- FT26 %>%
  mutate(
    parti = Parti,
    antal = Antal
  ) %>%
  mutate(pct_pop = antal / sum(antal) * 100)

# Slår de to datasæt sammen og sikre at variablene har samme navne

compare_parti <- parti_sample %>%
  full_join(population, by = "parti")


# Gør data klar til ggplot

combined_parti <- compare_parti %>%
  select(parti, pct_sample, pct_pop) %>%
  pivot_longer(
    cols = c(pct_sample, pct_pop),
    names_to = "source",
    values_to = "pct") %>%
  mutate(source = recode(source,
                    pct_sample = "Stikprøve",
                    pct_pop = "Population"))

# Sørger for at rækkefølgen alfabetisk efter partibogstav

levels_parti <- combined_parti %>%
  distinct(parti) %>%
  pull(parti) %>%
  setdiff(c("Kandidat uden for partierne", "Jeg ville stemme blankt")) %>%
  sort()

# Tilføjer de to værdier uden for parti

levels_parti <- c(levels_parti,
                  "Kandidat uden for partierne",
                  "Jeg ville stemme blankt")

# Anvender som factor

combined_parti$parti <- factor(combined_parti$parti,
                               levels = rev(levels_parti))

# Laver plottet 

plot_parti <- ggplot(combined_parti, aes(x = pct, y = parti, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Population" = "grey", "Stikprøve" = "cadetblue"),
                    labels = c("Population", "Stikprøve")) +
  labs(y = NULL, x = "Procent (%)", fill = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.title = element_text(size = 12, hjust = 0.5))

plot_parti

# Gemmer plottet

ggsave("parti_plot.png", plot = plot_parti, width = 8, height = 5)



