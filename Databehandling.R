### -------------------------------------------------------------------------###
###                       SPECIALE I STATSKUNDSKAB                           ###
###                            Databehandling                                ###
### -------------------------------------------------------------------------###

setwd("/Users/Louise/Desktop/Statskundskab/Speciale/R script")
getwd()

# Indlæser relevante pakker
library(haven)
library(tidyverse)
library(dbplyr)

# Indlæser vores datasæt fra conjointly 
load("Hoveddata.Rdata")

# Vores Hoveddata er en fil, som består af flere datasæt 
# Heriblandt er der tre datasæt, som er relevante for os
# Vi ønsker derfor at samle de tre datasæt til ét

data_r <- rawDataList[["Respondents"]]

# I dette datasæt svarer antallet af rækker til antallet af respondenter. 
# Desuden indeholder datasættet alle baggrundsspørgsmål og rangeringering af 
# kandidater (vores to rating outcomes). 

data_e <- rawDataList[["Experimental design"]]

# Her svarer antallet af rækker per respondentID til antallet af kandidater, hver 
# respondent har set, dvs. hver kandidat ser 6 kandidater. 
# Hver række viser den unikke kandidats karakteristika, dvs. hvilke niveauer i 
# hver attribut kandidaten har. 

data_fc <- rawDataList[["Raw responses"]]

# Antallet af rækker svarer til antallet af respondenter - for hver respondent
# kan man se, hvilken kandidat de har valgt i hver af de tre runder. 

# Vi ønsker at samle de tre datasæt til ét, så vi har ét datasæt hvor alle respondenternes 
# svar på baggrundsspørgsmålene fremgår, den unikke kandidat sammensætning, 
# men også hvordan respondenterne har rangeret kandidaterne, og
# hvem de har valgt ved tvunget valg. 


### --------------------- Samler data til ét datasæt ----------------------- ###

# Vi starter med at omdanne vores forced choice datasæt til at være langt 
# således der er 3 rækker per respondent - en for hver runde 

data_fc_long <- data_fc %>%
  pivot_longer(
    cols = c(q1, q2, q3),
    names_to = "runde",
    names_prefix = "q",
    values_to = "valgt"
  ) %>%
  mutate(runde = as.integer(runde))

# Så merger vi vores forced choice datasæt med datasættet, som indeholder 
# de kandidatprofiler og sammensætninger som respondenterne har set

data_e2 <- data_e %>%
  left_join(data_fc_long, by = c("participant_id" = "participant_id", "QES" = "runde")) %>%
  mutate(chosen = as.integer(ALT == valgt)) %>%
  select(-valgt)

# Nu har vi et datasæt med 6 rækker per respondent, hvor man kan se hvilke
# kandidater de har set, og om de har valgt dem frem for de andre.

# Datasæt data_e og data_fc indeholder kun de respondenter, som har svaret på 
# alle spørgsmål. Data.r indeholder alle, som har svaret, delvist svaret og åbnet det

# Hvis ikke man fuldfører spørgeskemaet, så har Conjointly automatisk givet 
# respondenterne NA på alle spørgsmål, også selv om  de rent faktisk svarede på
# nogle af dem. Derfor koder vi alle dem, som ikke fuldførte spørgeskemaet ud, 
# fordi vi alligevel ikke har data på dem. 


# Først omkoder vi variablenavnet for kommune, så vi derefter kan filtrere på kommune
# Kommune er en af de variable som vi er sikre på alle respondenter har svaret på,
# hvis de har fuldført eksperimentet. 

data_r <- data_r %>%
  rename(kommune = 132)

data_r_clean <- data_r %>%
  filter(!is.na(kommune))

# Nu indeholder datasættet kun de respondenter, som har fuldført surveyet - 1003 respondenter. 


# Vi ønsker nu at omdanne vores rating variable, både på support og voting

# Alle variabelnavne er taget fra Conjointly og meget lange, hvorfor vi bruger kolonne tal
# i stedet

names(data_r_clean)

# Omkodning af rating variable

data_r_clean_2 <- data_r_clean %>%
  rename(
    # Opgave 1
    t1_support_k1 = 166,
    t1_support_k2 = 167,
    t1_voting_k1    = 168,
    t1_voting_k2    = 169,
    # Opgave 2
    t2_support_k1 = 170,
    t2_support_k2 = 171,
    t2_voting_k1    = 172,
    t2_voting_k2    = 173,
    # Opgave 3
    t3_support_k1 = 174,
    t3_support_k2 = 175,
    t3_voting_k1    = 176,
    t3_voting_k2    = 177
  )

# Nu ønsker vi at sikre, at hver eneste opgave med rating forekommer på hver sin række, og 
# at de to outcomes har hver sin kolonne. 
# Derfor laver vi datasættet langt - der er nu 6 rækker pr. respondent. 

data_r_long <- data_r_clean_2 %>%
  select(participant_id, starts_with("t1_"), starts_with("t2_"), starts_with("t3_")) %>%
  pivot_longer(
    cols = -participant_id,
    names_to = c("task", "rating_type", "candidate"),
    names_sep = "_",
    values_to = "rating_value"
  ) %>%
  mutate(
    task      = as.integer(str_remove(task, "t")),
    candidate = as.integer(str_remove(candidate, "k"))
  ) %>%
  pivot_wider(
    names_from  = rating_type,
    values_from = rating_value,
    names_prefix = "rating_"
  )


# Nu kan vi samle hele vores datasæt til et 

conjoint_long <- data_e2 %>%
  left_join(data_r_long, by = c("participant_id", "QES" = "task", "ALT" = "candidate")) %>%
  left_join(
    data_r_clean_2 %>% select(participant_id, 30, 31,32,33,132,139,143,147, 148,165,178 ),
    by = "participant_id"
  )


# Vi tjekker, at konvergeringen er lykkedes, og at respondenterne har præcis 6 rækker hver
# altså 2 kandidater per runde i 3 runder

nrow(conjoint_long) == n_distinct(conjoint_long$participant_id) * 6

# True

conjoint_long %>%
  group_by(participant_id, QES) %>%
  summarise(n_chosen = sum(chosen), .groups = "drop") %>%
  count(n_chosen)

# Den tæller hvor mange opgave kombinationer der er - 3 for hver respondent.
# Idet vi har 1003 respondenter skal det give 3009 - hvilket det gør 

# Samt om der er nogle NAs 
conjoint_long %>% summarise(across(everything(), ~sum(is.na(.))))

# Det er der ikke

# Vores konvertering af datasættet er lykkedes. 


## -------------------------------------------------------------------------- ##
##                   Inspicering af variable og omkodning                     ##
## -------------------------------------------------------------------------- ##

# Vi giver vores variable mere passende og intuitive navne, hvilket også gør det
# nemmere, når vi skal udarbejde vores analyser

# Ser de konkrete nuværende navne i datasættet 
names(conjoint_long)

# Ændrer attributternes navne til mere intuitive 

conjoint_long <- conjoint_long %>% rename(A1_transport = A1)
conjoint_long <- conjoint_long %>% rename(A2_oekonomi = A2)
conjoint_long <- conjoint_long %>% rename(A3_familie = A3)
conjoint_long <- conjoint_long %>% rename(A4_klima = A4)
conjoint_long <- conjoint_long %>% rename(A5_andet = A5)

# Ændrer også navnene på baggrundsvariablene 
conjoint_long <- conjoint_long %>% rename(FC = chosen)
conjoint_long <- conjoint_long %>% rename(alder = 12)
conjoint_long <- conjoint_long %>% rename(uddannelse = 17)
conjoint_long <- conjoint_long %>% rename(seksuel_minoritet = 18)
conjoint_long <- conjoint_long %>% rename(etnisk_race_minoritet = 19)
conjoint_long <- conjoint_long %>% rename(ideologi = 20)
conjoint_long <- conjoint_long %>% rename(parti = 21)
conjoint_long <- conjoint_long %>% rename(open = 22)

conjoint_long <- conjoint_long %>% rename(mand = 13)
conjoint_long <- conjoint_long %>% rename(kvinde = 14)
conjoint_long <- conjoint_long %>% rename(andet = 15)

# Vi gemmer det i et nyt data sæt
conjoint_final <- conjoint_long

##________________________________KØN_________________________________________##

# Grundet opsætningen i Conjointly er der tre variable for køn, men vi ønsker
# kun at have en enkelt, som indeholder de tre kategorier. 

# Vi inspicerer variablene 
table(conjoint_final$mand)
table(conjoint_final$kvinde)
table(conjoint_final$andet)

# Laver den nye køns-variabel

conjoint_final <- conjoint_final %>%
  mutate(gender = case_when(
    mand == "M"   ~ "Mand",
    kvinde == "F" ~ "Kvinde",
    andet ==  "X" ~ "Andet",
    TRUE        ~ NA_character_
  ))

table(conjoint_final$gender)


# Vi vælger at lave det om til en factor, så det er nemmere at arbejde med i analysen

conjoint_final$gender <- factor(conjoint_final$gender,
                               levels = c("Mand", "Kvinde", "Andet"))

# Vi inspicerer den nye variabel
table(conjoint_final$gender)

# Tjekker herefter at det er korrekt
conjoint_final %>%
  group_by(participant_id) %>%
  summarise(n_gender = n_distinct(gender)) %>%
  count(n_gender)

# Det passer - den tæller 1003. 

# Vi konstruerer også en binær variabel for køn. Mand er referencekategorien. 
# Andet kodes NA - vi mister dermed 36 observationer

conjoint_final <- conjoint_final %>%
  mutate(
    gender_b = factor(case_when(
      gender == "Kvinde" ~ "Kvinde",
      gender == "Mand"   ~ "Mand",
      TRUE               ~ NA_character_
    ), levels = c("Mand", "Kvinde"))  
  )

table(conjoint_final$gender_b)
unique(conjoint_final$gender_b)

# Vi laver en dummy variabel for køn, hvor mand er 0 og kvinde er 1

conjoint_final <- conjoint_final %>%
  mutate(
    gender_d = case_when(
      gender == "Kvinde" ~ 1L,
      gender == "Mand"   ~ 0L,
      TRUE               ~ NA_integer_
    )
  )



##----------------------------------Kommune-----------------------------------##

# Inspicerer kommune variablen

table(conjoint_final$kommune)

# Vi har besvarelser fra alle kommuner undtagen Nordfyn

# Vi omdanner til en factor

class(conjoint_final$kommune)
conjoint_final$kommune <- factor(conjoint_final$kommune)

# Vi sætter reference-kategorien

conjoint_final$kommune <- relevel(conjoint_final$kommune, ref = "København")

# Laver en ny variabel, hvor vi grupperer kommuner i regionerne. 

conjoint_final <- conjoint_final %>%
  mutate(region = case_when(
    kommune %in% c("Hjørring","Brønderslev", "Frederikshavn","Jammerbugt", "Læsø", 
                   "Mariagerfjord", "Morsø", "Rebild", "Thisted", "Vesthimmerland", 
                   "Aalborg") ~ "Nordjylland",
    
    kommune %in% c("Favrskov", "Hedensted", "Herning", "Holstebro","Horsens",
                   "Ikast-Brande", "Lemvig", "Norddjurs", "Odder", "Randers", 
                   "Ringkøbing-Skjern","Samsø","Silkeborg","Skanderborg", "Skive", 
                   "Struer", "Syddjurs", "Viborg", "Aarhus") ~ "Midtjylland",
    
    kommune %in% c("Varde", "Billund", "Vejle", "Fanø", "Esbjerg", "Vejen", 
                   "Kolding", "Fredericia", "Tønder","Haderslev", "Aabenraa", 
                   "Sønderborg", "Middelfart", "Nordfyn", "Assens", "Odense", 
                   "Kerteminde", "Nyborg", "Svendborg", "Faaborg-Midtfyn", "Ærø", 
                   "Langeland") ~ "Syddanmark",
    
    kommune %in% c("Faxe", "Greve", "Guldborgsund", "Holbæk", "Kalundborg", "Køge", 
                   "Lejre", "Lolland", "Næstved","Odsherred", "Ringsted", "Roskilde", 
                   "Slagelse", "Solrød", "Sorø", "Stevns", "Vordingborg") ~ "Sjælland",
    
    kommune %in% c("Albertslund", "Allerød", "Ballerup", "Bornholm", "Brøndby", 
                   "Dragør", "Egedal", "Fredensborg", "Frederiksberg", "Frederikssund", 
                   "Furesø", "Gentofte", "Gladsaxe", "Glostrup", "Gribskov", "Halsnæs", "Helsingør",
                   "Herlev", "Hillerød", "Hvidovre", "Høje-Taastrup","Hørsholm", 
                   "Ishøj", "København", "Lyngby-Taarbæk", "Rudersdal",
                   "Rødovre", "Tårnby", "Vallensbæk") ~ "Hovedstaden",
    
    TRUE ~ NA_character_
  ))

# Omdanner til en factor og sætter reference-kategorien

class(conjoint_final$region)

conjoint_final$region <- factor(conjoint_final$region)

conjoint_final$region <- relevel(conjoint_final$region, ref = "Hovedstaden")


# Laver en ny variabel, hvor vi grupperer kommunerne i 5 grupper med inspiration fra Danmarks Statistik

conjoint_final <- conjoint_final %>%
  mutate(kommune_kategori = case_when(
    kommune %in% c("Billund", "Bornholm", "Brønderslev", "Fanø", "Frederikshavn",
                   "Guldborgssund", "Haderslev", "Hjørring","Jammerbugt", "Kalundborg",
                   "Langeland", "Lemvig", "Lolland", "Læsø", "Mariagerfjord", "Morsø", 
                   "Norddjurs", "Odsherred", "Ringkøbing-Skjern", "Samsø", "Skive", 
                   "Struer", "Svendborg", "Sønderborg", "Thisted", "Tønder", "Varde", 
                   "Vesthimmerland", "Vordingborg", "Ærø", "Aabenraa") ~ "Landkommune",
    
    kommune %in% c("Assens", "Favrskov", "Faxe", "Fredensborg", "Frederikssund",
                   "Faaborg-Midtfyn", "Gribskov", "Halsnæs", "Hedensted", "Holbæk",
                   "Ikast-Brande", "Kerteminde", "Lejre", "Middelfart", "Nordfyn",
                   "Nyborg","Odder", "Rebild", "Ringsted", "Skanderborg", "Sorø",
                   "Stevns", "Syddjurs", "Vejen") ~ "Oplandskommune",
    
    kommune %in% c("Esbjerg", "Fredericia", "Helsingør", "Herning", "Hillerød",
                   "Holstebro", "Horsens", "Kolding", "Køge", "Næstved",
                   "Randers", "Roskilde", "Silkeborg", "Slagelse", "Vejle", 
                   "Viborg","Allerød","Egedal","Hørsholm", "Solrød") ~ "Provinsbykommune",
    
    kommune %in% c("Albertslund", "Ballerup", "Brøndby", 
                   "Dragør", "Furesø", "Gentofte", "Gladsaxe", 
                   "Glostrup", "Greve", "Herlev", "Hvidovre", "Høje-Taastrup",
                   "Ishøj", "Lyngby-Taarbæk", "Rudersdal",
                   "Rødovre", "Tårnby", "Vallensbæk") ~ "Storkøbenhavn",
    
    kommune %in% c("Odense", "Aarhus", "Aalborg","København","Frederiksberg") ~ "Storbykommune",
    
    TRUE ~ NA_character_
  ))

class(conjoint_final$kommune_kategori)

# Omdanner til en factor og sætter referencekategorien

conjoint_final$kommune_kategori <- factor(conjoint_final$kommune_kategori)

conjoint_final$kommune_kategori <- relevel(conjoint_final$kommune_kategori, ref = "Storbykommune")


##-------------------------------Uddannelse----------------------------------##

# Inspicerer uddannelse

table(conjoint_final$uddannelse)

# Der er ingen ved ikke svar eller andre ikke-informative svar

# Koder til factor og sætter referencekategorien

class(conjoint_final$uddannelse)
conjoint_final$uddannelse <- factor(conjoint_final$uddannelse)

conjoint_final$uddannelse <- factor(
  conjoint_final$uddannelse,
  levels = c(
    "Grundskole/folkeskole (inkl. 10. klasse)",
    "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)",
    "Erhvervsuddannelse og EUX",
    "Kort videregående uddannelse (under 3 år)",
    "Mellemlang videregående uddannelse (3-4 år)",
    "Lang videregående uddannelse (5 år eller mere)"
  ))


##----------------------------------Alder-------------------------------------##

# Inspicerer alder 

table(conjoint_final$alder)
unique(conjoint_final$alder)

# Tjekker alder er nummerisk

class(conjoint_final$alder)

conjoint_final$alder <- as.numeric(conjoint_final$alder)


# Vi har aldre fra 15 til 99. 
# Da vores målgruppe kun er danske vælgere - de stemmeberettigede, kodes de 15 årige NA
# Derudover har vi kigget nærmere på den 99 åriges svar, og vi mistænker det er en 
# fake besvarelse, hvorfor vedkommende også kodes NA

# Koder alle NA

conjoint_final[conjoint_final$alder %in% c(15, 99),
                    names(conjoint_final) != "participant_id"] <- NA

table(conjoint_final$alder)

# Koder dem ud af datasættet

invalid_rep <- conjoint_final %>%
  group_by(participant_id) %>%
  summarise(all_na = all(is.na(FC))) %>%
  filter(all_na) %>%
  pull(participant_id)

conjoint_final <- conjoint_final %>%
  filter(!participant_id %in% invalid_rep)

nrow(conjoint_final)
# Vi har nu 6000 observationer - vores datasæt indeholder nu svar fra 1000 respondenter. 

# Laver en kategorisk variabel for alder 
conjoint_final <- conjoint_final %>%
  mutate(alder_kategori = factor(case_when(
    alder >= 18 & alder <= 29 ~ "18-29",
    alder >= 30 & alder <= 44 ~ "30-44",
    alder >= 45 & alder <= 59 ~ "45-59",
    alder >= 60               ~ "60+",
    TRUE                      ~ NA_character_
  ), levels = c("18-29", "30-44", "45-59", "60+")))

class(conjoint_final$alder_kategori)
table(conjoint_final$alder_kategori)


##----------------------------------Parti-------------------------------------##

# Inspicerer parti variablen

table(conjoint_final$parti)
unique(conjoint_final$parti)

# Der er både folk som ikke har stemmeret, ville stemme blankt samt ikke ved det.
# Der er 1 som ikke har stemmeret, 19 der ville stemme blankt og 47 som ikke ved det

# Vi beholder kateorien at ville stemme blankt. 
# Vi koder "ved ikke" om til NA
# Da det kun er én person som ikke har stemmeret, koder vi det til NA. 

# Inden vi koder ved-ikke svar om til NA, vælger vi at undersøge, hvordan ved-ikke svarene 
# fordeler sig på nogle centrale baggrundsvariable som køn, uddannelse og region

 conjoint_final %>%
  filter(parti=="Ved ikke" &!is.na(gender))%>%
  group_by(gender)%>%
  summarise( antal =n())%>%
  mutate(per = antal/sum(antal)*100)

# Der er flest kvinder, som har sagt ved ikke ved partivalg

conjoint_final %>%
  filter(parti=="Ved ikke" &!is.na(uddannelse))%>%
  group_by(uddannelse)%>%
  summarise( antal =n())%>%
  mutate(per = antal/sum(antal)*100)

# Det er særligt folk med mellemlang og langvideregående uddannelse om har sagt "ved ikke"

conjoint_final %>%
  filter(parti=="Ved ikke" &!is.na(region))%>%
  group_by(region)%>%
  summarise( antal =n())%>%
  mutate(per = antal/sum(antal)*100)

# Det er særligt folk fra Hovedstads-regionen som har sagt "ved ikke". 


# Vi har i forvejen en overrepræsentation af kvinder, højtuddannede og folk fra Københvan i vores stikprøve


# Koder ved ikke og ikke stemmeret til NA for partivalg

conjoint_final <- conjoint_final %>%
  mutate(
    parti = case_when(
      parti %in% c("Ved ikke", "Jeg har ikke stemmeret") ~ NA,
      TRUE ~ parti
    )
  )

table(conjoint_final$parti)
unique(conjoint_final$parti)

# Koder parti til factor

conjoint_final$parti<- factor(conjoint_final$parti)


# Laver ny parti-variabel for de to blokke og midten. 

conjoint_final <- conjoint_final %>%
  mutate(
    parti_kategori = factor(case_when(
      parti %in% c("Ø - Enhedslisten", "F - SF - Socialistisk Folkeparti", 
                   "Å - Alternativet", "B - Radikale Venstre", 
                   "A - Socialdemokratiet")    ~ "Rød blok",
      parti %in% c("M - Moderaterne")       ~ "Midten",
      parti %in% c("O - Dansk Folkeparti", "V - Venstre", 
                   "C - Det Konservative Folkeparti", "I - Liberal Alliance", "Æ - Danmarksdemokraterne",
                   "H - Borgernes Parti")   ~ "Blå blok",
      TRUE ~ NA_character_
    ), levels = c("Rød blok", "Midten", "Blå blok"))
  )

class(conjoint_final$parti_kategori)
table(conjoint_final$parti_kategori)


##----------------------------------Ideologi----------------------------------##

# Inspicerer ideologi

table(conjoint_final$ideologi)
unique(conjoint_final$ideologi)

# Vi kan se, at vi skal omkode vores ideologi-skala. Vores skala går fra 0-10, 
# men i R-data går den fra 1-11, dvs. har man sagt 10 i surveyen, har man 11 i data

class(conjoint_final$ideologi)

# Ideologi er en character - det skal laves om, da vi kan behandle den som intervalskaleret. 

conjoint_final$ideologi <- as.numeric(conjoint_final$ideologi)

# Vi omkoder ideologi

conjoint_final$ideologi <- dplyr::recode(conjoint_final$ideologi,
      '1'=0,'2'=1,'3'=2,'4'=3,'5'=4,'6'=5,'7'=6,'8'=7,'9'=8,'10'=9,'11'=10)

table(conjoint_final$ideologi)


# Vi laver en nu variabel, hvor vi opdeler ideologi i 3 kategorier 

conjoint_final <- conjoint_final %>%
  mutate(
    ideologi_kategori = factor(case_when(
      ideologi %in% c(0, 1, 2, 3)    ~ "Venstre",
      ideologi %in% c(4, 5, 6)       ~ "Centrum",
      ideologi %in% c(7, 8, 9, 10)   ~ "Højre",
      TRUE ~ NA_character_
    ), levels = c("Venstre", "Centrum", "Højre"))
  )

table(conjoint_final$ideologi_kategori)

class(conjoint_final$ideologi_kategori)


##--------------------------Seksuel Minoritet---------------------------------##

# Inspicerer seksuel minoritet 

table(conjoint_final$seksuel_minoritet)
unique(conjoint_final$seksuel_minoritet)

# Der er Ja, Nej og ønsker ikke at svare.
# Der er 17, som ikke har ønsket at svare. 

# Vi koder "ønsker ikke at svare" til NA. 

conjoint_final <- conjoint_final %>%
  mutate(
    seksuel_minoritet = case_when(
      seksuel_minoritet == "Ønsker ikke at svare" ~ NA,
      TRUE ~ seksuel_minoritet
    )
  )

table(conjoint_final$seksuel_minoritet)

# Vi koder til factor og sætter referencekategorien
class(conjoint_final$seksuel_minoritet)
conjoint_final$seksuel_minoritet <- factor(conjoint_final$seksuel_minoritet)

conjoint_final$seksuel_minoritet <- relevel(conjoint_final$seksuel_minoritet, ref = "Nej")

##------------------------------Race/etnisk minoritet-------------------------##

# Inspicerer Race/etnisk minoritet

table(conjoint_final$etnisk_race_minoritet)
unique(conjoint_final$etnisk_race_minoritet)

# Der er Ja, Nej og ønsker ikke at svare.
# Der er 12, som ikke har ønsket at svare. 

# Vi koder "ønsker ikke at svare" til NA. 

conjoint_final <- conjoint_final %>%
  mutate(
    etnisk_race_minoritet = case_when(
      etnisk_race_minoritet == "Ønsker ikke at svare" ~ NA,
      TRUE ~ etnisk_race_minoritet
    )
  )

table(conjoint_final$etnisk_race_minoritet)

# Vi koder til factor og sætter referencekategorien

class(conjoint_final$etnisk_race_minoritet)
conjoint_final$etnisk_race_minoritet <- factor(conjoint_final$etnisk_race_minoritet)

conjoint_final$etnisk_race_minoritet <- relevel(conjoint_final$etnisk_race_minoritet, ref = "Nej")

##------------------------------Treatment Variable----------------------------##

# Nu kigger vi på vores treatment-variable - dsv. de 5 attribut-variable

# Atrribut 1 - Transport
table(conjoint_final$A1_transport)
unique(conjoint_final$A1_transport)

# Der er 4 niveauer - de optræder næsten alle sammen lige mange gange, hvilket 
# tyder på at den uniforme randomisering er lykkedes

class(conjoint_final$A1_transport)
# Det er allerede en factor


# Attribut 2 - Økonomi
table(conjoint_final$A2_oekonomi)
unique(conjoint_final$A2_oekonomi)

# Der er 5 niveauer - de optræder næsten alle sammen lige mange gange

class(conjoint_final$A2_oekonomi)

# Det er en factor


# Attribut 3 - Familie & Børn
table(conjoint_final$A3_familie)
unique(conjoint_final$A3_familie)

# Der er 10 niveauer - de optræder næsten alle sammen lige mange gange
# De først 5 er identitære, de sidste fem er ikke-identitære.

class(conjoint_final$A3_familie)

# Det er en factor

# Vi definerer reference-kategorien, så den bliver ikke-identitær (A3L10)

conjoint_final$A3_familie <- factor(
  conjoint_final$A3_familie,
  levels = c("A3L10", "A3L9", "A3L8", "A3L7", "A3L6", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)


# Attribut 4 - Klima & Miljø
table(conjoint_final$A4_klima)
unique(conjoint_final$A4_klima)

# Der er 5 niveauer - de optræder næsten alle sammen lige mange gange

class(conjoint_final$A4_klima)

# Det er en factor


# Attribut 5 - Andet

table(conjoint_final$A5_andet)
unique(conjoint_final$A5_andet)

# Der er 11 niveauer - de optræder næsten alle sammen lige mange gange
# De første 6 er identiære, de sidste 5 er ikke-identitære. 

class(conjoint_final$A5_andet)
# Det er en factor

# Vi definerer reference-kategorien, så den bliver ikke-identitær (A5L11)

conjoint_final$A5_andet <- factor(
  conjoint_final$A5_andet,
  levels = c("A5L11", "A5L10","A5L9", "A5L8", "A5L7", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)



# Vi skal nu konstruere to nye variable for attributterne med identitetspolitik
# Det skyldes vores power-problem, som vi tidligere har adresseret samt at vi 
# konceptuelt mener, at de forskellige policies er grundlæggende udtryk for det samme begreb. 


# Familie & Børn - de 10 niveaer skal kollapses til 2 niveauer - identitære
# og ikke-identirære policies. 

conjoint_final <- conjoint_final %>%
  mutate(
    A3_familie_ny = case_when(
      A3_familie %in% c("A3L1", "A3L2", "A3L3", "A3L4", "A3L5") ~ "A3_identitet",
      A3_familie %in% c("A3L6", "A3L7", "A3L8", "A3L9", "A3L10") ~ "A3_ikke_identitet",
      TRUE  ~ NA_character_  
    ) )

table(conjoint_final$A3_familie_ny)

class(conjoint_final$A3_familie_ny)

# Omkoder til factor og sætter referencekategorien

conjoint_final <- conjoint_final %>%
  mutate(
    A3_familie_ny = factor(A3_familie_ny, levels = c("A3_ikke_identitet", "A3_identitet"))
  )


# Andet - de 11 niveaer skal kollapses til 2 niveauer - identitære
# og ikke-identirære policies. 

conjoint_final <- conjoint_final %>%
  mutate(
    A5_andet_ny = case_when(
      A5_andet %in% c("A5L1", "A5L2", "A5L3", "A5L4", "A5L5", "A5L6") ~ "A5_identitet",
      A5_andet %in% c("A5L7", "A5L8", "A5L9", "A5L10", "A5L11") ~ "A5_ikke_identitet",
      TRUE  ~ NA_character_  
    ) )

table(conjoint_final$A5_andet_ny)

class(conjoint_final$A5_andet_ny)


# Omkoder til factor og sætter reference-kategorien

conjoint_final <- conjoint_final %>%
  mutate(
    A5_andet_ny = factor(A5_andet_ny, levels = c("A5_ikke_identitet", "A5_identitet"))
  )

class(conjoint_final$A5_andet_ny)

##------------------------------Outcome Variable------------------------------##

# Inspicerer vores tre outcome-variable

# Vores Tvunget Valg variabel

table(conjoint_final$FC)
unique(conjoint_final$FC)

# Da man skulle vælge en af kandidaterne, er der kun to værdier: 0 og 1
# O for ikke valgt og 1 for valgt. 

class(conjoint_final$FC)
unique(conjoint_final$FC)

# Inspicerer rating outcome - støtte til policies

table(conjoint_final$rating_support)
unique(conjoint_final$rating_support)

# Det er en skala fra 1 til 7 - det ser fint ud. 
# Igen er der ingen ikke-informative svar, da det ikke var en mulighed 

class(conjoint_final$rating_support)

# Den læses som nummerisk

# Inspicerer rating outcome - stemmeintention

table(conjoint_final$rating_voting)
unique(conjoint_final$rating_voting)

# Det er en skala fra 1 til 7 - det ser fint ud. 
# Igen er der ingen ikke-informative svar, da det ikke var en mulighed. 

class(conjoint_final$rating_voting)

# Den læses som nummerisk


# Vi gemmer vores rensede datasæt

saveRDS(conjoint_final, file = "conjoint_data.rds")
