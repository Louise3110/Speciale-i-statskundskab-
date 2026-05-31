### -------------------------------------------------------------------------###
###                       SPECIALE I STATSKUNDSKAB                           ###
###                            Robusthedstests                               ###
### -------------------------------------------------------------------------###

## --------------------------- Randomisering----------------------------------##

#Vi tjekker om, vores randomisering er lykkedes. 

# Vi tjekker først fordelingen af atrributniveauerne

transport <- data %>%
  group_by(A1_transport) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Transport") %>%
  rename(level = A1_transport)

oekonomi <- data %>%
  group_by(A2_oekonomi) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Økonomi") %>%
  rename(level = A2_oekonomi)

familie <- data %>%
  group_by(A3_familie) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Familie & Børn") %>%
  rename(level = A3_familie)

klima <- data %>%
  group_by(A4_klima) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Klima & Miljø") %>%
  rename(level = A4_klima)

andet <- data %>%
  group_by(A5_andet) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Andet") %>%
  rename(level = A5_andet)


# Vi samler de 5 datasæt
samle <- bind_rows(transport, oekonomi, familie, klima, andet) %>%
  mutate(
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    )
  )

samle <- samle %>%
  mutate(
    feature = factor(feature, levels = c(
      "Transport", "Økonomi", "Familie & Børn", "Klima & Miljø", "Andet"
    )),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik", "Cykelinfrastrukturen", "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse", "Formueskat", "Forøgelse af SU", "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn", "Fire juridiske forældre", "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse", "Akut hjælp til PPR",
      "75% uddannede pædagoger", "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol", "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader", "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre", "Udvidelse af valgretten",
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter",
      "Handleplan mod hadforbrydelser", "Udvidelse af EP-valgret",
      "Center for demokratiudvikling", "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  )

# Vi plotter det 
plot_randomisering <- samle %>%
  filter(!is.na(level)) %>%
  ggplot(aes(x = andel, y = level, label = round(andel, 1))) +
  geom_col(fill = "cadetblue") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  geom_text(hjust = -0.2, size = 3) +
  ylab("") +
  xlab("Procent") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 6),
    strip.text.y.left = element_text(angle = 0, size = 9),
    strip.placement = "outside",
    strip.background = element_blank()
  ) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y")


plot_randomisering

# Gemmer plottet
ggsave("Fordeling_attribut.png",
       plot = plot_randomisering,
       width = 6,
       height = 8,
       units = "in")


# Vi laver det samme plot  med det to kollapsede identitetsattributter

familie_1 <- data %>%
  group_by(A3_familie_ny) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Familie & børn") %>%
  rename(level = A3_familie_ny)


andet_1 <- data %>%
  group_by(A5_andet_ny) %>%
  tally() %>%
  mutate(andel = (n / nrow(data)) * 100,
         feature = "Andet") %>%
  rename(level = A5_andet_ny)


# Vi samler
samle_1 <- bind_rows(transport,oekonomi, familie_1, klima, andet_1)

# Vi plotter det

plot_randomisering_1 <- samle_1 %>%
  filter(!is.na(level)) %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)),
                label = round(andel, 1))) +
  geom_col(fill = "cadetblue") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 110)) +
  geom_text(position = position_dodge(width = .9),
            hjust = -0.2, 
            size = 3) +
  ylab("") +
  xlab("Procent") +
  labs(title = "Fordelinger") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 10),
    axis.title.x = element_text(size = 6),
    strip.text.y.left = element_text(angle = 0, size = 9),
    strip.placement = "outside",
    strip.background = element_blank()  
  )

# Gemmer plottet
ggsave("Fordeling_attribut_1.pdf",
       plot = plot_randomisering_1,
       width = 6,
       height = 8,
       units = "in")


# Balancetest
# Vi tjekker for samvariation mellem baggrundskarakteristika (køn, uddannelse, ideologi og alder) og attributniveauer. 
# Vi estimerer MM modeller og inkluderer de 4 variable.

balance_gender <- cj(data,
                     gender_d ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                     estimate = "mm",
                     id = ~participant_id)


balance_ideologi <- cj(data,
                       ideologi ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                       estimate = "mm",
                       id = ~participant_id)

balance_alder <- cj(data,
                    alder ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                    estimate = "mm",
                    id = ~participant_id)

# Vi omkoder uddannelelse til en dummy variabel - alle med videregående uddannelse bliver 1

data <- data %>%
  mutate(
    videregående_dummy = as.integer(case_when(
      uddannelse %in% c("Mellemlang videregående uddannelse (3-4 år)",
                        "Kort videregående uddannelse (under 3 år)",
                        "Lang videregående uddannelse (5 år eller mere)") ~ 1L,
      uddannelse %in% c("Grundskole/folkeskole (inkl. 10. klasse)",
                        "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)",
                        "Erhvervsuddannelse og EUX") ~ 0L,
      TRUE ~ NA_integer_
    ))
  )

# Tjekker at omkodningen er lykkedes

table(data$videregående_dummy)

table(data$uddannelse)


balance_uddannelse <- cj(data,
                  videregående_dummy ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                  estimate = "mm",
                  id = ~participant_id)


# Vi giver hvert outcome det rigige label
balance_gender  <- balance_gender  %>% as_tibble() %>% mutate(outcome = "Kvinde")
balance_ideologi <- balance_ideologi %>% as_tibble() %>% mutate(outcome = "Ideologi")
balance_alder   <- balance_alder   %>% as_tibble() %>% mutate(outcome = "Alder")
balance_uddannelse     <- balance_uddannelse     %>% as_tibble() %>% mutate(outcome = "Uddannelse")

# Vi kombinerer alle resultaterne fra modellerne
balance_data <- bind_rows(balance_gender, balance_ideologi, balance_alder, balance_uddannelse)

# Vi plotter det
balancetest <- balance_data %>%
    mutate(
      feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie"    ~ "Familie & Børn",
      feature == "A4_klima"      ~ "Klima & Miljø",
      feature == "A5_andet"      ~ "Andet",
      TRUE ~ as.character(feature)
),
        
      level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
      ),
      level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
      )))
      ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_point(size = 2, color = "cadetblue") +
  geom_errorbar(aes(xmin = lower, xmax = upper),
    width = 0.2,
    linewidth = 0.4,
    orientation = "y",
    color = "cadetblue") + 
  facet_grid(feature ~ outcome, scales = "free") +
  labs(
    x = "MM",
    y = NULL) +
  theme_bw(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

  
# Gemmer plottet
ggsave("balancetest.png",
       plot = balancetest,
       width = 6,
       height = 8,
       units = "in")


# Vi laver en formel chi-i anden test for de kategoriske variable

# Køn
chisq.test(table(data$gender_b, data$A3_familie))
chisq.test(table(data$gender_b, data$A5_andet))
chisq.test(table(data$gender_b, data$A1_transport))
chisq.test(table(data$gender_b, data$A2_oekonomi))
chisq.test(table(data$gender_b, data$A4_klima))

#Uddannelse dummy
chisq.test(table(data$videregående_dummy, data$A3_familie))
chisq.test(table(data$videregående_dummy, data$A5_andet))
chisq.test(table(data$videregående_dummy, data$A1_transport))
chisq.test(table(data$videregående_dummy, data$A2_oekonomi))
chisq.test(table(data$videregående_dummy, data$A4_klima))

# Udannelse kategorisk
chisq.test(table(data$uddannelse, data$A3_familie))
chisq.test(table(data$uddannelse, data$A5_andet))
chisq.test(table(data$uddannelse, data$A1_transport))
chisq.test(table(data$uddannelse, data$A2_oekonomi))
chisq.test(table(data$uddannelse, data$A4_klima))


# Omnibus F (ANOVA) test for de intervalskalerede variable

# Alder 
model_balance_alder <- lm(alder ~ A1_transport + A2_oekonomi + A3_familie + 
                            A4_klima + A5_andet,
                          data = data)

anova(lm(alder ~ 1, data = data), model_balance_alder)


# Ideologi 
model_balance_ideologi <- lm(ideologi ~ A1_transport + A2_oekonomi + A3_familie + 
                               A4_klima + A5_andet,
                             data = data)

anova(lm(ideologi ~ 1, data = data), model_balance_ideologi)



##---------------------ANTAGELSER FOR CONJOINT EKSPERIMENTER------------------##

# Antagelse : Test af carry-over effekt - treatment i opgave 1 videreføres ikke til de
# næste runder/opgaver


# Vi laver 3 modeller, således vi udregner AMCEs for hver runde 

# Tvunget valg

model_opg1 <- cj(data %>% filter(QES == 1),
                  FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                  estimate = "amce",
                  id = ~participant_id)

model_opg2 <- cj(data %>% filter(QES == 2),
                  FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                  estimate = "amce",
                  id = ~participant_id)

model_opg3 <- cj(data %>% filter(QES == 3),
                  FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                  estimate = "amce",
                  id = ~participant_id)


model_opg1 <- model_opg1 %>% as_tibble() %>% mutate(task = "Runde 1")
model_opg2 <- model_opg2 %>% as_tibble() %>% mutate(task = "Runde 2")
model_opg3 <- model_opg3 %>% as_tibble() %>% mutate(task = "Runde 3")

# Kombinerer data fra de tre modeller
carryover_data <- bind_rows(model_opg1, model_opg2, model_opg3)

# Vi plotter det 
carryover_plot_amce <- carryover_data %>%
  mutate(
  feature = case_when(
    feature == "A1_transport"  ~ "Transport",
    feature == "A2_oekonomi"   ~ "Økonomi",
    feature == "A3_familie" ~ "Familie og Børn",
    feature == "A4_klima"      ~ "Klima og Miljø",
    feature == "A5_andet"   ~ "Andet",
    TRUE ~ as.character(feature)
  ),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = task)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Runde", title = "Tvunget Valg") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )
  


# Gemmer plot
ggsave("carryover_amce.png",
       plot = carryover_plot_amce ,
       width = 6,
       height = 10,
       units = "in")


# Omdanner QES til en faktor - det er variablen der bestemmer opgave-nummeret

data <- data %>%
  mutate(QES = factor(QES))

class(data$QES)

# Vi laver en formel test for at se om AMCE'er varierer på tværs af opgaver 

cj_anova(data,
         FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~QES)



# Policystøtte

model_opg1_RS <- cj(data %>% filter(QES == 1),
                 rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                 estimate = "amce",
                 id = ~participant_id)

model_opg2_RS <- cj(data %>% filter(QES == 2),
                 rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                 estimate = "amce",
                 id = ~participant_id)

model_opg3_RS <- cj(data %>% filter(QES == 3),
                 rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                 estimate = "amce",
                 id = ~participant_id)


model_opg1_RS <- model_opg1_RS %>% as_tibble() %>% mutate(task = "Runde 1")
model_opg2_RS <- model_opg2_RS %>% as_tibble() %>% mutate(task = "Runde 2")
model_opg3_RS <- model_opg3_RS %>% as_tibble() %>% mutate(task = "Runde 3")

# Kombinerer data fra de tre modeller
carryover_data_RS <- bind_rows(model_opg1_RS, model_opg2_RS, model_opg3_RS)

# Vi plotter det 
carryover_plot_amce_RS <- carryover_data_RS %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet",
      TRUE ~ as.character(feature)
    ),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = task)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Runde", title = "Policystøtte") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )
  
  
# Gemmer plot
ggsave("carryover_amce_RS.pdf",
       plot = carryover_plot_amce_RS ,
       width = 6,
       height = 8,
       units = "in")


# Vi laver en formel test for at se om AMCE'er varierer på tværs af opgaver 

cj_anova(data,
         rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~QES)


# Stemmesandsynlighed 

model_opg1_RV <- cj(data %>% filter(QES == 1),
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                    estimate = "amce",
                    id = ~participant_id)

model_opg2_RV <- cj(data %>% filter(QES == 2),
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                    estimate = "amce",
                    id = ~participant_id)

model_opg3_RV <- cj(data %>% filter(QES == 3),
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                    estimate = "amce",
                    id = ~participant_id)


model_opg1_RV <- model_opg1_RV %>% as_tibble() %>% mutate(task = "Runde 1")
model_opg2_RV <- model_opg2_RV %>% as_tibble() %>% mutate(task = "Runde 2")
model_opg3_RV <- model_opg3_RV %>% as_tibble() %>% mutate(task = "Runde 3")

# Kombinerer data fra de tre modeller
carryover_data_RV <- bind_rows(model_opg1_RV, model_opg2_RV, model_opg3_RV)

# Vi plotter det 
carryover_plot_amce_RV <- carryover_data_RV %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet",
      TRUE ~ as.character(feature)
    ),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = task)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Runde", title = "Stemmesandsynlighed") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )


# Gemmer plot
ggsave("carryover_amce_RV.pdf",
       plot = carryover_plot_amce_RV ,
       width = 6,
       height = 8,
       units = "in")


# Vi laver en formel test for at se om AMCE'er varierer på tværs af opgaver 

cj_anova(data,
         rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~QES)


# Samler de tre carryover plots i et:

combined_carryover <- carryover_plot_amce + carryover_plot_amce_RS + carryover_plot_amce_RV +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )


# Gemmer plot
ggsave("carryover_combined.png",
       combined_carryover,
       width = 14,
       height = 10)


# Idet vi fandt problemer for Tvungte Valg med carryover effects vælger vi
# nu kun at kigge på data fra første runde, og se om det skiller sig meget ud
# fra vores overordnede resultater for hypotese 1. 

data_runde1 <- data %>%
  filter(QES==1)

model_FC_amce_runde1 <- cj(data_runde1,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)

model_FC_amce_runde1 %>% as_tibble()


# Gemmer som tabel
table_FC_amce_runde1 <- model_FC_amce_runde1 %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error) %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"   ~ "Transport",
      feature == "A2_oekonomi"    ~ "Økonomi",
      feature == "A3_familie_ny"  ~ "Familie og børn",
      feature == "A4_klima"       ~ "Klima og miljø",
      feature == "A5_andet_ny"    ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ level  
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 2.58 ~ "***",
      abs(estimate / std.error) > 1.96 ~ "**",
      abs(estimate / std.error) > 1.64 ~ "*",
      TRUE ~ ""
    ),
    # 
    effect = case_when(
      is.na(std.error) ~ sprintf("%.3f (ref)", estimate),
      TRUE ~ paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
    )
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) - Runde 1") %>%
  tab_footnote("Note: * p<0.10, ** p<0.05, *** p<0.01. Klyngerobustestandardfejl per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# Gemmer som pdf
gtsave(table_FC_amce_runde1, "Amce_FC_table_runde1.pdf")

# Vi plotter det  
plot_FC_AMCE_runde1 <- model_FC_amce_runde1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies", "Identitære policies"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "AMCE for runde 1"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )


# Hypotese 1 - pooled for alle runder

model_FC_amce <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)

plot_FC_AMCE_1 <- model_FC_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies", "Identitære policies"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "AMCE for alle runder"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )


# Vi kombinerer AMCE for alle runder med AMCE for runde 1
runde1 <- plot_FC_AMCE_runde1 | plot_FC_AMCE_1

ggsave("runde1.png",
       plot = runde1,
       width = 12,
       height = 6,
       units = "in",
       device = "png")

##----------------------------------------------------------------------------##

# Antagelse: Test af profile-order effects - rækkefølgen på profilerne betyder ikke noget. 
# Dvs. hvis man byttede om på rækkefølgen, så skulle responden være konsekvent og vælge den samme profil.

# Vi bruger nu ALT-variablen, som indikerer, om det er kanddiat 1 eller 2. Vi skal se om effekterne
# varierer efter om profilen er vist først eller sidst. 

data <- data %>%
  mutate(ALT = factor(ALT))

# Tvunget Valg

model_profile_order_amce_FC <- cj(data,
                          FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                          estimate = "amce",
                          id = ~participant_id,
                          by = ~ALT)

plot_data_order_FC <- model_profile_order_amce_FC %>%
  as_tibble()


# Vi plotter det
profile_order_amce_FC <- plot_data_order_FC %>%
  mutate(
  feature = case_when(
    feature == "A1_transport"  ~ "Transport",
    feature == "A2_oekonomi"   ~ "Økonomi",
    feature == "A3_familie" ~ "Familie og Børn",
    feature == "A4_klima"      ~ "Klima og Miljø",
    feature == "A5_andet"   ~ "Andet",
    TRUE ~ as.character(feature)),
  level = case_when(
    level == "A1L1"  ~ "Kollektiv trafik",
    level == "A1L2"  ~ "Cykelinfrastrukturen",
    level == "A1L3"  ~ "Klimaafgift på luftfart",
    level == "A1L4"  ~ "Skærpelse af miljøzoner",
    level == "A2L1"  ~ "Officiel fattigdomsgrænse",
    level == "A2L2"  ~ "Formueskat",
    level == "A2L3"  ~ "Forøgelse af SU",
    level == "A2L4"  ~ "Dagpengegaranti",
    level == "A2L5"  ~ "Arne-pension",
    level == "A3L1"  ~ "Kønsskifte til børn",
    level == "A3L2"  ~ "Fire juridiske forældre",
    level == "A3L3"  ~ "Normkritisk seksualundervisning",
    level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
    level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
    level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
    level == "A3L7"  ~ "Akut hjælp til PPR",
    level == "A3L8"  ~ "75% uddannede pædagoger",
    level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
    level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
    level == "A4L1"  ~ "Forbedret miljøkontrol",
    level == "A4L2"  ~ "Sprøjteforbud",
    level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
    level == "A4L4"  ~ "Billigere grønne valg",
    level == "A4L5"  ~ "Klimaneutralitet i 2040",
    level == "A5L1"  ~ "Kønsneutrale CPR-numre",
    level == "A5L2"  ~ "Udvidelse af valgretten",
    level == "A5L3"  ~ "Kvindedrabsparagraf",
    level == "A5L4"  ~ "Handleplan mod racisme",
    level == "A5L5"  ~ "Kønskvoter",
    level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
    level == "A5L7"  ~ "Udvidelse af EP-valgret",
    level == "A5L8"  ~ "Center for demokratiudvikling",
    level == "A5L9"  ~ "Boliggaranti serviceloven",
    level == "A5L10" ~ "Minimering af hastelovgivning",
    level == "A5L11" ~ "Data hos efterretningstjenesterne",
    TRUE ~ as.character(level)
  ),
  level = factor(level, levels = rev(c(
    "Kollektiv trafik",
    "Cykelinfrastrukturen",
    "Klimaafgift på luftfart",
    "Skærpelse af miljøzoner",
    "Officiel fattigdomsgrænse",
    "Formueskat",
    "Forøgelse af SU",
    "Dagpengegaranti",
    "Arne-pension",
    "Kønsskifte til børn",
    "Fire juridiske forældre",
    "Normkritisk seksualundervisning",
    "LGBT+ i sundhedsvæsnet",
    "Afskaffelse af tolkegebyret",
    "Fritidshjem til og med 6. klasse",
    "Akut hjælp til PPR",
    "75% uddannede pædagoger",
    "Ekstra lærer/pædagog i folkeskolen",
    "Ny model for omsorgs- og sygedage",
    "Forbedret miljøkontrol",
    "Sprøjteforbud",
    "Virksomheders ansvar for miljøskader",
    "Billigere grønne valg",
    "Klimaneutralitet i 2040",
    "Kønsneutrale CPR-numre",
    "Udvidelse af valgretten",
    "Kvindedrabsparagraf",
    "Handleplan mod racisme",
    "Kønskvoter",
    "Handleplan mod hadforbrydelser",
    "Udvidelse af EP-valgret",
    "Center for demokratiudvikling",
    "Boliggaranti serviceloven",
    "Minimering af hastelovgivning",
    "Data hos efterretningstjenesterne"
  )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = ALT)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Profile position", title = "Tvunget Valg") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )


# Vi gemmer plot
ggsave("profiler_order_amce_FC.pdf",
       plot = profile_order_amce_FC ,
       width = 6,
       height = 8,
       units = "in")


# Omnibus F (ANOVA)- er der signifikant forskel på AMCE på tværs af profil-orden

cj_anova(data,
         FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~ALT)



# Policystøtte

model_profile_order_amce_RS <- cj(data,
                                  rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                                  estimate = "amce",
                                  id = ~participant_id,
                                  by = ~ALT)

plot_data_order_RS <- model_profile_order_amce_RS %>%
  as_tibble()


# Vi plotter det
profile_order_amce_RS <- plot_data_order_RS %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet",
      TRUE ~ as.character(feature)),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = ALT)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Profile position", title = "Policystøtte") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )

# Vi gemmer plot
ggsave("profiler_order_amce_RS.pdf",
       plot = profile_order_amce_RS ,
       width = 6,
       height = 8,
       units = "in")


# Omnibus F (ANOVA) - er der signifikant forskel på AMCE på tværs af profil-orden

cj_anova(data,
         rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~ALT)



# Stemmesandsynlighed 

model_profile_order_amce_RV <- cj(data,
                                  rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                                  estimate = "amce",
                                  id = ~participant_id,
                                  by = ~ALT)

plot_data_order_RV <- model_profile_order_amce_RV %>%
  as_tibble()


# Vi plotter det
profile_order_amce_RV <- plot_data_order_RV %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet",
      TRUE ~ as.character(feature)),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = rev(c(
      "Kollektiv trafik",
      "Cykelinfrastrukturen",
      "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse",
      "Formueskat",
      "Forøgelse af SU",
      "Dagpengegaranti",
      "Arne-pension",
      "Kønsskifte til børn",
      "Fire juridiske forældre",
      "Normkritisk seksualundervisning",
      "LGBT+ i sundhedsvæsnet",
      "Afskaffelse af tolkegebyret",
      "Fritidshjem til og med 6. klasse",
      "Akut hjælp til PPR",
      "75% uddannede pædagoger",
      "Ekstra lærer/pædagog i folkeskolen",
      "Ny model for omsorgs- og sygedage",
      "Forbedret miljøkontrol",
      "Sprøjteforbud",
      "Virksomheders ansvar for miljøskader",
      "Billigere grønne valg",
      "Klimaneutralitet i 2040",
      "Kønsneutrale CPR-numre",
      "Udvidelse af valgretten",
      "Kvindedrabsparagraf",
      "Handleplan mod racisme",
      "Kønskvoter",
      "Handleplan mod hadforbrydelser",
      "Udvidelse af EP-valgret",
      "Center for demokratiudvikling",
      "Boliggaranti serviceloven",
      "Minimering af hastelovgivning",
      "Data hos efterretningstjenesterne"
    )))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = ALT)) +
  geom_point(position = position_dodge(width = 0.3), size = 1.5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.3),
                 height = 0.15, 
                 linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y", ncol = 1) +
  labs(x = "AMCE", y = NULL, colour = "Profile position", title = "Stemmesandsynlighed") +
  theme_bw(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing.y = unit(0.3, "lines")
  )

# Vi gemmer plot
ggsave("profiler_order_amce_RV.pdf",
       plot = profile_order_amce_RV ,
       width = 6,
       height = 8,
       units = "in")



# Ombinus F (ANOVA) - er der signifikant forskel på AMCE på tværs af profil-orden

cj_anova(data,
         rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
         id = ~participant_id,
         by = ~ALT)



# Samler de tre profile position plots i et:

combined_profile_order <- profile_order_amce_FC + profile_order_amce_RS + profile_order_amce_RV +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )


# Gemmer plot
ggsave("profileposition_combined.png",
       combined_profile_order,
       width = 14,
       height = 12)




##----------------------------------TIES--------------------------------------##

# I vores conjoint eksperiment er der ties - dvs. situationer, hvor respondenten i en opgave
# ser enten (0,0), (1,1) eller (2,2) - kandidaterne med den samme mængde identitetspolitik. 
# Antagelsen er at det vil svække effekten for tvunget valg

# Derfor vil vi  udregne effekten for tvunget valg kun for de observationer, hvor der IKKE er TIES. 

# Vi skal bruge vores intensitet-variabel. Først skal den omkodes til at være nummerisk

data <- data %>%
  mutate(
    intensitet_num = case_when(
      identitet_intensitet == "Ingen" ~ 0,
      identitet_intensitet == "En"    ~ 1,
      identitet_intensitet == "Begge" ~ 2,
      TRUE ~ NA_real_ 
    )
  )

# Nu konstruerer vi en variabel, der indikerer hvorvidt en runde er en tie ved at 
# bruge denne nummeriske variabel

data <- data %>%
  group_by(participant_id, QES) %>%
  mutate(
    andet_intensitet = sum(intensitet_num) - intensitet_num,
    tie = as.integer(intensitet_num == andet_intensitet)
  ) %>%
  ungroup()


# Tæller antallet af ties på kandidat niveau
data %>% count(tie)

# Tæller antallet og andelen af ties på opgave niveau
data %>%
  distinct(participant_id, QES, tie) %>%
  count(tie) %>%
  mutate(pct = n / sum(n) * 100)


# Vi skaber et datasæt uden ties

data_no_ties <- data %>%
  filter(tie == 0)

# Vi laver nu vores hovedmodeller for Tvunget Valg med No-ties datasættet

model_FC_amce_no_ties <- cj(data_no_ties,
                            FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                            estimate = "amce",
                            id = ~participant_id)

model_FC_amce_no_ties %>% as_tibble() %>% view()

#Gemmer som tabel
table_FC_amce_no_ties <- model_FC_amce_no_ties %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error) %>%
  mutate(
    # omdøber attributter 
    feature = case_when(
      feature == "A1_transport"   ~ "Transport",
      feature == "A2_oekonomi"    ~ "Økonomi",
      feature == "A3_familie_ny"  ~ "Familie og børn",
      feature == "A4_klima"       ~ "Klima og miljø",
      feature == "A5_andet_ny"    ~ "Andet"
    ),
    # Omdøber niveauer
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ level  
    ),
    # Stjerner for signifikansnivau
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 2.58 ~ "***",
      abs(estimate / std.error) > 1.96 ~ "**",
      abs(estimate / std.error) > 1.64 ~ "*",
      TRUE ~ ""
    ),
    # 
    effect = case_when(
      is.na(std.error) ~ sprintf("%.3f (ref)", estimate),
      TRUE ~ paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
    )
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tabel 1: Tvunget Valg (AMCE) uden ties") %>%
  tab_footnote("Note: * p<0.10, ** p<0.05, *** p<0.01. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# Gemmer som pdf
gtsave(table_FC_amce_no_ties, "Amce_FC_table_noties.pdf")


# GGplot 
plot_FC_AMCE_no_ties <- model_FC_amce_no_ties %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies", "Identitære policies"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2) +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "AMCE uden ties"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("tvunget_valg_amce_noties.pdf",
       plot = plot_FC_AMCE_no_ties,
       width = 6,
       height = 8,
       units = "in")


# Laver en figur for Tvunget valg med og uden ties

# Hypotese 1 AMCE med ties

model_FC_amce <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)

plot_FC_AMCE <- model_FC_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies", "Identitære policies"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "AMCE med ties"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )


samlet_ties <- plot_FC_AMCE_no_ties | plot_FC_AMCE

ggsave("samlet_ties2.png",
       plot = samlet_ties,
       width = 12,
       height = 6,
       units = "in",
       device = "png")



#------------------------------------------------------------------------------#

# AMCE for alle attributter og niveauer 


# Tvunget Valg

model_FC_amce_all <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                    estimate = "amce",
                    id = ~participant_id)

plot_FC_AMCE_all <- model_FC_amce_all %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie"    ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"      ~ "Andet"
    ),
    feature = factor(feature, levels = c(
      "Transport", "Økonomi", "Familie og Børn", "Klima og Miljø", "Andet"
    )),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Investering i cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til Børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Opbevaring af data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Kollektiv trafik", "Investering i cykelinfrastrukturen", "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse", "Formueskat", "Forøgelse af SU", "Dagpengegaranti",
      "Arne-pension", 
      "Ny model for omsorgs- og sygedage", "Ekstra lærer/pædagog i folkeskolen",
      "75% uddannede pædagoger", "Akut hjælp til PPR",
      "Fritidshjem til og med 6. klasse", "Afskaffelse af tolkegebyret",
      "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning",
      "Fire juridiske forældre", "Kønsskifte til Børn",
      "Forbedret miljøkontrol","Klimaneutralitet i 2040", "Billigere grønne valg",
      "Virksomheders ansvar for miljøskader", "Sprøjteforbud",
      "Opbevaring af data hos efterretningstjenesterne",
      "Minimering af hastelovgivning", "Boliggaranti serviceloven",
      "Center for demokratiudvikling", "Udvidelse af EP-valgret",
      "Handleplan mod hadforbrydelser", "Kønskvoter",
      "Handleplan mod racisme", "Kvindedrabsparagraf",
      "Udvidelse af valgretten", "Kønsneutrale CPR-numre"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Tvunget valg (AMCE)"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

ggsave("FC_amce_all.pdf",
       plot = plot_FC_AMCE_all,
       width = 8,
       height = 12,
       units = "in",
       device = "pdf")


# Policystøtte

model_RS_amce_all <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                        estimate = "amce",
                        id = ~participant_id)

plot_RS_AMCE_all <- model_RS_amce_all %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie"    ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"      ~ "Andet"
    ),
    feature = factor(feature, levels = c(
      "Transport", "Økonomi", "Familie og Børn", "Klima og Miljø", "Andet"
    )),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Investering i cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til Børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Opbevaring af data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Kollektiv trafik", "Investering i cykelinfrastrukturen", "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse", "Formueskat", "Forøgelse af SU", "Dagpengegaranti",
      "Arne-pension", 
      "Ny model for omsorgs- og sygedage", "Ekstra lærer/pædagog i folkeskolen",
      "75% uddannede pædagoger", "Akut hjælp til PPR",
      "Fritidshjem til og med 6. klasse", "Afskaffelse af tolkegebyret",
      "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning",
      "Fire juridiske forældre", "Kønsskifte til Børn",
      "Forbedret miljøkontrol","Klimaneutralitet i 2040", "Billigere grønne valg",
      "Virksomheders ansvar for miljøskader", "Sprøjteforbud",
      "Opbevaring af data hos efterretningstjenesterne",
      "Minimering af hastelovgivning", "Boliggaranti serviceloven",
      "Center for demokratiudvikling", "Udvidelse af EP-valgret",
      "Handleplan mod hadforbrydelser", "Kønskvoter",
      "Handleplan mod racisme", "Kvindedrabsparagraf",
      "Udvidelse af valgretten", "Kønsneutrale CPR-numre"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Policystøtte (AMCE)"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

ggsave("RS_amce_all.pdf",
       plot = plot_RS_AMCE_all,
       width = 8,
       height = 12,
       units = "in",
       device = "pdf")




# Stemmesandsynlighed

model_RV_amce_all <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie + A4_klima + A5_andet,
                        estimate = "amce",
                        id = ~participant_id)

plot_RV_AMCE_all <- model_RV_amce_all %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie"    ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"      ~ "Andet"
    ),
    feature = factor(feature, levels = c(
      "Transport", "Økonomi", "Familie og Børn", "Klima og Miljø", "Andet"
    )),
    level = case_when(
      level == "A1L1"  ~ "Kollektiv trafik",
      level == "A1L2"  ~ "Investering i cykelinfrastrukturen",
      level == "A1L3"  ~ "Klimaafgift på luftfart",
      level == "A1L4"  ~ "Skærpelse af miljøzoner",
      level == "A2L1"  ~ "Officiel fattigdomsgrænse",
      level == "A2L2"  ~ "Formueskat",
      level == "A2L3"  ~ "Forøgelse af SU",
      level == "A2L4"  ~ "Dagpengegaranti",
      level == "A2L5"  ~ "Arne-pension",
      level == "A3L1"  ~ "Kønsskifte til Børn",
      level == "A3L2"  ~ "Fire juridiske forældre",
      level == "A3L3"  ~ "Normkritisk seksualundervisning",
      level == "A3L4"  ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"  ~ "Afskaffelse af tolkegebyret",
      level == "A3L6"  ~ "Fritidshjem til og med 6. klasse",
      level == "A3L7"  ~ "Akut hjælp til PPR",
      level == "A3L8"  ~ "75% uddannede pædagoger",
      level == "A3L9"  ~ "Ekstra lærer/pædagog i folkeskolen",
      level == "A3L10" ~ "Ny model for omsorgs- og sygedage",
      level == "A4L1"  ~ "Forbedret miljøkontrol",
      level == "A4L2"  ~ "Sprøjteforbud",
      level == "A4L3"  ~ "Virksomheders ansvar for miljøskader",
      level == "A4L4"  ~ "Billigere grønne valg",
      level == "A4L5"  ~ "Klimaneutralitet i 2040",
      level == "A5L1"  ~ "Kønsneutrale CPR-numre",
      level == "A5L2"  ~ "Udvidelse af valgretten",
      level == "A5L3"  ~ "Kvindedrabsparagraf",
      level == "A5L4"  ~ "Handleplan mod racisme",
      level == "A5L5"  ~ "Kønskvoter",
      level == "A5L6"  ~ "Handleplan mod hadforbrydelser",
      level == "A5L7"  ~ "Udvidelse af EP-valgret",
      level == "A5L8"  ~ "Center for demokratiudvikling",
      level == "A5L9"  ~ "Boliggaranti serviceloven",
      level == "A5L10" ~ "Minimering af hastelovgivning",
      level == "A5L11" ~ "Opbevaring af data hos efterretningstjenesterne",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Kollektiv trafik", "Investering i cykelinfrastrukturen", "Klimaafgift på luftfart",
      "Skærpelse af miljøzoner",
      "Officiel fattigdomsgrænse", "Formueskat", "Forøgelse af SU", "Dagpengegaranti",
      "Arne-pension", 
      "Ny model for omsorgs- og sygedage", "Ekstra lærer/pædagog i folkeskolen",
      "75% uddannede pædagoger", "Akut hjælp til PPR",
      "Fritidshjem til og med 6. klasse", "Afskaffelse af tolkegebyret",
      "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning",
      "Fire juridiske forældre", "Kønsskifte til Børn",
      "Forbedret miljøkontrol","Klimaneutralitet i 2040", "Billigere grønne valg",
      "Virksomheders ansvar for miljøskader", "Sprøjteforbud",
      "Opbevaring af data hos efterretningstjenesterne",
      "Minimering af hastelovgivning", "Boliggaranti serviceloven",
      "Center for demokratiudvikling", "Udvidelse af EP-valgret",
      "Handleplan mod hadforbrydelser", "Kønskvoter",
      "Handleplan mod racisme", "Kvindedrabsparagraf",
      "Udvidelse af valgretten", "Kønsneutrale CPR-numre"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Stemmesandsynlighed (AMCE)"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

ggsave("RV_amce_all.pdf",
       plot = plot_RV_AMCE_all,
       width = 8,
       height = 12,
       units = "in",
       device = "pdf")

