### -------------------------------------------------------------------------###
###                       SPECIALE I STATSKUNDSKAB                           ###
###                             Analyse                                      ###
### -------------------------------------------------------------------------###

setwd("/Users/Louise/Desktop/Statskundskab/Speciale/R script")
getwd()

data <- readRDS("conjoint_data.rds")

# Indlæser relevante pakker
library(cregg)   
library(haven)
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(gt)
library(sandwich)
library(lmtest)
library(marginaleffects)
library(car)
library(ggh4x)
library(emmeans)
library(patchwork)
library(broom)

## -------------------------------------------------------------------------- ##
##                      Hovedanalyse - Hypotese 1-3                           ##
## -------------------------------------------------------------------------- ##

##-------------------Average Marginal Component Effect - AMCE-----------------##

# Hypotese 1:Tvunget Valg
# AMCE-model med klyngerobuste standardfejl på respondetniveau 

model_FC_amce <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)


model_FC_amce %>% as_tibble()


# Gemmer som tabel
table_FC_amce <- model_FC_amce %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
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
      abs(estimate / std.error) > 3.29 ~ "***",  
      abs(estimate / std.error) > 2.58 ~ "**",  
      abs(estimate / std.error) > 1.96 ~ "*",    
      TRUE ~ ""
    ),
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
  tab_header(title = "Hypotese 1: Tvunget Valg (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_amce, "Amce_FC_table.pdf")
     
# Vi plotter det  
    
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
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("tvunget_valg_amce.png",
       plot = plot_FC_AMCE,
       width = 6,
       height = 6,
       units = "in")


# Hypotese 2:Policystøtte
# AMCE-model med klyngerobuste standardfejl på respondetniveau 

model_RS_amce <- cj(data,
                    rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)

model_RS_amce %>% as_tibble ()

# Gemmer i tabel
table_RS_amce <- model_RS_amce %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
   mutate(
    # omdøber attributter 
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
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
      abs(estimate / std.error) > 3.29 ~ "***",  
      abs(estimate / std.error) > 2.58 ~ "**",  
      abs(estimate / std.error) > 1.96 ~ "*",    
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
  tab_header(title = "Hypotese 2: Policy Støtte (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_amce, "RS_amce_table.pdf")


# Vi plotter det
plot_RS_AMCE <- model_RS_amce %>%
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
    title = ""
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce.png",
       plot = plot_RS_AMCE,
       width = 6,
       height = 6,
       units = "in")



# Hypotese 3: Stemmesandsynlighed
# AMCE-model med klyngerobuste standardfejl på respondetniveau 

model_RV_amce <- cj(data,
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "amce",
                    id = ~participant_id)

model_RV_amce %>% as_tibble ()


# Gemmer i en tabel
table_RV_amce <- model_RV_amce %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
  mutate(
    # omdøber attributter 
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
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
      abs(estimate / std.error) > 3.29 ~ "***",  
      abs(estimate / std.error) > 2.58 ~ "**",  
      abs(estimate / std.error) > 1.96 ~ "*",    
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
  tab_header(title = "Hypotese 3: Stemmesandsynlighed (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_amce, "RV_amce_table.pdf")


# Vi plotter det
plot_RV_AMCE <- model_RV_amce %>%
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
    title = ""
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("stemmesandsynlighed_amce.png",
       plot = plot_RV_AMCE,
       width = 6,
       height = 6,
       units = "in")



##-----------------------------Hybrid-modeller--------------------------------##

# Vi kigger nærmere på de enkelte identitære policy-niveauer
# Er kollapseringen i en samlet identitær kategori meningsfuld eller skjuler den 
# særlige effekter af  de forskellige identitære policies


# Vi laver en ny variabel for hver attribut, Andet og Familie & Børn, der bibeholder
# alle identitære policies, men samler alle ikke-identitære policies i en samlet referencekategori

# A3 - Familie og børn
data <- data %>%
  mutate(
    A3_familie_2 = case_when(
      A3_familie %in% c("A3L6", "A3L7", "A3L8", "A3L9", "A3L10") ~ "A3_ikke_identitet",
      TRUE ~ as.character(A3_familie)
    ) )

table(data$A3_familie_2)

class(data$A3_familie_2)

data <- data %>%
  mutate(
    A3_familie_2 = factor(A3_familie_2, levels = c("A3_ikke_identitet", "A3L1", "A3L2","A3L3",
                                                   "A3L4", "A3L5"))
  )

# A5 - Andet
data <- data %>%
  mutate(
    A5_andet_2 = case_when(
      A5_andet %in% c("A5L7", "A5L8", "A5L9", "A5L10", "A5L11") ~ "A5_ikke_identitet",
      TRUE ~ as.character(A5_andet)
    ) )

table(data$A5_andet_2)
class(data$A5_andet_2)

data <- data %>%
  mutate(
    A5_andet_2 = factor(A5_andet_2, levels = c("A5_ikke_identitet", "A5L1", "A5L2","A5L3",
                                               "A5L4", "A5L5", "A5L6"))
  )


# Hybridmodel AMCE - Tvunget valg

model_FC_amce_2 <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                      estimate = "amce",
                      id = ~participant_id)

model_FC_amce_2 %>% as_tibble() %>% (view)

# PLot
plot_FC_AMCE_2 <- model_FC_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  scale_x_continuous(
    limits = c(-0.3, 0.1),
    breaks = seq(-0.3, 0.1, by = 0.1),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )


# Gemmer plottet
ggsave("FC_amce_nc.pdf",
       plot = plot_FC_AMCE_2,
       width = 6,
       height = 8,
       units = "in")


# Gemmer i en tabel
table_FC_amce_2 <- model_FC_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Tvunget Valg (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_amce_2, "Amce_FC_table_2.pdf")


# Hybridmodel AMCE - Policy Støtte

model_RS_amce_2 <- cj(data,
                      rating_support ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                      estimate = "amce",
                      id = ~participant_id)

model_RS_amce_2 %>% as_tibble() %>% view()

# PLot
plot_RS_AMCE_2 <- model_RS_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
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
  scale_x_continuous(
    limits = c(-0.7, 0.25),
    breaks = seq(-0.6, 0.2, by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )  +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Policystøtte"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce_nc.pdf",
       plot = plot_RS_AMCE_2,
       width = 6,
       height = 8,
       units = "in")


# Gemmer som tabel
table_RS_amce_2 <- model_RS_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Policystøtte (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_amce_2, "Amce_RS_table_2.pdf")



# Hybridmodel AMCE - Stemmesandsynlighed

model_RV_amce_2 <- cj(data,
                      rating_voting ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                      estimate = "amce",
                      id = ~participant_id)

model_RV_amce_2 %>% as_tibble() %>% view()


# PLot
plot_RV_AMCE_2 <- model_RV_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
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
    title = "Stemmesandsynlighed"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_nc.pdf",
       plot = plot_RV_AMCE_2,
       width = 6,
       height = 8,
       units = "in")

# Gemmer som tabel
table_RV_amce_2 <- model_RV_amce_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Stemmesandsynlighed (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_amce_2, "Amce_RV_table_2.pdf")


# Vi kombinerer de 3 AMCE plot for de afhængige variable

combined_plot_AMCE_nc <- plot_FC_AMCE_2 | plot_RS_AMCE_2 | plot_RV_AMCE_2

ggsave("combined_amce_nc.png",
       plot = combined_plot_AMCE_nc,
       width = 20,
       height = 8,  
       units = "in",
       device = "png")



# Vi konstruerer nu modeller, hvor vi inkluderer alle niveauerne i alle attributter
# Idet AMCE afhænger af referencekategorien skifter vi referencekategorien for 
# de ikke-identitære policies i hver model. 
# Således laver vi 5 forskellige modeller for hver af de 3 outcome variable

# Tvunget valg - reference kategori A3L10 og A5L11
model_FC_amce_3 <-cj(data, FC ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_FC_amce_3 %>% as_tibble() %>% view()

# Plot
plot_FC_amce_3 <- model_FC_amce_3 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
  
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L10","A3L9", "A3L8", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L11","A5L10", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Tvunget Valg - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("FC_amce_v1.pdf",
       plot = plot_FC_amce_3,
       width = 6,
       height = 8,
       units = "in")


#Policystøtte

model_RS_amce_3 <-cj(data, rating_support ~  A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RS_amce_3 %>% as_tibble()

# Plot
plot_RS_amce_3 <- model_RS_amce_3 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
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
      "Ny model for omsorgs- og sygedage","Ekstra lærer/pædagog i folkeskolen", 
      "75% uddannede pædagoger", "Akut hjælp til PPR", "Fritidshjem til og med 6. klasse", 
      "Afskaffelse af tolkegebyret", "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning", 
      "Fire juridiske forældre", "Kønsskifte til Børn",
      "Opbevaring af data hos efterretningstjenesterne",  "Minimering af hastelovgivning",
      "Boliggaranti serviceloven", "Center for demokratiudvikling", 
      "Udvidelse af EP-valgret",  "Handleplan mod hadforbrydelser", "Kønskvoter", 
      "Handleplan mod racisme", 
      "Kvindedrabsparagraf", "Udvidelse af valgretten", "Kønsneutrale CPR-numre" 
      
    ))
  ) %>%
  filter(feature %in% c("Familie og Børn", "Andet")) %>%
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
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce_v3.pdf",
       plot = plot_RS_amce_3,
       width = 8,
       height = 8,
       units = "in")

# Gemmer som tabel
table_RS_amce_3 <- model_RS_amce_3 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    level = case_when(
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
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Policystøtte (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_amce_3, "table_RS_amce_3.pdf")


#Stemmesandsynlighed

model_RV_amce_3 <-cj(data, rating_voting ~A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RV_amce_3 %>% as_tibble()

# Plot
plot_RV_amce_3 <- model_RV_amce_3 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L10","A3L9", "A3L8", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L11","A5L10", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Stemmesandsynlighed - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_v1.pdf",
       plot = plot_RV_amce_3,
       width = 6,
       height = 8,
       units = "in")


# Skifter referencekategori til A3L9 og A5L10

data$A3_familie <- factor(
  data$A3_familie,
  levels = c("A3L9", "A3L10", "A3L8", "A3L7", "A3L6", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)

data$A5_andet <- factor(
  data$A5_andet,
  levels = c("A5L10", "A5L11","A5L9", "A5L8", "A5L7", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)



# Tvunget valg
model_FC_amce_4 <-cj(data, FC ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)


model_FC_amce_4 %>% as_tibble()

#Plot
plot_FC_amce_4 <- model_FC_amce_4 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L9","A3L10", "A3L8", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L10","A5L11", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Tvunget Valg - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("FC_amce_v2.pdf",
       plot = plot_FC_amce_4,
       width = 6,
       height = 8,
       units = "in")


#Policystøtte

model_RS_amce_4 <-cj(data, rating_support ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RS_amce_4 %>% as_tibble()


# Plot
plot_RS_amce_4 <- model_RS_amce_4 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L9","A3L10", "A3L8", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L10","A5L11", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Policy Støtte - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce_v2.pdf",
       plot = plot_RS_amce_4,
       width = 6,
       height = 8,
       units = "in")


#Stemmesandsynlighed

model_RV_amce_4 <-cj(data, rating_voting ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RV_amce_4 %>% as_tibble()


# Plot
plot_RV_amce_4 <- model_RV_amce_4 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L9","A3L10", "A3L8", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L10","A5L11", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Stemmesandsynlighed - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_v2.pdf",
       plot = plot_RV_amce_4,
       width = 6,
       height = 8,
       units = "in")



# Skifter referencekategori til A3L8 og A5L9

data$A3_familie <- factor(
  data$A3_familie,
  levels = c("A3L8", "A3L10", "A3L9", "A3L7", "A3L6", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)

data$A5_andet <- factor(
  data$A5_andet,
  levels = c("A5L9", "A5L11","A5L10", "A5L8", "A5L7", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)


# Tvunget Valg
model_FC_amce_5 <-cj(data, FC ~  A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)


model_FC_amce_5 %>% as_tibble()


# Plot
plot_FC_amce_5 <- model_FC_amce_5 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L8","A3L10", "A3L9", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L9","A5L11", "A5L10", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Tvunget Valg - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("FC_amce_v3.pdf",
       plot = plot_FC_amce_4,
       width = 6,
       height = 8,
       units = "in")


#Policystøtte

model_RS_amce_5 <-cj(data, rating_support ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RS_amce_5 %>% as_tibble()

# Plot
plot_RS_amce_5 <- model_RS_amce_5 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L8","A3L10", "A3L9", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L9","A5L11", "A5L10", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Policy Støtte - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce_v3.pdf",
       plot = plot_RS_amce_4,
       width = 6,
       height = 8,
       units = "in")


#Stemmesandsynlighed

model_RV_amce_5 <-cj(data, rating_voting ~A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RV_amce_5 %>% as_tibble()


# Plot
plot_RV_amce_5 <- model_RV_amce_5 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L8","A3L10", "A3L9", "A3L7", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L9","A5L11", "A5L10", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Stemmesandsynlighed - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_v3.pdf",
       plot = plot_RV_amce_5,
       width = 6,
       height = 8,
       units = "in")



# Skifter referencekategori til A3L7 og A5L8

data$A3_familie <- factor(
  data$A3_familie,
  levels = c("A3L7", "A3L10", "A3L9", "A3L8", "A3L6", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)

data$A5_andet <- factor(
  data$A5_andet,
  levels = c("A5L8", "A5L11","A5L10", "A5L9", "A5L7", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)

# Tvunget valg

model_FC_amce_6 <-cj(data, FC ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_FC_amce_6 %>% as_tibble()


# Plot
plot_FC_amce_6 <- model_FC_amce_6 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L7","A3L10", "A3L9", "A3L8", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L8","A5L11", "A5L10", "A5L9", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Tvunget Valg - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("FC_amce_v4.pdf",
       plot = plot_FC_amce_6,
       width = 6,
       height = 8,
       units = "in")


#Policystøtte

model_RS_amce_6 <-cj(data, rating_support ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RS_amce_6 %>% as_tibble()

# Plot
plot_RS_amce_6 <- model_RS_amce_6 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%  # filter on original names first
  mutate(
    feature = case_when(                                 # then rename
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
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
      "Akut hjælp til PPR", "Ny model for omsorgs- og sygedage", "Ekstra lærer/pædagog i folkeskolen", 
      "75% uddannede pædagoger", "Fritidshjem til og med 6. klasse", "Afskaffelse af tolkegebyret", 
      "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning", "Fire juridiske forældre", 
      "Kønsskifte til Børn",  
      "Center for demokratiudvikling", "Opbevaring af data hos efterretningstjenesterne", 
      "Minimering af hastelovgivning", "Boliggaranti serviceloven", "Udvidelse af EP-valgret", 
      "Handleplan mod hadforbrydelser", "Kønskvoter", "Handleplan mod racisme", 
      "Kvindedrabsparagraf", "Udvidelse af valgretten", "Kønsneutrale CPR-numre"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

ggsave("RS_amce_v6.pdf",
       plot = plot_RS_amce_6,
       width = 8,
       height = 8,
       units = "in",
       device = "pdf")


# Gemmer som tabel
table_RS_amce_6 <- model_RS_amce_6 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    level = case_when(
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
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Policystøtte (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_amce_6, "table_RS_amce_6.pdf")


#Stemmesandsynlighed

model_RV_amce_6 <-cj(data, rating_voting ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RV_amce_6 %>% as_tibble()


# Plot
plot_RV_amce_6 <- model_RV_amce_6 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L7","A3L10", "A3L9", "A3L8", "A3L6", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L8","A5L11", "A5L10", "A5L9", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Stemmesandsynlighed - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_v4.pdf",
       plot = plot_RV_amce_6,
       width = 6,
       height = 8,
       units = "in")



# Skifter referencekategori til A3L6 og A5L7

data$A3_familie <- factor(
  data$A3_familie,
  levels = c("A3L6", "A3L10", "A3L9", "A3L8", "A3L7", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)

data$A5_andet <- factor(
  data$A5_andet,
  levels = c("A5L7", "A5L11","A5L10", "A5L9", "A5L8", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)

# Tvunget valg

model_FC_amce_7 <-cj(data, FC ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_FC_amce_7 %>% as_tibble()


# Plot
plot_FC_amce_7 <- model_FC_amce_7 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L6","A3L10", "A3L9", "A3L8", "A3L7", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L7","A5L11", "A5L10", "A5L9", "A5L8", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Tvunget Valg - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("FC_amce_v5.pdf",
       plot = plot_FC_amce_7,
       width = 6,
       height = 8,
       units = "in")



#Policystøtte

model_RS_amce_7 <-cj(data, rating_support ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RS_amce_7 %>% as_tibble()


# Plot
plot_RS_amce_7 <- model_RS_amce_7 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
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
      "Fritidshjem til og med 6. klasse", "Ny model for omsorgs- og sygedage", 
      "Ekstra lærer/pædagog i folkeskolen", "75% uddannede pædagoger", "Akut hjælp til PPR", 
      "Afskaffelse af tolkegebyret", "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning", 
      "Fire juridiske forældre", "Kønsskifte til Børn",  
      "Udvidelse af EP-valgret", "Opbevaring af data hos efterretningstjenesterne", 
      "Minimering af hastelovgivning", "Boliggaranti serviceloven", "Center for demokratiudvikling", 
      "Handleplan mod hadforbrydelser", "Kønskvoter", "Handleplan mod racisme", 
      "Kvindedrabsparagraf", "Udvidelse af valgretten", "Kønsneutrale CPR-numre" 
    ))
  ) %>%
  filter(feature %in% c("Familie og Børn", "Andet")) %>%
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
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RS_amce_v7.pdf",
       plot = plot_RS_amce_7,
       width = 8,
       height = 8,
       units = "in")


# Gemmer som tabel
table_RS_amce_7 <- model_RS_amce_7 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    level = case_when(
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
    stars = case_when(
      is.na(std.error) ~ "",
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
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
  tab_header(title = "Policystøtte (AMCE)") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_amce_7, "table_RS_amce_7.pdf")


# Stemmesandsynlighed

model_RV_amce_7 <-cj(data, rating_voting ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "amce",
                     id = ~participant_id)

model_RV_amce_7 %>% as_tibble()


# Plot
plot_RV_amce_7 <- model_RV_amce_7 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L6","A3L10", "A3L9", "A3L8", "A3L7", "A3L5", "A3L4", "A3L3", "A3L2","A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L7","A5L11", "A5L10", "A5L9", "A5L8", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
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
    title = "Stemmesandsynlighed - AMCEs"
  )  +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside"
  )

# Gemmer plottet
ggsave("RV_amce_v5.pdf",
       plot = plot_RV_amce_7,
       width = 6,
       height = 8,
       units = "in")


# Skifter referencekategorien tilbage

data$A3_familie <- factor(
  data$A3_familie,
  levels = c("A3L10", "A3L9", "A3L8", "A3L7", "A3L6", "A3L5","A3L4", "A3L3",
             "A3L2", "A3L1")
)

data$A5_andet <- factor(
  data$A5_andet,
  levels = c("A5L11", "A5L10","A5L9", "A5L8", "A5L7", "A5L6", "A5L5","A5L4", "A5L3",
             "A5L2", "A5L1")
)


# Vi kombinerer de 3 udvalgte figurer for policystøtte i et plot

combined_plot_AMCE_an <- plot_RS_amce_7 | plot_RS_amce_6 | plot_RS_amce_3

ggsave("combined_amce_an.pdf",
       plot = combined_plot_AMCE_an,
       width = 20,
       height = 8,  
       units = "in",
       device = "pdf")


##------------------------------MARGINAL MEANS--------------------------------##

# Hypotese 1: Tvunget valg
# MM-model med klyngerobuste standardfejl på respondetniveau
model_FC_MM_1 <- cj(data,
               FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
               estimate = "mm",
               id = ~participant_id)


model_FC_MM_1 %>% as_tibble()


# Gemmer i en tabel
table_FC_MM_final <- model_FC_MM_1 %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error, lower, upper) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  # <-- add this
  mutate(
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
      feature == "A5_andet_ny"    ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ level  
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Hypotese 1: Tvunget Valg (MM)") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_final, "MM_FC_table.pdf")


# Vi plotter
plot_FC_MM_1 <- model_FC_MM_1 %>%
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
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer plottet
ggsave("FC_mm_1.png",
       plot = plot_FC_MM_1,
       width = 7,
       height = 6,
       units = "in")



# Hypotese 2: Policystøtte
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RS_MM_1 <- cj(data,
                    rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "mm",
                    id = ~participant_id)


model_RS_MM_1 %>% as_tibble()


# Gemmer i en tabel
table_RS_MM_final <- model_RS_MM_1 %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error, lower, upper) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    # Omdøber attributter 
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
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
    # Kombinerer estimat og konfidens intervaller
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Hypotese 2: Policystøtte (MM)") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_final, "MM_RS_table.pdf")


# Vi plotter
plot_RS_MM_1<- model_RS_MM_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    feature = factor(feature, levels = c(
                                         "Familie og Børn", "Andet")),
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
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer plottet
ggsave("RS_mm_1.png",
       plot = plot_RS_MM_1,
       width = 6,
       height = 8,
       units = "in")


# Hypotese 2: Stemmesandsynlighed
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RV_MM_1 <- cj(data,
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "mm",
                    id = ~participant_id)


model_RV_MM_1 %>% as_tibble()

# Gemmer i en tabel
table_RV_MM_final <- model_RV_MM_1 %>%
  as_tibble() %>%
  select(feature, level, estimate, std.error, lower, upper) %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%  
  mutate(
    # Omdøber attributter 
    feature = case_when(
      feature == "A3_familie_ny"  ~ "Familie og børn",
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
    # Kombinerer estimat og konfidens intervaller
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Hypotese 3: Stemmesandsynlighed (MM)") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_final, "MM_RV_table.pdf")


# Vi plotter
plot_RV_MM_1 <- model_RV_MM_1 %>%
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
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )


# Gemmer plottet
ggsave("RV_mm_1.png",
       plot = plot_RV_MM_1,
       width = 6,
       height = 8,
       units = "in")



## Hybrid-modeller - Marginal Means 

# Tvunget valg
# MM-model med klyngerobuste standardfejl på respondetniveau
model_FC_MM_2 <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                    estimate = "mm",
                    id = ~participant_id)


model_FC_MM_2 %>% as_tibble %>% View()


# Vi plotter
plot_FC_MM_2<- model_FC_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
    ))
  )%>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )


# Gemmer i en tabel
table_FC_MM_2 <- model_FC_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfej clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
    
  )

gtsave(table_FC_MM_2, "table_FC_MM_2.png")



#Policystøtte
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RS_MM_2 <- cj(data,
                    rating_support ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                    estimate = "mm",
                    id = ~participant_id)


model_RS_MM_2 %>% as_tibble() %>% view()


# Vi plotter
plot_RS_MM_2<- model_RS_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
    ))
  )  %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policy Støtte"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RS_MM_2 <- model_RS_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_2, "table_RS_MM_2.png")



#Stemmesandsynlighed
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RV_MM_2 <- cj(data,
                    rating_voting ~ A1_transport + A2_oekonomi + A3_familie_2 + A4_klima + A5_andet_2,
                    estimate = "mm",
                    id = ~participant_id)


model_RV_MM_2 %>% as_tibble() %>% view()


# Vi plotter
plot(model_RV_MM_2, vline = 0.5) + 
  guides(color = "none") +
  theme_minimal() +
  labs(title = "Marginal means - Stemmeintention")


plot_RV_MM_2<- model_RV_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (A3)",
      level == "A3L1"              ~ "Kønskifte til børn",
      level == "A3L2"              ~ "Fire juridiske forældre",
      level == "A3L3"              ~ "Normkritisk seksualundervisning",
      level == "A3L4"              ~ "LGBT+ i sundhedsvæsnet",
      level == "A3L5"              ~ "Afskaffelse af tolkegebyret",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (A5)",
      level == "A5L1"              ~ "Kønsneutrale CPR-numre",
      level == "A5L2"              ~ "Udvidelse af valgretten",
      level == "A5L3"              ~ "Kvindedrabsparagraf",
      level == "A5L4"              ~ "Handleplan mod racisme",
      level == "A5L5"              ~ "Kønskvoter",
      level == "A5L6"              ~ "Handleplan mod hadforbrydelser",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "Ikke-identitære policies (A3)", "Kønskifte til børn", "Fire juridiske forældre", 
      "Normkritisk seksualundervisning", "LGBT+ i sundhedsvæsnet", "Afskaffelse af tolkegebyret",
      "Ikke-identitære policies (A5)", "Kønsneutrale CPR-numre", "Udvidelse af valgretten", 
      "Kvindedrabsparagraf", "Handleplan mod racisme", "Kønskvoter", "Handleplan mod hadforbrydelser"
    ))
  )  %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_MM_2 <- model_RV_MM_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_2", "A5_andet_2")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_2" ~ "Familie og Børn",
      feature == "A5_andet_2"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_2, "table_RV_MM_2.png")


## Gemmer de tre MM plot in en figur

combined_plot_MM_nc <- plot_FC_MM_2 | plot_RS_MM_2  | plot_RV_MM_2

ggsave("combined_MM_nc.png",
       plot = combined_plot_MM_nc,
       width = 20,
       height = 8,  
       units = "in",
       device = "png")



## Modelspecifikation hvor alle attributniveauer er bibeholdt 

# Tvunget valg
# MM-model med klyngerobuste standardfejl på respondetniveau

model_FC_MM_3 <-cj(data, FC ~ A1_transport + A2_oekonomi + A3_familie 
                     + A4_klima + A5_andet,  estimate = "mm",
                     id = ~participant_id)

model_FC_MM_3 %>% as_tibble()

# Vi plotter
plot(model_FC_MM_3, vline = 0.5) + 
  guides(color = "none") +
  theme_minimal() +
  labs(title = "Marginal means - Tvunget valg")


plot_FC_MM_3<- model_FC_MM_3 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L10", "A3L9","A3L8", "A3L7", "A3L6", "A3L5","A3L4", "A3L3", "A3L2", "A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L11","A5L10", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
    ))) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Marginal Mean",
    y = NULL,
    title = "Tvunget Valg - Marginal Means"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer plottet
ggsave("FC_mm_3.pdf",
       plot = plot_FC_MM_3,
       width = 6,
       height = 8,
       units = "in")


# Policystøtte
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RS_MM_3 <-cj(data, rating_support ~ A1_transport + A2_oekonomi + A3_familie 
                   + A4_klima + A5_andet,  estimate = "mm",
                   id = ~participant_id)


model_RS_MM_3 %>% as_tibble() %>% view()

# Vi plotter
plot(model_RS_MM_3) + 
  guides(color = "none") +
  theme_minimal() +
  labs(title = "Marginal means - Policy Støtte")


plot_RS_MM_3 <- model_RS_MM_3 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Familie og Børn", "Andet")),
    level = case_when(
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
      "Ny model for omsorgs- og sygedage", "Ekstra lærer/pædagog i folkeskolen",
      "75% uddannede pædagoger", "Akut hjælp til PPR", "Fritidshjem til og med 6. klasse", 
      "Afskaffelse af tolkegebyret", "LGBT+ i sundhedsvæsnet", "Normkritisk seksualundervisning", 
      "Fire juridiske forældre", "Kønsskifte til Børn",
      "Opbevaring af data hos efterretningstjenesterne", "Minimering af hastelovgivning", 
      "Boliggaranti serviceloven", "Center for demokratiudvikling", "Udvidelse af EP-valgret", 
      "Handleplan mod hadforbrydelser", "Kønskvoter", "Handleplan mod racisme", "Kvindedrabsparagraf",
      "Udvidelse af valgretten", "Kønsneutrale CPR-numre"
    ))
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y", switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = ""
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer plottet
ggsave("RS_mm_3.pdf",
       plot = plot_RS_MM_3,
       width = 8,
       height = 8,
       units = "in")


# Gemmer som tabel
table_RS_MM_3 <- model_RS_MM_3 %>%
as_tibble() %>%
  filter(feature %in% c("A3_familie", "A5_andet")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A5_andet"   ~ "Andet"
    ),
    level = case_when(
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
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level = "Attribut niveau",
    effect = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte (MM)") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_3, "table_RS_MM_3.pdf")


# Stemmesandsynlighed
# MM-model med klyngerobuste standardfejl på respondetniveau

model_RV_MM_3 <-cj(data, rating_voting ~ A1_transport + A2_oekonomi + A3_familie 
                   + A4_klima + A5_andet,  estimate = "mm",
                   id = ~participant_id)


model_RV_MM_3 %>% as_tibble()

# Vi plotter
plot(model_RV_MM_3) + 
  guides(color = "none") +
  theme_minimal() +
  labs(title = "Marginal means - Stemmesandsynlighed")


plot_RV_MM_3<- model_RV_MM_3 %>%
  as_tibble() %>%
  mutate(
    feature = case_when(
      feature == "A1_transport"  ~ "Transport",
      feature == "A2_oekonomi"   ~ "Økonomi",
      feature == "A3_familie" ~ "Familie og Børn",
      feature == "A4_klima"      ~ "Klima og Miljø",
      feature == "A5_andet"   ~ "Andet"
    ),
    feature = factor(feature, levels = c("Transport", "Økonomi",
                                         "Familie og Børn",
                                         "Klima og Miljø", "Andet")),
    level = factor(level, levels = c(
      "A1L1", "A1L2", "A1L3", "A1L4",
      "A2L1", "A2L2", "A2L3", "A2L4", "A2L5",
      "A3L10", "A3L9","A3L8", "A3L7", "A3L6", "A3L5","A3L4", "A3L3", "A3L2", "A3L1",
      "A4L1", "A4L2", "A4L3", "A4L4", "A4L5",
      "A5L11","A5L10", "A5L9", "A5L8", "A5L7", "A5L6", "A5L5", "A5L4", "A5L3","A5L2","A5L1"
    ))) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  guides(colour = "none") +
  labs(
    x = "Marginal Mean",
    y = NULL,
    title = "Stemmesandsynlighed - Marginal Means"
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  )

# Gemmer plottet
ggsave("RV_mm_3.pdf",
       plot = plot_RV_MM_3,
       width = 6,
       height = 8,
       units = "in")


##----------------------------------------------------------------------------##
# Undersøger sammenhængen mellem Familie & Børn (A3) og Andet (A5) nærmere

# Vi laver en ny variabel, der opgør, hvorvidt man på kanddiatniveau ikke har set identitetspolitik, 
# har set en kandidate med en identitær policy på Andet, på Familie & Børn eller på begge områder. 

data <- data %>%
  mutate(
    identitarian_combo = case_when(
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_ikke_identitet" ~ "Ingen",
      A3_familie_ny == "A3_identitet"      & A5_andet_ny == "A5_ikke_identitet" ~ "Kun familie",
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_identitet"      ~ "Kun andet",
      A3_familie_ny == "A3_identitet"      & A5_andet_ny == "A5_identitet"      ~ "Begge",
      TRUE ~ NA_character_
    ),
    identitarian_combo = factor(identitarian_combo,
                                levels = c("Ingen", "Kun familie", "Kun andet", "Begge"))
  )

data %>% count(identitarian_combo)
# Ikke perfekt balance, fordi der er flere niveauer (11) i Andet attributten

data %>% count(A3_familie_ny)
data %>% count(A5_andet_ny)

# Vi bekræfter, at det er grundet de flere niveauer af identitetspolitik i A5, at 
# der er denne forskel


# Vi laver MM-model på kandidatprofilniveau, hvor vi anvender den nye variabel

# Tvunget Valg
model_combo <- cj(data,
                  FC ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
                  estimate = "mm",
                  id = ~participant_id)

# Vi udtrækker estimaterne
model_combo %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo") %>%
  select(level, estimate, std.error, lower, upper)


# Er forskellen i marginal means signifikant?
 
 model_combo_signifikans <- lm(
   FC ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
   data = data
 )
 
 FC_mm <- emmeans(model_combo_signifikans, ~ identitarian_combo)
 
 pairs(FC_mm)


#Policystøtte

# Vi laver marginal means model hvor vi anvender den nye variabel i stedet for A3 og A5

model_combo_RS <- cj(data,
                     rating_support ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
                     estimate = "mm",
                     id = ~participant_id)

#Udtrækker estimater
model_combo_RS %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo") %>%
  select(level, estimate, std.error, lower, upper)


#Tjekker om det er signifikant forskelligt

model_combo_signifikans_RS <- lm(
  rating_support ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
  data = data
)

RS_mm <- emmeans(model_combo_signifikans_RS, ~ identitarian_combo)

pairs(RS_mm)


#Stemmesandsynlighed

model_combo_RV <- cj(data,
                     rating_voting ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
                     estimate = "mm",
                     id = ~participant_id)

#Udtrækker estimater
model_combo_RV %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo") %>%
  select(level, estimate, std.error, lower, upper)


#Tjekker om det er signifikany
model_combo_signifikans_RV <- lm(
  rating_voting ~ A1_transport + A2_oekonomi + identitarian_combo + A4_klima,
  data = data
)

RV_mm <- emmeans(model_combo_signifikans_RV, ~ identitarian_combo)

pairs(RV_mm)


# Interaktionsmodeller mellem Andet og Familie & Børn

# Tvunget Valg

#Andet
model_interaction_A3_A5_1_FC <- cj(data,
                                FC ~ A1_transport + A2_oekonomi + A3_familie_ny +
                                  A4_klima,
                                estimate = "amce",
                                id = ~participant_id,
                                by = ~A5_andet_ny)


# Gemmer som en tabel
table_interaction_A3_A5_1_FC <- model_interaction_A3_A5_1_FC %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A5_andet_ny, level, effect) %>%
  gt(groupname_col = "A5_andet_ny") %>%
  cols_label(
    level  = "Familie & Børn Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Familie & Børn betinget på Andet - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_1_FC, "table_interaction_A3_A5_1_FC.png")

# Plotter
plot(model_interaction_A3_A5_1_FC,
     group = "A5_andet_ny",
     feature_headers = TRUE)

plot_interaction_A3_FC <- model_interaction_A3_A5_1_FC %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      level == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A5_andet_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6) 
  )

#Familie & Børn
model_interaction_A3_A5_2_FC <- cj(data,
                                FC ~ A1_transport + A2_oekonomi  +
                                  A4_klima + A5_andet_ny,
                                estimate = "amce",
                                id = ~participant_id,
                                by = ~ A3_familie_ny)

# Gemmer i en tabel
table_interaction_A3_A5_2_FC <- model_interaction_A3_A5_2_FC %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A3_familie_ny, level, effect) %>%
  gt(groupname_col = "A3_familie_ny") %>%
  cols_label(
    level  = "Andet Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Andet betinget på Familie & Børn - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_2_FC, "table_interaction_A3_A5_2_FC.png")

# Plotter
plot_interaction_A5_FC <- model_interaction_A3_A5_2_FC %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      level == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A3_familie_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6)
  )


# Forbinder de to plot for de to attributter

combined_plot_FC_in <- (plot_interaction_A3_FC| plot_interaction_A5_FC) +
  plot_annotation(title = " Tvunget Valg") 


ggsave("combined_plot_FC_in.pdf",
       plot = combined_plot_FC_in,
       width = 12,
       height = 8,  
       units = "in",
       device = "pdf")


# Ser om interaktionskoeffieicnten er signifikant
model_interaction_FC <- lm(
  FC ~ A3_familie_ny * A5_andet_ny + A1_transport + A2_oekonomi + A4_klima,
  data = data
)

coeftest(model_interaction_FC,
         vcov = vcovCL(model_interaction_FC, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_FC <- coeftest(
  model_interaction_FC,
  vcov = vcovCL(model_interaction_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:A5_andet_nyA5_identitet" ~ "A3 Identitær × A5 Identitær",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktion: A3 × A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_FC, "table_interaction_FC.png")


#Policystøtte

# Andet
model_interaction_A3_A5_1_RS <- cj(data,
                                   rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny +
                                     A4_klima,
                                   estimate = "amce",
                                   id = ~participant_id,
                                   by = ~A5_andet_ny)

# Gemmer i en tabel
table_interaction_A3_A5_1_RS <- model_interaction_A3_A5_1_RS %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A5_andet_ny, level, effect) %>%
  gt(groupname_col = "A5_andet_ny") %>%
  cols_label(
    level  = "Familie & Børn Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Familie & Børn betinget på Andet - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_1_RS, "table_interaction_A3_A5_1_RS.png")


# Plotter
plot_interaction_A3_RS <- model_interaction_A3_A5_1_RS %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      level == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A5_andet_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6) 
  )


# Familie & Børn
model_interaction_A3_A5_2_RS <- cj(data,
                                   rating_support ~ A1_transport + A2_oekonomi  +
                                     A4_klima + A5_andet_ny,
                                   estimate = "amce",
                                   id = ~participant_id,
                                   by = ~ A3_familie_ny)
# Gemmer i en tabel
table_interaction_A3_A5_2_RS <- model_interaction_A3_A5_2_RS %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A3_familie_ny, level, effect) %>%
  gt(groupname_col = "A3_familie_ny") %>%
  cols_label(
    level  = "Andet Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Andet betinget på Familie & Børn - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_2_RS, "table_interaction_A3_A5_2_RS.png")

# Plotter
plot_interaction_A5_RS <- model_interaction_A3_A5_2_RS %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      level == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A3_familie_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6)
  )


# Gemmer de to plot for attributterne i et samlet plot

combined_plot_RS_in <- (plot_interaction_A3_RS| plot_interaction_A5_RS) +
  plot_annotation(title = "Policystøtte") 


ggsave("combined_plot_RS_in.pdf",
       plot = combined_plot_RS_in,
       width = 12,
       height = 8,  
       units = "in",
       device = "pdf")

# Tjekker om interaktionskoefficienten er signifikant

model_interaction_RS <- lm(
  rating_support ~ A3_familie_ny * A5_andet_ny + A1_transport + A2_oekonomi + A4_klima,
  data = data
)

coeftest(model_interaction_RS,
         vcov = vcovCL(model_interaction_RS, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_RS <- coeftest(
  model_interaction_RS,
  vcov = vcovCL(model_interaction_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:A5_andet_nyA5_identitet" ~ "A3 Identitær × A5 Identitær",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktion: A3 × A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_RS, "table_interaction_RS.png")


#Stemmesandsynlighed

# Andet

model_interaction_A3_A5_1_RV <- cj(data,
                                   rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny +
                                     A4_klima,
                                   estimate = "amce",
                                   id = ~participant_id,
                                   by = ~A5_andet_ny)

# Gemmer i en tabel
table_interaction_A3_A5_1_RV <- model_interaction_A3_A5_1_RV %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A5_andet_ny, level, effect) %>%
  gt(groupname_col = "A5_andet_ny") %>%
  cols_label(
    level  = "Familie & Børn Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Familie & Børn betinget på Andet - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_1_RV, "table_interaction_A3_A5_1_RV.png")

# Plotter
plot_interaction_A3_RV <- model_interaction_A3_A5_1_RV %>%
  as_tibble() %>%
  filter(feature == "A3_familie_ny") %>%
  mutate(
    feature = "Familie og Børn",
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      level == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(level)
    ),
    A5_andet_ny = case_when(
      A5_andet_ny == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      A5_andet_ny == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(A5_andet_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A5_andet_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6) 
  )

# Familie & Børn

model_interaction_A3_A5_2_RV <- cj(data,
                                   rating_voting ~ A1_transport + A2_oekonomi  +
                                     A4_klima + A5_andet_ny,
                                   estimate = "amce",
                                   id = ~participant_id,
                                   by = ~ A3_familie_ny)
# Gemmer i en tabel
table_interaction_A3_A5_2_RV <- model_interaction_A3_A5_2_RV %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(A3_familie_ny, level, effect) %>%
  gt(groupname_col = "A3_familie_ny") %>%
  cols_label(
    level  = "Andet Niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Interaktion: Andet betinget på Familie & Børn - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_interaction_A3_A5_2_RV, "table_interaction_A3_A5_2_RV.png")

# Plotter
plot_interaction_A5_RV <- model_interaction_A3_A5_2_RV %>%
  as_tibble() %>%
  filter(feature == "A5_andet_ny") %>%
  mutate(
    feature = "Andet",
    level = case_when(
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies (Andet)",
      level == "A5_identitet"      ~ "Identitære policies (Andet)",
      TRUE ~ as.character(level)
    ),
    A3_familie_ny = case_when(
      A3_familie_ny == "A3_ikke_identitet" ~ "Ikke-identitære policies (Familie & Børn)",
      A3_familie_ny == "A3_identitet"      ~ "Identitære policies (Familie & Børn)",
      TRUE ~ as.character(A3_familie_ny)
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, colour = A3_familie_ny)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 6)
  )


# Kombinerer de to plot for attributterne i et

combined_plot_RV_in <- (plot_interaction_A3_RV| plot_interaction_A5_RV) +
  plot_annotation(title = "Stemmesandsynlighed") 


ggsave("combined_plot_RV_in.pdf",
       plot = combined_plot_RV_in,
       width = 12,
       height = 8,  
       units = "in",
       device = "pdf")


# Tjekker om interaktionskoefficienten er signifikant
model_interaction_RV <- lm(
  rating_voting ~ A3_familie_ny * A5_andet_ny + A1_transport + A2_oekonomi + A4_klima,
  data = data
)

coeftest(model_interaction_RV,
         vcov = vcovCL(model_interaction_RV, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_RV <- coeftest(
  model_interaction_RV,
  vcov = vcovCL(model_interaction_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:A5_andet_nyA5_identitet" ~ "A3 Identitær × A5 Identitær",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktion: A3 × A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_RV, "table_interaction_RV.png")


#Laver et datasæt kun bestående af runder, hvor man har set en kandidat med én 
# identitær policy over for en kandidat uden. 
# For at køre denne kode skal man først køre kode fra linje 7797

data <- data %>%
  group_by(participant_id, QES) %>%
  mutate(
    intensitet_num = case_when(
      identitet_intensitet == "Ingen" ~ 0,
      identitet_intensitet == "En"    ~ 1,
      identitet_intensitet == "Begge" ~ 2,
      TRUE ~ NA_real_ 
    ),
    andet_intensitet = sum(intensitet_num) - intensitet_num,
    tie = as.integer(intensitet_num == andet_intensitet),
    pair_sum = sum(intensitet_num),
    pure_binary = as.integer(pair_sum == 1)
  ) %>%
  ungroup()


# Filter for kun (1,0) eller (0,1) situationer
data_binary <- data %>%
  filter(pure_binary == 1)

# Vi tjekker
table(data_binary$intensitet_num, data_binary$andet_intensitet)


# Laver den samme variabel igen. 
data_binary <- data_binary %>%
  mutate(
    identitarian_combo_1 = case_when(
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_ikke_identitet" ~ "Ingen",
      A3_familie_ny == "A3_identitet"      & A5_andet_ny == "A5_ikke_identitet" ~ "Kun familie",
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_identitet"      ~ "Kun andet",
      TRUE ~ NA_character_
    ),
    identitarian_combo_1 = factor(identitarian_combo_1,
                                  levels = c("Ingen", "Kun familie", "Kun andet"))
  )

data_binary %>% count(identitarian_combo_1)


# Vi laver en model med marginal means hvor vi anvender den nye variabel på dette begrænsede datasæt

# Tvunget Valg

model_combo_FC_1 <- cj(data_binary,
                       FC ~ A1_transport + A2_oekonomi + identitarian_combo_1 + A4_klima,
                       estimate = "mm",
                       id = ~participant_id)

model_combo_FC_1 %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo_1") %>%
  select(level, estimate, std.error, lower, upper)

#Policystøtte

model_combo_RS_1 <- cj(data_binary,
                       rating_support ~ A1_transport + A2_oekonomi + identitarian_combo_1 + A4_klima,
                       estimate = "mm",
                       id = ~participant_id)

model_combo_RS_1 %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo_1") %>%
  select(level, estimate, std.error, lower, upper)


#Stemmesandsynlighed

model_combo_3 <- cj(data_binary,
                   rating_voting ~ A1_transport + A2_oekonomi + identitarian_combo_1 + A4_klima,
                   estimate = "mm",
                   id = ~participant_id)

model_combo_3 %>%
  as_tibble() %>%
  filter(feature == "identitarian_combo_1") %>%
  select(level, estimate, std.error, lower, upper)


##----------------------------------------------------------------------------##
##--------------------------HETEROGENE EFFEKTER-------------------------------##
##----------------------------------------------------------------------------##

##--------------------------Hypotese 4a - Uddannelse--------------------------##

#AMCE - Tvunget valg

model_FC_AMCE_H4a <- cj(data,
                        FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ uddannelse)

model_FC_AMCE_H4a %>% as_tibble() %>% view()

# Vi plotter
plot_data_uddannelse_amce_FC <- model_FC_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
uddannelse = case_when(
  uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
  uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
  uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
  uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
  uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
  uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
  TRUE ~ as.character(uddannelse)
), uddannelse = factor(uddannelse, levels = c(
  "Grundskole",
  "Gymnasium",
  "Erhvervsuddannelse",
  "KVU",
  "MVU",
  "LVU"
)))
  

plot_uddannelse_amce_FC <- ggplot(plot_data_uddannelse_amce_FC,
                                  aes(x = estimate, y = uddannelse, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Grundskole", "Gymnasium", "Erhvervsuddannelse",
                              "KVU", "MVU", "LVU")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )


# Gemmer som tabel
table_FC_AMCE_H4a <- model_FC_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Uddannelse") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_H4a, "table_FC_AMCE_H4a.pdf")


# Laver en interaktionsmodel - Lang videregående uddannelse er referencekatgori

data <- data %>%
  mutate(uddannelse = relevel(factor(uddannelse), 
                              ref = "Lang videregående uddannelse (5 år eller mere)"))

interaction_lm_H4a_FC <- lm(FC ~ A1_transport + A2_oekonomi + 
                       A3_familie_ny * uddannelse + 
                       A4_klima + A5_andet_ny * uddannelse,
                     data = data)

coeftest(interaction_lm_H4a_FC, vcov = vcovCL(interaction_lm_H4a_FC, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_H4a_FC <- coeftest(
  interaction_lm_H4a_FC,
  vcov = vcovCL(interaction_lm_H4a_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:uddannelseGrundskole/folkeskole (inkl. 10. klasse)"        ~ "A3 Identitær × Grundskole",
      term == "A3_familie_nyA3_identitet:uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "A3 Identitær × Gymnasium",
      term == "A3_familie_nyA3_identitet:uddannelseErhvervsuddannelse og EUX"                        ~ "A3 Identitær × Erhvervsuddannelse",
      term == "A3_familie_nyA3_identitet:uddannelseKort videregående uddannelse (under 3 år)"        ~ "A3 Identitær × KVU",
      term == "A3_familie_nyA3_identitet:uddannelseMellemlang videregående uddannelse (3-4 år)"      ~ "A3 Identitær × MVU",
      term == "uddannelseGrundskole/folkeskole (inkl. 10. klasse):A5_andet_nyA5_identitet"           ~ "A5 Identitær × Grundskole",
      term == "uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX):A5_andet_nyA5_identitet"   ~ "A5 Identitær × Gymnasium",
      term == "uddannelseErhvervsuddannelse og EUX:A5_andet_nyA5_identitet"                          ~ "A5 Identitær × Erhvervsuddannelse",
      term == "uddannelseKort videregående uddannelse (under 3 år):A5_andet_nyA5_identitet"          ~ "A5 Identitær × KVU",
      term == "uddannelseMellemlang videregående uddannelse (3-4 år):A5_andet_nyA5_identitet"        ~ "A5 Identitær × MVU",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Uddannelse × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: LVU. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4a_FC, "table_interaction_H4a_FC.png")


# AMCE - Policystøtte

model_RS_AMCE_H4a <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ uddannelse)


model_RS_AMCE_H4a %>% as_tibble() %>% view()

# Vi plotter
plot_data_uddannelse_amce_RS <- model_RS_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ), 
uddannelse = case_when(
  uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
  uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
  uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
  uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
  uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
  uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
  TRUE ~ as.character(uddannelse)
),uddannelse = factor(uddannelse, levels = c(
  "Grundskole",
  "Gymnasium",
  "Erhvervsuddannelse",
  "KVU",
  "MVU",
  "LVU"
)))

plot_uddannelse_amce_RS <- ggplot(plot_data_uddannelse_amce_RS,
                                  aes(x = estimate, y = uddannelse, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Grundskole", "Gymnasium", "Erhvervsuddannelse",
                              "KVU", "MVU", "LVU")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )


# Gemmer som tabel
table_RS_AMCE_H4a <- model_RS_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Uddannelse") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_H4a, "table_RS_AMCE_H4a.pdf")


# Laver en interaktions model - Lang videregående uddannelse er referencekatgori

interaction_lm_H4a_RS <- lm(rating_support ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * uddannelse + 
                              A4_klima + A5_andet_ny * uddannelse,
                            data = data)

coeftest(interaction_lm_H4a_RS, vcov = vcovCL(interaction_lm_H4a_RS, cluster = ~participant_id))


# Gemmer som tabel
tidy_interaction_H4a_RS <- coeftest(
  interaction_lm_H4a_RS,
  vcov = vcovCL(interaction_lm_H4a_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:uddannelseGrundskole/folkeskole (inkl. 10. klasse)"        ~ "A3 Identitær × Grundskole",
      term == "A3_familie_nyA3_identitet:uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "A3 Identitær × Gymnasium",
      term == "A3_familie_nyA3_identitet:uddannelseErhvervsuddannelse og EUX"                        ~ "A3 Identitær × Erhvervsuddannelse",
      term == "A3_familie_nyA3_identitet:uddannelseKort videregående uddannelse (under 3 år)"        ~ "A3 Identitær × KVU",
      term == "A3_familie_nyA3_identitet:uddannelseMellemlang videregående uddannelse (3-4 år)"      ~ "A3 Identitær × MVU",
      term == "uddannelseGrundskole/folkeskole (inkl. 10. klasse):A5_andet_nyA5_identitet"           ~ "A5 Identitær × Grundskole",
      term == "uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX):A5_andet_nyA5_identitet"   ~ "A5 Identitær × Gymnasium",
      term == "uddannelseErhvervsuddannelse og EUX:A5_andet_nyA5_identitet"                          ~ "A5 Identitær × Erhvervsuddannelse",
      term == "uddannelseKort videregående uddannelse (under 3 år):A5_andet_nyA5_identitet"          ~ "A5 Identitær × KVU",
      term == "uddannelseMellemlang videregående uddannelse (3-4 år):A5_andet_nyA5_identitet"        ~ "A5 Identitær × MVU",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Uddannelse × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: LVU. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4a_RS, "table_interaction_H4a_RS.png")


# AMCE -  Stemmesandsynlighed

model_RV_AMCE_H4a <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ uddannelse)


model_RV_AMCE_H4a %>% as_tibble() %>% view()

# Vi plotter
plot_data_uddannelse_amce_RV <- model_RV_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),uddannelse = factor(uddannelse, levels = c(
      "Grundskole",
      "Gymnasium",
      "Erhvervsuddannelse",
      "KVU",
      "MVU",
      "LVU"
    )))


plot_uddannelse_amce_RV <- ggplot(plot_data_uddannelse_amce_RV,
                                  aes(x = estimate, y = uddannelse, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Grundskole", "Gymnasium", "Erhvervsuddannelse",
                              "KVU", "MVU", "LVU")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_AMCE_H4a <- model_RV_AMCE_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Uddannelse") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_H4a, "table_RV_AMCE_H4a.pdf")

# Vi laver en interaktionsmodel, hvor LVU er referencekategori

interaction_lm_H4a_RV <- lm(rating_voting ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * uddannelse + 
                              A4_klima + A5_andet_ny * uddannelse,
                            data = data)

coeftest(interaction_lm_H4a_RV, vcov = vcovCL(interaction_lm_H4a_RV, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4a_RV <- coeftest(
  interaction_lm_H4a_RV,
  vcov = vcovCL(interaction_lm_H4a_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:uddannelseGrundskole/folkeskole (inkl. 10. klasse)"        ~ "A3 Identitær × Grundskole",
      term == "A3_familie_nyA3_identitet:uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "A3 Identitær × Gymnasium",
      term == "A3_familie_nyA3_identitet:uddannelseErhvervsuddannelse og EUX"                        ~ "A3 Identitær × Erhvervsuddannelse",
      term == "A3_familie_nyA3_identitet:uddannelseKort videregående uddannelse (under 3 år)"        ~ "A3 Identitær × KVU",
      term == "A3_familie_nyA3_identitet:uddannelseMellemlang videregående uddannelse (3-4 år)"      ~ "A3 Identitær × MVU",
      term == "uddannelseGrundskole/folkeskole (inkl. 10. klasse):A5_andet_nyA5_identitet"           ~ "A5 Identitær × Grundskole",
      term == "uddannelseGymnasial uddannelse (F.eks. STX, HHX, HF, HTX):A5_andet_nyA5_identitet"   ~ "A5 Identitær × Gymnasium",
      term == "uddannelseErhvervsuddannelse og EUX:A5_andet_nyA5_identitet"                          ~ "A5 Identitær × Erhvervsuddannelse",
      term == "uddannelseKort videregående uddannelse (under 3 år):A5_andet_nyA5_identitet"          ~ "A5 Identitær × KVU",
      term == "uddannelseMellemlang videregående uddannelse (3-4 år):A5_andet_nyA5_identitet"        ~ "A5 Identitær × MVU",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Uddannelse × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: LVU. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4a_RV, "table_interaction_H4a_RV.png")


# Samler de tre AMCE plots i en figur

uddannelse_AMCE <- (plot_uddannelse_amce_FC |plot_uddannelse_amce_RS | plot_uddannelse_amce_RV) 


ggsave("uddannelse_AMCE.png",
       plot = uddannelse_AMCE,
       width = 15,
       height = 8,  
       units = "in",
       device = "png")



# MM - Tvunget valg

model_FC_MM_H4a <- cj(data,
                    FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                    estimate = "mm",
                    id = ~participant_id,
                    by = ~ uddannelse )

model_FC_MM_H4a %>% as_tibble() %>% view()


# Vi plotter
plot_data_H4a_FC <- model_FC_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ), uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),uddannelse = factor(uddannelse, levels = c(
      "LVU",
      "MVU",
      "KVU",
      "Erhvervsuddannelse",
      "Gymnasium",
      "Grundskole" )))


plot_uddannelse_FC_MM_H4a <- ggplot(plot_data_H4a_FC,
                                    aes(x = estimate, y = uddannelse, colour = uddannelse)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_FC_MM_H4a <- model_FC_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Uddannelse") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_H4a, "table_FC_MM_H4a.png")


# MM - Policystøtte

model_RS_MM_H4a <- cj(data,
                      rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ uddannelse )

model_RS_MM_H4a %>% as_tibble() %>% view()


# Vi plotter
plot_data_H4a_RS <- model_RS_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ), uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),uddannelse = factor(uddannelse, levels = c(
      "LVU",
      "MVU",
      "KVU",
      "Erhvervsuddannelse",
      "Gymnasium",
      "Grundskole" )))


plot_uddannelse_RS_MM_H4a <- ggplot(plot_data_H4a_RS,
                                    aes(x = estimate, y = uddannelse, colour = uddannelse)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Uddannelse"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RS_MM_H4a <- model_RS_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Uddannelse") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_H4a, "table_RS_MM_H4a.png")


# MM - Stemmesandsynlighed

model_RV_MM_H4a <- cj(data,
                      rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ uddannelse )

model_RV_MM_H4a %>% as_tibble()


# Vi plotter
plot_data_H4a_RV <- model_RV_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ), uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"        ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),uddannelse = factor(uddannelse, levels = c(
      "LVU",
      "MVU",
      "KVU",
      "Erhvervsuddannelse",
      "Gymnasium",
      "Grundskole" )))


plot_uddannelse_RV_MM_H4a <- ggplot(plot_data_H4a_RV,
                                    aes(x = estimate, y = uddannelse, colour = uddannelse)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RV_MM_H4a <- model_RV_MM_H4a %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    uddannelse = case_when(
      uddannelse == "Grundskole/folkeskole (inkl. 10. klasse)"         ~ "Grundskole",
      uddannelse == "Gymnasial uddannelse (F.eks. STX, HHX, HF, HTX)" ~ "Gymnasium",
      uddannelse == "Erhvervsuddannelse og EUX"                        ~ "Erhvervsuddannelse",
      uddannelse == "Kort videregående uddannelse (under 3 år)"        ~ "KVU",
      uddannelse == "Mellemlang videregående uddannelse (3-4 år)"      ~ "MVU",
      uddannelse == "Lang videregående uddannelse (5 år eller mere)"   ~ "LVU",
      TRUE ~ as.character(uddannelse)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(uddannelse, feature, level, effect) %>%
  gt(groupname_col = "uddannelse") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Uddannelse") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_H4a, "table_RV_MM_H4a.png")


# Samler de tre MMplots i en figur

uddannelse_MM <- (plot_uddannelse_FC_MM_H4a|plot_uddannelse_RS_MM_H4a| plot_uddannelse_RV_MM_H4a) 


ggsave("uddannelse_MM.png",
       plot = uddannelse_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


##------------------------------ Hypotese 4b - Bopæl--------------------------##

# AMCE - Tvunget Valg

model_FC_AMCE_H4b <- cj(data,
                       FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                       estimate = "amce",
                       id = ~participant_id,
                       by = ~ kommune_kategori)

model_FC_AMCE_H4b %>% as_tibble() %>% view()


# Vi plotter
plot_data_kommune_amce_FC <- model_FC_AMCE_H4b %>%
  as_tibble() %>%
  filter(!is.na(std.error)) %>% 
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_FC_amce_H4b <- ggplot(plot_data_kommune_amce_FC,
                                  aes(x = estimate, y = kommune_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Landkommune", "Oplandskommune",
                              "Provinsbykommune", "Storkøbenhavn", "Storbykommune")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )


# Gemmer som tabel
table_FC_AMCE_H4b <- model_FC_AMCE_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Kommune") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_H4b, "table_FC_AMCE_H4b.png")



# Laver interaktionsmodel - storbykommune er referencekategori

interaction_lm_H4b_FC <- lm(FC ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * kommune_kategori + 
                              A4_klima + A5_andet_ny * kommune_kategori,
                            data = data)

coeftest(interaction_lm_H4b_FC, vcov = vcovCL(interaction_lm_H4b_FC, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4b_FC <- coeftest(
  interaction_lm_H4b_FC,
  vcov = vcovCL(interaction_lm_H4b_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:kommune_kategoriLandkommune"      ~ "A3 Identitær × Landkommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriOplandskommune"   ~ "A3 Identitær × Oplandskommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriProvinsbykommune" ~ "A3 Identitær × Provinsbykommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriStorkøbenhavn"    ~ "A3 Identitær × Storkøbenhavn",
      term == "kommune_kategoriLandkommune:A5_andet_nyA5_identitet"        ~ "A5 Identitær × Landkommune",
      term == "kommune_kategoriOplandskommune:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Oplandskommune",
      term == "kommune_kategoriProvinsbykommune:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Provinsbykommune",
      term == "kommune_kategoriStorkøbenhavn:A5_andet_nyA5_identitet"      ~ "A5 Identitær × Storkøbenhavn",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Kommune × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Storbykommune. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4b_FC, "table_interaction_H4b_FC.png")


# AMCE - Policystøtte

model_RS_AMCE_H4b <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ kommune_kategori)

model_RS_AMCE_H4b %>% as_tibble() %>% view()


# Vi plotter
plot_data_kommune_amce_RS <- model_RS_AMCE_H4b %>%
  as_tibble() %>%
  filter(!is.na(std.error)) %>% 
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )


plot_RS_amce_H4b <- ggplot(plot_data_kommune_amce_RS,
                           aes(x = estimate, y = kommune_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Landkommune", "Oplandskommune",
                              "Provinsbykommune", "Storkøbenhavn", "Storbykommune")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RS_AMCE_H4b <- model_RS_AMCE_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Kommune") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_H4b, "table_RS_AMCE_H4b.png")


# Laver en interaktionsmodel - storbykommune er referencekategory

interaction_lm_H4b_RS <- lm(rating_support ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * kommune_kategori + 
                              A4_klima + A5_andet_ny * kommune_kategori,
                            data = data)

coeftest(interaction_lm_H4b_RS, vcov = vcovCL(interaction_lm_H4b_RS, cluster = ~participant_id))


# Gemmer som tabel
tidy_interaction_H4b_RS <- coeftest(
  interaction_lm_H4b_RS,
  vcov = vcovCL(interaction_lm_H4b_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:kommune_kategoriLandkommune"      ~ "A3 Identitær × Landkommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriOplandskommune"   ~ "A3 Identitær × Oplandskommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriProvinsbykommune" ~ "A3 Identitær × Provinsbykommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriStorkøbenhavn"    ~ "A3 Identitær × Storkøbenhavn",
      term == "kommune_kategoriLandkommune:A5_andet_nyA5_identitet"        ~ "A5 Identitær × Landkommune",
      term == "kommune_kategoriOplandskommune:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Oplandskommune",
      term == "kommune_kategoriProvinsbykommune:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Provinsbykommune",
      term == "kommune_kategoriStorkøbenhavn:A5_andet_nyA5_identitet"      ~ "A5 Identitær × Storkøbenhavn",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Kommune × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Storbykommune. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4b_RS, "table_interaction_H4b_RS.png")


# AMCE - Stemmesandsynlighed

model_RV_AMCE_H4b <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ kommune_kategori)

model_RV_AMCE_H4b %>% as_tibble() %>% view()


# Vi plotter
plot_data_kommune_amce_RV <- model_RV_AMCE_H4b %>%
  as_tibble() %>%
  filter(!is.na(std.error)) %>% 
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_RV_amce_H4b <- ggplot(plot_data_kommune_amce_RV,
                           aes(x = estimate, y = kommune_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Landkommune", "Oplandskommune",
                              "Provinsbykommune", "Storkøbenhavn", "Storbykommune")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_AMCE_H4b <- model_RV_AMCE_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Kommune") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_H4b, "table_RV_AMCE_H4b.png")


# Laver en interaktionsmodel - storbykommune er referencekategori
interaction_lm_H4b_RV <- lm(rating_voting ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * kommune_kategori + 
                              A4_klima + A5_andet_ny * kommune_kategori,
                            data = data)

coeftest(interaction_lm_H4b_RV, vcov = vcovCL(interaction_lm_H4b_RV, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4b_RV <- coeftest(
  interaction_lm_H4b_RV,
  vcov = vcovCL(interaction_lm_H4b_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:kommune_kategoriLandkommune"      ~ "A3 Identitær × Landkommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriOplandskommune"   ~ "A3 Identitær × Oplandskommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriProvinsbykommune" ~ "A3 Identitær × Provinsbykommune",
      term == "A3_familie_nyA3_identitet:kommune_kategoriStorkøbenhavn"    ~ "A3 Identitær × Storkøbenhavn",
      term == "kommune_kategoriLandkommune:A5_andet_nyA5_identitet"        ~ "A5 Identitær × Landkommune",
      term == "kommune_kategoriOplandskommune:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Oplandskommune",
      term == "kommune_kategoriProvinsbykommune:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Provinsbykommune",
      term == "kommune_kategoriStorkøbenhavn:A5_andet_nyA5_identitet"      ~ "A5 Identitær × Storkøbenhavn",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Kommune × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Storbykommune. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4b_RV, "table_interaction_H4b_RV.png")

# Samler de 3 AMCE plot i et
kommune_amce <- (plot_FC_amce_H4b|plot_RS_amce_H4b| plot_RV_amce_H4b) 


ggsave("kommune_amce.png",
       plot = kommune_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# MM - Tvunget Valg

model_FC_MM_H4b_FC <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ kommune_kategori)


# Vi plotter
plot_data_MM_H4b_FC <- model_FC_MM_H4b_FC %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),kommune_kategori = case_when(
      kommune_kategori == "Storbykommune"    ~ "Storby",
      kommune_kategori == "Landkommune"      ~ "Landk",
      kommune_kategori == "Oplandskommune"   ~ "Opland",
      kommune_kategori == "Provinsbykommune" ~ "Provinsby",
      kommune_kategori == "Storkøbenhavn"    ~ "Storkbh",
      TRUE ~ as.character(kommune_kategori)
    )
  )


plot_MM_FC_H4b <- ggplot(plot_data_MM_H4b_FC,
                                    aes(x = estimate, y = kommune_kategori, colour = kommune_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_FC_MM_H4b <- model_FC_MM_H4b_FC %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Kommune") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_H4b, "table_FC_MM_H4b.png")


# MM - Policystøtte

model_RS_MM_H4b <- cj(data,
                         rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                         estimate = "mm",
                         id = ~participant_id,
                         by = ~ kommune_kategori)

# Vi plotter
plot_data_MM_H4b_RS <- model_RS_MM_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),kommune_kategori = case_when(
      kommune_kategori == "Storbykommune"    ~ "Storby",
      kommune_kategori == "Landkommune"      ~ "Land",
      kommune_kategori == "Oplandskommune"   ~ "Opland",
      kommune_kategori == "Provinsbykommune" ~ "Provinsby",
      kommune_kategori == "Storkøbenhavn"    ~ "Storkbh.",
      TRUE ~ as.character(kommune_kategori)
    )
  )


plot_MM_RS_H4b <- ggplot(plot_data_MM_H4b_RS,
                         aes(x = estimate, y = kommune_kategori, colour = kommune_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6))  +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Kommune"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RS_MM_H4b <- model_RS_MM_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Kommune") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_H4b, "table_RS_MM_H4b.png")


# MM -Stemmesandsynlighed

model_RV_MM_H4b <- cj(data,
                         rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                         estimate = "mm",
                         id = ~participant_id,
                         by = ~ kommune_kategori)


# Vi plotter
plot_data_MM_H4b_RV <- model_RV_MM_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),kommune_kategori = case_when(
      kommune_kategori == "Storbykommune"    ~ "Storby",
      kommune_kategori == "Landkommune"      ~ "Landk",
      kommune_kategori == "Oplandskommune"   ~ "Opland",
      kommune_kategori == "Provinsbykommune" ~ "Provinsby",
      kommune_kategori == "Storkøbenhavn"    ~ "Storkbh",
      TRUE ~ as.character(kommune_kategori)
    )
  )


plot_MM_RV_H4b <- ggplot(plot_data_MM_H4b_RV,
                         aes(x = estimate, y = kommune_kategori, colour = kommune_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6))  +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_RV_MM_H4b <- model_RV_MM_H4b %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(kommune_kategori, feature, level, effect) %>%
  gt(groupname_col = "kommune_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Kommune") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_H4b, "table_RV_MM_H4b.png")

# Samler de tre MM - plots i en figur

kommune_MM <- (plot_MM_FC_H4b|plot_MM_RS_H4b| plot_MM_RV_H4b) 


ggsave("kommune_MM.png",
       plot = kommune_MM,
       width = 20,
       height = 8,  
       units = "in",
       device = "png")



##-------------------------------- Hypotese 4d - Køn------------------------- ##

# AMCE - Tvunget Valg
model_FC_AMCE_H4d <- cj(data,
                       FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                       + A5_andet_ny,
                       estimate = "amce",
                       id = ~participant_id,
                       by = ~ gender_b)

model_FC_AMCE_H4d %>% as_tibble() %>% view()


# Vi plotter det
plot_data_gender_amce_FC <- model_FC_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    gender_b = factor(gender_b, levels = c("Mand", "Kvinde"))
  ) 


plot_amce_FC_H4d <-  ggplot(plot_data_gender_amce_FC,
                            aes(x = estimate, y = gender_b, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Mand", "Kvinde")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_FC_AMCE_H4d <- model_FC_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Køn") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_H4d, "table_FC_AMCE_H4d.png")


# Laver en interaktionsmodel - mand er referencekatgeorien

interaction_lm_H4d_FC <- lm(FC ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * gender_b + 
                              A4_klima + A5_andet_ny * gender_b,
                            data = data)

coeftest(interaction_lm_H4d_FC, vcov = vcovCL(interaction_lm_H4d_FC, cluster = ~participant_id))


# Gemmer som tabel
tidy_interaction_H4d_FC <- coeftest(
  interaction_lm_H4d_FC,
  vcov = vcovCL(interaction_lm_H4d_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:gender_bKvinde" ~ "A3 Identitær × Kvinde",
      term == "gender_bKvinde:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Kvinde",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Køn × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Mand. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4d_FC, "table_interaction_H4d_FC.png")


# AMCE - Policystøtte
model_RS_AMCE_H4d <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                        + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ gender_b)

model_RS_AMCE_H4d %>% as_tibble() %>% view()

# Vi plotter det
plot_data_gender_amce_RS <- model_RS_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    gender_b = factor(gender_b, levels = c("Mand", "Kvinde"))
  )


plot_amce_RS_H4d <-  ggplot(plot_data_gender_amce_RS,
                            aes(x = estimate, y = gender_b, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Mand", "Kvinde")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RS_AMCE_H4d <- model_RS_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Køn") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_H4d, "table_RS_AMCE_H4d.png")


# Laver en interaktionsmodel - mand er referencekatgeorien

interaction_lm_H4d_RS <- lm(rating_support ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * gender_b + 
                              A4_klima + A5_andet_ny * gender_b,
                            data = data)

coeftest(interaction_lm_H4d_RS, vcov = vcovCL(interaction_lm_H4d_RS, cluster = ~participant_id))


# Gemmer som tabel
tidy_interaction_H4d_RS <- coeftest(
  interaction_lm_H4d_RS,
  vcov = vcovCL(interaction_lm_H4d_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:gender_bKvinde" ~ "A3 Identitær × Kvinde",
      term == "gender_bKvinde:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Kvinde",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Køn × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Mand. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4d_RS, "table_interaction_H4d_RS.png")


# AMCE - Stemmesandsynlighed
model_RV_AMCE_H4d <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                        + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ gender_b)

model_RV_AMCE_H4d %>% as_tibble() %>% view()

# Vi plotter det
plot_data_gender_amce_RV <- model_RV_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    gender_b = factor(gender_b, levels = c("Mand", "Kvinde"))
  )


plot_amce_RV_H4d <-  ggplot(plot_data_gender_amce_RV,
                            aes(x = estimate, y = gender_b, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Mand", "Kvinde")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_AMCE_H4d <- model_RV_AMCE_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Køn") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_H4d, "table_RV_AMCE_H4d.png")


# Laver interaktionsmodel - mand er referencekategori

interaction_lm_H4d_RV <- lm(rating_voting ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * gender_b + 
                              A4_klima + A5_andet_ny * gender_b,
                            data = data)

coeftest(interaction_lm_H4d_RV, vcov = vcovCL(interaction_lm_H4d_RV, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4d_RV <- coeftest(
  interaction_lm_H4d_RV,
  vcov = vcovCL(interaction_lm_H4d_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:gender_bKvinde" ~ "A3 Identitær × Kvinde",
      term == "gender_bKvinde:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Kvinde",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Køn × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Mand. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4d_RV, "table_interaction_H4d_RV.png")


# Samler de 3 AMCE plots
koen_amce <- (plot_amce_FC_H4d|plot_amce_RS_H4d|plot_amce_RV_H4d) 


ggsave("koen_amce.png",
       plot = koen_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")



# MM - Tvunget Valg

model_FC_MM_H4d <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ gender_b)

# Vi plotter
plot_data_MM_H4d_FC <- model_FC_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_FC_H4d <- ggplot(plot_data_MM_H4d_FC,
                         aes(x = estimate, y = gender_b, colour = gender_b)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_FC_MM_H4d <- model_FC_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Køn") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_H4d, "table_FC_MM_H4d.png")

  
# MM - Policy Støtte
model_RS_MM_H4d <- cj(data,
                      rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ gender_b)


# Vi plotter
plot_data_MM_H4d_RS <- model_RS_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_RS_H4d <- ggplot(plot_data_MM_H4d_RS,
                                           aes(x = estimate, y = gender_b, colour = gender_b)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Køn"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_RS_MM_H4d <- model_RS_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Køn") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_H4d, "table_RS_MM_H4d.png")


# MM - Stemmesandsynlighed
model_RV_MM_H4d <- cj(data,
                      rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ gender_b)

# Vi plotter
plot_data_MM_H4d_RV <- model_RV_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )


plot_MM_RV_H4d <- ggplot(plot_data_MM_H4d_RV,
                         aes(x = estimate, y = gender_b, colour = gender_b)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RV_MM_H4d <- model_RV_MM_H4d %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(gender_b, feature, level, effect) %>%
  gt(groupname_col = "gender_b") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Køn") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_H4d, "table_RV_MM_H4d.png")


# Samler de 3 MM plot
koen_MM <- (plot_MM_FC_H4d|plot_MM_RS_H4d| plot_MM_RV_H4d) 


ggsave("koen_MM2.png",
       plot = koen_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")



##------------------ Hypotese 4c - ideologi (og partivalg)------------------- ##

# Ideologi

# AMCE -Tvunget Valg

model_FC_AMCE_H4c <- cj(data,
                        FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ ideologi_kategori)

model_FC_AMCE_H4c %>% as_tibble() %>% view()


plot_data_ideologi_amce_FC <- model_FC_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_amce_FC_H4c <-  ggplot( plot_data_ideologi_amce_FC,
                      aes(x = estimate, y = ideologi_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Venstre", "Centrum", "Højre")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_FC_AMCE_H4c <- model_FC_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Ideologi") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_H4c, "table_FC_AMCE_H4c.png")


# AMCE - Policystøtte

model_RS_AMCE_H4c <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ ideologi_kategori)

# Vi plotter det
plot_data_ideologi_amce_RS <- model_RS_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_amce_RS_H4c <- ggplot( plot_data_ideologi_amce_RS,
                            aes(x = estimate, y = ideologi_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Venstre", "Centrum", "Højre")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RS_AMCE_H4c <- model_RS_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Ideologi") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_H4c, "table_RS_AMCE_H4c.png")


# AMCE - Stemmesandsynlighed
model_RV_AMCE_H4c <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ ideologi_kategori)


plot_data_ideologi_amce_RV <- model_RV_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_amce_RV_H4c <- ggplot(plot_data_ideologi_amce_RV,
                            aes(x = estimate, y = ideologi_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Venstre", "Centrum", "Højre")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_AMCE_H4c <- model_RV_AMCE_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Ideologi") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_H4c, "table_RV_AMCE_H4c.png")


# Samler de 3 AMCE plot i et
ideologi_amce <- (plot_amce_FC_H4c|plot_amce_RS_H4c| plot_amce_RV_H4c) 


ggsave("ideologi_amce.png",
       plot = ideologi_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# MM - Tvunget Valg

model_FC_MM_H4c <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ ideologi_kategori)


## Vi plotter
plot_data_MM_H4c_FC <- model_FC_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_FC_H4c <- ggplot(plot_data_MM_H4c_FC,
                         aes(x = estimate, y = ideologi_kategori, colour = ideologi_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_FC_MM_H4c <- model_FC_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Ideologi") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_H4c, "table_FC_MM_H4c.png")


# MM - Policystøtte

model_RS_MM_H4c <- cj(data,
                      rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ ideologi_kategori)


## Vi plotter
plot_data_MM_H4c_RS <- model_RS_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )


plot_MM_RS_H4c <- ggplot(plot_data_MM_H4c_RS,
                         aes(x = estimate, y = ideologi_kategori, colour = ideologi_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Ideologi"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RS_MM_H4c <- model_RS_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Ideologi") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_H4c, "table_RS_MM_H4c.png")


# MM - Stemmesandsynlighed

model_RV_MM_H4c <- cj(data,
                      rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ ideologi_kategori)


## Vi plotter
plot_data_MM_H4c_RV <- model_RV_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_RV_H4c <- ggplot(plot_data_MM_H4c_RV,
                         aes(x = estimate, y = ideologi_kategori, colour = ideologi_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_RV_MM_H4c <- model_RV_MM_H4c %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(ideologi_kategori, feature, level, effect) %>%
  gt(groupname_col = "ideologi_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Ideologi") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_H4c, "table_RV_MM_H4c.png")


# Samler de 3 MM plot i et
ideologi_MM <- (plot_MM_FC_H4c|plot_MM_RS_H4c| plot_MM_RV_H4c) 


ggsave("idoelogi_MM.png",
       plot = ideologi_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# Laver interaktionsmodeller

# Tvunget Valg

model_FC_interaktion_H4c <- lm(FC ~ A3_familie_ny * ideologi + 
                          A5_andet_ny * ideologi +
                          A1_transport + A2_oekonomi + A4_klima,
                        data = data)

# Med klyngerobuste standardfejl på respondetniveau

coeftest(model_FC_interaktion_H4c,
         vcov = vcovCL(model_FC_interaktion_H4c, cluster = ~participant_id))


# Gemmer som tabel
tidy_interaction_H4c_FC <- coeftest(
  model_FC_interaktion_H4c,
  vcov = vcovCL(model_FC_interaktion_H4c, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:ideologi" ~ "A3 Identitær × Ideologi",
      term == "ideologi:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Ideologi",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Ideologi × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_FC, "table_interaction_H4c_FC.png")


# Laver et marginal effects plot 

# For A3 - Familie og børn

plot_ME_familie_FC_H4c <- plot_slopes(model_FC_interaktion_H4c,
            variables = "A3_familie_ny",
            condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Idelogi",
       y = "Marginale effekter",
       title = "Familie & Børn - Tvunget Valg")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()


# For A5 - Andet

plot_ME_andet_FC_H4c <- plot_slopes(model_FC_interaktion_H4c,
                                      variables = "A5_andet_ny",
                                      condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Ideologi",
       y = "Marginale effekter",
       title = "Andet - Tvunget Valg")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()



# Gemmer de to marginal effects plot i et
combined_ME_FC_H4c <- (plot_ME_familie_FC_H4c | plot_ME_andet_FC_H4c) 

ggsave("ME_FC_H4c.png",
       plot = combined_ME_FC_H4c,
       width = 12,
       height = 6,
       units = "in",
       device = "png")



# Policystøtte

model_RS_interaktion_H4c <- lm(rating_support ~ A3_familie_ny * ideologi + 
                                 A5_andet_ny * ideologi +
                                 A1_transport + A2_oekonomi + A4_klima,
                               data = data)

# Med klyngerobuste standardfejl på respondetniveau

coeftest(model_RS_interaktion_H4c,
         vcov = vcovCL(model_RS_interaktion_H4c, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4c_RS <- coeftest(
  model_RS_interaktion_H4c,
  vcov = vcovCL(model_RS_interaktion_H4c, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:ideologi" ~ "A3 Identitær × Ideologi",
      term == "ideologi:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Ideologi",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Ideologi × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_RS, "table_interaction_H4c_RS.png")


# Marginal Effects Plots

# For A3 - Familie og børn 

plot_ME_familie_RS_H4c <- plot_slopes(model_RS_interaktion_H4c,
                                      variables = "A3_familie_ny",
                                      condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Idelogi",
       y = "Marginale effekter",
       title = "Familie & Børn - Policy Støtte")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()



# For A5 - Andet

plot_ME_andet_RS_H4c <- plot_slopes(model_RS_interaktion_H4c,
                                    variables = "A5_andet_ny",
                                    condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Ideologi",
       y = "Marginale effekter",
       title = "Andet - Policy Støtte")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()



# Forbinder de to plots
combined_ME_RS_H4c <- (plot_ME_familie_RS_H4c | plot_ME_andet_RS_H4c) 

ggsave("ME_RS_H4c.png",
       plot = combined_ME_RS_H4c,
       width = 12,
       height = 6,
       units = "in",
       device = "png")



#Stemmesandsynlighed

model_RV_interaktion_H4c <- lm(rating_voting ~ A3_familie_ny * ideologi + 
                                 A5_andet_ny * ideologi +
                                 A1_transport + A2_oekonomi + A4_klima,
                               data = data)

# Med klyngerobuste standardfejl på respondetniveau

coeftest(model_RV_interaktion_H4c,
         vcov = vcovCL(model_RV_interaktion_H4c, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4c_RV <- coeftest(
  model_RV_interaktion_H4c,
  vcov = vcovCL(model_RV_interaktion_H4c, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:ideologi" ~ "A3 Identitær × Ideologi",
      term == "ideologi:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Ideologi",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Ideologi × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_RV, "table_interaction_H4c_RV.png")

# Marginal Effects Plot

# For A3 - Familie og børn 

plot_ME_familie_RV_H4c <- plot_slopes(model_RV_interaktion_H4c,
                                      variables = "A3_familie_ny",
                                      condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Idelogi",
       y = "Marginale effekter",
       title = "Familie & Børn - Stemmsandsynlighed")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()

# For A5 - Andet
plot_ME_andet_RV_H4c <- plot_slopes(model_RV_interaktion_H4c,
                                    variables = "A5_andet_ny",
                                    condition = "ideologi", vcov = ~participant_id) + 
  labs(x = "Ideologi",
       y = "Marginale effekter",
       title = "Andet - Stemmesandsynlighed")+
  scale_x_continuous(breaks = 0:10) + theme_minimal()


# Forbinder de to plots
combined_ME_RV_H4c <- (plot_ME_familie_RV_H4c | plot_ME_andet_RV_H4c) 

ggsave("ME_RV_H4c.png",
       plot = combined_ME_RS_H4c,
       width = 12,
       height = 6,
       units = "in",
       device = "png")


##----------------------------------Partivalg---------------------------------##

# AMCE - Tvunget Valg

model_FC_AMCE_H4c_2 <- cj(data,
                        FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~ participant_id,
                        by = ~ parti_kategori)


# Vi plotter
plot_data_parti_amce_FC <- model_FC_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_amce_parti_FC <-  ggplot(plot_data_parti_amce_FC,
                              aes(x = estimate, y = parti_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Rød blok", "Midten", "Blå blok")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_FC_AMCE_parti <- model_FC_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Parti") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_parti, "table_FC_AMCE_parti.png")


# Laver interaktionsmodel - Rød blok er referencekategori

interaction_lm_H4c_2_FC <- lm(FC ~ A1_transport + A2_oekonomi + 
                              A3_familie_ny * parti_kategori + 
                              A4_klima + A5_andet_ny * parti_kategori,
                            data = data)

coeftest(interaction_lm_H4c_2_FC, vcov = vcovCL(interaction_lm_H4c_2_FC, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4c_2_FC <- coeftest(
  interaction_lm_H4c_2_FC,
  vcov = vcovCL(interaction_lm_H4c_2_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:parti_kategoriMidten"   ~ "A3 Identitær × Midten",
      term == "A3_familie_nyA3_identitet:parti_kategoriBlå blok" ~ "A3 Identitær × Blå blok",
      term == "parti_kategoriMidten:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Midten",
      term == "parti_kategoriBlå blok:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Blå blok",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Parti × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Rød blok. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_2_FC, "table_interaction_H4c_2_FC.png")



# AMCE - Policystøtte

model_RS_AMCE_H4c_2 <- cj(data,
                          rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                          estimate = "amce",
                          id = ~ participant_id,
                          by = ~ parti_kategori)

# Vi plotter
plot_data_parti_amce_RS <- model_RS_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_amce_parti_RS <- ggplot(plot_data_parti_amce_RS,
                             aes(x = estimate, y = parti_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Rød blok", "Midten", "Blå blok")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )


# Gemmer som tabel
table_RS_AMCE_parti <- model_RS_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Parti") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_parti, "table_RS_AMCE_parti.png")



# Laver interaktionsmodel - rød blok er referencekategori

interaction_lm_H4c_2_RS <- lm(rating_support ~ A1_transport + A2_oekonomi + 
                                A3_familie_ny * parti_kategori + 
                                A4_klima + A5_andet_ny * parti_kategori,
                              data = data)

coeftest(interaction_lm_H4c_2_RS, vcov = vcovCL(interaction_lm_H4c_2_RS, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4c_2_RS <- coeftest(
  interaction_lm_H4c_2_RS,
  vcov = vcovCL(interaction_lm_H4c_2_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:parti_kategoriMidten"   ~ "A3 Identitær × Midten",
      term == "A3_familie_nyA3_identitet:parti_kategoriBlå blok" ~ "A3 Identitær × Blå blok",
      term == "parti_kategoriMidten:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Midten",
      term == "parti_kategoriBlå blok:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Blå blok",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Parti × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Rød blok. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_2_RS, "table_interaction_H4c_2_RS.png")


# AMCE - Stemmesandsynlighed

model_RV_AMCE_H4c_2 <- cj(data,
                          rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                          estimate = "amce",
                          id = ~ participant_id,
                          by = ~ parti_kategori)


# Vi plotter
plot_data_parti_amce_RV <- model_RV_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_amce_parti_RV <-ggplot(plot_data_parti_amce_RV,
                            aes(x = estimate, y = parti_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Rød blok", "Midten", "Blå blok")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RV_AMCE_parti <- model_RV_AMCE_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Parti") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_parti, "table_RV_AMCE_parti.png")

# Laver interaktionsmodel - rød blok er referencekategori

interaction_lm_H4c_2_RV <- lm(rating_voting ~ A1_transport + A2_oekonomi + 
                                A3_familie_ny * parti_kategori + 
                                A4_klima + A5_andet_ny * parti_kategori,
                              data = data)

coeftest(interaction_lm_H4c_2_RV, vcov = vcovCL(interaction_lm_H4c_2_RV, cluster = ~participant_id))

# Gemmer som tabel
tidy_interaction_H4c_2_RV <- coeftest(
  interaction_lm_H4c_2_RV,
  vcov = vcovCL(interaction_lm_H4c_2_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:parti_kategoriMidten"   ~ "A3 Identitær × Midten",
      term == "A3_familie_nyA3_identitet:parti_kategoriBlå blok" ~ "A3 Identitær × Blå blok",
      term == "parti_kategoriMidten:A5_andet_nyA5_identitet"     ~ "A5 Identitær × Midten",
      term == "parti_kategoriBlå blok:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Blå blok",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Parti × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Rød blok. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_H4c_2_RV, "table_interaction_H4c_2_RV.png")


# Samler de 3 AMCE plot i et
parti_amce <- (plot_amce_parti_FC|plot_amce_parti_RS| plot_amce_parti_RV) 


ggsave("parti_amce.png",
       plot = parti_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# MM - Tvunget Valg

model_FC_MM_H4c_2 <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ parti_kategori)

# Vi plotter
plot_data_MM_H4c_2_FC <- model_FC_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_mm_parti_FC <- ggplot(plot_data_MM_H4c_2_FC,
                           aes(x = estimate, y = parti_kategori, colour = parti_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  scale_x_continuous(limits = c(NA, 0.7)) + 
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_FC_MM_H4c_2 <- model_FC_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Parti") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_H4c_2, "table_FC_MM_H4c_2.png")

# MM - Policystøtte

model_RS_MM_H4c_2 <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "mm",
                        id = ~participant_id,
                        by = ~ parti_kategori)


# Vi plotter
plot_data_MM_H4c_2_RS <- model_RS_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )


plot_mm_parti_RS <- ggplot(plot_data_MM_H4c_2_RS,
                           aes(x = estimate, y = parti_kategori, colour = parti_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  scale_x_continuous(limits = c(2, NA)) + 
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Partiblok"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer som tabel
table_RS_MM_H4c_2 <- model_RS_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Parti") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_H4c_2, "table_RS_MM_H4c_2.png")



# MM - Stemmesandsynlighed

model_RV_MM_H4c_2 <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "mm",
                        id = ~participant_id,
                        by = ~ parti_kategori)

# Vi plotter
plot_data_MM_H4c_2_RV <- model_RV_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_mm_parti_RV <- ggplot(plot_data_MM_H4c_2_RV,
                           aes(x = estimate, y = parti_kategori, colour = parti_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  scale_x_continuous(limits = c(NA, 5)) + 
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer som tabel
table_RV_MM_H4c_2 <- model_RV_MM_H4c_2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(parti_kategori, feature, level, effect) %>%
  gt(groupname_col = "parti_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Parti") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_H4c_2, "table_RV_MM_H4c_2.png")

table(data$parti)

# Vi har et powerproblem for Midten. Består kun af Moderaterne
# Kun 62 respondenter har svaret Moderaterne. 


# Samler de 3 MM plot i et
parti_MM <- (plot_mm_parti_FC|plot_mm_parti_RS| plot_mm_parti_RV) 


ggsave("parti_MM2.png",
       plot = parti_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")



##-----------------------------Hypotese 6 - Intensitet------------------------##

# Vi konstruerer vores intensitetvariabel - har kandidaten haft 0, 1 eller 2 
# identitære policies

data <- data %>%
  mutate(
    identitet_intensitet = case_when(
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_ikke_identitet" ~ 0,
      A3_familie_ny == "A3_identitet"      & A5_andet_ny == "A5_ikke_identitet" ~ 1,
      A3_familie_ny == "A3_ikke_identitet" & A5_andet_ny == "A5_identitet"      ~ 1,
      A3_familie_ny == "A3_identitet"      & A5_andet_ny == "A5_identitet"      ~ 2,
      TRUE ~ NA_integer_
    ),
    identitet_intensitet= factor(identitet_intensitet,
                                 levels = c(0, 1, 2),
                                 labels = c("Ingen", "En", "Begge"))
  )

data %>% count(identitet_intensitet)


# MM - Tvunget valg

model_intensitet_FC <- cj(data,
                       FC ~ A1_transport + A2_oekonomi + identitet_intensitet + A4_klima,
                       estimate = "mm",
                       id = ~participant_id)

model_intensitet_FC %>%
  as_tibble() %>%
  filter(feature == "identitet_intensitet") %>%
  select(level, estimate, lower, upper)


# MM - Policystøtte 

model_intensitet_RS <- cj(data,
                       rating_support ~ A1_transport + A2_oekonomi + identitet_intensitet + A4_klima,
                       estimate = "mm",
                       id = ~participant_id)

model_intensitet_RS %>%
  as_tibble() %>%
  filter(feature == "identitet_intensitet") %>%
  select(level, estimate, lower, upper)


# MM - Stemmesandsynlighed


model_intensitet_RC <- cj(data,
                       rating_voting ~ A1_transport + A2_oekonomi + identitet_intensitet + A4_klima,
                       estimate = "mm",
                       id = ~participant_id)

model_intensitet_RC %>%
  as_tibble() %>%
  filter(feature == "identitet_intensitet") %>%
  select(level, estimate, lower, upper)



## Er kandidatprofilerne signifikant forskellige fra hinanden? 

# Referencekategori er ingen identitære polciies

# Tvunget Valg

model_intensitet_lm_FC <- lm(FC ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                          data = data)

coeftest(model_intensitet_lm_FC,
         vcov = vcovCL(model_intensitet_lm_FC, cluster = ~participant_id))


# Gemmer i en tabel
tidy_intensitet_FC <- coeftest(
  model_intensitet_lm_FC,
  vcov = vcovCL(model_intensitet_lm_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetEn"    ~ "Én identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Ingen identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_FC, "table_intensitet_FC.png")


# Policystøtte

model_intensitet_lm_RS <- lm(rating_support ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                             data = data)

coeftest(model_intensitet_lm_RS,
         vcov = vcovCL(model_intensitet_lm_RS, cluster = ~participant_id))


# Gemmer i en tabel
tidy_intensitet_RS <- coeftest(
  model_intensitet_lm_RS,
  vcov = vcovCL(model_intensitet_lm_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetEn"    ~ "Én identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Ingen identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_RS, "table_intensitet_RS.png")


# Stemmesandsynlighed

model_intensitet_lm_RV <- lm(rating_voting ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                             data = data)

coeftest(model_intensitet_lm_RV,
         vcov = vcovCL(model_intensitet_lm_RV, cluster = ~participant_id))



# Gemmer som tabel

tidy_intensitet_RV <- coeftest(
  model_intensitet_lm_RV,
  vcov = vcovCL(model_intensitet_lm_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetEn"    ~ "Én identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Ingen identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_RV, "table_intensitet_RV.png")


# Vi ændrer nu referencekategorien til at være én identitær policy

data <- data %>%
  mutate(identitet_intensitet = relevel(identitet_intensitet, ref = "En"))


# Tvunget Valg

model_intensity_lm_FC_2 <- lm(FC ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                           data = data)

coeftest(model_intensity_lm_FC_2,
         vcov = vcovCL(model_intensity_lm_FC_2, cluster = ~participant_id))


# Gemmer i en tabel
tidy_intensitet_FC_2 <- coeftest(
  model_intensity_lm_FC_2,
  vcov = vcovCL(model_intensity_lm_FC_2, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetIngen" ~ "Ingen identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Én identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_FC_2, "table_intensitet_FC_2.png")


# Policystøtte

model_intensity_lm_RS_2 <- lm(rating_support ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                              data = data)

coeftest(model_intensity_lm_RS_2,
         vcov = vcovCL(model_intensity_lm_RS_2, cluster = ~participant_id))

# Gemmer som en tabel
tidy_intensitet_RS_2 <- coeftest(
  model_intensity_lm_RS_2,
  vcov = vcovCL(model_intensity_lm_RS_2, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetIngen" ~ "Ingen identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Én identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_RS_2, "table_intensitet_RS_2.png")


# Stemmesandsynlighed

model_intensity_lm_RV_2 <- lm(rating_voting ~ identitet_intensitet + A1_transport + A2_oekonomi + A4_klima,
                              data = data)

coeftest(model_intensity_lm_RV_2,
         vcov = vcovCL(model_intensity_lm_RV_2, cluster = ~participant_id))

# Gemmer som en tabel
tidy_intensitet_RV_2 <- coeftest(
  model_intensity_lm_RV_2,
  vcov = vcovCL(model_intensity_lm_RV_2, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl("identitet_intensitet", term)) %>%
  mutate(
    term = case_when(
      term == "identitet_intensitetIngen" ~ "Ingen identitær policy",
      term == "identitet_intensitetBegge" ~ "To identitære policies",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Variabel",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Profileffekt: Intensitet - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Én identitær policy. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_intensitet_RV_2, "table_intensitet_RV_2.png")


##----------------------------------------------------------------------------##
##----------------------------EKSPLORATIVT------------------------------------##
##----------------------------------------------------------------------------##

## --------------------------------Alder--------------------------------------##

# AMCE - Tvunget Valg

model_FC_AMCE_E_1 <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "amce",
                      id = ~participant_id,
                      by = ~ alder_kategori)

model_FC_AMCE_E_1 %>% as_tibble() %>% view()


# Plot
plot_data_alder_amce_FC <- model_FC_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>% 
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_AMCE_alder_FC <-  ggplot(plot_data_alder_amce_FC,
                              aes(x = estimate, y = alder_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("18-29", "30-44", "45-59",
                              "60+")) +
  scale_x_continuous(limits = c(-0.3, NA)) + 
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som en tabel
table_FC_AMCE_E_1 <- model_FC_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Alder") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_E_1, "table_FC_AMCE_E_1.png")


# AMCE - Policystøtte

model_RS_AMCE_E_1 <- cj(data,
                        rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ alder_kategori)


# Plot
plot_data_alder_amce_RS <- model_RS_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>% 
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_AMCE_alder_RS <- ggplot(plot_data_alder_amce_RS,
                             aes(x = estimate, y = alder_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("18-29", "30-44", "45-59",
                              "60+")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_RS_AMCE_E_1 <- model_RS_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Alder") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_E_1, "table_RS_AMCE_E_1.png")


# AMCE - Stemmesandsynlighed

model_RV_AMCE_E_1 <- cj(data,
                        rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ alder_kategori)

model_RV_AMCE_E_1 %>% as_tibble() %>% view()

# Plot
plot_data_alder_amce_RV <- model_RV_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>% 
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    )
  )

plot_AMCE_alder_RV <-  ggplot(plot_data_alder_amce_RV,
                              aes(x = estimate, y = alder_kategori, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("18-29", "30-44", "45-59",
                              "60+")) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer det i en tabel
table_RV_AMCE_E_1 <- model_RV_AMCE_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Alder") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_E_1, "table_RV_AMCE_E_1.png")


# Samler de 3 AMCE plot i et 
alder_amce <- (plot_AMCE_alder_FC|plot_AMCE_alder_RS| plot_AMCE_alder_RV) 


ggsave("alder_amce2.png",
       plot = alder_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# MM - Tvunget Valg
model_FC_MM_E_1 <- cj(data,
                        FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                        estimate = "mm",
                        id = ~participant_id,
                        by = ~ alder_kategori)


# Vi plotter det 
plot_data_MM_E_1_FC <- model_FC_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_alder_FC <- ggplot(plot_data_MM_E_1_FC,
                           aes(x = estimate, y = alder_kategori, colour = alder_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_FC_MM_E_1 <- model_FC_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Alder") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_E_1, "table_FC_MM_E_1.png")


# MM -Policystøtte
model_RS_MM_E_1 <- cj(data,
                      rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ alder_kategori)

# Vi plotter det 
plot_data_MM_E_1_RS <- model_RS_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_alder_RS <- ggplot(plot_data_MM_E_1_RS,
                           aes(x = estimate, y = alder_kategori, colour = alder_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Alder"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )


# Gemmer i en tabel
table_RS_MM_E_1 <- model_RS_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Alder") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_E_1, "table_RS_MM_E_1.png")


# MM - Stemmesandsynlighed
model_RV_MM_E_1 <- cj(data,
                      rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ alder_kategori)

# Vi plotter det 
plot_data_MM_E_1_RV <- model_RV_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    )
  )

plot_MM_alder_RV <-ggplot(plot_data_MM_E_1_RV,
                          aes(x = estimate, y = alder_kategori, colour = alder_kategori)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_RV_MM_E_1 <- model_RV_MM_E_1 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(alder_kategori, feature, level, effect) %>%
  gt(groupname_col = "alder_kategori") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Alder") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_E_1, "table_RV_MM_E_1.png")


# Samler de 3 MM plot i et 
alder_MM <- (plot_MM_alder_FC |plot_MM_alder_RS | plot_MM_alder_RV) 


ggsave("alder_MM2.png",
       plot = alder_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")


# Laver en interaktionsmodel

# Tvunget valg

model_FC_interaktion_e1 <- lm(FC ~ A3_familie_ny * alder + 
                                 A5_andet_ny * alder +
                                 A1_transport + A2_oekonomi + A4_klima,
                               data = data)

coeftest(model_FC_interaktion_e1,
         vcov = vcovCL(model_FC_interaktion_e1, cluster = ~participant_id))

# Gemmer i en tabel
tidy_interaction_e1_FC <- coeftest(
  model_FC_interaktion_e1,
  vcov = vcovCL(model_FC_interaktion_e1, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:alder" ~ "A3 Identitær × Alder",
      term == "alder:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Alder",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.4f", estimate), stars,
                    " (", sprintf("%.4f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Alder × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e1_FC, "table_interaction_e1_FC.png")


# Marginal Effects Plot

# Familie & Børn

plot_ME_familie_FC_alder <- plot_slopes(model_FC_interaktion_e1,
                                        variables = "A3_familie_ny",
                                        condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Familie & Børn - Tvunget Valg") +
  theme_minimal()


# Andet

plot_ME_andet_FC_alder <- plot_slopes(model_FC_interaktion_e1,
                                        variables = "A5_andet_ny",
                                        condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Andet - Tvunget Valg") + theme_minimal()



# Gemmer de to marginal effects plot i et
combined_ME_FC_alder <- ( plot_ME_familie_FC_alder| plot_ME_andet_FC_alder) 

ggsave("ME_FC_alder.png",
       plot = combined_ME_FC_alder,
       width = 12,
       height = 6,
       units = "in",
       device = "png")


# Policystøtte

model_RS_interaktion_e1 <- lm(rating_support ~ A3_familie_ny * alder + 
                                A5_andet_ny * alder +
                                A1_transport + A2_oekonomi + A4_klima,
                              data = data)

coeftest(model_RS_interaktion_e1,
         vcov = vcovCL(model_RS_interaktion_e1, cluster = ~participant_id))

# Gemmer i en tabel
tidy_interaction_e1_RS <- coeftest(
  model_RS_interaktion_e1,
  vcov = vcovCL(model_RS_interaktion_e1, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:alder" ~ "A3 Identitær × Alder",
      term == "alder:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Alder",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.4f", estimate), stars,
                    " (", sprintf("%.4f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Alder × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e1_RS, "table_interaction_e1_RS.png")


# Marginal Effects Plot

# Familie & Børn 
plot_ME_familie_RS_alder <- plot_slopes(model_RS_interaktion_e1,
                                        variables = "A3_familie_ny",
                                        condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Familie & Børn - Policy Støtte") + theme_minimal()


# Andet

plot_ME_andet_RS_alder <- plot_slopes(model_RS_interaktion_e1,
                                      variables = "A5_andet_ny",
                                      condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Andet - Policy Støtte") + theme_minimal()


# Gemmer de to marginal effects plot i et
combined_ME_RS_alder <- ( plot_ME_familie_RS_alder| plot_ME_andet_RS_alder) 

ggsave("ME_RS_alder.png",
       plot = combined_ME_RS_alder,
       width = 12,
       height = 6,
       units = "in",
       device = "png")


# Stemmesandsynlighed
model_RV_interaktion_e1 <- lm(rating_voting ~ A3_familie_ny * alder + 
                                A5_andet_ny * alder +
                                A1_transport + A2_oekonomi + A4_klima,
                              data = data)

coeftest(model_RV_interaktion_e1,
         vcov = vcovCL(model_RV_interaktion_e1, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_e1_RV <- coeftest(
  model_RV_interaktion_e1,
  vcov = vcovCL(model_RV_interaktion_e1, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:alder" ~ "A3 Identitær × Alder",
      term == "alder:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Alder",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.4f", estimate), stars,
                    " (", sprintf("%.4f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Alder × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e1_RV, "table_interaction_e1_RV.png")


# Marginal Effects Plot

# Familie & Børn

plot_ME_familie_RV_alder <- plot_slopes(model_RV_interaktion_e1,
                                        variables = "A3_familie_ny",
                                        condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Familie & Børn - Stemmesandsynlighed")+ theme_minimal()


# Andet
plot_ME_andet_RV_alder <- plot_slopes(model_RV_interaktion_e1,
                                      variables = "A5_andet_ny",
                                      condition = "alder", vcov = ~participant_id) + 
  labs(x = "Alder",
       y = "Marginale effekter",
       title = "Andet - Stemmesandsynlighed")+ theme_minimal()


# Gemmer de to marginal effects plot i et
combined_ME_RV_alder <- ( plot_ME_familie_RV_alder| plot_ME_andet_RV_alder) 

ggsave("ME_RV_alder.png",
       plot = combined_ME_RV_alder,
       width = 12,
       height = 6,
       units = "in",
       device = "png")


##-------------------------------Gruppestatus---------------------------------##

# Vi skal lave en ny variabel: lavstatus-minoriet/målgruppen
# Man er i målgruppen, hvis man i køn har sagt andet, har angivet man er en seksuel minoritet eller race/etnisk minoritet

data <- data %>%
    mutate(
      disadvantaged = factor(case_when(
        gender == "Andet" | 
          seksuel_minoritet == "Ja" | 
          etnisk_race_minoritet == "Ja" ~ "Disadvantaged",
        TRUE ~ "Advantaged"
      ), levels = c("Advantaged", "Disadvantaged")))


class(data$disadvantaged)
table(data$disadvantaged)


# AMCE - Tvunget Valg
model_FC_AMCE_e2 <- cj(data,
                        FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                        + A5_andet_ny,
                        estimate = "amce",
                        id = ~participant_id,
                        by = ~ disadvantaged)

model_FC_AMCE_e2 %>% as_tibble() %>% view()


# Vi plotter det
plot_data_e2_amce_FC <- model_FC_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_amce_FC_e2 <- ggplot(plot_data_e2_amce_FC,
                          aes(x = estimate, y = disadvantaged, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Lavstatus-minoritetsgruppen", "Højstatus-majoritetsgruppen" )) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
    
)

# Gemmer i en tabel
table_FC_AMCE_e2 <- model_FC_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) efter Gruppestatus") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_FC_AMCE_e2, "table_FC_AMCE_e2.png")


#Interaktionsmodel - højstatusgruppen er referencekategori

interaction_lm_e2_FC <- lm(FC ~ A1_transport + A2_oekonomi + 
                                A3_familie_ny * disadvantaged + 
                                A4_klima + A5_andet_ny * disadvantaged,
                              data = data)

coeftest(interaction_lm_e2_FC, vcov = vcovCL(interaction_lm_e2_FC, cluster = ~participant_id))

# Gemmer i en tabel
tidy_interaction_e2_FC <- coeftest(
  interaction_lm_e2_FC,
  vcov = vcovCL(interaction_lm_e2_FC, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:disadvantagedDisadvantaged" ~ "A3 Identitær × Lavstatus-minoritetsgruppen",
      term == "disadvantagedDisadvantaged:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Lavstatus-minoritetsgruppen",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Gruppestatus × A3 og A5 - Tvunget Valg") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Højstatus-majoritetsgruppen. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e2_FC, "table_interaction_e2_FC.png")


# AMCE - Policystøtte

model_RS_AMCE_e2 <- cj(data,
                       rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                       + A5_andet_ny,
                       estimate = "amce",
                       id = ~participant_id,
                       by = ~ disadvantaged)


# Vi plotter det
plot_data_e2_amce_RS <- model_RS_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_amce_RS_e2 <- ggplot(plot_data_e2_amce_RS,
                          aes(x = estimate, y = disadvantaged, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Lavstatus-minoritetsgruppen", "Højstatus-majoritetsgruppen" )) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "Attribut",
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
    
  )

# Gemmer i en tabel
table_RS_AMCE_e2 <- model_RS_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) efter Gruppestatus") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RS_AMCE_e2, "table_RS_AMCE_e2.png")

# Interaktionsmodel - højstatusgruppen er referencekategory
interaction_lm_e2_RS <- lm(rating_support ~ A1_transport + A2_oekonomi + 
                             A3_familie_ny * disadvantaged + 
                             A4_klima + A5_andet_ny * disadvantaged,
                           data = data)

coeftest(interaction_lm_e2_RS, vcov = vcovCL(interaction_lm_e2_RS, cluster = ~participant_id))


# Gemmer i en tabel
tidy_interaction_e2_RS <- coeftest(
  interaction_lm_e2_RS,
  vcov = vcovCL(interaction_lm_e2_RS, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:disadvantagedDisadvantaged" ~ "A3 Identitær × Lavstatus-minoritetsgruppen",
      term == "disadvantagedDisadvantaged:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Lavstatus-minoritetsgruppen",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Gruppestatus × A3 og A5 - Policystøtte") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Højstatus-majoritetsgruppen. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e2_RS, "table_interaction_e2_RS.png")

# AMCE - Stemmesandsynlighed

model_RV_AMCE_e2 <- cj(data,
                       rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima 
                       + A5_andet_ny,
                       estimate = "amce",
                       id = ~participant_id,
                       by = ~ disadvantaged)

# Vi plotter det
plot_data_e2_amce_RV <- model_RV_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%  
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),   level = case_when(
      level == "A3_identitet" ~ "Identitær",
      level == "A5_identitet" ~ "Identitær",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_amce_RV_e2 <- ggplot(plot_data_e2_amce_RV,
                          aes(x = estimate, y = disadvantaged, colour = feature)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2,
                orientation = "y",
                position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Andet"         = "#00BFC4",  
    "Familie & Børn" = "#F8766D"  
  )) +
  scale_y_discrete(limits = c("Lavstatus-minoritetsgruppen", "Højstatus-majoritetsgruppen" )) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    colour = "",
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
    
  )

# Gemmer i en tabel
table_RV_AMCE_e2 <- model_RV_AMCE_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie & Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_identitet" ~ "Identitære policies",
      level == "A5_identitet" ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) efter Gruppestatus") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_RV_AMCE_e2, "table_RV_AMCE_e2.png")
  
# Interaktionsmodel - højstatusgruppen er referencekategori

interaction_lm_e2_RV <- lm(rating_voting ~ A1_transport + A2_oekonomi + 
                             A3_familie_ny * disadvantaged + 
                             A4_klima + A5_andet_ny * disadvantaged,
                           data = data)

coeftest(interaction_lm_e2_RV, vcov = vcovCL(interaction_lm_e2_RV, cluster = ~participant_id))

# Gemmer i en tabel
tidy_interaction_e2_RV <- coeftest(
  interaction_lm_e2_RV,
  vcov = vcovCL(interaction_lm_e2_RV, cluster = ~participant_id)
) %>%
  tidy() %>%
  filter(grepl(":", term)) %>%
  mutate(
    term = case_when(
      term == "A3_familie_nyA3_identitet:disadvantagedDisadvantaged" ~ "A3 Identitær × Lavstatus-minoritetsgruppen",
      term == "disadvantagedDisadvantaged:A5_andet_nyA5_identitet"   ~ "A5 Identitær × Lavstatus-minoritetsgruppen",
      TRUE ~ term
    ),
    stars = case_when(
      abs(statistic) > 3.29 ~ "***",
      abs(statistic) > 2.58 ~ "**",
      abs(statistic) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(term, effect) %>%
  gt() %>%
  cols_label(
    term   = "Interaktionsled",
    effect = "Koefficient (SE)"
  ) %>%
  tab_header(title = "Interaktioner: Gruppestatus × A3 og A5 - Stemmesandsynlighed") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Reference: Højstatus-majoritetsgruppen. Standardfejl clustered per respondent.") %>%
  fmt_markdown(columns = "effect")

gtsave(tidy_interaction_e2_RV, "table_interaction_e2_RV.png")


# Samler de 3 AMCE plot i et
målgruppe_amce <- (plot_amce_FC_e2|plot_amce_RS_e2| plot_amce_RV_e2) 


ggsave("målgruppe_amce2.png",
       plot = målgruppe_amce,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")

# MM - Tvunget Valg

model_FC_MM_e2 <- cj(data,
                      FC ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                      estimate = "mm",
                      id = ~participant_id,
                      by = ~ disadvantaged)


# Vi plotter
plot_data_MM_e2_FC <- model_FC_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_MM_e2_FC <-  ggplot(plot_data_MM_e2_FC,
                         aes(x = estimate, y = disadvantaged, colour = disadvantaged)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Tvunget valg",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_FC_MM_e2 <- model_FC_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Tvunget Valg - Marginal Means efter Gruppestatus") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_FC_MM_e2, "table_FC_MM_e2.png")


# MM - Policystøtte

model_RS_MM_e2 <- cj(data,
                     rating_support ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                     estimate = "mm",
                     id = ~participant_id,
                     by = ~ disadvantaged)


# Vi plotter
plot_data_MM_e2_RS <- model_RS_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitær",
      level == "A3_identitet"      ~ "Identitarian",
      level == "A5_ikke_identitet" ~ "Ikke-identitær",
      level == "A5_identitet"      ~ "Identitarian",
      TRUE ~ as.character(level)
    ), disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_MM_e2_RS <- ggplot(plot_data_MM_e2_RS,
                        aes(x = estimate, y = disadvantaged, colour = disadvantaged)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Policystøtte",
    colour = "Gruppestatus"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_RS_MM_e2 <- model_RS_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Policystøtte - Marginal Means efter Gruppestatus") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser.Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RS_MM_e2, "table_RS_MM_e2.png")



# MM - Stemmesandsynlighed

model_RV_MM_e2 <- cj(data,
                     rating_voting ~ A1_transport + A2_oekonomi + A3_familie_ny + A4_klima + A5_andet_ny,
                     estimate = "mm",
                     id = ~participant_id,
                     by = ~ disadvantaged)


# Vi plotter
plot_data_MM_e2_RV <- model_RV_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitær",
      level == "A3_identitet"      ~ "Identitarian",
      level == "A5_ikke_identitet" ~ "Ikke-identitær",
      level == "A5_identitet"      ~ "Identitarian",
      TRUE ~ as.character(level)
    ), disadvantaged = case_when(            
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    )
  )

plot_MM_e2_RV <-  ggplot(plot_data_MM_e2_RV,
                         aes(x = estimate, y = disadvantaged, colour = disadvantaged)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y",
                position = position_dodge(width = 0.6)) +
  facet_grid(level ~ feature, switch = "both") +
  labs(
    x = "Estimeret MM",
    y = NULL,
    title = "Stemmesandsynlighed",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y.left = element_text(angle = 0),
    strip.placement = "outside",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.spacing = unit(0.8, "lines")
  )

# Gemmer i en tabel
table_RV_MM_e2 <- model_RV_MM_e2 %>%
  as_tibble() %>%
  filter(feature %in% c("A3_familie_ny", "A5_andet_ny")) %>%
  mutate(
    feature = case_when(
      feature == "A3_familie_ny" ~ "Familie og Børn",
      feature == "A5_andet_ny"   ~ "Andet"
    ),
    level = case_when(
      level == "A3_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A3_identitet"      ~ "Identitære policies",
      level == "A5_ikke_identitet" ~ "Ikke-identitære policies",
      level == "A5_identitet"      ~ "Identitære policies",
      TRUE ~ as.character(level)
    ),
    disadvantaged = case_when(
      disadvantaged == "Advantaged"    ~ "Højstatus-majoritetsgruppen",
      disadvantaged == "Disadvantaged" ~ "Lavstatus-minoritetsgruppen",
      TRUE ~ as.character(disadvantaged)
    ),
    effect = sprintf("%.3f [%.3f, %.3f]", estimate, lower, upper)
  ) %>%
  select(disadvantaged, feature, level, effect) %>%
  gt(groupname_col = "disadvantaged") %>%
  cols_label(
    feature = "Attribut",
    level   = "Niveau",
    effect  = "MM [95% KI]"
  ) %>%
  tab_header(title = "Stemmesandsynlighed - Marginal Means efter Gruppestatus") %>%
  tab_footnote("Note: 95% konfidensintervaller i parenteser. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gtsave(table_RV_MM_e2, "table_RV_MM_e2.png")


# Samler de 3 MM plot i et
gruppestatus_MM <- (plot_MM_e2_FC|plot_MM_e2_RS| plot_MM_e2_RV) 


ggsave("gruppestatus_MM2.png",
       plot = gruppestatus_MM,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")



##--------------------------De identitære policytyper-----------------------##
      
# Vores sidste eksplorative hypotese handler om de målgrupper, som adresseres i de
# forskellige identitære policies

# Problemet er, at når kandidaterne har set en profil med 2 identitære policies, 
#kan disse have varieret ift. målgruppen.

# Vi kigger derfor kun observationer, hvor kandidaten har en identitær policy eller ingen

data_single <- data %>%
  filter(identitarian_combo %in% c("Ingen", "Kun familie", "Kun andet")) %>%
  mutate(
    #  A3
    A3_collapsed = case_when(
      A3_familie %in% c("A3L1", "A3L2", "A3L3", "A3L4") ~ "LGBT+ (A3)",
      A3_familie == "A3L5" ~ "Etnicitet/race (A3)",
      TRUE ~ "Ikke-identitær (A3)"
    ),
    A3_collapsed = factor(A3_collapsed,
                          levels = c("Ikke-identitær (A3)", "LGBT+ (A3)", "Etnicitet/race (A3)")),
    
    #  A5 
    A5_collapsed = case_when(
      A5_andet %in% c("A5L3", "A5L5") ~ "Kvinder (A5)",
      A5_andet %in% c("A5L1") ~ "LGBT+ (A5)",
      A5_andet %in% c("A5L2", "A5L4") ~ "Etnicitet/race (A5)",
      TRUE ~ "Ikke-identitær (A5)"
    ),
    A5_collapsed = factor(A5_collapsed,
                          levels = c("Ikke-identitær (A5)", "LGBT+ (A5)", 
                                     "Etnicitet/race (A5)", "Kvinder (A5)"))
  )

# Tjek
table(data_single$A3_collapsed)
table(data_single$A5_collapsed)


# Etnicitet A3 og LBGT+ A5 har relatvit få observationer som følge af de kun indebærer
# 2 policies. 

# AMCE - Tvunget valg

model_single_type_FC_amce <- cj(data_single,
                           FC ~ A1_transport + A2_oekonomi + A3_collapsed + A4_klima + A5_collapsed,
                           estimate = "amce",
                           id = ~participant_id)

model_single_type_FC_amce %>% as_tibble() %>% view()

# Plot
plot_single_type_FC_amce <- model_single_type_FC_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "LGBT+", "Etnicitet/race", "Kvinder", "Ikke-identitær"
    ))
  ) %>%
  filter(!is.na(std.error)) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y") +
  guides(colour = "none") +
  scale_colour_manual(values = c(
    "Familie & Børn" = "#F8766D",
    "Andet"           = "#00BFC4"
  )) + coord_cartesian(xlim = c(NA, 0.1)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Tvunget Valg"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_single_type_FC_amce <- model_single_type_FC_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Tvunget Valg (AMCE) - Policytype") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_single_type_FC_amce, "table_single_type_FC_amce.png")


# Er AMCE signifikant forskellige fra hinanden

model_FC_type <- lm(FC ~ A3_collapsed + A5_collapsed + A1_transport + A2_oekonomi + A4_klima,
               data = data_single)

# For Famlie & Børn
emmeans_A3 <- emmeans(model_FC_type, ~ A3_collapsed)
pairs(emmeans_A3)

# For Andet
emmeans_A5 <- emmeans(model_FC_type,  ~ A5_collapsed)
pairs(emmeans_A5)


# AMCE - Policystøtte

model_single_type_RS_amce <- cj(data_single,
                           rating_support ~ A1_transport + A2_oekonomi + A3_collapsed + A4_klima + A5_collapsed,
                           estimate = "amce",
                           id = ~participant_id)

# Plot
plot_single_type_RS_amce <- model_single_type_RS_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "LGBT+", "Etnicitet/race", "Kvinder", "Ikke-identitær"
    ))
  ) %>%
  filter(!is.na(std.error)) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y") +
  guides(colour = "none") +
  scale_colour_manual(values = c(
    "Familie & Børn" = "#F8766D",
    "Andet"           = "#00BFC4"
  )) + coord_cartesian(xlim = c(NA, 0.2)) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Policystøtte"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmes som tabel
table_single_type_RS_amce <- model_single_type_RS_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Policystøtte (AMCE) - Policytype") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_single_type_RS_amce, "table_single_type_RS_amce.png")


# Er AMCE signifikant forskellige fra hinanden

model_RS_type <- lm(rating_support ~ A3_collapsed + A5_collapsed + A1_transport + A2_oekonomi + A4_klima,
                    data = data_single)

# For Famlie & Børn
emmeans_A3 <- emmeans(model_RS_type , ~ A3_collapsed)
pairs(emmeans_A3)

# For Andet
emmeans_A5 <- emmeans(model_RS_type ,  ~ A5_collapsed)
pairs(emmeans_A5)


# AMCE - Stemmesandsynlighed

model_single_type_RV_amce <- cj(data_single,
                           rating_voting ~ A1_transport + A2_oekonomi + A3_collapsed + A4_klima + A5_collapsed,
                           estimate = "amce",
                           id = ~participant_id)

#Plot
plot_single_type_RV_amce <- model_single_type_RV_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    level = factor(level, levels = c(
      "LGBT+", "Etnicitet/race", "Kvinder", "Ikke-identitær"
    ))
  ) %>%
  filter(!is.na(std.error)) %>%
  ggplot(aes(x = estimate, y = level, colour = feature)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0.2, orientation = "y") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ feature, scales = "free_y") +
  guides(colour = "none") +
  scale_colour_manual(values = c(
    "Familie & Børn" = "#F8766D",
    "Andet"           = "#00BFC4"
  )) +
  labs(
    x = "Estimeret AMCE",
    y = NULL,
    title = "Stemmesandsynlighed"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )

# Gemmer som tabel
table_single_type_RV_amce <- model_single_type_RV_amce %>%
  as_tibble() %>%
  filter(feature %in% c("A3_collapsed", "A5_collapsed")) %>%
  filter(!is.na(std.error)) %>%
  mutate(
    feature = case_when(
      feature == "A3_collapsed" ~ "Familie & Børn",
      feature == "A5_collapsed" ~ "Andet"
    ),
    level = case_when(
      level == "Ikke-identitær (A3)" ~ "Ikke-identitær",
      level == "LGBT+ (A3)"          ~ "LGBT+",
      level == "Etnicitet/race (A3)" ~ "Etnicitet/race",
      level == "Ikke-identitær (A5)" ~ "Ikke-identitær",
      level == "LGBT+ (A5)"          ~ "LGBT+",
      level == "Etnicitet/race (A5)" ~ "Etnicitet/race",
      level == "Kvinder (A5)"        ~ "Kvinder",
      TRUE ~ as.character(level)
    ),
    stars = case_when(
      abs(estimate / std.error) > 3.29 ~ "***",
      abs(estimate / std.error) > 2.58 ~ "**",
      abs(estimate / std.error) > 1.96 ~ "*",
      TRUE ~ ""
    ),
    effect = paste0(sprintf("%.3f", estimate), stars,
                    " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(feature, level, effect) %>%
  gt(groupname_col = "feature") %>%
  cols_label(
    level  = "Attribut niveau",
    effect = "AMCE (SE)"
  ) %>%
  tab_header(title = "Stemmesandsynlighed (AMCE) - Policytype") %>%
  tab_footnote("Note: * p<0.05, ** p<0.01, *** p<0.001. Standardfejl clustered per respondent.") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  fmt_markdown(columns = "effect")

gtsave(table_single_type_RV_amce, "table_single_type_RV_amce.png")


# Er AMCE signifikant forskellige fra hinanden

model_RV_type <- lm(rating_voting ~ A3_collapsed + A5_collapsed + A1_transport + A2_oekonomi + A4_klima,
                    data = data_single)

# For Famlie & Børn
emmeans_A3 <- emmeans(model_RV_type , ~ A3_collapsed)
pairs(emmeans_A3)

# For Andet
emmeans_A5 <- emmeans(model_RV_type,  ~ A5_collapsed)
pairs(emmeans_A5)


# Samler de 3 AMCE plot i et

type_AMCE <- (plot_single_type_FC_amce|plot_single_type_RS_amce|plot_single_type_RV_amce) 


ggsave("type_AMCE.png",
       plot = type_AMCE,
       width = 18,
       height = 8,  
       units = "in",
       device = "png")



