setwd("/Users/Louise/Desktop/Statskundskab/Speciale")
getwd()


library(dplyr)

# Vi installerer Schuessler & Freitag (2020)'s pakke til at beregne
# størrelsen på stikprøven


install.packages("devtools")

if(!require (devtools)) install.packages ("devtools")
library(devtools)
devtools::install_github("m-freitag/cjpowR")

install.packages("remotes")


library(remotes)
library(cjpowR)

# Vi udregner antallet af minimum antal effektive observationer for vores 
# to atributter med identitære policies. 

# Vi laver nåde udregner med det minimale (2) og maksimale (11) antal attributniveauer

# Det er relativt kompliceret at gætte på AMCE. Derfor laves der beregninger 
# for forskellige AMCE-niveauer med baggrund i conjoint-littertauren og Avina et al. 

# Powerniveau på 80% og signifikansniveau på 5%


cjpowr_amce(amce = 0.03, power = 0.8, levels = 2, alpha =0.05)

# Minimum effektive antal observationer: 8713
# Antal respondenter: 8713/6= 1452 

cjpowr_amce(amce = 0.05, power = 0.8, levels = 2, alpha =0.05)

# Minimum effektive antal observationer: 3132
# Antal respondenter: 3132/6= 522

#Statistisk power på 90%
cjpowr_amce(amce = 0.05, power = 0.9, levels = 2, alpha =0.05)
# Minimum effektive antal observationer: 4192
# Antal respondenter: 4192/6= 698


cjpowr_amce(amce = 0.07, power = 0.8, levels = 2, alpha =0.05)

# Minimum effektive antal observationer: 1594
# Antal respondenter: 1594/6= 265

cjpowr_amce(amce = 0.1, power = 0.8, levels = 2, alpha =0.05)

# Minimum effektive antal observationer: 777
# Antal respondenter: 777/6= 129

cjpowr_amce(amce = 0.05, power = 0.8, levels = 10, alpha =0.05)

# Minimum effektive antal observationer: 15658
# Antal respondenter: 15658/6= 2609


cjpowr_amce(amce = 0.05, power = 0.8, levels = 11, alpha =0.05)

# Minimum effektive antal observationer: 17.224
# Antal respondenter: 17224/6= 2870

cjpowr_amce(amce = 0.07, power = 0.8, levels = 11, alpha =0.05)
# Minimum effektive antal obseravtioner: 8767
# Antal respondenter: 8767/6 = 1461.167


cjpowr_amce(amce = 0.1, power = 0.8, levels = 11, alpha =0.05)
# Minimum effektive antal observationer: 4273
# Antal respondenter: 4273/6 = 712


