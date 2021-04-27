#################################################################################
#
# The purpose is to extract monthly data of evapotranspiration for every Brazilians AMC
# proposito
#
#################################################################################

####################
# Folder Path
####################

user <- Sys.info()[["user"]]
message(sprintf("Current User: %s\n"))
if (user == "Bruno") {
  ROOT <- "C:/Users/Bruno/Desktop/MONOGRAFIA"
} else if (user == "f.cavalcanti") {
  ROOT <- "C:/Users/Francisco/Dropbox"
} else {
  stop("Invalid user")
}

home_dir <- file.path(ROOT, "build")
in_dir <- file.path(ROOT, "build", "input")
out_dir <- file.path(ROOT, "build", "output")
tmp_dir <- file.path(ROOT, "build", "tmp")
code_dir <- file.path(ROOT, "build", "code")


####################
# load library
####################

library(datazoom.pnadcontinua)
library(tidyverse)
library(haven)

###################
# call data
###################

data <- datazoom.pnadcontinua::load_pnadcontinua(sources = "C:/Users/Bruno/Desktop/MONOGRAFIA/build/input_sample")

data <- read_dta("C:/Users/Bruno/Desktop/MONOGRAFIA/build/input/PNADC2012.dta")

###################
# população de cada estado
###################

popuf <- data %>%
  select(UF, V1028, Trimestre) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(popuf = mean(aux))


write.csv(popuf, file = "C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/popuf.csv")

########################
# PIA de cada estado
########################

pia <- data %>%
  select(UF, V1028, Trimestre, V2009) %>%
  filter(V2009 >= 14) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(piauf = mean(aux))

write.csv(pia, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/piauf.csv")

########################
# PEA de cada estado
########################

pea <- data %>%
  select(UF, V1028, Trimestre, VD4002) %>%
  filter(VD4002 == 1 | VD4002 == 2) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(peauf = mean(aux))

write.csv(pea, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/peauf.csv")

########################
# ocupados de cada estado
########################

ocup <- data %>%
  select(UF, V1028, Trimestre, VD4002) %>%
  filter(VD4002 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupuf = mean(aux))

write.csv(ocup, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupuf.csv")

########################
# desocupados de cada estado
########################

desocup <- data %>%
  select(UF, V1028, Trimestre, VD4002) %>%
  filter(VD4002 == 2) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desocupuf = mean(aux))

write.csv(desocup, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desocupuf.csv")

########################
# desalentados de cada estado
########################

desalent <- data %>%
  select(UF, V1028, Trimestre, VD4005) %>%
  filter(VD4005 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desalentuf = mean(aux))

write.csv(desalent, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desalentpuf.csv")

########################
# nenem de cada estado
########################



########################
# trabalhadores formais de cada estado
########################


########################
# trabalhadores informais de cada estado
########################

########################
# carteira assinada de cada estado
########################

cart <- data %>%
  select(UF, V1028, Trimestre, V4029) %>%
  filter(V4029 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(cartuf = mean(aux))

write.csv(cart, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/cartuf.csv")

########################
# contribuintes de cada estado
########################

inss <- data %>%
  select(UF, V1028, Trimestre, V4032) %>%
  filter(V4032 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(inssuf = mean(aux))

write.csv(inss, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/inssuf.csv")


########################
# trabalhadores no setor privado de cada estado
########################


########################
# trabalhadores no setor público de cada estado
########################


########################
# quantidade de ocupados industria de cada estado
########################




class(data$pnadc_012015)

ab <- as_tibble(data$pnadc_012015)



