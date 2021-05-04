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

data <- read_dta("C:/Users/Bruno/Desktop/MONOGRAFIA/build/input/PNADC2012dta")

###################
# população de cada estado
###################

popuf <- data %>%
  select(UF, V1028, Trimestre, Ano) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(popuf = mean(aux))


write.csv(popuf, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/popuf2012.csv")

########################
# PIA de cada estado
########################

pia <- data %>%
  select(UF, V1028, Trimestre, Ano, V2009) %>%
  filter(V2009 >= 14) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(piauf = mean(aux))

write.csv(pia, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/piauf2012.csv")

########################
# PEA de cada estado
########################

pea <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == 1 | VD4002 == 2) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(peauf = mean(aux))

write.csv(pea, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/peauf2012.csv")

########################
# ocupados de cada estado
########################

ocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupuf = mean(aux))

write.csv(ocup, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupuf2012.csv")

########################
# desocupados de cada estado
########################

desocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == 2) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desocupuf = mean(aux))

write.csv(desocup, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desocupuf2012.csv")

########################
# desalentados de cada estado
########################

desalent <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4005) %>%
  filter(VD4005 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desalentuf = mean(aux))

write.csv(desalent, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desalentpuf2012.csv")

########################
# nemnem de cada estado
########################

#nemnem <- data %>%
  #select(UF, V1028, Trimestre, Ano, V2009, V3002, V4074, V4074A, VD4001, VD4002) %>%
  #filter(VD4002 == 2 | (VD4001 == 2 & V3002 == 2)) %>%
  #group_by(UF, Trimestre) %>%
  #mutate(aux = sum(V1028),
  #       aux2 = ifelse(.data$V4074 == 6)) %>%
  #summarise(nemnem = mean(aux),
  #          teste = mean(aux2))#

#write.csv(desalent, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/nemnem2012.csv")#



########################
# trabalhadores formais de cada estado
########################

formal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012) %>%
  filter(VD4009 == 1 | VD4009 == 3 | VD4009 == 5 | VD4009 == 7 |
        (VD4009 == 8 & VD4012 == 1) | (VD4009 == 9 & VD4012 == 1)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formal = mean(aux))

write.csv(desalent, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formal2012.csv")


########################
# trabalhadores informais de cada estado
########################

informal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informal = mean(aux))

write.csv(desalent, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informal2012.csv")


########################
# carteira assinada de cada estado
########################

cart <- data %>%
  select(UF, V1028, Trimestre, Ano, V4029) %>%
  filter(V4029 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(cartuf = mean(aux))

write.csv(cart, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/cartuf2012.csv")

########################
# contribuintes de cada estado
########################

inss <- data %>%
  select(UF, V1028, Trimestre, Ano, V4032) %>%
  filter(V4032 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(inssuf = mean(aux))

write.csv(inss, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/inssuf2012.csv")


########################
# trabalhadores no setor privado de cada estado
########################

priv <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == 3) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(privuf = mean(aux))

write.csv(priv, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/privuf2012.csv")

########################
# trabalhadores no setor público de cada estado
########################

publ <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == 4) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(publuf = mean(aux))

write.csv(publ, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/publuf2012.csv")

########################
# quantidade de ocupados agricultura (primario) de cada estado
########################

prim <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == 1) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(primuf = mean(aux))

write.csv(prim, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/primuf2012.csv")

########################
# quantidade de ocupados indústria (secundario) de cada estado
########################

sec <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == 2 | VD4010 == 3) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(secuf = mean(aux))

write.csv(sec, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/secuf2012.csv")

########################
# quantidade de ocupados comercio e serviços (terciario) de cada estado
########################

terc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == 4 | VD4010 == 5 | VD4010 == 6 | VD4010 == 7 | VD4010 == 8 | VD4010 == 9) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(tercuf = mean(aux))

write.csv(terc, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/tercuf2012.csv")
}


class(data$pnadc_012015)

ab <- as_tibble(data$pnadc_012015)



