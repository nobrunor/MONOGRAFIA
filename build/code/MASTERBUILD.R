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
library(dplyr)
library(PNADcIBGE)


#################################
### aumentando o memory limit ###
#################################

memory.limit (9999999999999999)


###################
# call data
###################


lista_ano <- c("012012",
               "022012",
               "032012",
               "042012",
               "012013",
               "022013",
               "032013",
               "042013",
               "012014",
               "022014",
               "032014",
               "042014",
               "012015",
               "022015",
               "032015",
               "042015",
               "012016",
               "022016",
               "032016",
               "042016",
               "012017",
               "022017",
               "032017",
               "042017",
               "012018",
               "022018",
               "032018",
               "042018",
               "012019",
               "022019",
               "032019",
               "042019",
               "012020",
               "022020",
               "032020",
               "042020",
               "012021")



for (yr in lista_ano) {

setwd(in_dir)

lista_pnad <- list.files(pattern = yr)

chave_input <- list.files(pattern = ".sas")

data <- read_pnadc(microdata = lista_pnad, input_txt = chave_input)


###################
# população de cada estado
###################

popuf <- data %>%
  select(UF, V1028, Trimestre, Ano) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(popuf = mean(aux))


write.csv(popuf, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/popuf", yr, ".csv"))

########################
# PIA de cada estado
########################

pia <- data %>%
  select(UF, V1028, Trimestre, Ano, V2009) %>%
  filter(V2009 >= 14) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(piauf = mean(aux))

write.csv(pia, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/piauf", yr, ".csv"))

########################
# PEA de cada estado
########################

pea <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" | VD4002 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(peauf = mean(aux))

write.csv(pea, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/peauf", yr, ".csv"))

########################
# ocupados de cada estado
########################

ocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupuf = mean(aux))

ocupfaixaetaria1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 18 & V2009 <= 24)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria1 = mean(aux))

ocupfaixaetaria2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 25 & V2009 <= 29)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria2 = mean(aux))

ocupfaixaetaria3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 30 & V2009 <= 39)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria3 = mean(aux))


ocupfaixaetaria4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 40 & V2009 <= 49)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria4 = mean(aux))


ocupfaixaetaria5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 50 & V2009 <= 59)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria5 = mean(aux))


ocupfaixaetaria6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & V2009 >= 60) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria6 = mean(aux))

ocupsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario1 = mean(aux))

ocupsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario2 = mean(aux))

ocupsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "3") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario3 = mean(aux))

ocupsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "4") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario4 = mean(aux))

ocupsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "5") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario5 = mean(aux))

ocupsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "6") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario6 = mean(aux))

ocupsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "7") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario7 = mean(aux))

ocupsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "8") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario8 = mean(aux))

ocupeduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc1 = mean(aux))

ocupeduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc2 = mean(aux))

ocupeduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "3") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc3 = mean(aux))

ocupeduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "4") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc4 = mean(aux))

ocupeduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "5") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc5 = mean(aux))

ocupeduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "6") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc6 = mean(aux))

ocupeduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "7") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc7 = mean(aux))

ocupraca1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca1 = mean(aux))

ocupraca2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca2 = mean(aux))

ocupraca3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "3") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca3 = mean(aux))

ocupraca4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "4") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca4 = mean(aux))

ocupraca5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "5") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca5 = mean(aux))

ocupgenero1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007) %>%
  filter(VD4002 == "1" & V2007 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupgenero1 = mean(aux))

ocupgenero2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007) %>%
  filter(VD4002 == "1" & V2007 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupgenero2 = mean(aux))

ocupurbana <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022) %>%
  filter(VD4002 == "1" & V1022 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupurbana = mean(aux))

ocuprural <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022) %>%
  filter(VD4002 == "1" & V1022 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocuprural = mean(aux))

ocupnorte <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupnorte = mean(aux))

ocupnordeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupnordeste = mean(aux))

ocupsudeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "31" | UF == "32" | UF == "33"| UF == "34"| UF == "35")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsudeste = mean(aux))

ocupsul <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "41" | UF == "42" | UF == "43")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsul = mean(aux))

ocupcentrooeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "50" | UF == "51" | UF == "52"| UF == "53")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupcentrooeste = mean(aux))



x <- merge(ocup, ocupfaixaetaria1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsalario8, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupeduc7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupraca1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupraca2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupraca3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupraca4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupraca5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupgenero1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupgenero2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupurbana, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocuprural, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupnorte, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupnordeste, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsudeste, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupsul, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocupcentrooeste, by = c("UF", "Trimestre"), all = TRUE)
x <- x %>% mutate(year = yr)

write.csv(x, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupuf", yr, ".csv"))

########################
# desocupados de cada estado
########################

desocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "2") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desocupuf = mean(aux))

write.csv(desocup, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desocupuf", yr, ".csv"))

########################
# desalentados de cada estado
########################

desalent <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4005) %>%
  filter(VD4005 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desalentuf = mean(aux))

write.csv(desalent, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desalentuf", yr, ".csv"))

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

#write.csv(nemnemuf, file ="C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/nemnem2012.csv")#



########################
# trabalhadores formais de cada estado
########################

formal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012) %>%
  filter(VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1")) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaluf = mean(aux))

formaleduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "1" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc1 = mean(aux))

formaleduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "2" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc2 = mean(aux))

formaleduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "3" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc3 = mean(aux))

formaleduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "4" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc4 = mean(aux))

formaleduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "5" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc5 = mean(aux))

formaleduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "6" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc6 = mean(aux))

formaleduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "7" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc7 = mean(aux))

formalsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "1" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario1 = mean(aux))

formalsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "2" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario2 = mean(aux))

formalsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "3" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario3 = mean(aux))

formalsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "4" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario4 = mean(aux))

formalsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "5" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario5 = mean(aux))

formalsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "6" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario6 = mean(aux))

formalsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "7" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario7 = mean(aux))

formalsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "8" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario8 = mean(aux))


x <- merge(formal, formaleduc1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formaleduc7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formalsalario8, by = c("UF", "Trimestre"), all = TRUE)
x <- x %>% mutate(year = yr)

write.csv(formal, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaluf", yr, ".csv"))


########################
# trabalhadores informais de cada estado
########################

informal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaluf = mean(aux))

informalfaixaetaria1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 18 & V2009 <= 24) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria1 = mean(aux))

informalfaixaetaria2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 25 & V2009 <= 29) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria2 = mean(aux))

informalfaixaetaria3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 30 & V2009 <= 39) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria3 = mean(aux))


informalfaixaetaria4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 40 & V2009 <= 49) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria4 = mean(aux))


informalfaixaetaria5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 50 & V2009 <= 59) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria5 = mean(aux))


informalfaixaetaria6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 60) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria6 = mean(aux))

informalsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario1 = mean(aux))


informalsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario2 = mean(aux))


informalsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario3 = mean(aux))

informalsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario4 = mean(aux))

informalsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario5 = mean(aux))


informalsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "6") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario6 = mean(aux))


informalsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "7") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario7 = mean(aux))


informalsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "8") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario8 = mean(aux))

informaleduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc1 = mean(aux))

informaleduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc2 = mean(aux))

informaleduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc3 = mean(aux))

informaleduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc4 = mean(aux))

informaleduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc5 = mean(aux))

informaleduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "6") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc6 = mean(aux))

informaleduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "7") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc7 = mean(aux))

informalraca1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca1 = mean(aux))

informalraca2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca2 = mean(aux))

informalraca3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca3 = mean(aux))

informalraca4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca4 = mean(aux))

informalraca5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca5 = mean(aux))

informalgenero1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2007 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero1 = mean(aux))

informalgenero2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2007 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero2 = mean(aux))

informalurbana <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V1022 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalurbana = mean(aux))

informalrural <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V1022 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalrural = mean(aux))

informalnorte <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalnorte = mean(aux))

informalnordeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalnordeste = mean(aux))

informalsudeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "31" | UF == "32" | UF == "33"| UF == "35") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsudeste = mean(aux))

informalsul <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "41" | UF == "42" | UF == "43") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsul = mean(aux))

informalcentrooeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "50" | UF == "51" | UF == "52"| UF == "53") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalcentrooeste = mean(aux))

x <- merge(informal, informalfaixaetaria1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsalario8, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc6, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informaleduc7, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalraca1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalraca2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalraca3, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalraca4, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalraca5, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalgenero1, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalgenero2, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalurbana, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalrural, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalnorte, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalnordeste, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsudeste, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalsul, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalcentrooeste, by = c("UF", "Trimestre"), all = TRUE)
x <- x %>% mutate(year = yr)

write.csv(x, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaluf", yr, ".csv"))

##############################################
# informais não contribuintes de cada estado #
##############################################

informalnc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & ((VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalncuf = mean(aux))

write.csv(informalnc, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalncuf", yr, ".csv"))

########################
# carteira assinada de cada estado
########################

cart <- data %>%
  select(UF, V1028, Trimestre, Ano, V4029) %>%
  filter(V4029 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(cartuf = mean(aux))

write.csv(cart, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/cartuf", yr, ".csv"))

########################
# contribuintes de cada estado
########################

inss <- data %>%
  select(UF, V1028, Trimestre, Ano, V4032) %>%
  filter(V4032 == "1") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(inssuf = mean(aux))

write.csv(inss, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/inssuf", yr, ".csv"))



########################
# trabalhadores no setor privado de cada estado
########################

priv <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == "3") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(privuf = mean(aux))

write.csv(priv, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/privuf", yr, ".csv"))

########################
# trabalhadores no setor público de cada estado
########################

publ <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == "4") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(publuf = mean(aux))

write.csv(publ, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/publuf", yr, ".csv"))

########################
# quantidade de ocupados agricultura (primario) de cada estado
########################

prim <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "01") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(primuf = mean(aux))

write.csv(prim, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/primuf", yr, ".csv"))

########################
# quantidade de ocupados indústria (secundario) de cada estado
# precisa fazer pra construção? #
########################

sec <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "02" | VD4010 == "03") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(secuf = mean(aux))

write.csv(sec, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/secuf", yr, ".csv"))

########################
# quantidade de ocupados comercio e serviços (terciario) de cada estado
# dividir em comercio e serviços? #
########################

terc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "04" | VD4010 == "05" | VD4010 == "06" | VD4010 == "07" | VD4010 == "08" | VD4010 == "09") %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(tercuf = mean(aux))

write.csv(terc, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/tercuf", yr, ".csv"))


###################################
### juntando as bases de dados ####
###################################

x <- merge(pea, pia, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, desocup, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, cart, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, desalent, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, formal, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informal, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, informalnc, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, inss, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, ocup, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, popuf, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, prim, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, priv, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, publ, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, sec, by = c("UF", "Trimestre"), all = TRUE)
x <- merge(x, terc, by = c("UF", "Trimestre"), all = TRUE)
x <- x %>% mutate(year = yr)

write.csv(x, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/output/pnadc", yr, ".csv"))
}



