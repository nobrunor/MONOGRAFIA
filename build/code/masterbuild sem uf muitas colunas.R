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

memory.limit (999999999999)


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
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(popuf = mean(aux))


write.csv(popuf, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/popuf", yr, ".csv"))

########################
# PIA de cada estado
########################

pia <- data %>%
  select(UF, V1028, Trimestre, Ano, V2009) %>%
  filter(V2009 >= 14) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(piauf = mean(aux))

write.csv(pia, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/piauf", yr, ".csv"))

########################
# PEA de cada estado
########################

pea <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" | VD4002 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(peauf = mean(aux))

write.csv(pea, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/peauf", yr, ".csv"))

########################
# ocupados de cada estado
########################

ocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupuf = mean(aux))

write.csv(ocup, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupuf", yr, ".csv"))

ocupfaixaetaria1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 18 & V2009 <= 24)) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria1 = mean(aux))

write.csv(ocupfaixaetaria1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria1", yr, ".csv"))

ocupfaixaetaria2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 25 & V2009 <= 29)) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria2 = mean(aux))

write.csv(ocupfaixaetaria2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria2", yr, ".csv"))

ocupfaixaetaria3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 30 & V2009 <= 39)) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria3 = mean(aux))

write.csv(ocupfaixaetaria3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria3", yr, ".csv"))

ocupfaixaetaria4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 40 & V2009 <= 49)) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria4 = mean(aux))

write.csv(ocupfaixaetaria4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria4", yr, ".csv"))

ocupfaixaetaria5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & (V2009 >= 50 & V2009 <= 59)) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria5 = mean(aux))

write.csv(ocupfaixaetaria5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria5", yr, ".csv"))

ocupfaixaetaria6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009) %>%
  filter(VD4002 == "1" & V2009 >= 60) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupfaixaetaria6 = mean(aux))

write.csv(ocupfaixaetaria6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupfaixaetaria6", yr, ".csv"))

ocupsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario1 = mean(aux))

write.csv(ocupsalario1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario1", yr, ".csv"))

ocupsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario2 = mean(aux))

write.csv(ocupsalario2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario2", yr, ".csv"))

ocupsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "3") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario3 = mean(aux))

write.csv(ocupsalario3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario3", yr, ".csv"))

ocupsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "4") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario4 = mean(aux))

write.csv(ocupsalario4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario4", yr, ".csv"))

ocupsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "5") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario5 = mean(aux))

write.csv(ocupsalario5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario5", yr, ".csv"))

ocupsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "6") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario6 = mean(aux))

write.csv(ocupsalario6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario6", yr, ".csv"))

ocupsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "7") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario7 = mean(aux))

write.csv(ocupsalario7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario7", yr, ".csv"))

ocupsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311) %>%
  filter(VD4002 == "1" & V403311 == "8") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsalario8 = mean(aux))

write.csv(ocupsalario8, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsalario8", yr, ".csv"))


ocupeduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc1 = mean(aux))

write.csv(ocupeduc1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc1", yr, ".csv"))


ocupeduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc2 = mean(aux))

write.csv(ocupeduc2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc2", yr, ".csv"))


ocupeduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "3") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc3 = mean(aux))

write.csv(ocupeduc3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc3", yr, ".csv"))


ocupeduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "4") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc4 = mean(aux))

write.csv(ocupeduc4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc4", yr, ".csv"))


ocupeduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "5") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc5 = mean(aux))

write.csv(ocupeduc5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc5", yr, ".csv"))


ocupeduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "6") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc6 = mean(aux))

write.csv(ocupeduc6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc6", yr, ".csv"))


ocupeduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004) %>%
  filter(VD4002 == "1" & VD3004 == "7") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupeduc7 = mean(aux))

write.csv(ocupeduc7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupeduc7", yr, ".csv"))

ocupraca1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca1 = mean(aux))

write.csv(ocupraca1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupraca1", yr, ".csv"))

ocupraca2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca2 = mean(aux))

write.csv(ocupraca2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupraca2", yr, ".csv"))

ocupraca3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "3") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca3 = mean(aux))

write.csv(ocupraca3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupraca3", yr, ".csv"))

ocupraca4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "4") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca4 = mean(aux))

write.csv(ocupraca4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupraca4", yr, ".csv"))

ocupraca5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010) %>%
  filter(VD4002 == "1" & V2010 == "5") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupraca5 = mean(aux))

write.csv(ocupraca5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupraca5", yr, ".csv"))


ocupgenero1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007) %>%
  filter(VD4002 == "1" & V2007 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupgenero1 = mean(aux))

write.csv(ocupgenero1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupgenero1", yr, ".csv"))


ocupgenero2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007) %>%
  filter(VD4002 == "1" & V2007 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupgenero2 = mean(aux))

write.csv(ocupgenero2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupgenero2", yr, ".csv"))


ocupurbana <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022) %>%
  filter(VD4002 == "1" & V1022 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupurbana = mean(aux))

write.csv(ocupurbana, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupurbana", yr, ".csv"))


ocuprural <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022) %>%
  filter(VD4002 == "1" & V1022 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocuprural = mean(aux))

write.csv(ocuprural, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocuprural", yr, ".csv"))


ocupnorte <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupnorte = mean(aux))

write.csv(ocupnorte, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupnorte", yr, ".csv"))


ocupnordeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupnordeste = mean(aux))

write.csv(ocupnordeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupnordeste", yr, ".csv"))


ocupsudeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "31" | UF == "32" | UF == "33"| UF == "34"| UF == "35")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsudeste = mean(aux))

write.csv(ocupsudeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsudeste", yr, ".csv"))


ocupsul <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "41" | UF == "42" | UF == "43")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupsul = mean(aux))

write.csv(ocupsul, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupsul", yr, ".csv"))


ocupcentrooeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "1" & (UF == "50" | UF == "51" | UF == "52"| UF == "53")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(ocupcentrooeste = mean(aux))

write.csv(ocupcentrooeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/ocupcentrooeste", yr, ".csv"))



########################
# desocupados de cada estado
########################

desocup <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002) %>%
  filter(VD4002 == "2") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desocupuf = mean(aux))

write.csv(desocup, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desocupuf", yr, ".csv"))

########################
# desalentados de cada estado
########################

desalent <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4005) %>%
  filter(VD4005 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(desalentuf = mean(aux))

write.csv(desalent, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/desalentuf", yr, ".csv"))

########################
# nemnem de cada estado
########################

#nemnem <- data %>%
  #select(UF, V1028, Trimestre, Ano, V2009, V3002, V4074, V4074A, VD4001, VD4002) %>%
  #filter(VD4002 == 2 | (VD4001 == 2 & V3002 == 2)) %>%
  #group_by(Trimestre) %>%
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
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaluf = mean(aux))

write.csv(formal, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaluf", yr, ".csv"))  

rendmedioformal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4019) %>%
  filter(VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1")) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028),
         aux2 = sum(VD4019 * V1028),
         aux3 = aux2/aux) %>%
  summarise(rendmedioformal = mean(aux3))

write.csv(rendmedioformal, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/rendmedioformal", yr, ".csv"))

formaleduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "1" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc1 = mean(aux))

write.csv(formaleduc1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc1", yr, ".csv"))

formaleduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "2" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc2 = mean(aux))

write.csv(formaleduc2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc2", yr, ".csv"))

formaleduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "3" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc3 = mean(aux))

write.csv(formaleduc3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc3", yr, ".csv"))

formaleduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "4" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc4 = mean(aux))

write.csv(formaleduc4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc4", yr, ".csv"))

formaleduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "5" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc5 = mean(aux))

write.csv(formaleduc5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc5", yr, ".csv"))

formaleduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "6" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc6 = mean(aux))

write.csv(formaleduc6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc6", yr, ".csv"))

formaleduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, VD3004) %>%
  filter(VD3004 == "7" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formaleduc7 = mean(aux))

write.csv(formaleduc7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formaleduc7", yr, ".csv"))

formalsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "1" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario1 = mean(aux))

write.csv(formalsalario1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario1", yr, ".csv"))

formalsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "2" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario2 = mean(aux))

write.csv(formalsalario2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario2", yr, ".csv"))  

formalsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "3" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario3 = mean(aux))

write.csv(formalsalario3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario3", yr, ".csv"))

formalsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "4" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario4 = mean(aux))

write.csv(formalsalario4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario4", yr, ".csv"))

formalsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "5" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario5 = mean(aux))

write.csv(formalsalario5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario5", yr, ".csv"))

formalsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "6" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario6 = mean(aux))

write.csv(formalsalario6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario6", yr, ".csv"))

formalsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "7" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario7 = mean(aux))

write.csv(formalsalario7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario7", yr, ".csv"))

formalsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4009, VD4012, V403311) %>%
  filter(V403311 == "8" & (VD4009 == "01" | VD4009 == "03" | VD4009 == "05" | VD4009 == "07" |
        (VD4009 == "08" & VD4012 == "1") | (VD4009 == "09" & VD4012 == "1"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(formalsalario8 = mean(aux))

write.csv(formalsalario8, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/formalsalario8", yr, ".csv"))


########################
# trabalhadores informais de cada estado
########################

informal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaluf = mean(aux))

write.csv(informal, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaluf", yr, ".csv"))

informalfaixaetaria1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 18 & V2009 <= 24) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria1 = mean(aux))

write.csv(informalfaixaetaria1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria1", yr, ".csv"))

informalfaixaetaria2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 25 & V2009 <= 29) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria2 = mean(aux))

write.csv(informalfaixaetaria2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria2", yr, ".csv"))

informalfaixaetaria3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 30 & V2009 <= 39) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria3 = mean(aux))

write.csv(informalfaixaetaria3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria3", yr, ".csv"))

informalfaixaetaria4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 40 & V2009 <= 49) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria4 = mean(aux))

write.csv(informalfaixaetaria4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria4", yr, ".csv"))

informalfaixaetaria5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 50 & V2009 <= 59) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria5 = mean(aux))

write.csv(informalfaixaetaria5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria5", yr, ".csv"))

informalfaixaetaria6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2009 >= 60) & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria6 = mean(aux))

write.csv(informalfaixaetaria6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalfaixaetaria6", yr, ".csv"))

informalsalario1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario1 = mean(aux))

write.csv(informalsalario1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario1", yr, ".csv"))


informalsalario2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario2 = mean(aux))

write.csv(informalsalario2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario2", yr, ".csv"))


informalsalario3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario3 = mean(aux))

write.csv(informalsalario3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario3", yr, ".csv"))


informalsalario4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario4 = mean(aux))

write.csv(informalsalario4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario4", yr, ".csv"))


informalsalario5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario5 = mean(aux))

write.csv(informalsalario5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario5", yr, ".csv"))


informalsalario6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "6") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario6 = mean(aux))

write.csv(informalsalario6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario6", yr, ".csv"))


informalsalario7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "7") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario7 = mean(aux))

write.csv(informalsalario7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario7", yr, ".csv"))

informalsalario8 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V403311, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V403311 == "8") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsalario8 = mean(aux))

write.csv(informalsalario8, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsalario8", yr, ".csv"))

informaleduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc1 = mean(aux))

write.csv(informaleduc1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc1", yr, ".csv"))

informaleduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc2 = mean(aux))

write.csv(informaleduc2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc2", yr, ".csv"))

informaleduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc3 = mean(aux))

write.csv(informaleduc3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc3", yr, ".csv"))

informaleduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc4 = mean(aux))

write.csv(informaleduc4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc4", yr, ".csv"))

informaleduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc5 = mean(aux))

write.csv(informaleduc5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc5", yr, ".csv"))

informaleduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "6") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc6 = mean(aux))

write.csv(informaleduc6, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc6", yr, ".csv"))

informaleduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (VD3004 == "7") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc7 = mean(aux))

write.csv(informaleduc7, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaleduc7", yr, ".csv"))

informalraca1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca1 = mean(aux))

write.csv(informalraca1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalraca1", yr, ".csv"))

informalraca2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca2 = mean(aux))

write.csv(informalraca2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalraca2", yr, ".csv"))

informalraca3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "3") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca3 = mean(aux))

write.csv(informalraca3, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalraca3", yr, ".csv"))

informalraca4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "4") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca4 = mean(aux))

write.csv(informalraca4, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalraca4", yr, ".csv"))

informalraca5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2010 == "5") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca5 = mean(aux))

write.csv(informalraca5, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalraca5", yr, ".csv"))

informalgenero1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2007 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero1 = mean(aux))

write.csv(informalgenero1, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalgenero1", yr, ".csv"))

informalgenero2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V2007 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero2 = mean(aux))

write.csv(informalgenero2, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalgenero2", yr, ".csv"))

informalurbana <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V1022 == "1") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalurbana = mean(aux))

write.csv(informalurbana, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalurbana", yr, ".csv"))  

informalrural <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V1022, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (V1022 == "2") & (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalrural = mean(aux))

write.csv(informalrural, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalrural", yr, ".csv"))

informalnorte <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalnorte = mean(aux))

write.csv(informalnorte, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalnorte", yr, ".csv"))

informalnordeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalnordeste = mean(aux))

write.csv(informalnordeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalnordeste", yr, ".csv"))

informalsudeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "31" | UF == "32" | UF == "33"| UF == "35") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsudeste = mean(aux))

write.csv(informalsudeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsudeste", yr, ".csv"))

informalsul <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "41" | UF == "42" | UF == "43") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsul = mean(aux))

write.csv(informalsul, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsul", yr, ".csv"))

informalcentrooeste <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & (UF == "50" | UF == "51" | UF == "52"| UF == "53") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalcentrooeste = mean(aux))

write.csv(informalcentrooeste, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalcentrooeste", yr, ".csv"))

informalprim <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4010) %>%
  filter(VD4002 == "1" & (VD4010 == "01") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalprim = mean(aux))

write.csv(informalprim, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalprim", yr, ".csv"))

informalsec <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4010) %>%
  filter(VD4002 == "1" & (VD4010 == "02" | VD4010 == "03") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsec = mean(aux))

write.csv(informalsec, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsec", yr, ".csv"))

informalterc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4010) %>%
  filter(VD4002 == "1" & (VD4010 == "02" | VD4010 == "03"| VD4010 == "04" | VD4010 == "05" | VD4010 == "06" | VD4010 == "07" |
            VD4010 == "08" | VD4010 == "09" | VD4010 == "10" | VD4010 == "11") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalterc = mean(aux))

write.csv(informalterc, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalterc", yr, ".csv"))

informalsecsemconstr <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4010) %>%
  filter(VD4002 == "1" & (VD4010 == "03") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalsecsemconstr = mean(aux))

write.csv(informalsecsemconstr, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalsecsemconstr", yr, ".csv"))


informaltercservicos <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012, VD4010) %>%
  filter(VD4002 == "1" & (VD4010 == "09" | VD4010 == "10" | VD4010 == "11") &
           (VD4009 == "02" | VD4009 == "04" | VD4009 == "06" | VD4009 == "10" |
           (VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaltercservicos = mean(aux))

write.csv(informaltercservicos, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informaltercservicos", yr, ".csv"))


##############################################
# informais não contribuintes de cada estado #
##############################################

informalnc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == "1" & ((VD4009 == "08" & VD4012 == "2") | (VD4009 == "09" & VD4012 == "2"))) %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalncuf = mean(aux))

write.csv(informalnc, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/informalncuf", yr, ".csv"))

########################
# carteira assinada de cada estado
########################

cart <- data %>%
  select(UF, V1028, Trimestre, Ano, V4029) %>%
  filter(V4029 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(cartuf = mean(aux))

write.csv(cart, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/cartuf", yr, ".csv"))

########################
# contribuintes de cada estado
########################

inss <- data %>%
  select(UF, V1028, Trimestre, Ano, V4032) %>%
  filter(V4032 == "1") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(inssuf = mean(aux))

write.csv(inss, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/inssuf", yr, ".csv"))



########################
# trabalhadores no setor privado de cada estado
########################

priv <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == "3") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(privuf = mean(aux))

write.csv(priv, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/privuf", yr, ".csv"))

########################
# trabalhadores no setor público de cada estado
########################

publ <- data %>%
  select(UF, V1028, Trimestre, Ano, V4012) %>%
  filter(V4012 == "4") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(publuf = mean(aux))

write.csv(publ, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/publuf", yr, ".csv"))

########################
# quantidade de ocupados agricultura (primario) de cada estado
########################

prim <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "01") %>%
  group_by(Trimestre) %>%
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
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(secuf = mean(aux))

write.csv(sec, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/secuf", yr, ".csv"))

########################
# quantidade de ocupados comercio e serviços (terciario) de cada estado
# dividir em comercio e serviços? #
########################

terc <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "04" | VD4010 == "05" | VD4010 == "06" | VD4010 == "07" | VD4010 == "08" | VD4010 == "09" |
         VD4010 == "10" |  VD4010 == "11") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(tercuf = mean(aux))

write.csv(terc, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/tercuf", yr, ".csv"))

secsemconstr <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter(VD4010 == "02") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(secsemconstr = mean(aux))

write.csv(secsemconstr, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/secsemconstr", yr, ".csv"))

tercservicos <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4010) %>%
  filter( VD4010 == "09" | VD4010 == "10" |  VD4010 == "11") %>%
  group_by(Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(tercservicos = mean(aux))

write.csv(tercservicos, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/tmp/tercservicos", yr, ".csv"))


###################################
### juntando as bases de dados ####
###################################

x <- merge(pea, pia, by = c("Trimestre"), all = TRUE)
x <- merge(x, desocup, by = c("Trimestre"), all = TRUE)
x <- merge(x, cart, by = c("Trimestre"), all = TRUE)
x <- merge(x, desalent, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocup, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria1, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria2, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria3, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria4, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria5, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupfaixaetaria6, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario1, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario2, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario3, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario4, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario5, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario6, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario7, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsalario8, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc1, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc2, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc3, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc4, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc5, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc6, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupeduc7, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupraca1, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupraca2, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupraca3, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupraca4, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupraca5, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupgenero1, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupgenero2, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupurbana, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocuprural, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupnorte, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupnordeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsudeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupsul, by = c("Trimestre"), all = TRUE)
x <- merge(x, ocupcentrooeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, formal, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc1, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc2, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc3, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc4, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc5, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc6, by = c("Trimestre"), all = TRUE)
x <- merge(x, formaleduc7, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario1, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario2, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario3, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario4, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario5, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario6, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario7, by = c("Trimestre"), all = TRUE)
x <- merge(x, formalsalario8, by = c("Trimestre"), all = TRUE)
x <- merge(x, informal, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria1, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria2, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria3, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria4, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria5, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalfaixaetaria6, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario1, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario2, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario3, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario4, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario5, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario6, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario7, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsalario8, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc1, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc2, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc3, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc4, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc5, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc6, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaleduc7, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalraca1, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalraca2, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalraca3, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalraca4, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalraca5, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalgenero1, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalgenero2, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalurbana, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalrural, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalnorte, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalnordeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsudeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsul, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalcentrooeste, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalnc, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalprim, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsec, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalterc, by = c("Trimestre"), all = TRUE)
x <- merge(x, informalsecsemconstr, by = c("Trimestre"), all = TRUE)
x <- merge(x, informaltercservicos, by = c("Trimestre"), all = TRUE)
x <- merge(x, inss, by = c("Trimestre"), all = TRUE)
x <- merge(x, popuf, by = c("Trimestre"), all = TRUE)
x <- merge(x, prim, by = c("Trimestre"), all = TRUE)
x <- merge(x, priv, by = c("Trimestre"), all = TRUE)
x <- merge(x, publ, by = c("Trimestre"), all = TRUE)
x <- merge(x, sec, by = c("Trimestre"), all = TRUE)
x <- merge(x, terc, by = c("Trimestre"), all = TRUE)
x <- merge(x, secsemconstr, by = c("Trimestre"), all = TRUE)
x <- merge(x, tercservicos, by = c("Trimestre"), all = TRUE)
x <- x %>% mutate(year = yr)

write.csv(x, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/output/pnadc", yr, ".csv"))
}

