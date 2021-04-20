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

popuf <- data %>% select(UF, V1028, Trimestre) %>% group_by(UF, Trimestre) %>% mutate(popuf = sum(V1028)) %>% summarise(aux = mean(popuf))

# salvar em um arquivo csv # - resolver isso (pendente) #

write.csv(popuf, file = "out_dir")

########################
# ocupados de cada estado
########################



class(data$pnadc_012015)

ab <- as_tibble(data$pnadc_012015)



