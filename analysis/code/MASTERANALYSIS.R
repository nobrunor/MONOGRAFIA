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

home_dir <- file.path(ROOT, "analysis")
in_dir <- file.path(ROOT, "build", "output")
out_dir <- file.path(ROOT, "analysis", "output")
tmp_dir <- file.path(ROOT, "analysis", "tmp")
code_dir <- file.path(ROOT, "analysis", "code")


####################
# load library
####################

library(tidyverse)
library(haven)
library(ggplot2)


###################################
### juntando as bases de dados ####
###################################

x <- merge(pea, pia, by = c("UF", "Trimestre"))
x <- merge(x, desocup, by = c("UF", "Trimestre"))
x <- merge(x, cart, by = c("UF", "Trimestre"))
x <- merge(x, desalent, by = c("UF", "Trimestre"))
x <- merge(x, formal, by = c("UF", "Trimestre"))
x <- merge(x, informal, by = c("UF", "Trimestre"))
x <- merge(x, inss, by = c("UF", "Trimestre"))
x <- merge(x, ocup, by = c("UF", "Trimestre"))
x <- merge(x, popuf, by = c("UF", "Trimestre"))
x <- merge(x, prim, by = c("UF", "Trimestre"))
x <- merge(x, priv, by = c("UF", "Trimestre"))
x <- merge(x, publ, by = c("UF", "Trimestre"))
x <- merge(x, sec, by = c("UF", "Trimestre"))
x <- merge(x, terc, by = c("UF", "Trimestre"))
x <- x %>% mutate(year = yr)

write.csv(x, paste0("C:/Users/Bruno/Desktop/MONOGRAFIA/build/output/pnadc", yr, ".csv"))
}


#####################
# juntando as pnadc #
#####################

setwd(in_dir)

x12 <- read.csv("pnadc2012.csv")
x13 <- read.csv("pnadc2013.csv")
x14 <- read.csv("pnadc2014.csv")
x15 <- read.csv("pnadc2015.csv")
x16 <- read.csv("pnadc2016.csv")
x17 <- read.csv("pnadc2017.csv")
x18 <- read.csv("pnadc2018.csv")
x19 <- read.csv("pnadc2019.csv")

x <- rbind(x12, x13, x14, x15, x16, x17, x18, x19)


######################
## fazendo graficos ##
######################
x %>% group_by(UF) %>%  mutate(taxadeinformalidade = (informaluf/ocupuf)*100) %>%
  ggplot(aes(year, taxadeinformalidade, colour = UF)) +
  geom_point()


g <- ggplot(x)
g + geom_point(aes(year, informaluf, colour = UF)) +
  geom_smooth(method = lm)


x %>% mutate(x, taxadeinformalidade = (informaluf/ocupuf)*100) %>%
  ggplot(aes(year, taxadeinformalidade, colour = UF)) +
  geom_line()


ggplot(x, aes(year, informaluf, colour = UF))



