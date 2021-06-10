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
library(hrbrthemes)
library(dplyr)
library(zoo)


#####################
# juntando as pnadc #
#####################

setwd(in_dir)

x121 <- read.csv("pnadc012012.csv")
x122 <- read.csv("pnadc022012.csv")
x123 <- read.csv("pnadc032012.csv")
x124 <- read.csv("pnadc042012.csv")
x131 <- read.csv("pnadc012013.csv")
x132 <- read.csv("pnadc022013.csv")
x133 <- read.csv("pnadc032013.csv")
x134 <- read.csv("pnadc042013.csv")
x141 <- read.csv("pnadc012014.csv")
x142 <- read.csv("pnadc022014.csv")
x143 <- read.csv("pnadc032014.csv")
x144 <- read.csv("pnadc042014.csv")
x151 <- read.csv("pnadc012015.csv")
x152 <- read.csv("pnadc022015.csv")
x153 <- read.csv("pnadc032015.csv")
x154 <- read.csv("pnadc042015.csv")
x161 <- read.csv("pnadc012016.csv")
x162 <- read.csv("pnadc022016.csv")
x163 <- read.csv("pnadc032016.csv")
x164 <- read.csv("pnadc042016.csv")
x171 <- read.csv("pnadc012017.csv")
x172 <- read.csv("pnadc022017.csv")
x173 <- read.csv("pnadc032017.csv")
x174 <- read.csv("pnadc042017.csv")
x181 <- read.csv("pnadc012018.csv")
x182 <- read.csv("pnadc022018.csv")
x183 <- read.csv("pnadc032018.csv")
x184 <- read.csv("pnadc042018.csv")
x191 <- read.csv("pnadc012019.csv")
x192 <- read.csv("pnadc022019.csv")
x193 <- read.csv("pnadc032019.csv")
x194 <- read.csv("pnadc042019.csv")
x201 <- read.csv("pnadc012020.csv")
x202 <- read.csv("pnadc022020.csv")
x203 <- read.csv("pnadc032020.csv")
x204 <- read.csv("pnadc042020.csv")
x211 <- read.csv("pnadc012021.csv")

x <- rbind(x121, x122, x123, x124, x131, x132, x133, x134, x141, x142, x143, x144, x151, x152, x153, x154,
           x161, x162, x163, x164, x171, x172, x173, x174, x181, x182, x183, x184, x191, x192, x193, x194,
           x201, x202, x203, x204, x211)

x <- x %>%
  mutate(time = paste0("0", year))

x <- x %>%
  mutate(time = paste0("0", year),
         xx = substr(x$time, 1, 2),
         zz = substr(x$time, 3, 6),
         tempo = paste0 (zz, "-", xx))

x$tempo <- as.Date(as.yearqtr(x$tempo))


################################
## fazendo graficos          ###
## GRAFICOS DO DIA 08 DE JUNHO #
## tentativa 1                 #
################################

item1 <- x %>%
  mutate(informal = sum(informaluf),
         ocup = sum(ocupuf)) %>%
  summarise()
  mutate(taxadeinformalidade = (informaluf/ocupuf)*100) %>%
  ggplot(aes(year, taxadeinformalidade, color = UF)) +
  geom_point()

item2 <- x %>%
  mutate(nc = sum(informalnc),
          ocup = sum (ocupuf)) %>%
  summarise()
  mutate(taxadeinformalidadenc = (nc/ocupuf)*100) %>%
  ggplot(aes(year,taxadeinformalidadenc, color = UF)) +
  geom_point()

lalala <- x %>%
  filter(UF == 33) %>%
  mutate(taxadeinformalidade = (informaluf/ocupuf)*100) %>%
  ggplot(aes(tempo, informaluf), color = UF) +
  geom_line(color = UF)


##############################
## taxa de informalidade    ##
##############################

x <- x %>%
    mutate(informal = sum(informaluf),
           ocup = sum(ocupuf),
           informalnc = sum(informalncuf),
           taxadeinformalidade = (informaluf/ocupuf)*100)

ggplot(data = item1, aes(tempo,
                          taxadeinformalidade)) +
     geom_line(color = "grey") +
     geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 2) +
     theme_bw() +
     scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
     labs(x = "Ano",
          y = "Taxa de Informalidade",
          title = "Evolução da Taxa de Informalidade")


#################
## inf x infnc ##
#################

x <- x %>%
    mutate(taxadeinformalidadenc =(informalncuf/ocupuf)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidade)) +
    geom_line(aes(col = "Taxa de Informalidade")) +
    geom_line(aes(y = taxadeinformalidadenc, col = "Taxa de Informalidade dos Não Contribuintes")) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Informalidade X Informalidade Não Contribuintes") +
    theme(legend.position = 'top')

##############################
## informalidade por gênero ##
##############################

x <- x %>%
         mutate(taxadeinformalidadehomem = (informalgenero1/ocupuf)*100,
                taxadeinformalidademulher = (informalgenero2/ocupuf)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadehomem)) +
    geom_line(aes(col="Homens")) +
    geom_line(aes(y = taxadeinformalidademulher, col = "Mulheres")) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Gênero") +
    theme(legend.position = 'top')

####################################
## informalidade por faixa etária ##
####################################

x <- x %>%
         mutate(taxadeinformalidadeFE1 = (informalfaixaetaria1/ocupuf)*100,
                taxadeinformalidadeFE2 = (informalfaixaetaria2/ocupuf)*100,
                taxadeinformalidadeFE3 = (informalfaixaetaria3/ocupuf)*100,
                taxadeinformalidadeFE4 = (informalfaixaetaria4/ocupuf)*100,
                taxadeinformalidadeFE5 = (informalfaixaetaria5/ocupuf)*100,
                taxadeinformalidadeFE6 = (informalfaixaetaria6/ocupuf)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadeFE1)) +
    geom_line(aes(col = "18-24 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE2, col = "25-29 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE3, col = "30-39 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE4, col = "40-49 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE5, col = "50-59 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE6, col = "60+ anos")) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Idade") +
    theme(legend.position = 'top')




# tentando usar scatterplot #

item1 %>%
  tail(10) %>%
  ggplot( aes(x=Trimestre, y=value)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    theme_ipsum() +
    ggtitle("Evolução da Taxa de Informalidade")

# retomando o que fizemos em reunião #

g <- ggplot(x)
g + geom_point(aes(year, informaluf, colour = UF)) +
  geom_smooth(method = lm)


x %>% mutate(x, taxadeinformalidade = (informaluf/ocupuf)*100) %>%
  ggplot(aes(year, taxadeinformalidade, colour = UF)) +
  geom_line()


ggplot(x, aes(year, informaluf, colour = UF))



