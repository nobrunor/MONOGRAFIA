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
library(scales)


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



##############################
## taxa de informalidade    ##
##############################

item1 <- x %>%
    mutate(informal = sum(informaluf),
           ocup = sum(ocupuf),
           informalnc = sum(informalncuf),
           taxadeinformalidade = (informaluf/ocupuf)*100,
           taxadeinformalidadenc = (informalncuf/ocupuf)*100,
           taxadeinformalidadescart = (informalscart/ocupuf)*100)

ggplot(data = item1, aes(tempo,
                          taxadeinformalidade)) +
     geom_line(color = "blue") +
     geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 2) +
     geom_vline(xintercept = item1$tempo[33], linetype = 8) +
     theme_bw() +
     labs(x = "Ano",
          y = "Em %",
          title = "Evolução da Taxa de Informalidade")


############################
## inf x infnc x infscart ##
############################

ggplot(item1, aes(x = tempo, y = taxadeinformalidade)) +
    geom_line(aes(col = "Taxa de Informalidade")) +
    geom_line(aes(y = taxadeinformalidadenc, col = "Taxa de Informalidade dos Não Contribuintes")) +
    geom_line(aes(y = taxadeinformalidadescart, col = "Taxa de Informalidade dos Sem Carteira Assinada")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Decomposição da Informalidade") +
    theme(legend.position = 'bottom')

##############################
## informalidade por gênero ##
##############################

item1 <- x %>%
         mutate(taxadeinformalidadehomem = (informalgenero1/ocupgenero1)*100,
                taxadeinformalidademulher = (informalgenero2/ocupgenero2)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadehomem)) +
    geom_line(aes(col= "Homens")) +
    geom_line(aes(y = taxadeinformalidademulher, col = "Mulheres")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Gênero") +
    theme(legend.position = 'bottom')

####################################
## informalidade por faixa etária ##
####################################

item1 <- x %>%
         mutate(taxadeinformalidadeFE1 = (informalfaixaetaria1/ocupfaixaetaria1)*100,
                taxadeinformalidadeFE2 = (informalfaixaetaria2/ocupfaixaetaria2)*100,
                taxadeinformalidadeFE3 = (informalfaixaetaria3/ocupfaixaetaria3)*100,
                taxadeinformalidadeFE4 = (informalfaixaetaria4/ocupfaixaetaria4)*100,
                taxadeinformalidadeFE5 = (informalfaixaetaria5/ocupfaixaetaria5)*100,
                taxadeinformalidadeFE6 = (informalfaixaetaria6/ocupfaixaetaria6)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadeFE1)) +
    geom_line(aes(col = "18-24 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE2, col = "25-29 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE3, col = "30-39 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE4, col = "40-49 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE5, col = "50-59 anos")) +
    geom_line(aes(y = taxadeinformalidadeFE6, col = "60+ anos")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Idade") +
    theme(legend.position = 'bottom')

#############################
## informalidade por renda ##
#############################

item1 <- x %>%
         mutate(taxadeinformalidaderenda1 = (informalsalario1/ocupsalario1)*100,
                taxadeinformalidaderenda2 = (informalsalario2/ocupsalario2)*100,
                taxadeinformalidaderenda3 = (informalsalario3/ocupsalario3)*100,
                taxadeinformalidaderenda4 = (informalsalario4/ocupsalario4)*100,
                taxadeinformalidaderenda5 = (informalsalario5/ocupsalario5)*100,
                taxadeinformalidaderenda6 = (informalsalario6/ocupsalario6)*100,
                taxadeinformalidaderenda7 = (informalsalario7/ocupsalario7)*100,
                taxadeinformalidaderenda8 = (informalsalario8/ocupsalario8)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidaderenda1)) +
    geom_line(aes(col = "até 0,5 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda2, col = "0,5 - 1 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda3, col = "1 - 2 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda4, col = "2 - 3 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda5, col = "3 - 5 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda6, col = "5 - 10 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda7, col = "10 - 20 SM")) +
    geom_line(aes(y = taxadeinformalidaderenda8, col = "Mais de 20 SM")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Renda") +
    theme(legend.position = 'bottom')

######################################
##  informalidade por escolaridade  ##
######################################

item1 <- x %>%
         mutate(taxadeinformalidadeeduc1 = (informaleduc1/ocupeduc1)*100,
                taxadeinformalidadeeduc2 = (informaleduc2/ocupeduc2)*100,
                taxadeinformalidadeeduc3 = (informaleduc3/ocupeduc3)*100,
                taxadeinformalidadeeduc4 = (informaleduc4/ocupeduc4)*100,
                taxadeinformalidadeeduc5 = (informaleduc5/ocupeduc5)*100,
                taxadeinformalidadeeduc6 = (informaleduc6/ocupeduc6)*100,
                taxadeinformalidadeeduc7 = (informaleduc7/ocupeduc7)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadeeduc1)) +
    geom_line(aes(col = "Sem instrução e menos de 1 ano de estudo")) +
    geom_line(aes(y = taxadeinformalidadeeduc2, col = "Fundamental incompleto")) +
    geom_line(aes(y = taxadeinformalidadeeduc3, col = "Fundamental completo")) +
    geom_line(aes(y = taxadeinformalidadeeduc4, col = "Médio incompleto")) +
    geom_line(aes(y = taxadeinformalidadeeduc5, col = "Médio completo")) +
    geom_line(aes(y = taxadeinformalidadeeduc6, col = "Superior incompleto")) +
    geom_line(aes(y = taxadeinformalidadeeduc7, col = "Superior completo")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Escolaridade") +
    theme(legend.position = 'bottom')

##############################
##  informalidade por raca  ##
##############################

item1 <- x %>%
         mutate(taxadeinformalidaderaca1 = (informalraca1/ocupraca1)*100,
                taxadeinformalidaderaca2 = (informalraca2/ocupraca2)*100,
                taxadeinformalidaderaca3 = (informalraca3/ocupraca3)*100,
                taxadeinformalidaderaca4 = (informalraca4/ocupraca4)*100,
                taxadeinformalidaderaca5 = (informalraca5/ocupraca5)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidaderaca1)) +
    geom_line(aes(col = "Branca")) +
    geom_line(aes(y = taxadeinformalidaderaca2, col = "Preta")) +
    geom_line(aes(y = taxadeinformalidaderaca3, col = "Amarela")) +
    geom_line(aes(y = taxadeinformalidaderaca4, col = "Parda")) +
    geom_line(aes(y = taxadeinformalidaderaca5, col = "Indígena")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Cor") +
    theme(legend.position = 'bottom')

###########################################
##  informalidade por tipo de domicilio  ##
###########################################

item1 <- x %>%
         mutate(taxadeinformalidadeurbana = (informalurbana/ocupurbana)*100,
                taxadeinformalidaderural = (informalrural/ocuprural)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadeurbana)) +
    geom_line(aes(col = "Urbano")) +
    geom_line(aes(y = taxadeinformalidaderural, col = "Rural")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Tipo de Domicílio") +
    theme(legend.position = 'bottom')

################################
##  informalidade por região  ##
################################

item1 <- x %>%
         mutate(taxadeinformalidadenorte = (informalnorte/ocupnorte)*100,
                taxadeinformalidadenordeste = (informalnordeste/ocupnordeste)*100,
                taxadeinformalidadesudeste = (informalsudeste/ocupsudeste)*100,
                taxadeinformalidadesul = (informalsul/ocupsul)*100,
                taxadeinformalidadecentrooeste = (informalcentrooeste/ocupcentrooeste)*100)

ggplot(item1, aes(x = tempo, y = taxadeinformalidadenorte)) +
    geom_line(aes(col = "Norte")) +
    geom_line(aes(y = taxadeinformalidadenordeste, col = "Nordeste")) +
    geom_line(aes(y = taxadeinformalidadesudeste, col = "Sudeste")) +
    geom_line(aes(y = taxadeinformalidadesul, col = "Sul")) +
    geom_line(aes(y = taxadeinformalidadecentrooeste, col = "Centro Oeste")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Região") +
    theme(legend.position = 'bottom')


################################
##  informalidade por setor   ##
################################

item1 <- x %>%
         mutate(infprim = (informalprim/primuf)*100,
                infsec = (informalsec/secuf)*100,
                infterc = (informalterc/tercuf)*100,
                infsecsemconstr = (informalsecsemconstr/secsemconstr)*100,
                inftercservicos = (informaltercservicos/tercservicos)*100)

ggplot(item1, aes(x = tempo, y = infprim)) +
    geom_line(aes(col = "Setor Primário")) +
    geom_line(aes(y = infsec, col = "Setor Secundário")) +
    geom_line(aes(y = infterc, col = "Setor Terciário")) +
    geom_line(aes(y = infsecsemconstr, col = "Setor Secundário (Exceto Construção)")) +
    geom_line(aes(y = inftercservicos, col = "Setor Terciário (Serviços)")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Informalidade por Setor") +
    theme(legend.position = 'bottom')


###############################
##    taxa de desocupação    ##
###############################

item1 <- x %>%
         mutate(taxadesocup = (desocupuf/peauf)*100)

ggplot(data = item1, aes(tempo,
                          taxadesocup)) +
     geom_line(color = "blue") +
     geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 2) +
     geom_vline(xintercept = item1$tempo[33], linetype = 8) +
     theme_bw() +
     labs(x = "Ano",
          y = "Em %",
          title = "Evolução da Taxa de Desocupação")

############################
##    taxa de ocupação    ##
############################

item1 <- x %>%
         mutate(taxaocup = (ocupuf/peauf)*100)

ggplot(data = item1, aes(tempo,
                          taxaocup)) +
     geom_line(color = "blue") +
     geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 2) +
     geom_vline(xintercept = item1$tempo[33], linetype = 8) +
     theme_bw() +
     labs(x = "Ano",
          y = "Em %",
          title = "Evolução da Taxa de Ocupação")


###################################
##    evolução da formalidade    ##
###################################

item1 <- x %>%
    mutate(formal = sum(formaluf),
           ocup = sum(ocupuf),
           taxadeformalidade = (formaluf/ocupuf)*100)

ggplot(data = item1, aes(tempo,
                          taxadeformalidade)) +
     geom_line(color = "blue") +
     geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 2) +
     geom_vline(xintercept = item1$tempo[33], linetype = 8) +
     theme_bw() +
     labs(x = "Ano",
          y = "Em %",
          title = "Evolução da Taxa de Formalidade")


######################################
##  formalidade por escolaridade  ##
######################################

item1 <- x %>%
         mutate(taxadeformalidadeeduc1 = (formaleduc1/ocupeduc1)*100,
                taxadeformalidadeeduc2 = (formaleduc2/ocupeduc2)*100,
                taxadeformalidadeeduc3 = (formaleduc3/ocupeduc3)*100,
                taxadeformalidadeeduc4 = (formaleduc4/ocupeduc4)*100,
                taxadeformalidadeeduc5 = (formaleduc5/ocupeduc5)*100,
                taxadeformalidadeeduc6 = (formaleduc6/ocupeduc6)*100,
                taxadeformalidadeeduc7 = (formaleduc7/ocupeduc7)*100)

ggplot(item1, aes(x = tempo, y = taxadeformalidadeeduc1)) +
    geom_line(aes(col = "Sem instrução e menos de 1 ano de estudo")) +
    geom_line(aes(y = taxadeformalidadeeduc2, col = "Fundamental incompleto")) +
    geom_line(aes(y = taxadeformalidadeeduc3, col = "Fundamental completo")) +
    geom_line(aes(y = taxadeformalidadeeduc4, col = "Médio incompleto")) +
    geom_line(aes(y = taxadeformalidadeeduc5, col = "Médio completo")) +
    geom_line(aes(y = taxadeformalidadeeduc6, col = "Superior incompleto")) +
    geom_line(aes(y = taxadeformalidadeeduc7, col = "Superior completo")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Formalidade por Escolaridade") +
    theme(legend.position = 'bottom')

#############################
## informalidade por renda ##
#############################

item1 <- x %>%
         mutate(taxadeformalidaderenda1 = (formalsalario1/ocupsalario1)*100,
                taxadeformalidaderenda2 = (formalsalario2/ocupsalario2)*100,
                taxadeformalidaderenda3 = (formalsalario3/ocupsalario3)*100,
                taxadeformalidaderenda4 = (formalsalario4/ocupsalario4)*100,
                taxadeformalidaderenda5 = (formalsalario5/ocupsalario5)*100,
                taxadeformalidaderenda6 = (formalsalario6/ocupsalario6)*100,
                taxadeformalidaderenda7 = (formalsalario7/ocupsalario7)*100,
                taxadeformalidaderenda8 = (formalsalario8/ocupsalario8)*100)

ggplot(item1, aes(x = tempo, y = taxadeformalidaderenda1)) +
    geom_line(aes(col = "até 0,5 SM")) +
    geom_line(aes(y = taxadeformalidaderenda2, col = "0,5 - 1 SM")) +
    geom_line(aes(y = taxadeformalidaderenda3, col = "1 - 2 SM")) +
    geom_line(aes(y = taxadeformalidaderenda4, col = "2 - 3 SM")) +
    geom_line(aes(y = taxadeformalidaderenda5, col = "3 - 5 SM")) +
    geom_line(aes(y = taxadeformalidaderenda6, col = "5 - 10 SM")) +
    geom_line(aes(y = taxadeformalidaderenda7, col = "10 - 20 SM")) +
    geom_line(aes(y = taxadeformalidaderenda8, col = "Mais de 20 SM")) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Em %",
         title = "Evolução da Formalidade por Renda") +
    theme(legend.position = 'bottom')



################################
##     ocupados em barra      ##
################################


transf_eixo <- number_format(scale = 1e-6, accuracy = .1, decimal.mark = ",")


ggplot(item1) +
    aes(x = tempo, weight = ocupuf) +
    geom_bar(fill = "#112446") +
    scale_y_continuous(labels = transf_eixo) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Ocupados (em milhões)")

################################
##    informais em barra      ##
################################

ggplot(item1) +
    aes(x = tempo, weight = informaluf) +
    geom_bar(fill = "#112446") +
    scale_y_continuous(labels = transf_eixo) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Informais (em milhões)")


#################################
##    desocupados em barra     ##
#################################

ggplot(item1) +
    aes(x = tempo, weight = desocupuf) +
    geom_bar(fill = "#112446") +
    scale_y_continuous(labels = transf_eixo) +
    geom_vline(xintercept = item1$tempo[33], linetype = 8) +
    geom_vline(xintercept = item1$tempo[37], linetype = 8) +
    theme_bw() +
    labs(x = "Ano",
         y = "Desocupados (em milhões)")


####################################################
##   histograma informalidade por faixa etária    ##
##               faz sentido?                     ##
####################################################

item1 <- x %>%
         mutate(taxadeinformalidadeFE1 = (informalfaixaetaria1/ocupfaixaetaria1)*100,
                taxadeinformalidadeFE2 = (informalfaixaetaria2/ocupfaixaetaria2)*100,
                taxadeinformalidadeFE3 = (informalfaixaetaria3/ocupfaixaetaria3)*100,
                taxadeinformalidadeFE4 = (informalfaixaetaria4/ocupfaixaetaria4)*100,
                taxadeinformalidadeFE5 = (informalfaixaetaria5/ocupfaixaetaria5)*100,
                taxadeinformalidadeFE6 = (informalfaixaetaria6/ocupfaixaetaria6)*100)

ggplot(data = item1, aes(x = taxadeinformalidadeFE1)) +
  geom_histogram() +
  ggtitle('Faixa Etária 1')+
  facet_wrap(~tempo)



