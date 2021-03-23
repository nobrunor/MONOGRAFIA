setwd("C:/Users/bruno/Desktop/MONOGRAFIA/build/input")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/PNAD_Continua/r-package")
library(datazoom.pnadcontinua)



# Chamar uma base de dados #

dates <- list(c(1, 2012), c(2, 2012))

microdata <- load_pnadc(panel = 'no', lang = 'portuguese',
                        sources = dates,
                        download_directory = './Desktop')

# Limpar a base de dados (deixar só as variáveis de interesse) #
data <- microdata%>%mutate

# Salvar a base de dados limpa #
write.csv(data, "C:/Users/bruno/Desktop/MONOGRAFIA/build/output/data1.csv")

