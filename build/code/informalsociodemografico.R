informal <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD4009, VD4012) %>%
  filter(VD4002 == 1) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaluf = mean(aux))

informalfaixaetaria1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 18 & V2009 <= 24) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria1 = mean(aux))

informalfaixaetaria2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 25 & V2009 <= 29) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria2 = mean(aux))

informalfaixaetaria3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 30 & V2009 <= 39) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria3 = mean(aux))


informalfaixaetaria4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 40 & V2009 <= 49) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria4 = mean(aux))


informalfaixaetaria5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 50 & V2009 <= 59) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria5 = mean(aux))


informalfaixaetaria6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2009, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2009 >= 60) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalfaixaetaria6 = mean(aux))

informaleduc1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 1) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc1 = mean(aux))

informaleduc2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 2) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc2 = mean(aux))

informaleduc3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 3) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc3 = mean(aux))

informaleduc4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 4) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc4 = mean(aux))

informaleduc5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 5) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%  
  summarise(informaleduc5 = mean(aux))

informaleduc6 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 6) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc6 = mean(aux))

informaleduc7 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, VD3004, VD4009, VD4012) %>%
  filter(VD4002 == 1 & VD3004 = 7) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informaleduc7 = mean(aux))

informalraca1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 1) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca1 = mean(aux))

informalraca2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 2) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca2 = mean(aux))

informalraca3 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 3) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca3 = mean(aux))

informalraca4 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 4) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca4 = mean(aux))

informalraca5 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2010, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 5) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalraca5 = mean(aux))

informalgenero1 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 1) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero1 = mean(aux))

informalgenero2 <- data %>%
  select(UF, V1028, Trimestre, Ano, VD4002, V2007, VD4009, VD4012) %>%
  filter(VD4002 == 1 & V2010 = 2) %>%
  filter(VD4009 == 2 | VD4009 == 4 | VD4009 == 6 | VD4009 == 10 |
           (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF, Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise(informalgenero2 = mean(aux))

x <- merge(informal, informalfaixaetaria1, by = c("UF", "Trimestre"))
x <- merge(x, informalfaixaetaria2, by = c("UF", "Trimestre"))
x <- merge(x, informalfaixaetaria3, by = c("UF", "Trimestre"))
x <- merge(x, informalfaixaetaria4, by = c("UF", "Trimestre"))
x <- merge(x, informalfaixaetaria5, by = c("UF", "Trimestre"))
x <- merge(x, informalfaixaetaria6, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc1, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc2, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc3, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc4, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc5, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc6, by = c("UF", "Trimestre"))
x <- merge(x, informaleduc7, by = c("UF", "Trimestre"))
x <- merge(x, informalraca1, by = c("UF", "Trimestre"))
x <- merge(x, informalraca2, by = c("UF", "Trimestre"))
x <- merge(x, informalraca3, by = c("UF", "Trimestre"))
x <- merge(x, informalraca4, by = c("UF", "Trimestre"))
x <- merge(x, informalraca5, by = c("UF", "Trimestre"))
x <- merge(x, informalgenero1, by = c("UF", "Trimestre"))
x <- merge(x, informalgenero2, by = c("UF", "Trimestre"))
x <- x %>% mutate(year = 2012)
