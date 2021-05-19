rm(list=ls(all=T))

library(tidyverse)
library(showtext)

setwd("~/falsos_amigos")

# Cargamos los archivos
# Fuente: https://siceen.ine.mx:8080/static/downloads/PRESIDENCIA.zip 
voto_pre_18 <- read.csv("PRESIDENCIA/PRESIDENCIA_csv/2018_SEE_PRE_NAL_ENTCAND.csv")
gobernadores <- read.csv("gobers_2018.csv")

#Limpiamos datos y agregamos nuevas columnas

colnames(gobernadores)[1] <- "ID_ESTADO"
  
voto_pre_18 <- voto_pre_18 %>%
  filter(TERRITORIO == "VOTO EN TERRITORIO NACIONAL")%>%
  mutate(pc_AMLO = PT_MORENA_ES_CAND/TOTAL_VOTOS) %>%
  mutate(pc_MEADE = PRI_PVEM_NA_CAND/TOTAL_VOTOS)

datos_pre18 <- voto_pre_18 %>%
  select(ID_ESTADO, pc_AMLO, pc_MEADE) %>%
  arrange(desc(pc_AMLO)) %>%
  left_join(y= gobernadores, by ="ID_ESTADO")

datos_pre18$gob_PRI <- "Otro"
datos_pre18$gob_PRI[datos_pre18$PARTIDO_GOBERNADOR == "PRI"] <- "PRI"

datos_pre18$gob_PRI <- as_factor(datos_pre18$gob_PRI)
datos_pre18$gob_PRI <- fct_relevel(datos_pre18$gob_PRI, c("PRI","Otro"))

# Creamos dos grupos, separando estados entre los que son gobernados por el PRI
# y los demás
friends <- datos_pre18$pc_AMLO[datos_pre18$gob_PRI == "PRI"]
foes <- datos_pre18$pc_AMLO[datos_pre18$gob_PRI == "Otro"]

#Hacemos una prueba de comparación de medias
amlo <- t.test(friends, foes, alternative= "greater")

p_amlo <- round(amlo$p.value, 3)

#Vemos el tamaño del efecto

t_amlo <- amlo$statistic[[1]]
df_amlo <- amlo$parameter[[1]]
r_amlo <- sqrt( (t_amlo**2) / ( (t_amlo**2) + df_amlo))

#Hacemos lo mismo con la campaña de Meade

meade_friends <- datos_pre18$pc_MEADE[datos_pre18$gob_PRI == "PRI"]
meade_foes <-datos_pre18$pc_MEADE[datos_pre18$gob_PRI == "Otro"]

meade <- t.test(meade_friends, meade_foes, alternative= "greater")
p_meade <- round(meade$p.value, 3)

#Calculamos el tamaño del efecto para Meade

t_meade <- meade$statistic[[1]]
df_meade <- meade$parameter[[1]]
r_meade <- sqrt( (t_meade**2) / ( (t_meade**2) + df_meade))

#Visualizamos los datos
font_add_google("Lato", "lato")
showtext_auto()


# Gráfica 1: voto AMLO

ggplot(datos_pre18, aes(gob_PRI, pc_AMLO)) +
  geom_boxplot(color="#347B98") +
  stat_summary(fun.y="mean", color="#347B98", shape=15) + #Agregamos la media 
  geom_jitter(alpha = 0.1, fill = "#433498") +
  
  labs(title= "No hubo diferencia en el porcentaje de voto a AMLO entre los estados \ngobernados por el PRI y los demás",
       subtitle = "Porcentaje del voto a AMLO en los estados gobernados por el PRI y por otros partidos en 2018.\nCada punto representa un estado. El cuadrado azul representa la media del grupo.",
       caption = paste0("p =", p_amlo, "\nFuente: Elaboración propia con datos del INE"),
       x = "Partido del gobernador(a)",
       y = "Porcentaje de voto a AMLO") +
  
  theme_classic() +
  
  theme(text = element_text(family = "lato", color = "#110934"),
        plot.title = element_text(hjust = 0, face = "bold", color= "#347B98"),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0))

# Gráfica 2: voto Meade

ggplot(datos_pre18, aes(gob_PRI, pc_MEADE)) +
  geom_boxplot(color="#347B98") +
  stat_summary(fun.y="mean", color="#347B98", shape=15) + #Agregamos la media 
  geom_jitter(alpha = 0.1, fill = "#433498") +
  
  labs(title= "...pero sí hubo diferencias en el voto a Meade",
       subtitle = "Porcentaje del voto a Meade en los estados gobernados por el PRI y por otros partidos en 2018.\nCada punto representa un estado. El cuadrado azul representa la media del grupo.",
       caption = paste0("p =", p_meade, "\nFuente: Elaboración propia con datos del INE"),
       x = "Partido del gobernador(a)",
       y = "Porcentaje de voto a Meade") +
  
  theme_classic() +
  
  theme(text = element_text(family = "lato", color = "#110934"),
        plot.title = element_text(hjust = 0, face = "bold", color= "#347B98"),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0))

showtext_auto(FALSE)
