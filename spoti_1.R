
source("read_data.R")

artistasmas<-datos_compl %>%  group_by(artistName) %>% 
  summarise(total_msPlayed = sum(msPlayed, na.rm = T)) %>%
  arrange(desc(total_msPlayed))

artistasmas<- artistasmas %>% mutate(minutos= total_msPlayed/60000)

cancionmas<- datos_compl %>% group_by(trackName) %>% 
  summarise(totalms= sum(msPlayed, na.rm = T)) %>% 
  arrange(desc(totalms)) %>% 
  mutate(minutos= totalms/60000)

datos_compl_mes<- datos_compl %>% mutate(endTime = ymd_hm(endTime),
                                         mes = floor_date(endTime, "month")) %>% 
  mutate(mes= as.Date(mes))

cancion_mes<- datos_compl_mes %>% group_by(trackName, mes) %>% 
  summarise(totalms= sum(msPlayed, na.rm = T)) %>% 
  mutate(minutos= totalms/60000) %>% 
  arrange(mes,desc(minutos)) %>% 
  group_by(mes) %>%  
  slice_max(order_by = minutos, n = 5, with_ties = FALSE) %>%
  ungroup() 

artista_mes<- datos_compl_mes %>% group_by(artistName, mes) %>% 
  summarise(totalms= sum(msPlayed, na.rm = T)) %>% 
  mutate(minutos= totalms/60000) %>% 
  arrange(mes,desc(minutos)) %>% 
  group_by(mes) %>% 
  slice_max(order_by = minutos, n = 5, with_ties = FALSE) %>% 
  ungroup() 

custom_colors <- c(
  "Natos y Waor" = "#1f77b4",  
  "Coldplay" = "#ff7f0e",  
  "Fyahbwoy" = "#2ca02c",  
  "Quevedo" = "#d62728", 
  "Linkin Park" = "#9467bd",  
  "Eminem" = "#8c564b",  
  "Nina Simone" = "#7C112A",  # 
  "The Ghost Inside" = "#7f7f7f",  
  "System Of A Down" = "#bcbd22",  
  "Berri Txarrak" = "#17becf", 
  "Kaze" = "#ff9896",
  "Hard GZ" = "#c5b0d5" 
)


#lineas
artista_mes %>%
  ggplot(aes(x = mes, y = minutos, color = artistName, group = artistName)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Evolución de los Artistas más escuchados",
    x = "Mes",
    y = "Minutos reproducidos",
    color = "Artistas") +
  scale_y_log10() +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + 
  theme_minimal() +  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom") 

# individual
artista_mes %>%
  ggplot(aes(x = mes, y = minutos)) +
  geom_line(aes(color = artistName, group = artistName), size = 1) +
  geom_point(aes(color = artistName), size = 2) +
  scale_color_viridis_d(option = "") +
  facet_wrap(~artistName, scales = "free_y") +
  labs(
    title = "Evolución de minutos reproducidos por artista (facetado)",
    x = "Mes",
    y = "Minutos reproducidos",
    color = "Artista"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


#### canciones

custom_colors2 <- c(
  "A 300" = "#1f77b4",   
  "Aftermath" = "#ff7f0e",   
  "Breaking the Habit" = "#2ca02c",   
  "COMPOSTELA 12" = "#d62728",   
  "Condenados" = "#9467bd",   
  "Condenados al Desastre" = "#8c564b",   
  "CUANDO DESPERTÓ" = "#e377c2",   
  "Cut the Bridge" = "#7f7f7f",   
  "Cypher #4 (Reggae Cypher)" = "#bcbd22",  
  "Cypher #5 (Reggae Cypher)" = "#17becf",  
  "Denak Ez Du Balio" = "#aec7e8",   
  "El camino" = "#ff9896",   
  "EL CIELO" = "#c5b0d5",   
  "El Don" = "#8dd3c7",  
  "El Sol" = "#ffffb3",   
  "Estás Con Él" = "#bebada",  
  "Fall" = "#fb8072",   
  "Habits" = "#80b1d3",   
  "Heavy Is the Crown" = "#fdb462",  
  "Houdini" = "#b3de69",   
  "Hoy volví a beber" = "#fccde5",   
  "In the End" = "#d9d9d9",   
  "Kaixo!" = "#bc80bd",   
  "Kikiki (feat. Yay)" = "#ccebc5",  
  "Maravillas" = "#ffed6f",   
  "Mi Fe" = "#e41a1c",   
  "Mona Lisa (feat. Kendrick Lamar)" = "#377eb8",   
  "New Divide" = "#4daf4a",   
  "No More Sorrow" = "#984ea3",
  "Numb / Encore" = "#ff7f00",  
  "Oreka" = "#ffff33",   
  "Over Each Other" = "#a65628",
  "Overflow" = "#f781bf",   
  "POWERLESS" = "#999999",  
  "Rara Vez" = "#66c2a5",   
  "See You Again (feat. Charlie Puth)" = "#fc8d62",
  "Shadow of the Day" = "#8da0cb",   
  "Sing For The Moment" = "#e78ac3",
  "Soldier" = "#a6d854",   
  "Solo pienso en ti" = "#ffd92f",  
  "Sunny" = "#e5c494",   
  "The Catalyst" = "#b3b3b3",
  "The Empress" = "#66a61e", 
  "Tu Silueta" = "#e6ab02",  
  "Veneno" = "#a6761d",  
  "VISTA AL" = "#666666",  
  "YO LO SÉ" = "#1b9e77")   


# lienas
cancion_mes %>%
  ggplot(aes(x = mes, y = minutos, color = trackName, group = trackName)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors2) +
  labs(
    title = "Evolución de las Canciones más escuchadas",
    x = "Mes",
    y = "Minutos reproducidos",
    color = "Canciones"
  ) +
  scale_y_log10() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

cancion_mes %>%
  ggplot(aes(x = mes, y = minutos)) +
  geom_line(aes(color = trackName, group = trackName), size = 1) +
  geom_point(aes(color = trackName), size = 2) +
  scale_color_manual(values = custom_colors2) +
  facet_wrap(~trackName, scales = "free_y") +
  labs(
    title = "Canciones más escuchadas",
    x = "Mes",
    y = "Minutos reproducidos",
    color = "Canciones"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



### para meter en Datawrapper




