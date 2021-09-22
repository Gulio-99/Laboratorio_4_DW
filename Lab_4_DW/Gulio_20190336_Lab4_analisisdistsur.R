# Laboratorio 4 DW 
# Gulio Valenzuela

# Librerias
library(dplyr)
library(tidyverse)
library(highcharter)
library(ggplot2)
library(qcc)
library(plotly)


# Data Frames
df <- read_delim("tabla_completa.csv", delim = ";", 
                 escape_double = FALSE, col_types = cols(Q = col_number()), 
                 trim_ws = TRUE,locale = locale(encoding = "Latin1"))

glimpse(df)
df <- mutate_if(df,is.character,as.factor)



df <- mutate(df,Mes_entra = (CREDITO/30)+MES)

# Analisis Distribuidora del sur

# FEntrada x mes
Q_month <- df %>% 
  group_by(Mes_entra) %>% 
  summarise(Q = sum(Q)) %>% 
  filter(Mes_entra<=12)

gg_flujo_de_caja <- ggplot(Q_month,aes(x = Mes_entra,y = Q))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Q), vjust=1.5, color="black", size=3)+
  ggtitle("Flujo de Caja")+
  labs(x = "Meses", y = "Flujo de Caja")+
  theme_minimal()
gg_flujo_de_caja2 <- ggplotly(gg_flujo_de_caja)
gg_flujo_de_caja2

mean(Q_month$Q)

# analisis de personas y pedidos


Personal <- df %>% 
  summarise(Nombre = unique(PILOTO))

# Envios realizados x piloto y envios realizados 
No_cliente_por_piloto <- df %>% 
  select(PILOTO,CLIENTE,Q) %>% 
  group_by(PILOTO, CLIENTE) %>% 
  summarise(Q = sum(Q),
            Entregas = n())

# Enrtrgas y envios x piloto

Ingreso_por_piloto <- df %>% 
  group_by(PILOTO) %>% 
  summarise(Q = sum(Q),
            Entregas = n())

Ingreso_por_piloto <- Ingreso_por_piloto[order(Ingreso_por_piloto$Q,
                                               Ingreso_por_piloto$Entregas,
                                               decreasing = TRUE),]

mean(Ingreso_por_piloto$Q) 
# Promedio de ingreso x piloto:
# Respuesta: 66,538.69
# Los pilotos destacados son:
# Respuesta: Fernando, Ismael, Pedro y Hector A.

# Ingreso por piloto
gg_ingreso_por_piloto <- ggplot(Ingreso_por_piloto,aes(x = reorder(PILOTO, Q), y = Q))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Q), vjust=0.5, color="black", size=4)+
  ggtitle("Ingreso por piloto")+
  labs(x = "Piloto", y = "Ingreso")+
  coord_flip()+
  theme_minimal()
gg_ingreso_por_piloto2 <- ggplotly(gg_ingreso_por_piloto)
gg_ingreso_por_piloto2


# Entregas por piloto
gg_entregas_por_piloto <- ggplot(Ingreso_por_piloto,
                                 aes(x = reorder(PILOTO, Entregas),
                                     y = Entregas))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Entregas), vjust=0.5, color="black", size=4)+
  ggtitle("Entregas por piloto")+
  labs(x = "Piloto", y = "Entrega")+
  coord_flip()+
  theme_minimal()
gg_entregas_por_piloto2 <- ggplotly(gg_entregas_por_piloto)
gg_entregas_por_piloto2

# Analisis de los clientes y envios

# Los tres tipos de envio son, Desácho, Faltante y Devolución
# analisando esto se puede asumir que estos son con pedidos predios ya realizados por lo que el unico con pedidos devueltos en Gallo Negro por lo que resalta que debe tener problemas con este.
Clientes <- df %>% 
  summarise(Cliente = unique(CLIENTE))


tabla_clientes <- table(df$CLIENTE)
Pareto_clientes <- pareto.chart(tabla_clientes,
                                main = "Pareto de Clientes")
Pareto_clientes
# Podemos ver que el 80% de los ingresos viene de un porsentaje minimo de los clientes



# Analsisi de precios 

# Podemos ver que no hay robo en la empresa
Precio <- df$CANTIDAD/df$Q
unique(Precio)


# Analsiis tipos de entrega

Tipo_de_entrega_por_cliente <- df %>% 
  group_by(CLIENTE, `TIPO DE ENTREGA`) %>% 
  summarise(Cantidad = n())

Tipo_total <- Tipo_de_entrega_por_cliente %>% 
  group_by(`TIPO DE ENTREGA`) %>% 
  summarise(Cantidad = sum(Cantidad))

# Los tipos de entrega x cliente
gg_tipo_de_entrega_por_cliente <- ggplot(Tipo_de_entrega_por_cliente,
                                         aes(x = reorder(CLIENTE, Cantidad),y = Cantidad, fill = `TIPO DE ENTREGA`))+
  geom_bar(stat='identity', width = 0.5)+
  ggtitle("Tipo de entrega por cliente")+
  labs(x = "Cliente", y = "Cantidad")+
  coord_flip()+
  theme_minimal()
gg_tipo_de_entrega_por_cliente2 <- ggplotly(gg_tipo_de_entrega_por_cliente)
gg_tipo_de_entrega_por_cliente2

Despacho_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "Despacho a cliente")
# El cliente con más Despachos es Taqueria el chinito

Faltante_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "Faltante")
# El cliente con más Faltantes es El pinche obelisco y tambien tiene más 
# faltantes que depachos normales

Devolucion_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "DEVOLUCION")
# EL unico cliente con devoluciones es Gallo Negro

# Entregas por Mes
entregas_por_mes <- df %>% 
  group_by(MES) %>% 
  summarise(Entregas = n())

gg_entregas_por_mes <- ggplot(entregas_por_mes,
                              aes(x = MES,y = Entregas))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  ggtitle("Entregas por mes")+
  labs(x = "Mes", y = "Entregas")+
  theme_minimal()
gg_entregas_por_mes2 <- plotly::ggplotly(gg_entregas_por_mes)
gg_entregas_por_mes2


# Vehiculos -------------------------------------------------------------

Vehiculos <- df %>% 
  group_by(UNIDAD) %>% 
  summarise(Entregas = n(),
            Cantidad = sum(CANTIDAD),
            Q = sum(Q))

# Entregas por unidad
gg_entrega_por_unidad <- ggplot(Vehiculos,
                                aes(x = UNIDAD,y = Entregas, fill = UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Entregas), vjust=1.5, color="black", size=4)+
  ggtitle("Entregas por Unidad")+
  labs(x = "Unidad", y = "Entrega")+
  theme_minimal()
gg_entrega_por_unidad2 <- plotly::ggplotly(gg_entrega_por_unidad)
gg_entrega_por_unidad2

# Cantidad por unidad
gg_cantidad_por_unidad <- ggplot(Vehiculos,
                                 aes(x = UNIDAD,y = Cantidad, fill = UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Cantidad), vjust=1.5, color="black", size=4)+
  ggtitle("Cantidades por Unidad")+
  labs(x = "Unidad", y = "Cantidad")+
  theme_minimal()
gg_cantidad_por_unidad2 <- plotly::ggplotly(gg_cantidad_por_unidad)
gg_cantidad_por_unidad2

# Q por unidad
ggp_q_por_unidad <- ggplot(Vehiculos,aes(x = UNIDAD,y = Q, fill=UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Q), vjust=1.5, color="black", size=4)+
  ggtitle("Ingreso por Unidad")+
  labs(x = "Unidad", y = "Ingreso")+
  theme_minimal()
ggp_q_por_unidad2 <- plotly::ggplotly(ggp_q_por_unidad)
ggp_q_por_unidad2


# Creditos ----------------------------------------------------------------

creditos <- df %>% 
  group_by(CREDITO) %>% 
  summarise(Cantidad = n())

gg_creditos <- ggplot(creditos,aes(x = CREDITO,y = Cantidad, fill = CREDITO))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Cantidad), vjust=1.5, color="black", size=4)+
  ggtitle("Créditos")+
  labs(x = "Crédito", y = "Cantidad")+
  theme_minimal()
gg_creditos2 <- plotly::ggplotly(gg_creditos)
gg_creditos2