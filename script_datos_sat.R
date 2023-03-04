##################################################################
###   Trabajando con millones de datos de las                 ###
###   Declaraciones Anuales Anonimizadas del SAT en R         ###
##################################################################

#Limpieza del ambiente
rm(list=ls())
#Paqueterías
if (!require(pacman)) install.packages("pacman") 
p_load("tidyverse", "readr", "dtplyr", 'data.table',"parallel",  "kableExtra")

#Tiempo de ejecución
start_time  <- Sys.time()

#Definimos función paste 
`%-%` <- paste0

#Función para descargar datos 
func_download_SAT <- function(p1, p2) {
for(i in p1:p2) {
  url <- "http://omawww.sat.gob.mx/cifras_sat/Documents/Anuales_ISR_PF_"%-%i%-%".zip"
  if(!file.exists("Datos/df_pf_r_"%-%i%-%".zip")){
     
     print(url)
     #Descarga
     download.file(url, destfile="Datos/df_pf_r_"%-%i%-%".zip", destmethod = "curl")
     #Descomprimir
     zip::unzip("Datos/df_pf_r_"%-%i%-%".zip", exdir = "Datos" )
     
    
    }
  }
} 

#Implementamos descarga
func_download_SAT(2012, 2015)
#(Opcional:Medimos rendimiento de la descarga)
#microbenchmark(func_download_SAT(2012, 2015))


lista_archivos <- gtools :: mixedsort(list.files(path = "Datos/DECLA/" ,
                                                 recursive = TRUE,
                                                 full.names = TRUE))
lista_archivos
#Carga de datos 
for (i in lista_archivos){
 
  df_PF   <-  read_delim(i,delim = "\t",
                         escape_double = FALSE, trim_ws = TRUE)  %>% 
    
                  dplyr::select( ejercicio = ejercicio,
                                 rfc = RFC_ANON,                     #rfc anonimizado
                                 isr_ctar = I_DEC_IRCTAME1_AA,       #ISR CONFORME A TARIFA ANUAL
                                 total_ingresos_acum = I_DEC_TIAONCT1_AA, #TOTAL DE INGRESOS ACUMULABLES
                                 isr_causado         = I_DEC_IRCEMEA1_AA ) %>%   #ISR CAUSADO

                  dtplyr::lazy_dt()
  
  assign(paste("df_PF",str_extract(i, "(\\d)+" ), sep ='_'), df_PF )
  
  rm(df_PF)
}

#Observaciones totales de las cuatro bases
n_total <- sum(nrow(df_PF_2012),nrow(df_PF_2013),nrow(df_PF_2014), nrow(df_PF_2015))
n_total
#Promedio de contribuyentes por año 
n_contribuyentes <- n_total/4
n_contribuyentes

### Unión de las bases ###
dt_2012_2015  <- df_PF_2012 %>% 
                 full_join(df_PF_2013) %>% 
                 full_join(df_PF_2014) %>% 
                 full_join(df_PF_2015) %>% 
                 as.data.table()

#Missings
missings <- dt_2012_2015  %>% 
            summarise_all(~ sum((is.na(.)/sum(n()))*100))  
missings

#Tabla Top 5 de ingresos acumulables por año 
#(dplyr)
df_top_5 <- dt_2012_2015 %>%  
             arrange(desc(ejercicio),desc(total_ingresos_acum))%>% 
             group_by(ejercicio) %>% 
             slice(1:5) %>% 
             as.data.frame() 

#(data.table)
#dt_top_5 <- dt_2012_2015[,.SD[order(total_ingresos_acum, decreasing  = TRUE),][1:5]
#                        ,by = "rfc"]

#### TABLA ###
#Formato de cifras
options(scipen = 1000000, digits = 4)


df_top_5 %>% 
mutate(tasa_efectiva = round(isr_causado/total_ingresos_acum*100,2),
       ejercicio = factor(ejercicio),
       rfc = as.character(rfc)) %>%
select(ejercicio, rfc, total_ingresos_acum, isr_causado, tasa_efectiva)%>% 
mutate_if(is.numeric, funs(prettyNum(., big.mark=","))) %>% 
kable(caption=text_spec("Top 5 de contribuyentes por ingresos acumulables (2012-2015)",
                        bold = TRUE,
                        color= "black",
                        font_size = 20),
      format.args = list(big.mark = ","),
                        
                        #format="html",
                        col.names = c("Ejercicio", 
                                       "RFC", 
                                       "Total de ingresos acumulables",
                                       "ISR causado",
                                       "Tasa efectiva"))%>%
  kable_styling(
                font_size = 16,
                html_font = "Montserrat Medium")%>%
  
  row_spec(row = 0, bold = F, color = "white", background = "#235B4E")%>%
  footnote(general = "",
           general_title = "Fuente:Datos provenientes del SAT | @diegosreco ") %>%  
  save_kable(file = "table_2.png",
             zoom = 2)



### Tasa efectiva ###
df_personas_físicas_n1 <- df_2012_2015 %>%  
                              filter((!is.na(total_ingresos_acum) & 
                                        total_ingresos_acum !=0 )) %>% 
                              mutate( rate_efectiva =  isr_causado/ total_ingresos_acum) %>% 
                              arrange(total)
                              filter( rate_efectiva <= 1) %>% 
                              mutate_each_(list(~scale(.) %>% as.vector),
                                           vars = c("total_ingresos_acum","rate_efectiva"))
#Plot
df_personas_físicas_n1 %>% 
  
           ggplot(aes(x =  total_ingresos_acum, y = rate_efectiva)) + 
           geom_point() +
           labs( title = "Relación entre ingresos totales acumulados en 2015 y tasa efectiva de ISR",
                 x= "Ingresos" , y = "Tasa efectiva de ISR")+
           theme_minimal() 

end_time  <- Sys.time()
end_time - start_time 
