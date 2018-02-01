#' obtener agregados mensuales con tolerancias por meses de epocas 
#' estacionales: 
#' Se obtiene df agr_tol usando funcion dftol
#' 
#' Si es necesario reemplazar datos con captor mensual de enrique: 
#' se obtiene agr_wide_tol siguiendo el for loop 
#' 
library(openxlsx)
library(dplyr)
library(lubridate)

# aqui empieza la magia
seco   = c(5:8)                             ; tolseco  = 5
humedo = c(1:3,11:12)                       ; tolhum   = 1
trans  = setdiff(1:12, union(seco, humedo)) ; toltrans = 0


todoenuno = read.csv('C:/Users/HP/Desktop/EST/desde 1981/todo en uno/todoenuno.csv', 
                     stringsAsFactors = F) %>%
        mutate(fecha = as.Date(fecha)) %>% 
        filter(fecha>=as.Date('1998-01-01')) %>%
        mutate(mes=month(fecha), ano=year(fecha)) %>%
        select(241:242,2:240) 
        
agreg_seco = todoenuno %>% 
        filter(mes %in% seco) %>% #View() # para escoger meses secos y aplicar tolerancia de __ dias 
        group_by(mes,ano) %>% 
        summarise_all(function(x) if(sum(is.na(x)) <= tolseco) sum(x,na.rm=TRUE) else NA) %>% 
        arrange(ano) 

agreg_hum = todoenuno %>% 
        filter(mes %in% humedo) %>% #View() # para escoger meses humedos y aplicar tolerancia de __ dias 
        group_by(mes,ano) %>% 
        summarise_all(function(x) if(sum(is.na(x)) <= tolhum ) sum(x,na.rm=TRUE) else NA) %>% 
        arrange(ano) 

agr_tr = todoenuno %>% 
        filter(mes %in% trans) %>% #View() # para escoger meses humedos y aplicar tolerancia de __ dias 
        group_by(mes,ano) %>% 
        summarise_all(function(x) if(sum(is.na(x)) <= toltrans ) sum(x,na.rm=TRUE) else NA) %>% 
        arrange(ano)

rbind(agreg_hum, agreg_seco, agr_tr) %>% 
        arrange(mes) %>% 
        arrange(ano) %>% View()

# la magia fue resumida en funcion dftol

#aqui termina la magia

###########################################################

# aqui vuelve la magia

#' dftol : funcion para agregar todoenuno.csv con tolerancias por epocas
source('H:/@Documentos/Tesis/ProyR_T_CH/Rainmerging_edu/scripts/func_dftolerancia.R')

agr_tol <- dftol('C:/Users/HP/Desktop/EST/desde 1981/todo en uno/todoenuno.csv', 
                sec =  5:8, 
                hum =  c(1:3,11:12), 
                tolsec =  5, tolhum =  1, toltr = 0) 

# write.csv(agr_tol, "H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/agr_tol.csv", row.names = F)
#' al existir la necesidad de comparar valores con el captor de enrique y luego 
#' reemplazar los valores diferntes no se escribe agr_tol pero si se escribe agr_wide_tol


#' aqui termina la magia si no existe captor enrique

###########################################################

#' si es necesario reemplazar datos con captor de enrique continuar desde aqui

#' nombres de estaciones de enrique extraidos de tooenuno.csv
comparar <- read.csv('C:/Users/HP/Desktop/EST/desde 1981/todo en uno/todoenuno.csv', 
                    stringsAsFactors = F) %>%
        mutate(fecha = as.Date(fecha)) %>% 
        filter(fecha>=as.Date('1998-01-01')) %>% 
        dplyr::select(c(1, 20, 87, 26, 27, 33, 41, 65, # selecciono estaciones de enrique
                        68, 86, 88, 93, 95, 111, 127, 
                        133, 136, 143, 150, 155, 156, 
                        160, 167, 169, 178, 183, 186, 
                        207, 226, 229, 231, 232, 233)) %>%
        mutate(mes=month(fecha), ano=year(fecha)) %>%
        select(34,35,2:33) %>% 
        names()

#' captor cc de enrique
library(openxlsx)
captorcc = read.xlsx("H:/@Documentos/Tesis/Hydracces_captor corregido mensual enrique.xlsx",
                     2, 9, detectDates = T) %>% 
        mutate(mes=month(Fecha), ano=year(Fecha)) %>% 
        select(34,35,2:33) %>% 
        filter(ano<=2016) %>% 
        data.table::setnames(comparar) 

#' seleccion de estaciones de enrique de df agregado a partir de datos todoenuno.csv
agr_tol_cc = agr_tol %>% 
        dplyr::select(c(1:2, 21, 88, 27, 28, 34, 42, 66, 
                        69, 87, 89, 94, 96, 112, 128, 
                        134, 137, 144, 151, 156, 157, 
                        161, 168, 170, 179, 184, 187, 
                        208, 227, 230, 232, 233, 234)) 
#' lista con df que comparan estaciones:
#' captorcc : valores de captorcc
#' agr_tol : valores de agregago con tolerancias
#' dif : diferencia entre captor cc y agregado con tolerancias
#' los df solo tienen observaciones (filas) con una diferencia
#' mayor a +-0.5
dif_list = list()
for (i in 3:34) {
        data.frame(mes = agr_tol_cc$mes, ano = agr_tol_cc$ano, captorcc = captorcc[i], agr_tol=agr_tol_cc[i]) %>%
                data.table::setnames(c(names(captorcc)[i], 'ano', 'captorcc', 'agr_tol')) %>% 
                mutate(dif = round(captorcc - agr_tol,3)) %>% 
                filter(abs(dif) >= 0.5) -> tmp
        dif_list[[i - 2]] = tmp
}        

#' cambio de formato de agr_tol (agregados con toleancia de todoenuno)
#' de wide a long
#' (se crea columna con nombre de estacion apra cada dato)
agr_melt = reshape2::melt(as.data.frame(agr_tol), id.vars = c('mes', 'ano'))

#' reemplazo de valores de agregado con tolerancia con valores
#' de captor cc de enrique
for (j in 1:length(dif_list)) {
        tmp <- dif_list[[j]] 
        if(nrow(tmp)!= 0) { 
                for (i in 1:nrow(tmp)) {
                        m = tmp[i,1] ; 
                        y = tmp[i,2] ; 
                        estacion = names(tmp)[1] ; 
                        val = tmp[i,3]
                        
                        agr_melt[,4][agr_melt$mes == m & agr_melt$ano == y & agr_melt$variable == estacion] <- val
                }
                
        }
        
}

#' retorno a formato wide despues de reemplazo de valores
agr_wide_tol = reshape(agr_melt, 
                       idvar = c('mes', 'ano'), 
                       timevar = 'variable', 
                       direction = 'wide' ) %>% 
        data.table::setnames(names(agr_tol))

write.csv(agr_wide_tol, "H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/agr_wide_tol.csv", row.names = F)

###########################################################

# a partir de aqui solo es para verificar que no hay diferencias
agr_tol_cc2 = agr_wide_tol %>% 
        dplyr::select(c(1:2, 21, 88, 27, 28, 34, 42, 66, 
                        69, 87, 89, 94, 96, 112, 128, 
                        134, 137, 144, 151, 156, 157, 
                        161, 168, 170, 179, 184, 187, 
                        208, 227, 230, 232, 233, 234)) 
dif_list2 = list()
for (i in 3:34) {
        data.frame(mes=agr_tol_cc2$mes, ano=agr_tol_cc2$ano, captorcc=captorcc[i], agr_tol=agr_tol_cc2[i]) %>%
                data.table::setnames(c(names(captorcc)[i], 'ano', 'captorcc', 'agr_tol')) %>% 
                mutate(dif=round(captorcc-agr_tol,3)) %>% 
                filter(abs(dif)>=0.5) -> tmp
        dif_list2[[i-2]] = tmp
}        
str(dif_list2)
#



# nos : estaciones sin observaciones (sin nada que reemplazar) dentro 
# de dif_list:
#nos = c(5, 6, 12, 15, 17, 18, 19, 22, 24, 25, 28, 29, 30, 31, 32)
# reempl : estaciones a las que se hara el reemplazo dentro de 
# agr_tol:
#reempl = c(21,88,27,28,66,69,87,89,94,112,128,137,157,161,170,187,208)