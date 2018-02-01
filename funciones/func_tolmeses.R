#' @author Eduardo Noriega
#' @description obtener estaciones y sus datos de estaciones aceptadas 
#' de acuerdo a una tolerancia de numero de datos minimo por cada mes
#' @param df es un data frame con los datos agregados a nivel mensual segun el 
#' sigueinte formato:
#' 
#' | mes |  ano |  A  |  B  | ... |
#' |   1 | 1998 | ... | ... | ... |
#' |   2 | 1998 | ... | ... | ... |
#' | ... | .... | ... | ... | ... |
#' | ... | .... | ... | ... | ... |
#' |  11 | 2016 | ... | ... | ... |
#' |  12 | 2016 | ... | ... | ... |
#' 
#' Donde A, B, etc son las estaciones.
#' Las fechas del ejemplo son nominales, puede tener cualquier inicio y fin 
#' pero el paso temporal es diario.
#' Ej: agr_wide_tol (si hay captor enrique) o agr_tol
#' "H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/agr_wide_tol.csv"
#' 
#' @param tol es un numero que representa la tolerancia que se aplicara
#' al numero de datos minimo que tendra cada mes.
#' @param excl es un vector de caracteres que incluye los nombres de las estaciones a las 
#' que no se desea aplicar la tolerancia del parametro tol.
#' Ej: c('Chuspipata', 'Huaylipaya')
#' @param mesesXestacion es un valor logico que si se establece en TRUE se ignora el  
#' valor de la tolerancia (tol) y el vector de estaciones excluidas (excl) y la funcion 
#' retornara un df con el numero de datos de cada mes y cada estacion de todas las 
#' estaciones.
#' @param coordest valor logico que indica si se escriben las coordenadas de las
#' estaciones aceptadas en el archivo coord_est_acep_mes.txt.
#' El archivo que contiene las coordenadas de todas las estaciones es:
#' "C:/Users/HP/Desktop/EST/coord_todas_est.xlsx"
#' 
#' @return si se asigna TRUE a mesesXestacion (objetivo inicial de la 
#' funcion) se retornara el df con las estaciones y los meses que cumplen la 
#' tolerancia establecida de numero de datos por mes

tolmeses = function(df, tol, excl, mesesXestacion = F, coordest = F){
        require(dplyr)
        require(openxlsx)
        library(magrittr)
        
        if (mesesXestacion == F) {
                tmp <- df %>% 
                        group_by(mes) %>% 
                        summarise_all(function(x) sum(!is.na(x))) %>% # numero de datos por cada mes dentro de la serie (max 19) 
                        dplyr::select(-1) %>% # elimina columna mes
                        replace(. < tol,NA) #si mes tiene menos de 10 datos dentro de la serie temporal se reemplaza el dato existente por NA
                
                if (!missing(excl)) {
                        tmp_excl <- df %>%
                                group_by(mes) %>% 
                                summarise_all(function(x) sum(!is.na(x))) %>% #numero de datos por cada mes dentro de la serie (max 19) 
                                dplyr::select(-1)  %>% # elimina columna mes
                                dplyr::select(excl) # selecciona las estaciones excluidas de la tolerancia de numero de datos minimo por mes
                        tmp[,excl] <- tmp_excl
                }
                tmp %<>%
                        do(data.frame(est = colnames(.[-1]), sum = colSums(!is.na(.[-1])))) %>% # se crea df con estaciones y numero de meses con 10 o mas datos. Se elimina fila de ano.
                        filter(sum > 0) %>% # se filtra el anterior df con estaciones que tengan por lo menos 1 mes con 10 o mas datos
                        .$est %>% # se obtiene los nombres de las estaciones aceptadas
                        as.character() 
                
                if (coordest == T) {
                        todas_est_info <- read.xlsx("C:/Users/HP/Desktop/EST/coord_todas_est.xlsx")
                        
                        match(tmp, todas_est_info$est) %>% # obtener vector de posicion de cada estacion acepada dentro del archivo de todas las estaciones
                                todas_est_info[.,] %>% # con el vector anterior se extrae las estaciones del archivo de todas las estaciones (las estaciones estan pro fila por eso [.,])
                                do(data.frame(est = .$est, long = .$long.W*-1, lat = .$lat.S*-1)) %>% # se crea un df con el nombre de las estaciones y sus coordenadas
                                write.table( "H:/@Documentos/Tesis/ProyR_T_CH/data/coord_est_acep_mes.txt", row.names = F) 
                }
                
                tmp <- df %>% 
                        dplyr::select(one_of(c('mes', 'ano', tmp))) # se filtra df con la informacion de todas las estaciones obteniendo datos de las estaciones aceptadas
                        
        } else {
                tmp <- df %>% 
                        group_by(mes) %>% 
                        summarise_all(function(x) sum(!is.na(x))) %>% # numero de datos por cada mes dentro de la serie (max 19) 
                        dplyr::select(-2)  # elimina columna ano

        }

        return(tmp)
}