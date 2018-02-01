#' @author Eduardo Noriega
#' @description crear df con valores agregados mensuales
#' aplcando tolerancias de dias faltantes para 
#' epoca seca y humeda.
#' @param file es un string con la direccion del archivo todoenuno.csv
#' Ej: 'C:/Users/HP/Desktop/EST/desde 1981/todo en uno/todoenuno.csv'
#' El archivo todoenuno.csv tiene que tener en el siguiente formato:
#' 
#' | fecha      |  A  |  B  | ... | ... |
#' | 1998-01-01 | ... | ... | ... | ... |
#' | 1998-01-02 | ... | ... | ... | ... |
#' |     ...    | ... | ... | ... | ... |
#' |     ...    | ... | ... | ... | ... |
#' | 2016-12-31 | ... | ... | ... | ... |
#' 
#' Donde A, B, etc son las estaciones. 
#' La columna fecha estara en formato Date. Las fechas del ejemplo
#' son nominales, puede tener cualquier inicio y fin pero el paso 
#' temporal es diario.

#' @param sec es un vector con los meses secos. Ej: 5:8
#' @param hum es un vector con los meses humedos. Ej. c(1:3,11:12)
#' @param tolsec es un entero que indica el numero de dias faltantes 
#' que se acepta para epoca seca.
#' @param tolhum es un entero que indica el numero de dias faltantes 
#' que se acepta para epoca humeda.
#' 
#' @return data frame con valores agregados mensuales con las tolerancias
#' estcionales aplicadas.

dftol <- function(file, sec, hum, tolsec, tolhum, toltr){
        require(dplyr)
        require(lubridate)
        seco   = sec ; tolseco = tolsec
        humedo = hum ; tolhum  = tolhum
        trans  = setdiff(1:12, union(seco, humedo)) ; toltrans = toltr
        
        todoenuno <- read.csv(file, 
                             stringsAsFactors = F) %>%
                mutate(fecha = as.Date(fecha)) %>% 
                filter(fecha >= as.Date('1998-01-01')) %>%
                mutate(mes = month(fecha), ano = year(fecha)) %>%
                dplyr::select(241:242,2:240) # puede variar de acuerdo a numero de estaciones?
        
        agreg_seco <- todoenuno %>% 
                filter(mes %in% seco) %>% #View() # para escoger meses secos y aplicar tolerancia de __ dias 
                group_by(mes,ano) %>% 
                summarise_all(function(x) if (sum(is.na(x)) <= tolseco) sum(x, na.rm = TRUE) else NA) %>% 
                arrange(ano) 
        
        agreg_hum <- todoenuno %>% 
                filter(mes %in% humedo) %>% #View() # para escoger meses humedos y aplicar tolerancia de __ dias 
                group_by(mes,ano) %>% 
                summarise_all(function(x) if (sum(is.na(x)) <= tolhum ) sum(x, na.rm = TRUE) else NA) %>% 
                arrange(ano) 
        
        agr_tr <- todoenuno %>% 
                filter(mes %in% trans) %>% #View() # para escoger meses humedos y aplicar tolerancia de __ dias 
                group_by(mes,ano) %>% 
                summarise_all(function(x) if (sum(is.na(x)) <= toltrans ) sum(x, na.rm = TRUE) else NA) %>% 
                arrange(ano)
        
        rbind(agreg_hum, agreg_seco, agr_tr) %>% 
                arrange(mes) %>% 
                arrange(ano) %>% 
                return()
}