# en bases a nnnnn.R

agr_wide_tol <- read.csv("H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/agr_wide_tol.csv")

tt = agr_wide_tol
tt_ch = tt
#tt_t = tt_t_orig = filter(tt, ano>=1998)
#tt_t = tt_t_orig = tt


excl = c('Chuspipata', 'Huaylipaya')

tmp_excl = tt_ch %>% 
        group_by(mes) %>% 
        summarise_all(function(x) sum(!is.na(x))) %>% #numero de datos por cada mes dentro de la serie (max 19) 
        select(-1)  %>% # elimina columna mes
        select(excl) 

tolan = 10
tmp <- tt_ch %>% 
        group_by(mes) %>% 
        summarise_all(function(x) sum(!is.na(x))) %>% #numero de datos por cada mes dentro de la serie (max 19) 
        select(-1) %>% # elimina columna mes
        replace(. < tolan, NA) #si mes tiene menos de 10 datos dentro de la serie temporal se reemplaza el dato existente por NA
tmp[,excl] <- tmp_excl # se inlcuye los valores de estaciones excluidas de tolerancia minima
tmp %<>%        
        do(data.frame(est = colnames(.[-1]), sum = colSums(!is.na(.[-1])))) %>% # se crea df con estaciones y numero de meses con 10 o mas datos. Se elimina fila de ano.
        filter(sum > 0) %>% # se filtra el anterior df con estaciones que tengan por lo menos 1 mes con 10 o mas datos
        .$est %>%
        as.character()

todas_est_info <- read.xlsx("C:/Users/HP/Desktop/EST/coord_todas_est.xlsx")
match(tmp, todas_est_info$est) %>% 
        todas_est_info[.,] %>%
        do(data.frame(est = .$est, long = .$long.W*-1, lat = .$lat.S*-1)) %>%
        write.table( "H:/@Documentos/Tesis/ProyR_T_CH/data/coord_est_acep_mes.txt", row.names = F)

tmp = tt_ch %>% 
        select(one_of(c('mes','ano', tmp))) %>% # se filtra df con la informacion de todas las estaciones obteniendo datos de las estaciones aceptadas
        View()

#' todo lo anterior esta resumido en funcion tolmeses

#' a continuacion se escribe resultado de funcion tolmeses en 
#' archivo para uso posterior.
#' tambien se escribe las coordenadas de las estaciones 
source('H:/@Documentos/Tesis/ProyR_T_CH/Rainmerging_edu/scripts/func_tolmeses.R')
tolmeses(agr_wide_tol, 10, c('Chuspipata', 'Huaylipaya'), coordest = T) %>%
        write.csv("H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/est_mes.csv",
                  row.names=F)

#' si hay totalizadores correr el siguiente codigo:
source('H:/@Documentos/Tesis/ProyR_T_CH/Rainmerging_edu/scripts/reemplazar totalizadores chuspipata.R')


