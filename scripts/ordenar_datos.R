#detach("package:xlsx", unload = T)
library(openxlsx)
#library(xlsx)
library(lubridate)
library(tidyverse)

is.leapyear <- function(year){
        return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

####
RUTA <- "C:/Users/HP/Desktop/EST/desde 1981"

###====== LEEME ===
# de los 357 estaciones originales solo quedan 239. (inicialemnte, con 
# Rest.csv exisitian 353 estaciones verificar cuales se amuentaron)
# Esto debido al descarte de estaciones con pocos anos
# para mas info ver el archivo C:\Users\HP\Desktop\EST/Rest_2.xlsx
# en este archivo estan las observaciones a las estaciones descartadas 
# (descritas con la palabra chau y el porque)

exc_list <- list.files(RUTA, pattern = ".xlsx", full.names = T)
nom_est <- read.xlsx("C:/Users/HP/Desktop/EST/Rest_2.xlsx", sheet = 2)
nom_est <- nom_est[,1]
f <- seq.Date(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day") #vector de fechas

start.time <- Sys.time()
exc_list <- lapply(exc_list, read.xlsx, startRow = 5, cols = c(1:13), colNames = F)

t <- data.frame(fecha = f)
for (k in 1:length(nom_est)) { # numero de estaciones 239
      cc_est <- exc_list[[k]]  
      cc_est[cc_est == "****"] <- NA #reemplaza todos los ****
      
      #datos ordenados:
      d <- data.frame(NULL) #data frame vacio para llenar datos
      #sub <- cc[3:33, 2:13] #filas y columnas a ser leidas
      for (j in 1:36) { #2016-1981+1 = 36, anos del periodo 1981:2016
              sub <- cc_est[eval(3 + (j - 1)*38):eval(3 + (j - 1)*38 + 30),2:13]
              bis <- is.leapyear(eval(1980 + j)) #1981-1+j, para que anos vayan de acuerdo al indice j 
              for (i in 1:12) {
                      diasmes <- days_in_month(i)
                      if (i == 2 & bis == T)  diasmes <- diasmes + 1
                      msub <- as.numeric(as.character(sub[,i]))
                      msub <- msub[1:diasmes]
                      msub <- data.frame(msub) 
                      d <- rbind(d, msub)
              }
      }
      t <- cbind(t, d)
}

names(t) <- c("fecha", nom_est)
#write.csv(t, paste0(RUTA,"/todo en uno/todoenuno.csv"), row.names = F)

# write.csv(t, "H:/@Documentos/Tesis/Proy_R/resultados/todoenuno.csv", row.names = F)
# no correr, evitar sobrrescritura cambiando rutas de salida

#' ojo existe duplicado en 'C:/Users/HP/Desktop/EST/desde 1981/todo en uno/todoenuno.csv', al parecer son 
#' lo mismo, verificar

end.time <- Sys.time()
end.time-start.time
