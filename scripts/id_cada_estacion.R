library(xlsx)
RUTA <- "C:/Users/HP/Desktop/EST/todos originales"
arch <- list.files(RUTA,pattern = ".xls", full.names = T)
arch_list <- lapply(arch, read.xlsx, 1, rowIndex = c(1:3), colIndex = c(1,3,10,12), header = F)

df_est <- data.frame(NULL)
for (i in 1:length(arch_list)) {
        aa <- arch_list[[i]]
        # grados
        latS <- aa[1,4]
        latS <- as.character(latS)
        latS <- substring(latS, c(1,6,9), c(2,7,11))
        
        lonW <- aa[2,4]
        lonW <- as.character(lonW)
        lonW <- substring(lonW, c(1,6,9), c(2,7,11))
        
        # est
        est <- as.character(aa[1,2])
        #alt
        alt <- as.character(aa[3,4])
        ##dpto 
        dpto <- as.character(aa[2,2])
        #prov
        prov <- as.character(aa[3,2])
        #bind
        df <- cbind(est, dpto, prov, latS[1], latS[2], latS[3], lonW[1], lonW[2], lonW[3], alt)
        df <- data.frame(df)
        df_est <- rbind(df_est, df)
}

#write.csv(df_est, "C:/Users/HP/Desktop/EST/Rest.csv")
# no correr, modificar ruta de salidas para evitar sobreescritura

# en excel corrijo los caracteres " y '  
# ademas, reemplazo los espacios con _ y
# reemplazo parentesis ( ) con .
# ver Rest_2.xlsx

