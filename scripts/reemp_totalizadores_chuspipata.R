#' reemplazar valores de toatalizadores en est_mes.csv

est_mes = read.csv("H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/est_mes.csv")
reempl = read.table("C:/Users/HP/Desktop/EST/desde 1981/chuspipata_totalizadores.txt",
                    header = T)

est_mes[,names(reempl[2])] = reempl[2]

write.csv(est_mes, "H:/@Documentos/Tesis/ProyR_T_CH/data/lluvias_2017/resumidos/est_mes.csv", row.names=F)

