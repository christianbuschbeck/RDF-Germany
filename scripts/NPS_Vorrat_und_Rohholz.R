require(stringr)
pfad <- str_remove(dirname(rstudioapi::getSourceEditorContext()$path),"scripts")
setwd(pfad)

########## K O N S T A N T E N ###############

Flaeche <- c(1.68,1.13,1.14,0.77,2.76,2.43,0.185,0.308,0.217)
Flaeche <- sum(Flaeche)* 1000000  # in ha

C_Nadel <- 495 * 0.5 # in kg C / m? Holz
C_Laub <- 680* 0.5   # in kg C / m? Holz

C_Fichte <- 0.5 * 470 #in kg C / m? Holz
C_Buche  <- 0.5 * 690  #in kg C / m? Holz
C_Kiefer <- 0.5 * 520  #in kg C / m? Holz 
C_Eiche  <- 0.5 * 670 #in kg C / m? Holz

th_nachlieferung <- 1.71 * Flaeche / 1000000 # in Mio m? 

### B?ume ###

nps_delta_vorrat <- data.frame("Nadel"=numeric(40),   ## in mio m? 
                               "Laub"=numeric(40))

rownames(nps_delta_vorrat) <- seq(from=2013,to = 2052,by = 1)

start <- Flaeche* 345 / 1000000
ende <- Flaeche* 374 / 1000000

interpol <- (ende - start)/40
ratio_Laub <- c(rep(1.4 / (1.4+2.2),5), rep(1.5 / (1.5+2.2),5), rep(1.6 / (1.6+2.2),5), rep(1.6 / (1.6+2.1),5), rep(1.7 / (1.7+2.1),5), rep(1.8 / (1.8+2.1),5), rep(1.9 / (1.9+2.1),5), rep(1.9 / (1.9+2.0),5))
ratio_Nadel <- c(rep(2.2 / (1.4+2.2),5), rep(2.2 / (1.5+2.2),5), rep(2.2 / (1.6+2.2),5), rep(2.1 / (1.6+2.1),5), rep(2.1 / (1.7+2.1),5), rep(2.1 / (1.8+2.1),5), rep(2.1 / (1.9+2.1),5), rep(2.0 / (1.9+2.0),5))

nps_delta_vorrat$Nadel <- interpol * ratio_Nadel
nps_delta_vorrat$Laub <- interpol * ratio_Laub

nadelplot <- c(2.24, 2.22, 2.18, 2.16, 2.14, 2.12, 2.09, 2.06, 2.04)
laubplot <- c(1.42,  1.44, 1.48 , 1.55, 1.63, 1.7, 1.78, 1.85, 1.93)

png(filename="plots/nps_stock_custom.png",width=600,height=300)
par(mar=c(3,3,3,1))
barplot(rbind(nadelplot,laubplot),width = 1,space = 0.2,xlab="Years",ylab="Billion m?",col=c("darkolivegreen4","darkolivegreen2"),main="Wood Stock in NPS",ylim=c(0,4.5))
text(cumsum(rep(1.2,9))-0.5,3,laubplot)
text(cumsum(rep(1.2,9))-0.5,1.5,nadelplot)
axis(1,at=cumsum(rep(1.2,9))-0.5,labels=(c(2012)+seq(from=0,to=40,by=5)))
legend("bottomleft",legend=c("Hardwood","Softwood"),fill=c("darkolivegreen","darkseagreen"))
dev.off()

nps_delta_vorrat$Nadel <- c(rep(-0.02/5,5),rep(-0.04/5,5),rep(-0.02/5,5),rep(-0.02/5,5),rep(-0.02/5,5),rep(-0.03/5,5),rep(-0.03/5,5),rep(-0.02/5,5)) *1000 
nps_delta_vorrat$Laub <- c(rep(0.02/5,5),rep(0.04/5,5),rep(0.07/5,5),rep(0.08/5,5),rep(0.07/5,5),rep(0.08/5,5),rep(0.07/5,5),rep(0.08/5,5)) * 1000


nps_wald_C <- nps_delta_vorrat
nps_wald_C$Nadel <- nps_wald_C$Nadel * -C_Nadel  # in mio kg C| als negative Emission
nps_wald_C$Laub  <- nps_wald_C$Laub * -C_Laub 

nps_wald_emission <- nps_wald_C  * 44/12  # in mio kg CO2 

### Totholz ###

nps_th_incr   <- rep(((35- 14.6) * Flaeche) / 40,40)/1000000 # in mio m?
C_th <- 33637225000 / (22.2 * 8598490) # aus BWI: kg / (m?/ha * ha) -> kg/m?
nps_th_incr_C <- nps_th_incr * C_th
nps_th_incr_emission <- nps_th_incr_C * -44/12


### Rohholz ###
st_max <- 100

nps_rohholz_raw <- data.frame("Fichte"=numeric(40),     ## in mio m? / a 
                              "Buche"=numeric(40),
                              "Kiefer"=numeric(40),
                              "Eiche" =numeric(40))
rownames(nps_rohholz_raw) <- seq(from=2013,to = 2052,by = 1)

nps_rohholz_raw$Fichte <- c(rep(35.6,5),rep(37.8,5),rep(35.3,5),rep(34.6,5),rep(35.2,5),rep(34.3,5),rep(34.3,5),rep(33.1,5))
nps_rohholz_raw$Buche <- c(rep(22.8,5),rep(21.8,5),rep(20.1,5),rep(20.8,5),rep(20.3,5),rep(21.5,5),rep(21.5,5),rep(23,5))
nps_rohholz_raw$Kiefer <- c(rep(21.9,5),rep(22.7,5),rep(20.8,5),rep(19.2,5),rep(19.2,5),rep(18.1,5),rep(16.3,5),rep(15.3,5))
nps_rohholz_raw$Eiche <- c(rep(5.7,5),rep(5.3,5),rep(4.8,5),rep(4.7,5),rep(4.7,5),rep(4.7,5),rep(4.8,5),rep(4.8,5))

nps_rohholz <- nps_rohholz_raw

## Totholz abziehen
for(i in 1:nrow(nps_rohholz)){
  tot_buche <- th_nachlieferung * nps_rohholz[i,"Buche"] / sum(nps_rohholz[i,])  
  tot_fichte <- th_nachlieferung * nps_rohholz[i,"Fichte"] / sum(nps_rohholz[i,])  
  tot_kiefer <- th_nachlieferung * nps_rohholz[i,"Kiefer"] / sum(nps_rohholz[i,])  
  tot_eiche <- th_nachlieferung * nps_rohholz[i,"Eiche"] / sum(nps_rohholz[i,])  
  
  nps_rohholz[i,"Buche"] <- nps_rohholz[i,"Buche"] - tot_buche
  nps_rohholz[i,"Fichte"] <- nps_rohholz[i,"Fichte"] - tot_fichte
  nps_rohholz[i,"Kiefer"] <- nps_rohholz[i,"Kiefer"] - tot_kiefer
  nps_rohholz[i,"Eiche"] <- nps_rohholz[i,"Eiche"] - tot_eiche
  
}

nps_C <- nps_rohholz #mio kg C

nps_C$Fichte <- nps_C$Fichte * C_Fichte
nps_C$Buche  <- nps_C$Buche  * C_Buche
nps_C$Kiefer <- nps_C$Kiefer * C_Kiefer
nps_C$Eiche  <- nps_C$Eiche  * C_Eiche

nps_CO2 <- nps_C * 44/12  # auf mio kg CO2 umrechnen

nps_em_profs <- list()
nps_storage_product <- numeric(st_max)

for(st in 1:st_max){
  print(paste("nps Stor:", st))
  nps_em_prof <- as.data.frame(matrix(numeric(500*40),ncol=500,nrow=40))
  
  for(y in 1:nrow(nps_CO2)){
    amount<-sum(nps_CO2[y,])
    
    nps_em_prof[y,y] <- -amount
    nps_em_prof[y,y+st] <- amount
    
  }
  
  nps_em_profs[[st]]    <- nps_em_prof

}

### Aggregate ###

NWE <- as.numeric(apply(nps_wald_emission,1,sum)) + nps_th_incr_emission # Natur Wald Emission
nps_roh_C <- c(as.numeric(apply(nps_C,1,sum)))                                 # Natur Use C   

start_vorr <- 1.42*1000 *C_Laub +  2.24*1000 * C_Nadel +  (14.6*Flaeche/1000000)* C_th
nps_vorr <-cumsum(c(start_vorr,as.numeric(apply(-nps_wald_C,1,sum))+nps_th_incr_C))



