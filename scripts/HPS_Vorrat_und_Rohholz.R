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

th_nachlieferung <- 0.58 * Flaeche / 1000000 # in Mio m? 


### B?ume ###

hps_delta_vorrat <- data.frame("Nadel"=numeric(40),   ## in mio m? 
                               "Laub"=numeric(40))

rownames(hps_delta_vorrat) <- seq(from=2013,to = 2052,by = 1)
hps_delta_vorrat$Nadel <- c(rep(-90/5,5),rep(-125/5,5),rep(-98/5,5),rep(-110/5,5),rep(-28/5,5),rep(2/5,5),rep(51/5,5),rep(67/5,5))
hps_delta_vorrat$Laub <- c(rep(-96/5,5),rep(-92/5,5),rep(-56/5,5),rep(-51/5,5),rep(-8/5,5),rep(-2/5,5),rep(19/5,5),rep(26/5,5))

hps_wald_C <- hps_delta_vorrat
hps_wald_C$Nadel <- hps_wald_C$Nadel * -C_Nadel  # in mio kg CO2 | als negative Emission
hps_wald_C$Laub  <- hps_wald_C$Laub * -C_Laub 

hps_wald_emission <- hps_wald_C  * 44/12  # in mio kg CO2 

### Totholz ### 

C_th <- 33637225000 / (22.2 * 8598490) # aus BWI: kg / (m?/ha * ha) -> kg/m?
hps_th_incr_C <- rep(0,40)
### Rohholz ###

st_max <- 100

hps_rohholz_raw <- data.frame("Fichte"=numeric(40),     ## in mio m? / a 
                              "Buche"=numeric(40),
                              "Kiefer"=numeric(40),
                              "Eiche" =numeric(40))
rownames(hps_rohholz_raw) <- seq(from=2013,to = 2052,by = 1)

hps_rohholz_raw$Fichte <- c(rep(45.3,5),rep(48.2,5),rep(47.8,5),rep(50.1,5),rep(44.9,5),rep(45.4,5),rep(41.9,5),rep(42.6,5))
hps_rohholz_raw$Buche <- c(rep(36.1,5),rep(34.7,5),rep(32.2,5),rep(31.8,5),rep(26.1,5),rep(25.3,5),rep(22.7,5),rep(21.9,5))
hps_rohholz_raw$Kiefer <- c(rep(23.1,5),rep(25.4,5),rep(25,5),rep(27.6,5),rep(22.1,5),rep(20.4,5),rep(17.5,5),rep(16.1,5))
hps_rohholz_raw$Eiche <- c(rep(9.8,5),rep(10.1,5),rep(9.3,5),rep(8.8,5),rep(7.1,5),rep(7.1,5),rep(6,5),rep(5.8,5))

hps_rohholz <- hps_rohholz_raw

for(i in 1:nrow(hps_rohholz)){
  tot_buche  <- th_nachlieferung * hps_rohholz[i,"Buche"] / sum(hps_rohholz[i,])  
  tot_fichte <- th_nachlieferung * hps_rohholz[i,"Fichte"] / sum(hps_rohholz[i,])  
  tot_kiefer <- th_nachlieferung * hps_rohholz[i,"Kiefer"] / sum(hps_rohholz[i,])  
  tot_eiche  <- th_nachlieferung * hps_rohholz[i,"Eiche"] / sum(hps_rohholz[i,])  
  
  hps_rohholz[i,"Buche"] <- hps_rohholz[i,"Buche"] - tot_buche
  hps_rohholz[i,"Fichte"] <- hps_rohholz[i,"Fichte"] - tot_fichte
  hps_rohholz[i,"Kiefer"] <- hps_rohholz[i,"Kiefer"] - tot_kiefer
  hps_rohholz[i,"Eiche"] <- hps_rohholz[i,"Eiche"] - tot_eiche
  
}

hps_C <- hps_rohholz

hps_C$Fichte <- hps_C$Fichte * C_Fichte
hps_C$Buche  <- hps_C$Buche  * C_Buche
hps_C$Kiefer <- hps_C$Kiefer * C_Kiefer
hps_C$Eiche  <- hps_C$Eiche  * C_Eiche

hps_CO2 <- hps_C * 44/12  # auf mio kg CO2 umrechnen

hps_em_profs <- list()
hps_storage_product <- numeric(st_max)

for(st in 1:st_max){
  print(paste("hps Stor:", st))
  hps_em_prof <- as.data.frame(matrix(numeric(500*40),ncol=500,nrow=40))
  
  for(y in 1:nrow(hps_CO2)){
    amount<-sum(hps_CO2[y,])
    
    hps_em_prof[y,y] <- -amount
    hps_em_prof[y,y+st] <- amount
    
  }
  
  hps_em_profs[[st]]    <- hps_em_prof
  
}


### Aggregate ###

HWE <- as.numeric(apply(hps_wald_emission,1,sum))+hps_th_incr_C  # Natur Wald Emission
hps_roh_C <- c(as.numeric(apply(hps_C,1,sum)))                                 # Natur Use C   

start_vorr <- 1.42*1000 *C_Laub +  2.24*1000 * C_Nadel +  (14.6*Flaeche/1000000)* C_th
hps_vorr <-cumsum(c(start_vorr,as.numeric(apply(-hps_wald_C,1,sum))+hps_th_incr_C))


