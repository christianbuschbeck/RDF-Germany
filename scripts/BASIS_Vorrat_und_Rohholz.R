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


### Totholz ### 

C_th <- 33637225000 / (22.2 * 8598490) # aus BWI: kg / (m?/ha * ha) -> kg/m?
base_th_incr_C <- rep(0,40)


### B?ume ###

base_delta_vorrat <- data.frame("Fichte"=numeric(40),     ## in mio m? / a 
                              "Buche"=numeric(40),
                              "Kiefer"=numeric(40),
                              "Eiche" =numeric(40))

#fichte 
fichte_csv <- read.csv2("data/Vorrat_Fichte.csv", header=FALSE)
colnames(fichte_csv) <- c("Jahr","Vorrat") # in 1000 m?/a
fichte_csv$Jahr <- round(fichte_csv$Jahr)
fichte_csv$Vorrat <- fichte_csv$Vorrat / 1000 #in mio m?/a
fichte_csv$Vorrat[2:nrow(fichte_csv)] <- fichte_csv$Vorrat[2:nrow(fichte_csv)] -  fichte_csv$Vorrat[1:(nrow(fichte_csv)-1)]
fichte_csv <- fichte_csv[-1,]

#kiefer 
kiefer_csv <- read.csv2("data/Vorrat_Kiefer.csv", header=FALSE)
colnames(kiefer_csv) <- c("Jahr","Vorrat") # in 1000 m?/a
kiefer_csv$Jahr <- round(kiefer_csv$Jahr)
kiefer_csv$Vorrat <- kiefer_csv$Vorrat / 1000 #in mio m?/a
kiefer_csv$Vorrat[2:nrow(kiefer_csv)] <- kiefer_csv$Vorrat[2:nrow(kiefer_csv)] -  kiefer_csv$Vorrat[1:(nrow(kiefer_csv)-1)]
kiefer_csv <- kiefer_csv[-1,]

#buche 
buche_csv <- read.csv2("data/Vorrat_Buche.csv", header=FALSE)
colnames(buche_csv) <- c("Jahr","Vorrat") # in 1000 m?/a
buche_csv$Jahr <- round(buche_csv$Jahr)
buche_csv$Vorrat <- buche_csv$Vorrat / 1000 #in mio m?/a
buche_csv$Vorrat[2:nrow(buche_csv)] <- buche_csv$Vorrat[2:nrow(buche_csv)] -  buche_csv$Vorrat[1:(nrow(buche_csv)-1)]
buche_csv <- buche_csv[-1,]

#eiche 
eiche_csv <- read.csv2("data/Vorrat_Eiche.csv", header=FALSE)
colnames(eiche_csv) <- c("Jahr","Vorrat") # in 1000 m?/a
eiche_csv$Jahr <- round(eiche_csv$Jahr)
eiche_csv$Vorrat <- eiche_csv$Vorrat / 1000 #in mio m?/a
eiche_csv$Vorrat[2:nrow(eiche_csv)] <- eiche_csv$Vorrat[2:nrow(eiche_csv)] -  eiche_csv$Vorrat[1:(nrow(eiche_csv)-1)]
eiche_csv <- eiche_csv[-1,]

base_delta_vorrat$Fichte <- c(rep(fichte_csv$Vorrat[1]/5,5),rep(fichte_csv$Vorrat[2]/5,5),rep(fichte_csv$Vorrat[3]/5,5),rep(fichte_csv$Vorrat[4]/5,5),rep(fichte_csv$Vorrat[5]/5,5),rep(fichte_csv$Vorrat[6]/5,5),rep(fichte_csv$Vorrat[7]/5,5),rep(fichte_csv$Vorrat[8]/5,5))
base_delta_vorrat$Buche  <- c(rep(buche_csv$Vorrat[1]/5,5),rep(buche_csv$Vorrat[2]/5,5),rep(buche_csv$Vorrat[3]/5,5),rep(buche_csv$Vorrat[4]/5,5),rep(buche_csv$Vorrat[5]/5,5),rep(buche_csv$Vorrat[6]/5,5),rep(buche_csv$Vorrat[7]/5,5),rep(buche_csv$Vorrat[8]/5,5))
base_delta_vorrat$Kiefer <- c(rep(kiefer_csv$Vorrat[1]/5,5),rep(kiefer_csv$Vorrat[2]/5,5),rep(kiefer_csv$Vorrat[3]/5,5),rep(kiefer_csv$Vorrat[4]/5,5),rep(kiefer_csv$Vorrat[5]/5,5),rep(kiefer_csv$Vorrat[6]/5,5),rep(kiefer_csv$Vorrat[7]/5,5),rep(kiefer_csv$Vorrat[8]/5,5))
base_delta_vorrat$Eiche  <- c(rep(eiche_csv$Vorrat[1]/5,5),rep(eiche_csv$Vorrat[2]/5,5),rep(eiche_csv$Vorrat[3]/5,5),rep(eiche_csv$Vorrat[4]/5,5),rep(eiche_csv$Vorrat[5]/5,5),rep(eiche_csv$Vorrat[6]/5,5),rep(eiche_csv$Vorrat[7]/5,5),rep(eiche_csv$Vorrat[8]/5,5))

base_wald_C <- base_delta_vorrat
base_wald_C$Fichte <- base_wald_C$Fichte * -C_Fichte  # in mio kg CO2 | als negative Emission
base_wald_C$Buche  <- base_wald_C$Buche * -C_Buche 
base_wald_C$Kiefer <- base_wald_C$Kiefer * -C_Kiefer  # in mio kg CO2 | als negative Emission
base_wald_C$Eiche  <- base_wald_C$Eiche * -C_Eiche 

base_wald_emission <- base_wald_C  * 44/12  # in mio kg CO2 


### Rohholz ###

st_max <- 100


Basis_Rohholz_csv <- read.csv2("data/Basis_Rohholz.csv", header=FALSE)
colnames(Basis_Rohholz_csv) <- c("Jahr","Rohholz")
Basis_Rohholz_csv$Jahr <- round(Basis_Rohholz_csv$Jahr)
Basis_Rohholz_csv$Rohholz <-Basis_Rohholz_csv$Rohholz/ 1000 # in mio m?

rohholz_nadel <- numeric(8)
rohholz_laub  <- numeric(8)

count <-0
for(y in unique(Basis_Rohholz_csv$Jahr)){
  count <- count +1
  sub <- subset(Basis_Rohholz_csv,Jahr ==y)
  rohholz_nadel[count] <- sub[1,2] - sub[2,2] 
  rohholz_laub[count] <-  sub[2,2] 
}


base_rohholz_raw<- data.frame("Nadel"=numeric(40),     ## in mio m? / a 
                              "Laub"=numeric(40))
                              
rownames(base_rohholz_raw) <- seq(from=2013,to = 2052,by = 1)

base_rohholz_raw$Nadel<- c(rep(rohholz_nadel[1],5),rep(rohholz_nadel[2],5),rep(rohholz_nadel[3],5),rep(rohholz_nadel[4],5),rep(rohholz_nadel[5],5),rep(rohholz_nadel[6],5),rep(rohholz_nadel[7],5),rep(rohholz_nadel[8],5))
base_rohholz_raw$Laub<- c(rep(rohholz_laub[1],5),rep(rohholz_laub[2],5),rep(rohholz_laub[3],5),rep(rohholz_laub[4],5),rep(rohholz_laub[5],5),rep(rohholz_laub[6],5),rep(rohholz_laub[7],5),rep(rohholz_laub[8],5))

base_rohholz <- base_rohholz_raw

base_C <- base_rohholz

base_C$Nadel <- base_C$Nadel * C_Nadel
base_C$Laub  <- base_C$Laub  * C_Laub

base_CO2 <- base_C * 44/12  # auf mio kg CO2 umrechnen

base_em_profs <- list()
base_storage_product <- numeric(st_max)

for(st in 1:st_max){
  print(paste("base Stor:", st))
  base_em_prof <- as.data.frame(matrix(numeric(500*40),ncol=500,nrow=40))
  
  for(y in 1:nrow(base_CO2)){
    amount<-sum(base_CO2[y,])
    
    base_em_prof[y,y] <- -amount
    base_em_prof[y,y+st] <- amount
    
  }
  
  base_em_profs[[st]]    <- base_em_prof
  
}



### Aggregate ###

BAS <- as.numeric(apply(base_wald_emission,1,sum))+base_th_incr_C  # Natur Wald Emission
base_roh_C <- c(as.numeric(apply(base_C,1,sum)))                                 # Natur Use C   

start_vorr <- 1.42*1000 *C_Laub +  2.24*1000 * C_Nadel +  (14.6*Flaeche/1000000)* C_th
base_vorr <-cumsum(c(start_vorr,as.numeric(apply(-base_wald_C,1,sum))+base_th_incr_C))


