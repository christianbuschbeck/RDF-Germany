### Laden und so ###
require(RColorBrewer)
require(pracma)
require(DescTools)
require(stringr)
#require(xlsx)

pfad <- str_remove(dirname(rstudioapi::getSourceEditorContext()$path),"scripts")
setwd(pfad)

source("scripts/NPS_Vorrat_und_Rohholz.R")
source("scripts/HPS_Vorrat_und_Rohholz.R")
source("scripts/BASIS_Vorrat_und_Rohholz.R")

levassheur <- function(pulse){
  TH=500
  pulse <- c(pulse,0)
  f <- function(t){
    
    a0 = 0.2173
    a1 = 0.224
    a2 = 0.2824
    a3 = 0.2763
    
    tau1 = 394.4
    tau2 = 36.54
    tau3 = 4.304
    
    alpha = 1.81E-15
    res <- alpha*(a0 + a1*exp(-t/tau1) + a2*exp(-t/tau2) + a3*exp(-t/tau3))
    return(res)
  }
  
  DCFinst <- NULL
  for(i in 1:TH){
    DCFinst <- c(DCFinst,integral(f,xmin=i-1,xmax=i))
    
  }
  
  GWIinst = NULL
  for(t in 0:(TH-1)){
    GWI = NULL
    for(i in 0:t){
      
      DCF <- DCFinst[(t-i)+1]
      em   <-pulse[i+1]
      
      GWI <- c(GWI,em * DCF)
    }
    GWIinst <- c(GWIinst,sum(GWI))
    
  }
  
  GWIcum <- sum(GWIinst)
  
  DLCA<-GWIcum/integral(f,xmin=0,xmax=TH)
  
  res <- list()
  res[["GWIinst"]] <- GWIinst
  res[["GWP"]] <- DLCA
  return(res)
}



### Vorarbeiten ###


use_diff1 <- base_roh_C - nps_roh_C
use_diff2 <- hps_roh_C - base_roh_C
use_diff3 <- hps_roh_C - nps_roh_C

p_raw <- numeric(500)

profiles_use1 <- as.data.frame(matrix(nrow=40,ncol=500))
profiles_use2 <- as.data.frame(matrix(nrow=40,ncol=500))
profiles_use3 <- as.data.frame(matrix(nrow=40,ncol=500))


for(j in 1:40){
  profiles_use1[j,] <- p_raw
  profiles_use1[j,j] <- use_diff1[j]
  
  profiles_use2[j,] <- p_raw
  profiles_use2[j,j] <- use_diff2[j]
  
  profiles_use3[j,] <- p_raw
  profiles_use3[j,j] <- use_diff3[j]
}

### R D F ###
szmaxmin <- c(1,75)
RDFS <- list()

for(sz in szmaxmin){
  
  profiles_H <- as.data.frame(matrix(nrow=40,ncol=500))
  profiles_B <- as.data.frame(matrix(nrow=40,ncol=500))
  profiles_N <- as.data.frame(matrix(nrow=40,ncol=500))
  
  
  for(j in 1:40){
    
    profiles_H[j,] <- p_raw
    profiles_B[j,] <- p_raw
    profiles_N[j,] <- p_raw
    
    #C_Wald 
    profiles_H[j,j] <- profiles_H[j,j] + HWE[j] 
    profiles_B[j,j] <- profiles_B[j,j] + BAS[j] 
    profiles_N[j,j] <- profiles_N[j,j] + NWE[j] 
    
    #C_Produkt
    profiles_H[j,j] <- profiles_H[j,j] + as.numeric(hps_em_profs[[sz]][j,])[j]
    profiles_B[j,j] <- profiles_B[j,j] + as.numeric(base_em_profs[[sz]][j,])[j]
    profiles_N[j,j] <- profiles_N[j,j] + as.numeric(nps_em_profs[[sz]][j,])[j]
    
    profiles_H[j,j+sz] <- profiles_H[j,j+sz] + as.numeric(hps_em_profs[[sz]][j,])[j+sz]
    profiles_B[j,j+sz] <- profiles_B[j,j+sz] + as.numeric(base_em_profs[[sz]][j,])[j+sz]
    profiles_N[j,j+sz] <- profiles_N[j,j+sz] + as.numeric(nps_em_profs[[sz]][j,])[j+sz]
  }
  
  
  profiles_diff1  <- profiles_B - profiles_N
  profiles_diff2  <- profiles_H - profiles_B
  profiles_diff3  <- profiles_H - profiles_N
  
  #!! Noch in CO2 muss in C umgewandelt werden
  profiles_diff1 <- profiles_diff1*12/44
  profiles_diff2 <- profiles_diff2*12/44
  profiles_diff3 <- profiles_diff3*12/44
  
  RDF1 <- numeric(40)
  RDF2 <- numeric(40)
  RDF3 <- numeric(40)
  for(j in 1:40){  
    
    RDF1[j] <-levassheur(as.numeric(profiles_diff1[j,]))$GWP/levassheur(as.numeric(profiles_use1[j,]))$GWP 
    RDF2[j] <-levassheur(as.numeric(profiles_diff2[j,]))$GWP/levassheur(as.numeric(profiles_use2[j,]))$GWP 
    RDF3[j] <-levassheur(as.numeric(profiles_diff3[j,]))$GWP/levassheur(as.numeric(profiles_use3[j,]))$GWP 
  }

  if(sz==szmaxmin[1]){
    RDFS[["min"]][[1]] <- RDF1
    RDFS[["min"]][[2]] <- RDF2
    RDFS[["min"]][[3]] <- RDF3
    
    
  }
  
  if(sz==szmaxmin[2]){
    RDFS[["max"]][[1]] <- RDF1
    RDFS[["max"]][[2]] <- RDF2
    RDFS[["max"]][[3]] <- RDF3
    
    
  }

}

################# C U R R E N T  R D F ##############
#aus weham - regionalisierte Verwendung

energy_df <- 0.67#0.8
paper_df  <- 0 
log_df    <- 1.5#1.45

construction_anteil <- 51 + 10.9 +  1
energy_anteil       <- 27.8
paper_anteil       <- 9.3 


wm<-weighted.mean(c(log_df,energy_df,paper_df),c(construction_anteil,energy_anteil,paper_anteil))

### Decarbonization
material_decarb_kon <- (100-50)/100 #aus VDZ 2020
material_decarb_opt <- (100-100)/100 #aus VDZ 2020


energy_decarb_kon <- 19.6 / 33 #aus Pehl et. al & IEA
energy_decarb_opt <- 0.77 / 33 #aus Pehl et. al & IEA

wm_2052_Kon<-weighted.mean(c(log_df*material_decarb_kon,energy_df*energy_decarb_kon,0),c(construction_anteil,energy_anteil,paper_anteil))
wm_2052_Opt<-weighted.mean(c(log_df*material_decarb_opt,energy_df*energy_decarb_opt,0),c(construction_anteil,energy_anteil,paper_anteil))

#Future RDF
current <- 2022

df_seq_Opt <- c(rep(wm,length(2013:(current-1))),seq(from=wm,to = wm_2052_Opt ,length.out = length(current:2045 )),rep(wm_2052_Opt,length(2046:2052)))
df_seq_Kon <- c(rep(wm,length(2013:(current-1))),seq(from=wm,to = wm_2052_Kon ,length.out = length(current:2045 )),rep(wm_2052_Kon,length(2046:2052)))






####################### P L O T ##########



#######################   NPS und HPS #############
basecol <- "gold"
wpscol  <- rgb(252,141,89,maxColorValue = 255) 
npscol  <- rgb(145,191,219,maxColorValue = 255)

THGI_biomass <- 1061407264 * 1000 / 1000000
THGI_abgang <- 26262141 * 1000 / 1000000
#plot Vorrat

png(filename="plots/Vorrat.png",width=1000,height=550)
par(mfrow=c(1,2),cex.main=2,cex.axis=2,cex.lab=2,mar=c(5,6,4,2))
plot(nps_vorr/1000~c(2012:2052),type="l",ylim=c(range(c(hps_vorr,nps_vorr)/1000)),col=npscol,main="Carbon stocks in the forest",xlab="Time [years]",ylab="Mio t",lwd=3)
lines(hps_vorr/1000~c(2012:2052),col=wpscol,lwd=3)
lines(base_vorr/1000~c(2012:2052),col=basecol,lwd=3)

#points(2017,THGI_biomass/1000,pch=3,col="purple",cex=3)
legend("topleft",legend=c("NPS","WPS","BL"),lty=c(1,1,1),col=c(npscol,wpscol,basecol),title = "Scenario",lwd=c(2,2,2),cex=1.5)

plot(nps_roh_C/1000~c(2013:2052),type="l",ylim=c(range(c(hps_roh_C,nps_roh_C)/1000)),col=npscol,main="In wood extracted \n carbon",xlab="Time [years]",ylab="Mio t",lwd=3)
lines(hps_roh_C/1000~c(2013:2052),col=wpscol,lwd=3)
lines(base_roh_C/1000~c(2013:2052),col=basecol,lwd=3)

#points(2017,THGI_abgang/1000,pch=3,col="purple",cex=3)
legend("topright",legend=c("WPS","NPS","BL"),lty=c(1,1,1),col=c(wpscol,npscol,basecol),title = "Scenario",lwd=c(2,2,2),cex=1.5)

dev.off()

colbasenps <-MixColor(npscol,basecol)
colbasewps <-MixColor(wpscol,basecol)
colwpsnps  <-MixColor(wpscol,npscol)


alp = 0.6
#order: colbasenps,colbasewps,colwpsnps
cols = c(rgb(col2rgb(colbasenps)[1,1]/255,col2rgb(colbasenps)[2,1]/255,col2rgb(colbasenps)[3,1]/255,alp),
         rgb(col2rgb(colbasewps)[1,1]/255,col2rgb(colbasewps)[2,1]/255,col2rgb(colbasewps)[3,1]/255,alp),
         rgb(col2rgb(colwpsnps)[1,1]/255,col2rgb(colwpsnps)[2,1]/255,col2rgb(colwpsnps)[3,1]/255,alp))

alp = 0.9
cols_bor = c(rgb(col2rgb(colbasenps)[1,1]/255,col2rgb(colbasenps)[2,1]/255,col2rgb(colbasenps)[3,1]/255,alp),
         rgb(col2rgb(colbasewps)[1,1]/255,col2rgb(colbasewps)[2,1]/255,col2rgb(colbasewps)[3,1]/255,alp),
         rgb(col2rgb(colwpsnps)[1,1]/255,col2rgb(colwpsnps)[2,1]/255,col2rgb(colwpsnps)[3,1]/255,alp))

 
png(filename="plots/RDF.png",width=1500,height=1000)
par(mfrow=c(1,1),cex.main=3,cex.axis=2.5,cex.lab=2.5,mar=c(5,6,6,2))
plot(2013:2052,RDFS[["max"]][[1]],type="l",ylim=c(-0.5,1.5),xlab="Time [years]",ylab="DF or RDF [ton of C/ton of C]",main="Required Displacement Factors \n intensive vs. extensive forestry",col="white")
for(i in 1:3){
polygon(c(2013:2052, rev(2013:2052)), c(RDFS[["min"]][[i]], rev(RDFS[["max"]][[i]])),col =cols[i],border = NA)
  lines(RDFS[["min"]][[i]]~c(2013:2052),col =cols_bor[i],lty=1,lwd=4)
  lines(RDFS[["max"]][[i]]~c(2013:2052),col =cols_bor[i],lty=6,lwd=4)
  }
lines(df_seq_Opt ~c(2013:2052),col="red",lty=3,lwd=4)
lines(df_seq_Kon ~c(2013:2052),col="red",lty=2,lwd=4)
abline(h=0,lty=2,lwd=3)
points(current,wm,col="red",cex=4,pch=3)

legend("topright",legend = as.character(c("WPS vs. BL","WPS vs. NPS","BL vs. NPS")),fill=cols[c(2,3,1)],title="Scenario Comparison" ,cex=3)
#legend("bottomleft",legend = c("Mean DF - current","Mean. DF - Con.","Mean. DF - Opt."),col=c("red","red","red"),pch=c(3,NA,NA),lty=c(NA,2,3),lwd=c(NA,3,3),cex=3)
legend("bottomleft",legend = c("Current","Conservative","Optimistic"),col=c("red","red","red"),pch=c(3,NA,NA),lty=c(NA,2,3),lwd=c(NA,2,2),cex=3,title="DF")
legend("bottomright",legend = c("ST: 1", "ST: 75"),col=c("black","black"),lty=c(1,6),lwd=c(2,2),cex=3,title="RDF")

dev.off()



#cols <- brewer.pal(9,"Oranges")[c(4,6,9)]

use_diffs <- list()
use_diffs[[1]] <- use_diff1
use_diffs[[2]] <- use_diff2
use_diffs[[3]] <- use_diff3

dfs <- list()
dfs_cum <- list()
for(i in 1:3){
dfs[[i]] <- as.data.frame(matrix(rep(0,40),ncol=40,nrow=1))
dfs[[i]][1,] <- (RDFS[["max"]][[i]]-df_seq_Kon)*use_diffs[[i]]*44/12 / 1000
dfs[[i]][2,] <- (RDFS[["min"]][[i]]-df_seq_Kon)*use_diffs[[i]]*44/12 / 1000
dfs[[i]][3,] <- (RDFS[["max"]][[i]]-df_seq_Opt)*use_diffs[[i]]*44/12 / 1000
dfs[[i]][4,] <- (RDFS[["min"]][[i]]-df_seq_Opt)*use_diffs[[i]]*44/12 / 1000

dfs_cum[[i]] <- as.data.frame(matrix(rep(0,40),ncol=40,nrow=1))
dfs_cum[[i]][1,] <- cumsum((RDFS[["max"]][[i]]-df_seq_Kon)*use_diffs[[i]]*44/12 / 1000)
dfs_cum[[i]][2,] <- cumsum((RDFS[["min"]][[i]]-df_seq_Kon)*use_diffs[[i]]*44/12 / 1000)
dfs_cum[[i]][3,] <- cumsum((RDFS[["max"]][[i]]-df_seq_Opt)*use_diffs[[i]]*44/12 / 1000)
dfs_cum[[i]][4,] <- cumsum((RDFS[["min"]][[i]]-df_seq_Opt)*use_diffs[[i]]*44/12 / 1000)
}


png(filename="plots/DiffGWP.png",width=1500,height=1000)
par(mfrow=c(1,1),cex.main=3,cex.axis=2,cex.lab=2.5,mar=c(5,6,6,10))
plot(-100,-100,ylim=c(min(rbind(dfs[[1]],dfs[[2]],dfs[[3]]))*1.2,max(rbind(dfs[[1]],dfs[[2]],dfs[[3]]))*1.2),xlim=c(2013,2052),xlab="Time [years]",ylab = "GWP [Mt CO2-equ.]"
     ,main="GWP differences of annual wood extractions \n (intensive minus extensive)")
for(i in 1:3){
  polygon(c(2013:2052, rev(2013:2052)), c(dfs[[i]][which(dfs[[i]][,40] == max(dfs[[i]][,40])),], rev(dfs[[i]][which(dfs[[i]][,40] == min(dfs[[i]][,40])),])),col =cols[i],border=NA)
  lines(as.numeric(dfs[[i]][1,])~c(2013:2052),col=cols_bor[i],lty=2,lwd=3)
  lines(as.numeric(dfs[[i]][4,])~c(2013:2052),col=cols_bor[i],lty=3,lwd=3)
}
abline(h=0,lty=2,lwd=2)
arrows(x0=2055,y0= 1, x1=2055,y1=14,lwd= 4,xpd =T)
arrows(x0=2055,y0= -1, x1=2055,y1=-14,lwd= 4,xpd =T)
text(2056,17,"Less \n GWP",xpd=T,cex=2.5)
text(2056,10,"Extensive",xpd=T,srt=-90,cex=2.5)
text(2056,-10,"Intensive",xpd=T,srt=-90,cex=2.5)

legend("topleft",legend = as.character(c("WPS vs. BL","WPS vs. NPS","BL vs. NPS")),fill=cols[c(2,3,1)],title="Scenario Comparison" ,cex=3)
legend("bottomleft",legend = as.character(c("Conservative DF, ST: 75","Optimistic DF, ST: 1")),col="black",title="Conditions",lty=c(2,3),lwd=2 ,cex=3)

dev.off()

png(filename="plots/KumGWP.png",width=1500,height=1000)
par(mfrow=c(1,1),cex.main=3,cex.axis=2,cex.lab=2.5,mar=c(5,6,6,10))
plot(-100,-100,ylim=c(min(rbind(dfs_cum[[1]],dfs_cum[[2]],dfs_cum[[3]])),max(rbind(dfs_cum[[1]],dfs_cum[[2]],dfs_cum[[3]])))
     ,xlim=c(2013,2052),main="Cumulative GWP differences of annual wood extractions \n (intensive minus extensive)",
     xlab="Time [years]",ylab = "GWP [Mt CO2-equ.]")
for(i in 1:3){
  polygon(c(2013:2052, rev(2013:2052)), c(dfs_cum[[i]][which(dfs_cum[[i]][,40] == max(dfs_cum[[i]][,40])),], rev(dfs_cum[[i]][which(dfs_cum[[i]][,40] == min(dfs_cum[[i]][,40])),])),col =cols[i],border=NA)
  lines(as.numeric(dfs_cum[[i]][1,])~c(2013:2052),col=cols_bor[i],lty=2,lwd=3)
  lines(as.numeric(dfs_cum[[i]][4,])~c(2013:2052),col=cols_bor[i],lty=3,lwd=3)
}
abline(h=0,lty=2,lwd=2)
arrows(x0=2055,y0= 50, x1=2055,y1=240,lwd= 4,xpd =T)
arrows(x0=2055,y0= -50, x1=2055,y1=-240,lwd= 4,xpd =T)
text(2056,270,"Less \n GWP",xpd=T,cex=2.5)
text(2056,150,"Extensive",xpd=T,srt=-90,cex=2.5)
text(2056,-150,"Intensive",xpd=T,srt=-90,cex=2.5)

legend("topleft",legend = as.character(c("WPS vs. BL","WPS vs. NPS","BL vs. NPS")),fill=cols[c(2,3,1)],title="Scenario Comparison" ,cex=2.5)
legend("bottomleft",legend = as.character(c("Conservative DF, ST: 75","Optimistic DF, ST: 1")),col="black",title="Conditions",lty=c(2,3),lwd=2 ,cex=2.5)

dev.off()

####### Export ####

RDF_table <- as.data.frame(matrix(NA,ncol=42,nrow = 6))

RDF_table[1,3:42] <- RDFS[["min"]][[1]]
RDF_table[2,3:42] <- RDFS[["max"]][[1]]
RDF_table[3,3:42] <- RDFS[["min"]][[2]]
RDF_table[4,3:42] <- RDFS[["max"]][[2]]
RDF_table[5,3:42] <- RDFS[["min"]][[3]]
RDF_table[6,3:42] <- RDFS[["max"]][[3]]

RDF_table[,2] <- c(rep(c("1","75"),3))
RDF_table[,1] <- c(rep("BL vs. NPS",2),rep("WPS vs. BL",2),rep("WPS vs. NPS",2))

colnames(RDF_table) <- c("Comparison","Storage Period",as.character(2013:2052))
write_xlsx(RDF_table,"results/RDFs.xlsx")

###
df_table <- as.data.frame(matrix(NA,ncol=43,nrow = 6))

df_table[1,4:43] <- dfs[[1]][1,]
df_table[2,4:43] <- dfs[[1]][4,]
df_table[3,4:43] <- dfs[[2]][1,]
df_table[4,4:43] <- dfs[[2]][4,]
df_table[5,4:43] <- dfs[[3]][1,]
df_table[6,4:43] <- dfs[[3]][4,]

df_table[,3] <- c(rep(c("Optimistic","Conservative"),3))
df_table[,2] <- c(rep(c("1","75"),3))
df_table[,1] <- c(rep("BL vs. NPS",2),rep("WPS vs. BL",2),rep("WPS vs. NPS",2))

colnames(df_table) <- c("Comparison","Storage Period","DF Estimate",as.character(2013:2052))
write_xlsx(df_table,"results/GWP_DIFF.xlsx")

###

df_cum_table <- as.data.frame(matrix(NA,ncol=43,nrow = 6))

df_cum_table[1,4:43] <- dfs_cum[[1]][1,]
df_cum_table[2,4:43] <- dfs_cum[[1]][4,]
df_cum_table[3,4:43] <- dfs_cum[[2]][1,]
df_cum_table[4,4:43] <- dfs_cum[[2]][4,]
df_cum_table[5,4:43] <- dfs_cum[[3]][1,]
df_cum_table[6,4:43] <- dfs_cum[[3]][4,]

df_cum_table[,3] <- c(rep(c("Optimistic","Conservative"),3))
df_cum_table[,2] <- c(rep(c("1","75"),3))
df_cum_table[,1] <- c(rep("BL vs. NPS",2),rep("WPS vs. BL",2),rep("WPS vs. NPS",2))

colnames(df_cum_table) <- c("Comparison","Storage Period","DF Estimate",as.character(2013:2052))
write_xlsx(df_cum_table,"results/CUM_GWP_DIFF.xlsx")








