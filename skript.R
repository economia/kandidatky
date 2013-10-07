/// načtení

df <- read.csv("kandidati.csv", sep=";")
df$Jmeno <- paste(df$Jmeno2, df$Jmeno1, "")
df$Pohlavi <- grepl("ová |ská |cká ", df$Jmeno)

muzizeny <- c(sum(!df$Pohlavi), sum(df$Pohlavi))
aggregate(Pohlavi ~ Strana, data=df, FUN=T)



/// pohlaví

zenybystrana <- aggregate(Pohlavi ~ Strana, data=df[df$Pohlavi==T,], length)
muzibystrana <- aggregate(Pohlavi ~ Strana, data=df[df$Pohlavi==F,], length)
colnames(zenybystrana) <- c("Strana", "PocetZ")
colnames(muzibystrana) <- c("Strana", "PocetM")
pohlavi <- merge(muzibystrana, zenybystrana)
pohlavi[23,] <- c("RDS", 5, 0)
pohlavi <- pohlavi[order(pohlavi$Strana),]
colnames(pohlavi) <- c("Strana", "Muzi", "Zeny")
pohlavi$Muzi <- as.numeric(pohlavi$Muzi)
pohlavi$Zeny <- as.numeric(pohlavi$Zeny)
pohlavi$ZenyProc <- pohlavi$Zeny/(pohlavi$Zeny + pohlavi$Muzi)
rm(muzibystrana)
rm(zenybystrana)
pohlavi <- pohlavi[order(pohlavi$ZenyProc, decreasing=T),]



/// příslušnost

sbystrana <- aggregate(Clen ~ Strana, data=df[df$Clen!="BEZPP",], length)
bezbystrana <- aggregate(Clen ~ Strana, data=df[df$Clen=="BEZPP",], length)
colnames(sbystrana) <- c("Strana", "VeStrane")
colnames(bezbystrana) <- c("Strana", "BezStrany")
clenstvi <- merge(sbystrana, bezbystrana)
clenstvi[23,] <- c("RDS", 5, 0)
clenstvi <- clenstvi[order(clenstvi$Strana),]
colnames(clenstvi) <- c("Strana", "VeStrane", "BezStrany")
clenstvi$VeStrane <- as.numeric(clenstvi$VeStrane)
clenstvi$BezStrany <- as.numeric(clenstvi$BezStrany)
clenstvi$StraniciProc <- clenstvi$VeStrane/(clenstvi$BezStrany + clenstvi$VeStrane)
rm(bezbystrana)
rm(sbystrana)
clenstvi <- clenstvi[order(clenstvi$StraniciProc, decreasing=T),]



/// Věk

vek <- aggregate(Vek ~ Strana, data=df, FUN=mean)
vek <- vek[order(vek$Vek, decreasing=T),]



/// tituly

tituly <- as.data.frame(pohlavi$Strana)
colnames(tituly) <- "Strana"
seznam <- unique(c(levels(df$Titul1), levels(df$Titul2), levels(df$Titul3), levels(df$Titul4)))
seznam[1] <- "-"
tituly[,seznam] <- rep(0)

for (i in 1:nrow(df)) {
  strana <- df$Strana[i]  
  if (df[i,6] == "") tituly[tituly$Strana == strana, 2] <- tituly[tituly$Strana == strana, 2] + 1 
  for (j in 1:5) { 
    for (k in 3:50) {
      if (df[i,5+j] == colnames(tituly)[k]) {
        tituly[tituly$Strana == strana, k] <- tituly[tituly$Strana == strana, k] + 1
        print(paste(i, strana))
      }
    }    
  }
}
rm(i)
rm(j)
rm(k)
rm(seznam)
rm(strana)



/// grafy - bez titulů

library("RColorBrewer")
barva.vek <- c(rev(brewer.pal(9, "Reds")), rep("White", 5), brewer.pal(9, "Greens"))
barva.pohlavi <- c(rev(brewer.pal(9, "Reds")), rep("White", 5), brewer.pal(9, "Blues"))

barplot(vek$Vek, names.arg=vek$Strana, las=2, col=barva.vek, ylim=c(0,68), main="Průměrný věk kandidátky")
text(1:23*1.19-0.3, vek$Vek, round(vek$Vek, 1), pos=3, cex=0.6)
# text(1:23*1.19-0.3, 2, (clenstvi$VeStrana+clenstvi$BezStrany)[order(vek$Vek, decreasing=T)], cex=0.6, col="white")

barplot(pohlavi$ZenyProc*100, names.arg=pohlavi$Strana, las=2, col=barva.pohlavi, ylim=c(0,75), ylab="%", main="Zastoupení žen na kandidátce")
text(1:23*1.19-0.3, pohlavi$ZenyProc*100, paste(round(pohlavi$ZenyProc*100, 1),"%"), pos=3, cex=0.6)
text(1:22*1.19-0.3, 2, pohlavi$Muzi+pohlavi$Zeny, cex=0.6)

barplot(clenstvi$StraniciProc*100, names.arg=clenstvi$Strana, las=2, col=barva, ylim=c(0,110), ylab="%", main="Zastoupení straníků na kandidátce")
text(1:23*1.19-0.3, clenstvi$StraniciProc*100, paste(round(clenstvi$StraniciProc*100, 1),"%"), pos=3, cex=0.6, col=barva)
text(1:23*1.19-0.3, 2, clenstvi$VeStrane+clenstvi$BezStrany, cex=0.6, col="white")




/// grafy - tituly


// ČSSD

CSSD <- tituly[3,]
CSSD <- CSSD[,-1]
CSSD <- CSSD[,order(CSSD, decreasing=T)]
barplot(t(t(CSSD/nrow(df[df$Strana=="ČSSD",])*100))[1:11], names.arg=colnames(CSSD)[1:11], las=2, ylim=c(0,max(CSSD/nrow(df[df$Strana=="ČSSD",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v ČSSD")
text(1:11*1.19-0.5, t(t(CSSD/nrow(df[df$Strana=="ČSSD",])*100))[1:11], paste(round(t(t(CSSD/nrow(df[df$Strana=="ČSSD",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// KSČM

KSCM <- tituly[9,]
KSCM <- KSCM[,-1]
KSCM <- KSCM[,order(KSCM, decreasing=T)]
barplot(t(t(KSCM/nrow(df[df$Strana=="KSČM",])*100))[1:11], names.arg=colnames(KSCM)[1:11], las=2, ylim=c(0,max(KSCM/nrow(df[df$Strana=="KSČM",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v KSČM")
text(1:11*1.19-0.5, t(t(KSCM/nrow(df[df$Strana=="KSČM",])*100))[1:11], paste(round(t(t(KSCM/nrow(df[df$Strana=="KSČM",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// ODS

ODS <- tituly[12,]
ODS <- ODS[,-1]
ODS <- ODS[,order(ODS, decreasing=T)]
barplot(t(t(ODS/nrow(df[df$Strana=="ODS",])*100))[1:11], names.arg=colnames(ODS)[1:11], las=2, ylim=c(0,max(ODS/nrow(df[df$Strana=="ODS",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v ODS")
text(1:11*1.19-0.5, t(t(ODS/nrow(df[df$Strana=="ODS",])*100))[1:11], paste(round(t(t(ODS/nrow(df[df$Strana=="ODS",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// TOP 09

TOP <- tituly[21,]
TOP <- TOP[,-1]
TOP <- TOP[,order(TOP, decreasing=T)]
barplot(t(t(TOP/nrow(df[df$Strana=="TOP 09",])*100))[1:11], names.arg=colnames(TOP)[1:11], las=2, ylim=c(0,max(TOP/nrow(df[df$Strana=="TOP 09",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v TOP 09")
text(1:11*1.19-0.5, t(t(TOP/nrow(df[df$Strana=="TOP 09",])*100))[1:11], paste(round(t(t(TOP/nrow(df[df$Strana=="TOP 09",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// KDU-ČSL

KDU <- tituly[8,]
KDU <- KDU[,-1]
KDU <- KDU[,order(KDU, decreasing=T)]
barplot(t(t(KDU/nrow(df[df$Strana=="KDU-ČSL",])*100))[1:11], names.arg=colnames(KDU)[1:11], las=2, ylim=c(0,max(KDU/nrow(df[df$Strana=="KDU-ČSL",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v KDU-ČSL")
text(1:11*1.19-0.5, t(t(KDU/nrow(df[df$Strana=="KDU-ČSL",])*100))[1:11], paste(round(t(t(KDU/nrow(df[df$Strana=="KDU-ČSL",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// ANO 2011

ANO <- tituly[2,]
ANO <- ANO[,-1]
ANO <- ANO[,order(ANO, decreasing=T)]
barplot(t(t(ANO/nrow(df[df$Strana=="ANO 2011",])*100))[1:11], names.arg=colnames(ANO)[1:11], las=2, ylim=c(0,max(ANO/nrow(df[df$Strana=="ANO 2011",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v ANO 2011")
text(1:11*1.19-0.5, t(t(ANO/nrow(df[df$Strana=="ANO 2011",])*100))[1:11], paste(round(t(t(ANO/nrow(df[df$Strana=="ANO 2011",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// SPOZ

SPOZ <- tituly[16,]
SPOZ <- SPOZ[,-1]
SPOZ <- SPOZ[,order(SPOZ, decreasing=T)]
barplot(t(t(SPOZ/nrow(df[df$Strana=="SPOZ",])*100))[1:11], names.arg=colnames(SPOZ)[1:11], las=2, ylim=c(0,max(SPOZ/nrow(df[df$Strana=="SPOZ",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v SPOZ")
text(1:11*1.19-0.5, t(t(SPOZ/nrow(df[df$Strana=="SPOZ",])*100))[1:11], paste(round(t(t(SPOZ/nrow(df[df$Strana=="SPOZ",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// Úsvit

USVIT <- tituly[22,]
USVIT <- USVIT[,-1]
USVIT <- USVIT[,order(USVIT, decreasing=T)]
barplot(t(t(USVIT/nrow(df[df$Strana=="Úsvit",])*100))[1:11], names.arg=colnames(USVIT)[1:11], las=2, ylim=c(0,max(USVIT/nrow(df[df$Strana=="Úsvit",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů v Úsvitu")
text(1:11*1.19-0.5, t(t(USVIT/nrow(df[df$Strana=="Úsvit",])*100))[1:11], paste(round(t(t(USVIT/nrow(df[df$Strana=="Úsvit",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)


// SZ

SZ <- tituly[20,]
SZ <- SZ[,-1]
SZ <- SZ[,order(SZ, decreasing=T)]
barplot(t(t(SZ/nrow(df[df$Strana=="SZ",])*100))[1:11], names.arg=colnames(SZ)[1:11], las=2, ylim=c(0,max(SZ/nrow(df[df$Strana=="SZ",]))*1.2*100), col=barva, ylab="%", main="Zastoupení akademických titulů ve SZ")
text(1:11*1.19-0.5, t(t(SZ/nrow(df[df$Strana=="SZ",])*100))[1:11], paste(round(t(t(SZ/nrow(df[df$Strana=="SZ",])*100))[1:11],1),"%"), pos=3, cex=0.6, col=barva)