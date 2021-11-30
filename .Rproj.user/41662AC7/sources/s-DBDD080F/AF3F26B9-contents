#Laurin Gessler, Sarina Traub, Esra Akdeniz, Christoph Kiesl

library(UsingR)
library(tidyverse)
#Aufgabe 3
myBier <- read.csv("Data/myBier.txt", sep = " ")

#3 a)
pt=prop.table(table(myBier$Geschlecht,myBier$Bier),margin = 1)
pt.rand = addmargins(pt)
options(digits = 1)
ftable(pt.rand) #Erstellen der Kontigenztafel der bedingten Häufigkeiten

mosaicplot(table(myBier$Geschlecht,myBier$Bier)) #Erstellen eines Mosaikplots

chisq.test(table(myBier$Geschlecht,myBier$Bier))$statistic #Berechnung der chi-quadrat-größe

#korrigierter Kontingenzkoeffizient fehlt noch
#Interpretation des Ergebnisses


#3 b)
pt=prop.table(table(myBier$Wohnort,myBier$Bier), margin = 1)
pt.rand = addmargins(pt)
options(digits = 1)
ftable(pt.rand) #Erstellen der Kontigenztafel der bedingten Häufigkeiten

mosaicplot(table(myBier$Wohnort,myBier$Bier))#Erstellen eines Mosaikplots

chisq.test(table(myBier$Wohnort,myBier$Bier))$statistic #Berechnung der chi-quadrat-größe

#korrigierter Kontingenzkoeffizient fehlt noch
#Interpretation des Ergebnisses

#Augabe 4

#Aufgabe 5

chiq <-function(x){ #Funktion, um die Chi-Quadrat-Größe zu bestimmen
  t <- table(x[,1],x[,2])
  t <- addmargins(t)
  m <- length(t[1,])
  r <- length(t[,1])
  ergebnis <- 0
  for(j in 1:m){
    for(k in 1:r){
      njk <- (t[j,10]*t[10,k])/t[m,r]
      ergebnis <- ergebnis + (((t[j,k] - njk)^2)/njk)
    }
  }
  
  return(ergebnis)
}

ObereSchranke <- function(x){ #Funktion, um die obere Schranke zu bestimmen
  n <- sum(table(x))
  
  ergebnis <- n*(min(c(nlevels(x[,1]),nlevels(x[,2])))-1)#Formel aus dem Skript
    
  return(ergebnis)
}

KonKoe <- function(x){ #Funktion zur Berechnung des Kontigenzkoeffizienten
  n <- sum(table(x))
  C <- sqrt((chiq(x))/(chiq(x)+n)) #Formel aus dem Skript
  
  return(C)
}

korKonKoe <- function(x){#Funktion zur Berechnung des korrigierten Kontigenzkoeffizienten
  t <- table(x[,1],x[,2])
  t <- addmargins(t)
  m <- length(t[1,])
  r <- length(t[,1])
  
  C <- KonKoe(x)*sqrt(min(r,m)/(min(r,m)-1))#Formel aus dem Skript
  
  return(C)
}

myOrdinal <- function(x){
  
}

#Aufgabe 6 
load("Data/Wissen.Rdata")
#a) Man sieht, dass je mehr Forscher pro 1000 Beschäftigte, desto mehr Triadepatente je 1000000 Beschäftigte im Jahr 2002

#b)
var(Wissen$anzahl.forscher)
var(Wissen$anzahl.triadepatente)
cov(Wissen$anzahl.forscher, Wissen$anzahl.triadepatente)
cor(Wissen$anzahl.forscher, Wissen$anzahl.triadepatente)

#c)
z<-Wissen$anzahl.triadepatente
y<-(x/1000000)*1000
y

cor(Wissen$anzahl.forscher,y)

# Die Korrelationskoeffizienten bleiben identisch

