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
