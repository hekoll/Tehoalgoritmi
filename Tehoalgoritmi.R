# LASNAOLOKOODI

# 4.6.2018 Helena Ollila

# Datan rakenteesta: (Data muokattu SAS, Tiedosto: Tarvittava data R)

# 1) Ajanhetket ja timesumma Sas aikana eli min*60
# 2) Valitaan vain osastolla lasna rivit TYPE=1 ja TYPE=4 
# 3) Puuttuvat ajanhetket korvattu numerolla -999
# 4) Data jarjestetty aina niin etta aidin lasnaolo ensin
# 5) Puuttuvat osallistumistiedot poistettu

#########################################################################
                              # KOODI #
########################################################################

setwd("C:/Users/hekoll/Desktop/Siirretty verkkolevylle päivityksen ajaksi/Kotona/Löyttyniemi Eliisa, Tehoalgoritmi")

library(rlist)
library(sas7bdat)
library(dplyr)
library(foreign)

install.packages("rlist")
install.packages("sas7bdat")
install.packages("dplyr")

Data <- read.sas7bdat("data_27052019.sas7bdat")

# Tulosmatriisissa aina ID, DAY ja paivan lasnaoloaika

datanPituus <- length(Data[,1])
tulos <- matrix(nrow = datanPituus, ncol = 3)

# Uusi muuttuja, PAIKALLA = Kuka on ollut sina paivana paikalla: 1 = Aiti, 2 = Isa, 3 = Molemmat 

Data$PAIKALLA <- numeric(length = datanPituus)

Data$uusiID<- as.character(Data$ID)
Data$uusiDay <- as.character(Data$DAY)
Data$uusiDay <- as.integer(paste0(Data$uusiID, Data$uusiDay))

for(i in 2:datanPituus){
  if(Data$uusiDay[i-1] == Data$uusiDay[i]){ 
    Data$PAIKALLA[i] <- 3
    Data$PAIKALLA[i-1] <- 3
  } else if(Data$TYPE[i] == 1){
    Data$PAIKALLA[i] = 1  
  } else if(Data$TYPE[i] == 4){
    Data$PAIKALLA[i] = 2  
  }  
}

# Taulukko, jossa vertailua varten ajat

Ajat <- Data[,c(60,3,4,5:46)]
Ajat <- Ajat[ -c(6,9,12,15,18,21,24,27,30,33,36,39,42,45) ]
Aloitusajat <- Ajat[,c(4,6,8,10,12,14,16,18,20,22,24,26,28,30)]
Lopetusajat <- Ajat[,c(5,7,9,11,13,15,17,19,21,23,25,27,29,31)]

# FOR silmukka, joka laskee jokaiselle päivälle yhteensä lasnaoloajan

for(i in 1:datanPituus){

  # Vain aiti osallistuu tutkimukseen -> Vain aidin aika 
  
  if(Data$PAR[i] == 1){
    tulos[i,1] <- Data$ID[i]
    tulos[i,2] <- Data$DAY[i]
    tulos[i,3] <- Data$timesummaSAS[i]
  }
  
  # Vain isa osallistuu tutkimukseen -> Vain isan aika
  
  else if(Data$PAR[i] == 2){
    tulos[i,1] <- Data$ID[i]
    tulos[i,2] <- Data$DAY[i]
    tulos[i,3] <- Data$timesummaSAS[i]
  }

  # Molemmat osallistuvat tutkimukseen
  
  else if(Data$PAR[i] == 3){
    
    # Vain aiti on ollut sina paivana paikalla -> Vain aidin aika
    
    if(Data$PAIKALLA[i] == 1){
      tulos[i,1] <- Data$ID[i]
      tulos[i,2] <- Data$DAY[i]
      tulos[i,3] <- Data$timesummaSAS[i]
    }
    
    # Vain isa on ollut sina paivana paikalla -> Vain isan aika
    
    else if(Data$PAIKALLA[i] == 2){
      tulos[i,1] <- Data$ID[i]
      tulos[i,2] <- Data$DAY[i]
      tulos[i,3] <- Data$timesummaSAS[i]
    }
    
    # Molemmat olleet sina paivana paikalla
    
    else if(Data$PAIKALLA[i] == 3){
      
      if(i != 1 ){
        if(Data$uusiDay[i]==Data$uusiDay[i-1]){ # Tehdaan vertailu kahden rivin valilla 
          next                                  # vain kerran (jos vertailu tehty, hypataan yli)
        }
      }
      
      isarivi <- rep(2,14)  # Muodostetaan matriisi, jossa kellonajat ja tieto siita, 
      aitirivi <- rep(1,14) # onko kyseessa saapuminen vai poistuminen ja
      saapuu <- rep(1,14)   # onko kyseessä aiti vai isa
      poistuu <- rep(0,14)
      
      aitisaapuu <- mapply(list, Aloitusajat[i,], saapuu, aitirivi )
      aitipoistuu <- mapply(list, Lopetusajat[i,], poistuu, aitirivi)
      isasaapuu <- mapply(list, Aloitusajat[i+1,], saapuu, isarivi)
      isapoistuu <-mapply(list, Lopetusajat[i+1,], poistuu, isarivi)
      
      lista <- cbind(aitisaapuu, aitipoistuu, isasaapuu, isapoistuu)
      listanPituus <- length(lista[1,])
      lista <-  t(matrix(unlist(lista), ncol =listanPituus , byrow = FALSE)) 
      lista <- lista[order(lista[,1], decreasing = FALSE),]
      a <- 1
      lista <- t(lista)
      uusilista <- lista
      
      for (j in 1:listanPituus){ # Poistetaan listasta puuttuvat aikavalit eli kaikki -999 arvot
        if (lista[1,j] == -999){
          uusilista <- uusilista[,-1]
        }
      }
            
      alku <- uusilista[1,1] 
      aiti <- FALSE
      isa <- FALSE
      vastaus <- 0
        
      for(j in 1:length(uusilista[1,])){ # Kaydaan muodostettua matriisia lapi, asetetaan alkuaika kun toinen saapuu ja lasketaan erotus
                                         # kun molemmat poistuneet, tallennetaan paivan kokonaisaikaa muuttujaan vastaus
        if(uusilista[2,j] == 1 && uusilista[3,j] == 2){ # Isa saapuu
          isa <- TRUE
        } 
        else if(uusilista[2,j] == 1 && uusilista[3,j] == 1){ # Aiti saapuu
          aiti <- TRUE
        }
        else if(uusilista[2,j] == 0 && uusilista[3,j] == 2) { # Isa poistuu
          isa <- FALSE
        }
        else if (uusilista[2,j] == 0 && uusilista[3,j] == 1){ # Aiti poistuu
          aiti <- FALSE
        }
        if(aiti== FALSE && isa == FALSE){ # Molemmat poistuneet, lasketaan valin pituus
          erotus <- uusilista[1,j] - alku
          vastaus <- vastaus + erotus
          
          if(j == length(uusilista[1,])){ # Lopetetaan silmukan kayminen lapi, kun matriisi loppuu tai asetetaan uusi alkuaika
            break
          } else {
          alku <- uusilista[1,j+1]
          }
        }
          
      } # LISTA FOR LOOPIN SULKU
        
      tulos[i,1] <- Data$ID[i] # Tallennetaan vastaus tulos matriisiin
      tulos[i,2] <- Data$DAY[i]
      tulos[i,3] <- vastaus
      tulos[i+1,1] <- Data$ID[i]
      tulos[i+1,2] <- Data$DAY[i]
      tulos[i+1,3] <- vastaus
      
    } # IF PAIKALLA ==3 SULKU
  } # IF PAR == 3 SULKU
  
} # KOKO DATAN FOR LOOPIN SULKU
      
# Suhdeluvun laskeminen yhdelle ID sisalla

Tulos <- as.data.frame(tulos)
Tulos <- Tulos[!duplicated(Tulos), ] # Poistetaan duplikaatit

Tulos$Oltuaika <- numeric(length = length(Tulos[,1]))
Tulos$Totalaika <- numeric(length = length(Tulos[,1]))

Tulos <-        # Lasketaan yhteensa lasnaoloaika ja totalaika (mahdollinen lasnaoloaika) yhden ID sisalla
  Tulos %>%
  group_by(V1) %>%
  mutate(Oltuaika = sum(V3)/60) %>%
  mutate(Totalaika = length(V2)*24*60)

Tulos$Läsnä_suhde <- Tulos$Oltuaika/Tulos$Totalaika # Lasketaan suhde

LoppuTulos <- Tulos[,c(1,6)]
LoppuTulos <- LoppuTulos[!duplicated(LoppuTulos), ]
colnames(LoppuTulos)[1] <- "ID"

write.foreign(LoppuTulos, "C:/Users/hekoll/Desktop/Siirretty verkkolevylle päivityksen ajaksi/Kotona/Löyttyniemi Eliisa, Tehoalgoritmi/LoppuTulos_28052019.txt",
              "C:/Users/hekoll/Desktop/Siirretty verkkolevylle päivityksen ajaksi/Kotona/Löyttyniemi Eliisa, Tehoalgoritmi/LoppuTulos_28052019.sas", package="SAS")
