###################################
####        REGRASIJA		#####
###################################
### setwd("C:/Users/sabina/Desktop/Faks/UI/seminarska")
###

source("funkcije.R")
podatki = read.table("regular.txt" , sep="," , header = T)

rezultat_razlika = vector()
rezultat_razlika = podatki$HPTS - podatki$APTS
podatki$rezultat_razlika = rezultat_razlika

povprecje5tekm = vector()
povprecjeHTOV = vector()
povprecjeH3PM = vector()
povprecjeHDRB = vector()
for(i in 1:nrow(podatki)){
	datum = podatki[i,2]  
	ime_ekipe=podatki[i,3]	
	tekme = podatki[podatki$DATE<datum & podatki$HOME ==ime_ekipe, 4]
	zoge = podatki[podatki$DATE<datum & podatki$HOME ==ime_ekipe, 15] 
	trojke = podatki[podatki$DATE<datum & podatki$HOME ==ime_ekipe, 7] 
	obramba = podatki[podatki$DATE<datum & podatki$HOME ==ime_ekipe, 12] 

	if(length(tekme)>5){
		pov_tekm = sum(tail(tekme, 5)) / 5
		pov_zog = sum(tail(zoge, 5)) / 5
		pov_trojk = sum(tail(trojke, 5)) / 5
		pov_obrambe = sum(tail(obramba, 5)) / 5

	}
	else{
		pov_tekm = mean(tekme)
		pov_zog = mean(zoge)
		pov_trojk = mean(trojke)
		pov_obrambe = mean(obramba)
	}
	povprecje5tekm[length(povprecje5tekm)+1] = pov_tekm
	povprecjeHTOV[length(povprecjeHTOV)+1] = pov_zog
	povprecjeH3PM[length(povprecjeH3PM)+1] = pov_trojk 
	povprecjeHDRB[length(povprecjeHDRB)+1] = pov_obrambe
}
podatki$povprecje5tekm = povprecje5tekm
podatki$povprecjeHTOV = povprecjeHTOV
podatki$povprecjeH3PM = povprecjeH3PM
podatki$povprecjeHDRB = povprecjeHDRB


podatki = na.omit(podatki)
podatki$HOME = NULL
podatki$HPTS = NULL
podatki$AWAY = NULL
podatki$APTS = NULL
podatki$DATE = NULL
podatki$SEASON = NULL

delitev = sample(1:nrow(podatki), size = as.integer(nrow(podatki) * 0.7), replace=F)

ucni = podatki[delitev, ]
testni = podatki[-delitev, ]


#formule
formula = as.formula("rezultat_razlika ~ .")
predicted<-predict(lm.model,test)

