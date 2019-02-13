###################################
####        KLASIFIKACIJA	#####
###################################
### setwd("C:/Users/sabina/Desktop/Faks/UI/seminarska")
###

source("funkcije.R")
podatki = read.table("regular.txt" , sep="," , header = T)

domaci_rezultat = vector()
domaci_rezultat[(podatki$HPTS-podatki$APTS)>0]=1
domaci_rezultat[(podatki$HPTS-podatki$APTS)<0]=0
podatki$domaci_rezultat = as.factor(domaci_rezultat)

##izracun povprecje tock zadnih 5 tekm in izgubljenih zog
# 2-> DATE
# 3-> HOME
# 4-> HPTS
# 15 ->HTOV
# 7 -> H3PM


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
formula = as.formula("domaci_rezultat ~ .")
formula2 = as.formula("domaci_rezultat ~ HDRB + ADRB + ATOV + HTOV + H3PM")

observed = testni$domaci_rezultat



### ODLOCITVENO DREVO
dt = rpart(formula, data = ucni)


predicted = predict(dt, testni, type="class")
CA(observed, predicted)


### Odlocitveno drevo na podlagi informacijskega prispevka je zelo slab model 

dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="InfGain")
dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="Gini")
dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="MDL")

dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="Relief")
dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="ReliefFequalK")
dt <- CoreModel(formula, ucni, model="tree", selectionEstimator="ReliefFexpRank")
### Ocenjevanje atributov
# Vse spodnje ocene so kratkovidne (ne morejo zaznati atributov v interakciji)
sort(attrEval(formula, podatki, "InfGain"), decreasing = TRUE)
sort(attrEval(formula, podatki, "Gini"), decreasing = TRUE)
sort(attrEval(formula, podatki, "MDL"), decreasing = TRUE)

# Ocene, ki niso kratkovidne (Relief in ReleifF)
sort(attrEval(formula, podatki, "Relief"), decreasing = TRUE)
sort(attrEval(formula, podatki, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(formula, podatki, "ReliefFexpRank"), decreasing = TRUE)


### GLASOVANJE

modelDT <- CoreModel(formula, ucni, model="tree")
modelRF <- CoreModel(formula, ucni, model="rf")
modelRFN <- CoreModel(formula, ucni, model="rfNear")
modelKNN <- CoreModel(formula, ucni, model="knn", kInNN = 20)

predDT <- predict(modelDT, testni, type = "class")
predRF <- predict(modelRF, testni, type="class")
predKNN <- predict(modelKNN, testni, type="class")
predRFN <- predict(modelRFN, testni, type="class")


pred <- data.frame(predDT, predKNN, predRF, predRFN)
predicted <- voting(pred)

cv.dt <- CA(testni$domaci_rezultat, predicted)
cv.dt


### BAGGING
bag = bagging(formula, ucni, nbagg=25)
bag.pred <- predict(bag, testni, type="class")

cv.dt <- CA(testni$domaci_rezultat, bag.pred)
cv.dt


### UMETNE NEVRONSKE MREZE

norm.data <- scale.data(podatki)
norm.learn <- norm.data[delitev,]
norm.test <- norm.data[-delitev,]

nn <- nnet(formula, data = norm.learn, size = 8, decay = 0.005, maxit = 1000)
predicted <- predict(nn, norm.test, type = "class")
cv.dt <- CA(testni$domaci_rezultat, predicted)
cv.dt



### KNN
# Za k od 5 do 25
modelKNN = CoreModel(formula, ucni, model="knn", kInNN = 15)
predKNN = predict(modelKNN, testni, type="class")
cv.dt = CA(testni$domaci_rezultat, predKNN)
cv.dt

