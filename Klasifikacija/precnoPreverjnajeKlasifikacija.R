###############################################
#### PRECNO PREVERJANJE KLASIFIKACIJA	#####
###############################################
### setwd("C:/Users/sabina/Desktop/Faks/UI/seminarska")
### library( , lib.loc="C:/Users/sabina/Desktop/Faks/UI/knjiznica")



### KNN

n = nrow(podatki)
k = 5
bucket.id = rep(1:k, length.out=n)
s = sample(1:n, n, FALSE)
bucket.id = bucket.id[s]
formula = as.formula("domaci_rezultat ~.")


cv.dt <- vector()
for (i in 1:k)
{	
	print(paste("Processing fold", i))
	flush.console()

	sel = bucket.id == i	
	learn = podatki[!sel,]
	test = podatki[sel,]
	observed = podatki[sel,]$domaci_rezultat

	modelKNN = CoreModel(formula, learn, model="knn", kInNN = 15)
	predKNN = predict(modelKNN, test, type="class")
	cv.dt[i] = CA(observed, predKNN)
}

mean(cv.dt)



### Glasovanje


n <- nrow(podatki)
k <- 15
bucket.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
bucket.id <- bucket.id[s]
formula = as.formula("domaci_rezultat ~.")


cv.dt <- vector()
for (i in 1:k)
{	
	print(paste("Processing fold", i))
	flush.console()

	sel <- bucket.id == i	
	learn <- podatki[!sel,]
	test <- podatki[sel,]
	observed <- podatki[sel,]$domaci_rezultat

	
	modelDT <- CoreModel(formula, learn, model="tree")
	modelRF <- CoreModel(formula, learn, model="rf")
	modelRFN <- CoreModel(formula, learn, model="rfNear")
	modelKNN <- CoreModel(formula, learn, model="knn", kInNN = 20)

	predDT <- predict(modelDT, test, type = "class")
	predRF <- predict(modelRF, test, type="class")
	predKNN <- predict(modelKNN, test, type="class")
	predRFN <- predict(modelRFN, test, type="class")


	pred <- data.frame(predDT, predKNN, predRF, predRFN)
	predicted <- voting(pred)

	
	cv.dt[i] <- CA(observed, predicted)
}

mean(cv.dt)

### Bagging

n <- nrow(podatki)
k <- 5
bucket.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
bucket.id <- bucket.id[s]
formula = as.formula("domaci_rezultat ~.")

cv.dt <- vector()
for (i in 1:k)
{	
	print(paste("Processing fold", i))
	flush.console()

	sel <- bucket.id == i	
	learn <- podatki[!sel,]
	test <- podatki[sel,]
	observed <- podatki[sel,]$domaci_rezultat

	bag <- bagging(formula, learn, nbagg=25)
	bag.pred <- predict(bag, test, type="class")
	
	cv.dt[i] <- CA(observed, bag.pred)
}

mean(cv.dt)


### UMETNE NEVRONSKE MREZE



norm.data <- scale.data(podatki)
norm.learn <- norm.data[delitev,]
norm.test <- norm.data[-delitev,]

n <- nrow(podatki)
k <- 5
bucket.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
bucket.id <- bucket.id[s]
formula = as.formula("domaci_rezultat ~.")


cv.dt <- vector()
for (i in 1:k)
{	
	print(paste("Processing fold", i))
	flush.console()

	sel <- bucket.id == i	
	learn <- norm.data[!sel,]
	test <- norm.data[sel,]
	observed <- norm.data$domaci_rezultat

	nn <- nnet(formula, data = learn, size = 8, decay = 0.005, maxit = 10000)
	predicted <- predict(nn, norm.data, type = "class")	
	
	cv.dt[i] <- CA(observed, predicted)
}

mean(cv.dt)

