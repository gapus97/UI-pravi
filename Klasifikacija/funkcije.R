CA <- function(observed, predicted)
{
	t <- table(observed, predicted)

	sum(diag(t)) / sum(t)
}

voting <- function(predictions)
{
	res <- vector()

  	for (i in 1 : nrow(predictions))  	
	{
		vec <- unlist(predictions[i,])
    		res[i] <- names(which.max(table(vec)))
  	}

  	factor(res, levels=levels(predictions[,1]))
}
scale.data <- function(data)
{
	norm.data <- data

	for (i in 1:ncol(data))
	{
		if (!is.factor(data[,i]))
			norm.data[,i] <- scale(data[,i])
	}
	
	norm.data
}

mae <- function(observed, predicted)
{
	mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
	sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
	mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
	sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}


izrisiDistribucijoTock <- function(ime_ekipe, df = podatki){
  tekme <- podatki[podatki$HOME==ime_ekipe & podatki$SEASON=="2016-17", ]
  tocke <- (tekme$HPTS)
  hist(tocke,main="Distribucija tock 2016",xlab=ime_ekipe)
}
