source("RL.R")

# set.seed(63160171)


#
# Lahko eksperimentirate z razlicnimi nastavitvami simuliranega okolja
#

# MAPWIDTH <- 50
# MAPHEIGHT <- 50
# NUMPREYS <- 3
# NUMPREDATORS <- 3


# za podan (nepopoln) primer v datoteki "RL.R"
# predstavlja vektor c(30, 4, 5) najvecje vrednosti istoleznih elementov
# v opisu stanja

qmat <- qlearning(c(3, 4, 5, 2, 4, 3, 4, 3, 4), maxtrials=1000)
	

## ORIGINAL
# qmat <- qlearning(c(30, 4, 5), maxtrials=500) #
# load(file="qmat_ORG.RData")


# save(qmat, file="qmatZ.RData")
# load(file="qmat.RData")

set.seed(123456789)
simulation(Q=qmat)

# Za testeranje

#v <- c()

#set.seed(63160171)
#for (i in 1:20){
#	print(i)
#	flush.console()
#	v[i] <- simulation(Q=qmat)
#}

#mean(v)
#sd(v)


