source("simulation.R")
source ("utils.R")

# Argument dimStateSpace je vektor iste dolzine kot opisi stanj, ki jih vraca funkcija getStateDesc. 
# Vsak element vektorja dimStateSpace doloca najvecjo vrednost, ki jo lahko zavzame istolezni element v opisu stanja.


# Mnozica stanj mora biti koncna in diskretna, zato morate upostevati naslednje omejitve:
# - vsa stanja morajo biti opisana z vektorjem enake dolzine
# - vsak element vektorja opisa mora biti pozitivno celo stevilo
#
# Zaradi hitrosti in zanesljivosti ucenja je zazeleno, da je razlicnih stanj cim manj!

#
# V tem primeru (nepopolnem) je v stanje zakodirana:
# - razdalja do najblizjega plenilca (navzgor omejena na 30), 
# - smer v kateri se nahaja ta plenilec in 
# - ali se agent nahaja na robu mape (vrednosti 1-4 označujejo ustrezne robove, vrednost 5 pa, da agent ni na robu)
#

getStateDesc <- function(simData, preyId)
{
	pos <- getPreyPos(simData, 1)

	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)
	distance <- cut(distance , c(-Inf, 10, 20, Inf), c(1,2,3))

	direction <- res[2]

	if (pos[2] == 1)
		border <- 1
	else if (pos[2] == MAPHEIGHT)
		border <- 2
	else if (pos[1] == MAPWIDTH)
		border <- 3
	else if (pos[1] == 1)
		border <- 4
	else
		border <- 5

	hidden <- isPreyHidden(simData, preyId)
	if (hidden){
		hidden <- 1
	} else {
		hidden <- 2
	}

	thirstL <- getPreyThirstLevel(simData, preyId)
	thirstL <- cut(thirstL, c(-Inf, floor(PREY_FATAL_THIRST / 3), floor(2 * PREY_FATAL_THIRST / 3), Inf), c(1,2,3))
	
	hungerL <- getPreyThirstLevel(simData, preyId)
	hungerL <- cut(hungerL, c(-Inf, floor(PREY_FATAL_HUNGER / 3), floor(2 * PREY_FATAL_HUNGER / 3), Inf), c(1,2,3))

	dForest <- getPreyDirectionToNearestForest(simData, preyId)
	dWater <- getPreyDirectionToNearestWater(simData, preyId)
	dGrass <- getPreyDirectionToNearestGrass(simData, preyId)

	c(distance, direction, border, hidden, dForest, thirstL, dWater, hungerL, dGrass)

	# c(distance, direction, border)

}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

#
# V tem primeru (nepopolnem) je nagrada definirana kot razdalja do najblizjega plenilca.
# Kaznuje se tudi akcija, ki premakne agenta v smeri najblizjega plenilca. 
#

getReward <- function(oldstate, action, newstate)
{
	reward <- ((newstate[1]-3) * 15)

	# lovc blizu pejt prot gozdu
	if (oldstate[1] <= 2 & action == newstate[5]){
		reward <- reward + 15
	}

	if (oldstate[2] == action & newstate[1] != 3) {
		reward <- reward - 40
	}

	if (oldstate[3] == action){ # glede zabijanja v zid
		reward <- reward - 10
	}
	if (newstate[3] != 5) {
		reward <- reward
	}

	# ali je skrit
	if (newstate[4] == 1){
		reward <- reward + 10
	}
	
	# če je žejn se premiki prot vodi
	if (oldstate[6] >= 2 & action != oldstate[7]){
		reward <- reward - 20
	}

	# če je lačn se premiki prot travi 130
	if (oldstate[8] >= 2 & action != oldstate[9]){
		reward <- reward - 20
	}

	# reward <- ((1 - newstate[5]) * 2)	

	reward	
}

