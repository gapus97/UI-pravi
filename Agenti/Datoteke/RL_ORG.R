getStateDesc <- function(simData, preyId)
{
	pos <- getPreyPos(simData, 1)

	res <- getPreyDistAndDirectionToNearestPredator(simData, preyId)
	distance <- max(res[1], 1) 
	distance <- min(distance, 30)

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

	
	c(distance, direction, border)
}

# Rezultat funkcije je nagrada (ali kazen), ki jo agent sprejme v opisani situaciji.
# Nagrada mora spodbujati agenta, da izvaja koristne akcije oziroma ga odvracati od negativnih akcij

#
# V tem primeru (nepopolnem) je nagrada definirana kot razdalja do najblizjega plenilca.
# Kaznuje se tudi akcija, ki premakne agenta v smeri najblizjega plenilca. 
#

getReward <- function(oldstate, action, newstate)
{
	reward <- (newstate[1]-30)

	if (oldstate[3] == action)
		reward <- reward - 10

	reward	
}