### get hold of 'webPageDF', it will be slicker in future
# git test
suppressWarnings(library(GGally))
suppressWarnings(library(network))
suppressWarnings(library(sna))
suppressWarnings(library(ggplot2))

#webPageDF$seriesSeason = with(webPageDF, paste(series, season))
#unseriesSeason = unique(webPageDF$seriesSeason)

unSeriesSeasonCircuit = raceDF %>%
						distinct(series, season, circuitPerim)
unSeriesSeasonCircuit$seriesSeason = with(unSeriesSeasonCircuit, paste(series, season))
unseriesSeason = unique(unSeriesSeasonCircuit$seriesSeason)

tmmatrix = matrix(0, nrow = length(unseriesSeason), ncol = length(unseriesSeason))
rownames(tmmatrix) = unseriesSeason
colnames(tmmatrix) = unseriesSeason

for (j in 1:length(unseriesSeason)) {
	for (k in 1:length(unseriesSeason)) {
		if (j != k) {
			overlap = with(unSeriesSeasonCircuit,
							intersect(circuitPerim[seriesSeason == unseriesSeason[j]],
										circuitPerim[seriesSeason == unseriesSeason[k]]))
			if (length(overlap) > 0) tmmatrix[j,k] = 1
		}
	}
}

net = network(tmmatrix, directed = FALSE)
network.vertex.names(net) = unseriesSeason
ggnet2(net, size = 12, label = TRUE, label.size = 5)

### GOAL
