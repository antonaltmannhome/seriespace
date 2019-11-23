
USERPATH = 'c:/research/miscracing/seriespacecomparison/'
source(paste0(USERPATH, 'startup.r'))

dum = LoadData()
raceDF = dum$raceDF
qualiDF = dum$qualiDF
resultDF = dum$qualiDriverDF

#### how about this: for every session, fit model for all data not including that session. then, get hold of the list of all participants in the series that year. predict their qualy times. compare to actual. use fastest race lap to check predictions

seriesSeasonFullField = resultDF %>%
						group_by(series, season) %>%
						distinct(dtss)
# what's the situation with oval races?
# they're gone.

# think we can still cleanse out unhelpful stuff. drivers doing one off races contribute nothing (e.g le mans) so ditch them
# in fact 30% of all data is one-offs
# looking at imsa, dtss is bullshit. better off modelling the car i think, although we'd need to collect the entry data for that.
# also surely you can do outliers within sereis/season combo before combining everything?

resultDF$isValid = !is.na(resultDF$qualSec)
qualiDF$isValid = TRUE
while(TRUE) {

	# this has got a bug, will look into that later
	dum = GetDtssCircuitCoef(resultDF %>% filter(isValid))
	dtssCoefDF = dum$dtssCoefDF
	circuitPerimCoefDF = dum$circuitPerimCoefDF
	estSD = dum$estSD
	
	allModelList = NULL

	for (qi in which(qualiDF$isValid)) {
		
		fullField = inner_join(seriesSeasonFullField,
								qualiDF[qi,] %>%
									select(qualiId, series, season, circuitPerim),
								c('series', 'season')) %>%
								lazy_left_join(dtssCoefDF, 'dtss', 'dtssCoef') %>%
								left_join(circuitPerimCoefDF, 'circuitPerim') %>%
								mutate(predSec = exp(circuitPerimCoef + dtssCoef))
		# and then, what were the actual results
		fullField = lazy_left_join(fullField,
									resultDF,
									c('qualiId', 'dtss'),
									'qualSec')
		# haven't writtem fastestRaceSec to disk yet though. damn. will do that later.
		allModelList[[qi]] = fullField
		
		if ( (qi %% 10) == 0) {
			message('Have calculated ', qi, ' out of ', nrow(qualiDF), ' qualifying sessions')
		}
	}

	allModel = bind_rows(allModelList)

	# should you look at sessions or individual outliers?
	allModel = allModel %>%
				mutate(logLik = log(dnorm(log(qualSec), log(predSec), estSD)))

	logLikBySession = allModel %>%
						filter(!is.na(qualSec)) %>%
						group_by(qualiId, circuitPerim, series, season) %>%
						summarise(meanLogLik = mean(logLik),
									meanQualSec = mean(qualSec),
									meanPredSec = mean(predSec)) %>%
						ungroup()

	candidateWetSession = logLikBySession %>%
							filter(meanQualSec > meanPredSec) %>%
							filter(meanLogLik == min(meanLogLik)) %>%
							select(qualiId, series, season)
	# display to user, to check it seems sound
	dispInfo1 = logLikBySession %>%
				semi_join(candidateWetSession, c('series', 'season')) %>%
				mutate(isWet = (qualiId == candidateWetSession$qualiId))

	print(dispInfo1)
	message('Do you agree it is likely to be a wet session?')
	userYN = askcond(F, F)
	if (userYN == 'y') {
		resultDF$isValid[resultDF$qualiId == candidateWetSession$qualiId] = FALSE
		qualiDF$isValid[qualiDF$qualiId == candidateWetSession$qualiId] = FALSE
	}
}

# let's get median proportion out by session
propBySession = allModel %>%
				group_by(qualiId) %>%
				summarise(medianPropDiff = median(qualSec/ predSec, na.rm = TRUE))

### hmm, got 1.05s that are definitely wet, but 1.03s that are not. 

