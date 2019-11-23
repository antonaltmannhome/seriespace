
USERPATH = 'c:/research/miscracing/seriespacecomparison/'
source(paste0(USERPATH, 'startup.r'))

dum = LoadData()
raceDF = dum$raceDF
qualiDF = dum$qualiDF
resultDF = dum$qualiDriverDF

#### how about this: for every session, fit model for all data not including that session. then, get hold of the list of all participants in the series that year. predict their qualy times. compare to actual. use fastest race lap to check predictions

### let's pick one of the weak sessions for sports cars for this

resultDF$oosPredSec = NA

#myQualiId = 'wec~2017~4~1'
myQualiId = 'f1~2017~2017italy~3'

allModelList = NULL

for (qi in 1:nrow(qualiDF)) {

	validResultDF = resultDF %>% filter(qualiId != qualiDF$qualiId[qi])
	dum = GetDtssCircuitCoef(validResultDF)
	dtssCoefDF = dum$dtssCoefDF
	circuitPerimCoefDF = dum$circuitPerimCoefDF
	
	fullField = semi_join(resultDF,
							myQSInfo,
							c('series', 'season')) %>%
							distinct(dtss) %>%
							lazy_left_join(dtssCoefDF, 'dtss', 'dtssCoef')
	# but we want the circuit/session info as well
	fullField = cbind(fullField, myQSInfo) %>%
							left_join(circuitPerimCoefDF, 'circuitPerim') %>%
							mutate(predSec = exp(circuitPerimCoef + dtssCoef))
	# and then, what were the actual results
	fullField = lazy_left_join(fullField,
								resultDF,
								c('qualiId', 'dtss'),
								'qualSec')
	# haven't writtem fastestRaceSec to disk yet though. damn. will do that later.
	allModelList[[qi]] = fullField
	
	message('Have calculated ', qi, ' out of ', nrow(allQualiId), ' qualifying sessions')
}

allModel = bind_rows(allModelList)

# there must be some sort of offsetting that we can do to speed up the lms. e.g surely the dtsses for other series can all be anchored to the original lm, then we can loop through each series and filter out obvious wet sessions
# not sure i agree with that, the offset could be wrong due to wet sessions, who knows how seriously that could hurt the series in focus?

# really don't think we need to cross validate like this. think the full field thing along with using the fastestRaceSec should be suitable

# can't do fastestRaceSec thing til we've added it to f1 data, in the meantime let's ditch the crossv

resultDF$isValid = TRUE
while(TRUE) {

	# this has got a bug, will look into that later
	dum = GetDtssCircuitCoef(resultDF %>% filter(isValid))
	dtssCoefDF = dum$dtssCoefDF
	circuitPerimCoefDF = dum$circuitPerimCoefDF
	estSD = dum$estSD

	seriesSeasonFullField = resultDF %>%
							group_by(series, season) %>%
							distinct(dtss)
	
	allModelList = NULL

	for (qi in 1:nrow(qualiDF)) {
		
		fullField = inner_join(seriesSeasonFullField,
								qualiDF[qi,],
								c('series', 'season')) %>%
								lazy_left_join(dtssCoefDF, 'dtss', 'dtssCoef')
		# but we want the circuit/session info as well
		fullField = cbind(fullField, myQSInfo) %>%
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
			message('Have calculated ', qi, ' out of ', nrow(allQualiId), ' qualifying sessions')
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
		resultDF$isValid[resultDF$qualiId == candidateWetSession$qualiId] = F
	}
}

# let's get median proportion out by session
propBySession = allModel %>%
				group_by(qualiId) %>%
				summarise(medianPropDiff = median(qualSec/ predSec, na.rm = TRUE))

### hmm, got 1.05s that are definitely wet, but 1.03s that are not. 

