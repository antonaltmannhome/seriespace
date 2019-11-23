
USERPATH = 'c:/research/miscracing/seriespacecomparison/'
source(paste0(USERPATH, 'startup.r'))

multiSeriesResultDF = read.csv(paste0(USERPATH, 'result.csv'), as.is = TRUE)
multiSeriesResultDF$driver = gsub('~', '-', multiSeriesResultDF$driver)
multiSeriesResultDF$driver = gsub('\\.', '', multiSeriesResultDF$driver)
multiSeriesResultDF$dtss = with(multiSeriesResultDF, paste(driver, vehicle, series, season, sep = '~'))
multiSeriesResultDF$circuit = with(multiSeriesResultDF, gsub('[^a-z]', ' ', tolower(circuit)))

f1ResultDF = read.csv(paste0(USERPATH, 'f1result1718.csv'), as.is = TRUE) %>%
				rename(season = year,
						qualSec = sec) %>%
				mutate(qualiId = paste(series, season, race, session, sep = '~'),
						dtss = paste(driver, team, series, season, sep = '~'))

resultDF = bind_rows(multiSeriesResultDF %>%
						select(dtss, series, season, qualSec, qualiId, circuit, perim),
					f1ResultDF %>%
						select(dtss, series, season, qualSec, qualiId, circuit, perim))

resultDF = resultDF %>%
			filter(!is.na(qualSec))

# a few perimeters need fixing
resultDF = resultDF %>%
			mutate(perim = case_when(
					circuit == 'laguna seca' & perim == '3602' ~ '3610',
					circuit == 'marina bay' & perim == '5063' ~ '5065',
					circuit == 'motegi' & perim == '4801.379' ~ '4801',
					circuit == 'phillip island' & perim == '4445' ~ '4448',
					circuit == 'silverstone' & perim == '5891' ~ '5901',
					circuit == 'silverstone' & perim == '5900' ~ '5901',
					circuit == 'spa francorchamps' & perim == '7003.9' ~ '7004',
					TRUE ~ as.character(perim)))

resultDF$circuitPerim = with(resultDF, paste(circuit, perim, sep = '~'))

mod = lm(log(qualSec) ~ factor(circuitPerim) + factor(dtss) - 1, data = resultDF)

resultDF$predSec = exp(predict(mod, resultDF))

# let's get vaguely nice coefs too
uglyDtssCoef = coef(mod)[grep('factor\\(dtss\\)', names(coef(mod)))]
dtssCoefDF = tibble::enframe(uglyDtssCoef, name = 'dtss', value = 'value')
dtssCoefDF$dtss = gsub('^.+\\)', '', dtssCoefDF$dtss)
dtssCoefDF = dtssCoefDF %>%
				tidyr::separate(dtss,
								c('driver', 'team', 'series', 'year'),
								sep = '~',
								remove = FALSE)


### now let's see which sessions are suspiciously slow

# or don't, just predict a lap time for everybody at sepang

fastestBySeriesYear = dtssCoefDF %>%
						group_by(series, year) %>%
						filter(value == min(value)) %>%
						arrange(value) %>%
						mutate(dtss = paste(driver, team, series, year, sep = '~'),
								circuitPerim = 'sepang~5543')
fastestBySeriesYear$predictedTime = exp(predict(mod, fastestBySeriesYear))

# starting to look like what we want but need to weed out the crap, not an easy job

# so, let's try to identify wet sessions.
# easiest way to identify is, the qual times are significantly slower than the fastestRaceSec
# after that, you get out-of-sample predicted times, taking out each race. then look at outliers, but determining what is an outlier might be tricky. but let's get the data in place

# actually fastestRaceSec isn't that easy to use. you might have a slower group of qualifiers, so fastestRaceSec isn't representative of what they could have done. should we model each series on its own to get out of samples for each sessions maybe. but i'm too tired to figure it out right now.

sessionSlowness = resultDF %>%
					group_by(qualiId) %>%
					summarise(medOE = median(qualSec / predSec))

# can we have an intercept for every session?
mod = lm(log(qualSec) ~ factor(qualiId) + factor(circuitPerim) + factor(dtss) - 1, data = resultDF)
# no, that's not useful either

#### how about this: for every session, fit model for all data not including that session. then, get hold of the list of all participants in the series that year. predict their qualy times. compare to actual. use fastest race lap to check predictions

### let's pick one of the weak sessions for sports cars for this

resultDF$oosPredSec = NA

#myQualiId = 'wec~2017~4~1'
myQualiId = 'f1~2017~2017italy~3'

allQualiId = resultDF %>% distinct(qualiId)
allModelList = NULL

for (qi in 1:nrow(allQualiId)) {
	myQualiId = allQualiId$qualiId[qi]
	myQSInfo = resultDF %>%
				filter(qualiId == myQualiId) %>%
				distinct(series, season, circuitPerim, qualiId)

	validResultDF = resultDF %>% filter(qualiId != myQualiId)
	modelMatrix = model.matrix(~ factor(circuitPerim) + factor(dtss) - 1,
									data = validResultDF)
	#mod = lm.fit(x = modelMatrix, y = log(validResultDF$qualSec)) # slow
	mod = speedglm::speedglm.wfit(y = log(validResultDF$qualSec),
									X = modelMatrix)
	uglyDtssCoef = coef(mod)[grep('factor\\(dtss\\)', names(coef(mod)))]
	names(uglyDtssCoef) = gsub('^.+\\)', '', names(uglyDtssCoef))
	missingDtssCoef = setdiff(validResultDF$dtss, names(uglyDtssCoef))
	uglyDtssCoef = c(0, uglyDtssCoef)
	names(uglyDtssCoef)[1] = missingDtssCoef
	dtssCoefDF = tibble::enframe(uglyDtssCoef, name = 'dtss', value = 'dtssCoef')
	dtssCoefDF = dtssCoefDF %>%
					tidyr::separate(dtss,
									c('driver', 'team', 'series', 'year'),
									sep = '~',
									remove = FALSE)
	uglyCircuitPerimCoef = coef(mod)[grep('circuitPerim', names(coef(mod)))]
	names(uglyCircuitPerimCoef) = gsub('^.+\\)', '', names(uglyCircuitPerimCoef))
	circuitPerimCoefDF = tibble::enframe(uglyCircuitPerimCoef,
											name = 'circuitPerim',
											value = 'circuitPerimCoef')

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

with(fullField, plot(predSec, qualSec))

# or (although this doesn't compare to the fastest race lap)

currentQualiIndex = with(resultDF, qualiId == myQualiId)
resultDF$oosPredSec[currentQualiIndex] =
	exp(predict(mod, resultDF[currentQualiIndex,]))
	
# interesting they're actually quicker than expected there, maybe there's another wet session slowing all the predictions down

# interesting, f1 monzao 2018 is properly slow using this
# can you use predict with lm.wfit though? if we want to properly cross validate the speed of this could get annoying
