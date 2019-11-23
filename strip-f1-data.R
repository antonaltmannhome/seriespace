# we've already got f1 lap times, let's write them in multi-series friendly format

LoadAllData()
fastestRaceSecDF = lbl %>%
					group_by(race) %>%
					summarise(fastestRaceSec = min(sec))
numQualiSessionDF = qualifyingSessionDF %>%
					group_by(race) %>%
					summarise(numQual = n())
raceDFPlus = raceDF %>%
			left_join(fastestRaceSecDF, 'race') %>%
			left_join(numQualiSessionDF, 'race') %>%
			mutate(series = 'f1') %>%
			group_by(series, year) %>%
			arrange(date) %>%
			mutate(raceNumber = 1:n()) %>%
			ungroup() %>%
			rename(season = year) # %>%
			# select(raceNumber, series, year, circuit, perim, fastestRaceSec, numQual)

qualiDF = qualifyingSessionDF %>%
			lazy_left_join(raceDFPlus, 'race', c('raceNumber', 'series')) %>%
			mutate(qualiId = paste(series, year, raceNumber, session, sep = '~')) %>%
			rename(season = year, qualiSession = session)

qualiDriverDF = qdf %>%
				rename(qualiSession = session) %>%
				lazy_left_join(qualiDF, c('race', 'qualiSession'), 'qualiId') %>%
				lazy_left_join(driverDF, 'driver', 'forixId') %>%
				lazy_left_join(rddf, c('race', 'driver'), 'team') %>%
				rename(qualSec = sec, vehicle = team, drivId = forixId) %>%
				mutate(drivId = as.character(drivId))

raceFileOut = paste0('c:/research/miscracing/seriespacecomparison/f1-race.csv')
qualiFileOut = paste0('c:/research/miscracing/seriespacecomparison/f1-quali.csv')
qualiDriverFileOut = paste0('c:/research/miscracing/seriespacecomparison/f1-qualidriver.csv')
write.csv(file = raceFileOut,
			raceDFPlus %>%
			select(circuit, perim, raceNumber, series, season, numQual, fastestRaceSec),
			row.names = FALSE)
write.csv(file = qualiFileOut,
			qualiDF %>%
			select(series, season, raceNumber, qualiSession, qualiId),
			row.names = FALSE)
write.csv(file = qualiDriverFileOut,
			qualiDriverDF %>%
			select(driver, drivId, vehicle, qualSec, qualiId),
			row.names = FALSE)

