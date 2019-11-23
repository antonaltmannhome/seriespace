LoadData = function() {

	raceDF = read.csv(paste0(USERPATH, 'race.csv'), as.is = TRUE)
	qualiDF = read.csv(paste0(USERPATH, 'quali.csv'), as.is = TRUE)
	qualiDriverDF = read.csv(paste0(USERPATH, 'qualidriver.csv'), as.is = TRUE)

	f1RaceDF = read.csv(paste0(USERPATH, 'f1-race.csv'), as.is = TRUE) %>%
				mutate(perim = as.numeric(perim))
	f1QualiDF = read.csv(paste0(USERPATH, 'f1-quali.csv'), as.is = TRUE)
	f1QualiDriverDF = read.csv(paste0(USERPATH, 'f1-qualidriver.csv'), as.is = TRUE) %>%
						mutate(drivId = as.character(drivId))
	
	# but i think we're only using f1 data from 1718 atm
	f1RaceDF = f1RaceDF %>%
				filter(season %in% c(2017, 2018))
	f1QualiDF = semi_join(f1QualiDF, f1RaceDF, c('season', 'raceNumber'))
	f1QualiDriverDF = semi_join(f1QualiDriverDF, f1QualiDF, 'qualiId')
	
	raceDF = bind_rows(raceDF, f1RaceDF)
	qualiDF = bind_rows(qualiDF, f1QualiDF)
	qualiDriverDF = bind_rows(qualiDriverDF, f1QualiDriverDF)
	
	raceDF = raceDF %>% filter(!isOval)
	
	raceDF$circuit = with(raceDF, gsub('[^a-z]', ' ', tolower(circuit)))
	# a few perimeters need fixing
	raceDF = raceDF %>%
				mutate(perim = case_when(
						circuit == 'jerez' & perim == '4428' ~ '4423',
						circuit == 'laguna seca' & perim == '3602' ~ '3610',
						circuit == 'marina bay' & perim == '5063' ~ '5065',
						circuit == 'motorland' & perim == '5078' ~ '5077',
						circuit == 'motegi' & perim == '4801.379' ~ '4801',
						circuit == 'nurburgring' & perim == '5148' ~ '5137',
						circuit == 'phillip island' & perim == '4445' ~ '4448',
						circuit == 'red bull ring' & perim == '4326' ~ '4318',
						circuit == 'silverstone' & perim == '5891' ~ '5901',
						circuit == 'silverstone' & perim == '5900' ~ '5901',
						circuit == 'spa francorchamps' & perim == '7003.9' ~ '7004',
						TRUE ~ as.character(perim)))
	raceDF$circuitPerim = with(raceDF, paste(circuit, perim, sep = '~'))

	raceDF$raceId = with(raceDF, paste(series, season, raceNumber, sep = '~'))
	qualiDF$raceId = with(qualiDF, paste(series, season, raceNumber, sep = '~'))
	
	qualiDF = lazy_left_join(qualiDF,
								raceDF,
								'raceId',
								'circuitPerim')
	
	qualiDriverDF$driver = gsub('~', '-', qualiDriverDF$driver)
	qualiDriverDF$driver = gsub('\\.', '', qualiDriverDF$driver)
	
	qualiDriverDF = lazy_left_join(qualiDriverDF,
									qualiDF,
									'qualiId',
									c('series', 'season', 'raceNumber', 'qualiSession'))
	qualiDriverDF$raceId = with(qualiDriverDF, paste(series, season, raceNumber, sep = '~'))
	qualiDriverDF = lazy_left_join(qualiDriverDF,
									raceDF,
									'raceId',
									c('circuitPerim'))
									
	qualiDriverDF$dtss = with(qualiDriverDF, paste(driver, vehicle, series, season, sep = '~'))
	
	# get rid of stuff that's irrelevant for model
	qualiDF = qualiDF %>% select(-qualiAddress)
	raceDF = raceDF %>% select(-c(covshAddress, fetchit))
	
	return(list(raceDF = raceDF,
				qualiDF = qualiDF,
				qualiDriverDF = qualiDriverDF))
}

GetDtssCircuitCoef = function(myResultDF) {
	modelMatrix = model.matrix(~ factor(circuitPerim) + factor(dtss) - 1,
									data = myResultDF)
	#mod = lm.fit(x = modelMatrix, y = log(validResultDF$qualSec)) # slow
	mod = speedglm::speedglm.wfit(y = log(myResultDF$qualSec),
									X = modelMatrix)
	uglyDtssCoef = coef(mod)[grep('factor\\(dtss\\)', names(coef(mod)))]
	names(uglyDtssCoef) = gsub('^.+\\)', '', names(uglyDtssCoef))
	missingDtssCoef = setdiff(myResultDF$dtss, names(uglyDtssCoef))
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
	
	myResultDF = myResultDF %>%
					left_join(dtssCoefDF, 'dtss') %>%
					left_join(circuitPerimCoefDF, 'circuitPerim')
	
	estSD = with(myResultDF, sqrt(mean( (log(qualSec) - (dtssCoef + circuitPerimCoef))^2)))
	
	return(list(dtssCoefDF = dtssCoefDF,
				circuitPerimCoefDF = circuitPerimCoefDF,
				estSD = estSD))
}
