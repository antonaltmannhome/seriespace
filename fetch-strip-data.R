## let's scan all the cover pages for each series of interest

USERPATH = 'c:/research/miscracing/seriespacecomparison/'
source(paste0(USERPATH, 'startup.r'))

browserchoice='Firefox'
seriesSeasonDF = read.csv(paste0(USERPATH, 'web-address.dat'), sep = '~', as.is = TRUE)
ovalDF = read.csv(paste0(USERPATH, 'oval.csv'), sep = '~', as.is = TRUE)
multiDriverDF = read.csv(paste0(USERPATH, 'multidriver.csv'), sep = '~', as.is = TRUE)

raceInfoList = NULL
for (wi in 1:nrow(seriesSeasonDF)) {
	htmlName = paste0(HTMLPATH, seriesSeasonDF$series[wi], seriesSeasonDF$season[wi], '.html')
	if (!file.exists(htmlName)) {
		visitsave(seriesSeasonDF$webAddress[wi], thiscomputer, htmlName)
	}

	raceHtml = scan(htmlName, sep = '\n', what = '', quiet = TRUE)

	#miniRaceHtml = raceHtml[grep('Perimeter', raceHtml): length(raceHtml)]
	#raceData = miniRaceHtml[grep('autosport\\.com/gp\\.php.+[0-9]{8}',miniRaceHtml)]

	raceData = raceHtml[grep('gp\\.php.+ci\\.php',raceHtml)]

	# first line is always doubled up so get rid of all the crap at the start of it
	raceData[1] = gsub('(^.+)(<tr class=\"\")', '\\2', raceData[1])

	StripRaceDataLine = function(myRaceData) {
		dum = strsplit(myRaceData, split = '>')[[1]]
		# clear out all the junk lines
		dum = dum[-grep('td nowrap', dum)]
		dum = dum[-which(dum == '</td')]
		dum = dum[-grep('span class', dum)]
		dum = dum[-grep('<tr class', dum)]
		covshAddress = gsub('(^.+")(gp\\.php.+c=0)(".+$)', '\\2', dum[grep('gp\\.php', dum)])
		perim = gsub('</span', '', dum[9])
		circuit = gsub('</a', '', dum[7])
		return(data.frame(covshAddress = covshAddress, perim = perim, circuit = circuit))
	}

	raceWebPage = purrr::map_df(raceData, StripRaceDataLine)
	raceWebPage$raceNumber = 1:nrow(raceWebPage)
	raceWebPage$series = seriesSeasonDF$series[wi]
	raceWebPage$season = seriesSeasonDF$season[wi]

	raceInfoList[[wi]] = raceWebPage
}

# no link to indycar yet

raceDF = bind_rows(raceInfoList)

### then we download the actual qualifying sessions as well as the cover sheet (to check for wet sessions)

raceDF = lazy_left_join(raceDF, seriesSeasonDF, c('series', 'season'), c('numQual', 'fetchit'))
## hopefully it's only f1 that has silly qualifying
## no, superbikes do too, and 2nd race has no quali
raceDF = raceDF %>%
			mutate_cond(series %in% c('worldsuperbike', 'gp2', 'f2') &
						(raceNumber %% 2) == 0,
						fetchit = FALSE)

raceDF = raceDF %>%
			mutate_cond(series %in% c('gp2', 'f2') &
						circuit == 'Monte Carlo',
						numQual = 2)

raceDF = indicate_overlapping_combination(raceDF,
											ovalDF,
											c('circuit', 'perim'),
											'isOval')

raceDF$fetchit[raceDF$isOval] = FALSE
raceDF = left_join(raceDF, multiDriverDF, 'series')
if (any(is.na(raceDF$multiDriver))) {
	stop('You need to add new series to multidriver.csv\n')
}
				
MakeQualiPage = function(myCovshAddress, mySeason, mySeries, myRaceNumber, myNumQual) {

	qualiPageNumber = NA
	if (mySeries != 'worldsuperbike') {
		if (myNumQual > 1) {
			qualiPageNumber = 130 + (1:myNumQual)
		}
		if (myNumQual == 1) {
			qualiPageNumber = 130
		}
	}
	if (mySeries == 'worldsuperbike') {
		qualiPageNumber = 136:137
	}
	qualiAddress = paste0('http://forix.autosport.com/',
							gsub('[0-9]+$', '', myCovshAddress),
							qualiPageNumber)
	
	myQualiPageDF = tibble(series = mySeries,
							season = mySeason,
							raceNumber = myRaceNumber,
							qualiSession = 1:myNumQual,
							qualiAddress = qualiAddress)
	
	return(myQualiPageDF)
}
qualiDF = raceDF %>%
				filter(fetchit) %>%
				rowwise() %>%
				do(MakeQualiPage(.$covshAddress, .$season, .$series, .$raceNumber, .$numQual)) %>%
				ungroup()


for (qi in 1:nrow(qualiDF)) {
	htmlName = with(qualiDF[qi,],
						paste0(HTMLPATH,
								series,
								season,
								'r', raceNumber,
								'q', qualiSession,
								'.html'))
	if (!file.exists(htmlName)) {
		visitsave(qualiDF$qualiAddress[qi], thiscomputer, htmlName)
	}
}
# doesn't work for superbikes, due to their weird qualy format

### then we also want the fastest lap of the race to detect wetness
for (wi in 1:nrow(raceDF)) {
	if (raceDF$fetchit[wi]) {
		webAddress = paste0('http://forix.autosport.com/', raceDF$covshAddress[wi])
		htmlName = with(raceDF[wi,],
						paste0(HTMLPATH,
								series,
								season,
								'r', raceNumber,
								'covsh.html'))
		if (!file.exists(htmlName)) {
				visitsave(webAddress, thiscomputer, htmlName)
		}
	}
}

	
qualiDF$qualiId = with(qualiDF, paste(series, season, raceNumber, qualiSession, sep = '~'))
qualiDF$noRecords = FALSE

ExtractInfoFromQTLine = function(myQTLine, mySeries) {
	mySplitQTLine = strsplit(myQTLine, split = '>')[[1]]
	# reduce it down only to info from drier id onwards
	# filter out large amounts of the rubbish
	# if it's the fastest driver, then we get the header info, get rid
	kmhIndex = grep('&nbsp;km/h&nbsp;', mySplitQTLine)
	if (length(kmhIndex) > 0) {
		mySplitQTLine = mySplitQTLine[(kmhIndex + 1) : length(mySplitQTLine)]
	}
	mySplitQTLine = mySplitQTLine[-grep('^<span class=\"TXT-Standard-[0-9][B]*\"', mySplitQTLine)]
	mySplitQTLine = mySplitQTLine[-grep('\\<td nowrap align', mySplitQTLine)]
	miscShortRubbish = grep('(^</td$)|(^</tr$)|(^<br$)',mySplitQTLine)
	if (length(miscShortRubbish) > 0) mySplitQTLine = mySplitQTLine[-miscShortRubbish]
	# there may be no quali time info, need to detect it first
	classIndex = grep('pa\\.php\\?.+r=[0-9]+\\&', mySplitQTLine)
	myClass = gsub('\\&.+$','',mySplitQTLine[classIndex - 1])
	delaySpeedIndex = grep('^[0-9]+\\.[0-9]{3}', mySplitQTLine)
	delayIndex = tail(delaySpeedIndex, 2)[1]
	speedIndex = tail(delaySpeedIndex, 1)
	possibleQualiLineIndex = grep('^([0-9]\\\')*[0-9]{2}\\.[0-9]{3}', mySplitQTLine)
	if (myClass != '1') {
		qualiTimeIndex = setdiff(possibleQualiLineIndex, c(delayIndex, speedIndex))
	}
	if (myClass == '1') {
		qualiTimeIndex = setdiff(possibleQualiLineIndex, speedIndex)
	}
	driverSetATime = (length(qualiTimeIndex) == 1) & 
						(myClass == '1' | (myClass != '1' & length(delayIndex) > 0)) &
						(length(speedIndex) > 0)
	driverLineIndex = grep('driver\\.php\\?.+r=[0-9]{10}\\&',mySplitQTLine)
	myDrivId = gsub('(^.+\\&r=)([0-9]+)(.+$)', '\\2', mySplitQTLine[driverLineIndex])
	# but there might be multiple ones
	myDrivId = paste(myDrivId, collapse = '~')
	myDriver = gsub('<.+$', '', mySplitQTLine[driverLineIndex+1])
	myDriver = paste(myDriver, collapse = '~')
	myVehicle = gsub('<.+$', '', mySplitQTLine[max(driverLineIndex)+3])
	if (driverSetATime) {
		myQualTime = gsub('<.+$', '', mySplitQTLine[qualiTimeIndex])
		myQualSec = ConvertStringTimeToSec(myQualTime)
	}
	if (!driverSetATime) {
		myQualSec = NA
	}

	return(c(myDriver, myDrivId, myVehicle, myQualSec))
}

myList = NULL
### now strip the bastards
for (qi in 1:nrow(qualiDF)) {
	htmlName = with(qualiDF[qi,],
						paste0(HTMLPATH,
								series,
								season,
								'r', raceNumber,
								'q', qualiSession,
								'.html'))
	html = scan(htmlName, what = '', sep = '\n', quiet = TRUE)
	
	### there is a 'fastest lap by driver table' on pages with multiple drivers, let's use that
	fastestLapByDriverIndex = grep('Fastest lap by driver', html)
	hasFastLapByDriver = length(fastestLapByDriverIndex) > 0
	if (hasFastLapByDriver) {
		html = html[(fastestLapByDriverIndex + 1):length(html)]
	}
	
	qtline = grep('pa\\.php\\?l=0\\&d=[0-9]+\\&r=[0-9]{11}.+TXT-Standard-4B',html)
	qualiActuallyHappened = TRUE
	if (length(qtline) == 0) {
		noRecordsIndex = grep('No records', html)
		if (length(noRecordsIndex) > 0) {
			qualiDF$noRecords[qi] = TRUE
			qualiActuallyHappened = FALSE
		}
	}
	
	if (qualiActuallyHappened) {
		# the first one include a lot of bollox, need to filter that out
		myResultDF = tibble(driver = rep(NA, length(qtline)),
							drivId = rep(NA, length(qtline)),
							vehicle = rep(NA, length(qtline)),
							qualSec = rep(NA, length(qtline)))
		for (di in 1:length(qtline)) {
			myResultDF[di,c('driver', 'drivId', 'vehicle', 'qualSec')] =
				ExtractInfoFromQTLine(html[qtline[di]], qualiDF$series[qi])
		}
		# then decorate it with event details
		myResultDF$qualiId = qualiDF$qualiId[qi]
		myList[[qi]] = myResultDF
	}
}

### getting there, needs tweaking for individual series though
qualiDriverDF = bind_rows(myList)
### ok, main question is, should you put the car in as well? that is prbably easier to do manually

ExtractInfoFromCoverPage = function(myFastestLapLine, myMultiDriver) {
	mySplitFLLine = strsplit(myFastestLapLine, split = '>')[[1]]
	firstDriverIndex = grep('driver\\.php\\?.+\\&r=[0-9]{10}\\&', mySplitFLLine)[1]
	mySplitFLLine = mySplitFLLine[-(1:(firstDriverIndex - 1))]
	mySplitFLLine = mySplitFLLine[-grep('^<span class=\"TXT-Standard-[(4|1)][B]*\"', mySplitFLLine)]
	mySplitFLLine = mySplitFLLine[-grep('\\<td nowrap align', mySplitFLLine)]
	miscShortRubbish = grep('(^</td$)|(^</tr$)|(^<tr$)|(^</span$)',mySplitFLLine)
	if (length(miscShortRubbish) > 0) mySplitFLLine = mySplitFLLine[-miscShortRubbish]
	# then we can get at actual race fastest lap:
	driverLineIndex = grep('driver\\.php\\?.+\\&r=[0-9]{10}\\&', mySplitFLLine)
	if (!myMultiDriver) {
		myFastestLapTime = gsub('<.+$', '', mySplitFLLine[max(driverLineIndex) + 3])
	}
	if (myMultiDriver) {
		myFastestLapTime = gsub('<.+$', '', mySplitFLLine[max(driverLineIndex) + 4])
	}
	myFastestLapSec = ConvertStringTimeToSec(myFastestLapTime)
	
	# think that's all we need to return:
	return(myFastestLapSec)
}

# let's get hold of fastest race lap
raceDF$fastestRaceSec = NA
for (wi in 1:nrow(raceDF)) {
	if (raceDF$fetchit[wi]) {
		htmlName = with(raceDF[wi,],
							paste0(HTMLPATH,
									series,
									season,
									'r', raceNumber,
									'covsh.html'))
		html = scan(htmlName, what = '', sep = '\n', quiet = TRUE)
		
		fastestLapIndex = grep('Fastest lap<', html)
		if (length(fastestLapIndex) > 0) {
			myFastestLapLine = html[fastestLapIndex]
			raceDF$fastestRaceSec[wi] = ExtractInfoFromCoverPage(myFastestLapLine, raceDF$multiDriver[wi])
		}
	}
}

## then need to join the bastards up
if(FALSE) {
resultDF = lazy_left_join(resultDF, qualiDF, 'qualiId', c('series', 'season', 'raceNumber'))
resultDF = lazy_left_join(resultDF,
							raceDF,
							c('series', 'season', 'raceNumber'),
							c('fastestRaceSec', 'circuit', 'perim'))

# now try to detect wetness

fastestQualSec = resultDF %>%
					group_by(qualiId, series, season, raceNumber) %>%
					summarise(fastestQualSec = min(qualSec, na.rm = TRUE))
fastestQualSec = lazy_left_join(fastestQualSec,
								raceDF,
								c('series', 'season', 'raceNumber'),
								'fastestRaceSec')
}
# no, that's not quite the way to do it. for wec, where not everybody takes part, the slower drivers will always be a long way off the faste time.

# write that to disk and crack on with analysis
write.csv(file = paste0(USERPATH, 'race.csv'), raceDF, row.names = FALSE)
write.csv(file = paste0(USERPATH, 'quali.csv'), qualiDF, row.names = FALSE)
write.csv(file = paste0(USERPATH, 'qualidriver.csv'), qualiDriverDF, row.names = FALSE)
