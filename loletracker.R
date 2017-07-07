library(jsonlite)
library(curl)
library(httr)
library(plyr)
library(randomForest)

# LoL Ranked Statistics Scouter
# author: Kier Groulx (kgroulx@uci.edu)
#  This app assumes that, in ranked play, you are playing more or less the same role.
#  Implementations to support multiple roles will come later.

# insert target summoner name here; spaces can be left out
sName <- "yourSummonerNameHere"
apiKey <- "yourApiKeyHere"
sID_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/",sName,"?api_key=",apiKey))
sID <- sID_json[[1]]$id
matchlist_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/matchlist/by-summoner/",sID,"?seasons=SEASON2016&api_key=",apiKey))

# clean the data slightly
matchlist <- matchlist_json$matches[,-c(1,2)]
head(matchlist)
playerStats <- data.frame() # initialize data frame for player and team
teamStats <- data.frame()
# implement try/catch here with error 429 from server; don't want to overwhelm the API server
for (i in 1:matchlist_json$totalGames){
#for (i in 1:1){
    status <- 0
    while (status != 200){
        t <- GET(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",matchlist$matchId[i],"?includeTimeline=false&api_key=",apiKey))
        status <- t$status_code
        Sys.sleep(1)
        if (status != 200){
            Sys.sleep(1) 
        }
    }
    j <- httr::content(t, as = "text")
    match <- fromJSON(j)
    
    # delve into match info
    pID <- which(match$participantIdentities$player$summonerId == sID)
    # let's start by just pulling from the player base and stats
    # leave timeline for further analysis; don't include now
    # also leave out masteries and runes; those can be theorycrafted separate but introduce too much variance now
    psh <- match$participants[pID,]
    relevant <- cbind(psh$teamId,psh$spell1Id,psh$spell2Id,psh$championId,psh$highestAchievedSeasonTier)
    playerStatsT <- cbind(relevant,psh$stats)
    names(playerStatsT)[1:5] <- c("teamID","ss1","ss2","championId","highestAchievedSeasonTier")
    teamStatsT <- match$teams[psh$teamId/100,]
    teamStatsT <- teamStatsT[-c(14,15)]
    
    # for each iteration, bind instance information to a 'team' frame and 'player' frame
    playerStats <- rbind.fill(playerStats,playerStatsT)
    teamStats <- rbind.fill(teamStats,teamStatsT)
}

# get rid of outdated / deprecated variables that have little to no impact
dropPlayer <- c("highestAchievedSeasonTier", "unrealKills", "largestCriticalStrike", 
                "combatPlayerScore", "objectivePlayerScore", "totalPlayerScore", "totalScoreRank", 
                "sightWardsBoughtInGame", "totalUnitsHealed")
playerStats <- playerStats[,!(names(playerStats) %in% dropPlayer)]
# add in a couple of other engineered variables to reduce effects of highly correlated / redundant information
playerStats$totalDamageDelta <- playerStats$totalDamageDealt-playerStats$totalDamageTaken
playerStats$goldExcess <- playerStats$goldEarned-playerStats$goldSpent
playerStats$firstBlood <- playerStats$firstBloodKill | playerStats$firstBloodAssist
playerStats$firstTower <- playerStats$firstTowerKill | playerStats$firstTowerAssist
playerStats$firstInhibitor <- playerStats$firstInhibitorKill | playerStats$firstInhibitorAssist

# run a random forest classifier on the team information
# why random forest?  we care about what divisions produce the greatest error reduction.  ultimately, this could be
#  built into a strong classifier given enough data, but seeing as this is a scouting tool to determine best practices
#  at an individual level, we want to know what factors are most correlated with a win/loss.  random forests easily
#  show us the most influential splits, allowing us to quickly draw inferences from the data.
teamFit <- randomForest(winner ~ teamId + firstBlood + firstTower + firstInhibitor + firstBaron + firstDragon 
                    + firstRiftHerald + towerKills + inhibitorKills + baronKills + dragonKills + riftHeraldKills,
                    data=teamStats, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(teamFit)
# preliminary analysis: towers, inhibs, dragons taken better predict winning than barons, 'firsts', or team side
#  however, this may change at higher or lower elos; highly subject to individual variance and role played

# different fit on player data
playerFit <- randomForest(winner ~ champLevel + kills + deaths + assists + totalDamageDelta + minionsKilled + 
                            neutralMinionsKilled + neutralMinionsKilledEnemyJungle + goldEarned + goldSpent + 
                            firstBlood + firstTower + firstInhibitor + wardsPlaced + wardsKilled,
                        data=playerStats, 
                        importance=TRUE, 
                        ntree=2000)
varImpPlot(playerFit)
# inspection of this plot explains the most predictive variables on an individual player level in the given dataset.
# for most, obvious correlations emerge (i.e. firstInhibitor is a strong predictor).  However, deaths matter more than
#  kills or assists based on the small sample size examined thus far.

# Domain knowledge comes into play here, as you need to know whether there is a positive or negative correlation
#  between given variables and the outcome of a match.  In a pinch, you can use 
#  cor(playerStats$winner,playerStats$targetVariable) to get the sign of the relationship.  As an example, assuming 
#  there is an effect, goldEarned will most likely positively correlate with winning, while deaths will negatively
#  correlate with winning.

# todo:
#  implement SQL storage / cache / interface