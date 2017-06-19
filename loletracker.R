library(jsonlite)
library(curl)
library(httr)
library(plyr)
library(randomForest)

# LoL Ranked Statistic Predictor
# author: Kier Groulx (kgroulx@uci.edu)
#  This app assumes that, in ranked play, you are playing more or less the same role.
#  Implementations to support multiple roles will come later.

# insert target summoner name here; spaces can be left out
sName <- "yourSummonerNameHere"
apiKey <- "yourApiKeyHere"
sID_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/",sName,"?api_key=",apiKey))
sID <- sID_json[[1]]$id
matchlist_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/matchlist/by-summoner/",sID,"?seasons=SEASON2016&api_key=",apiKey))

matchlist <- matchlist_json$matches[,-c(1,2)]
head(matchlist)
playerStats <- data.frame() # initialize data frame for player and team
teamStats <- data.frame()
# implement try/catch here with error 429 from server
for (i in 1:matchlist_json$totalGames){
#for (i in 1:1){
    status <- 0
    while (status != 200){
        #match <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",matchlist$matchId[i],"?includeTimeline=false&api_key=",apiKey))
        #match <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",2340433904,"?includeTimeline=false&api_key=",apiKey))
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
    # also leave out masteries and runes; those can be theorycrafted separate
    psh <- match$participants[pID,]
    relevant <- cbind(psh$teamId,psh$spell1Id,psh$spell2Id,psh$championId,psh$highestAchievedSeasonTier)
    playerStatsT <- cbind(relevant,psh$stats)
    names(playerStatsT)[1:5] <- c("teamID","ss1","ss2","championId","highestAchievedSeasonTier")
    playerStatsT
    teamStatsT <- match$teams[psh$teamId/100,]
    teamStatsT <- teamStatsT[-c(14,15)]
    teamStatsT
    
    playerStats <- rbind.fill(playerStats,playerStatsT)
    teamStats <- rbind.fill(teamStats,teamStatsT)
}

# get rid of outdated / deprecated variables that have little to no impact
dropPlayer <- c("highestAchievedSeasonTier", "unrealKills", "largestCriticalStrike", 
                "combatPlayerScore", "objectivePlayerScore", "totalPlayerScore", "totalScoreRank", 
                "sightWardsBoughtInGame", "totalUnitsHealed")
playerStats <- playerStats[,!(names(playerStats) %in% dropPlayer)]
# add in a couple of other engineered variables
playerStats$totalDamageDelta <- playerStats$totalDamageDealt-playerStats$totalDamageTaken
playerStats$goldExcess <- playerStats$goldEarned-playerStats$goldSpent
playerStats$firstBlood <- playerStats$firstBloodKill | playerStats$firstBloodAssist
playerStats$firstTower <- playerStats$firstTowerKill | playerStats$firstTowerAssist
playerStats$firstInhibitor <- playerStats$firstInhibitorKill | playerStats$firstInhibitorAssist

# to start with, run a decision tree on the team statistics
teamFit <- randomForest(winner ~ teamId + firstBlood + firstTower + firstInhibitor + firstBaron + firstDragon 
                    + firstRiftHerald + towerKills + inhibitorKills + baronKills + dragonKills + riftHeraldKills,
                    data=teamStats, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(teamFit)
# preliminary analysis: towers, inhibs, dragons taken better predict winning than barons, 'firsts', or team side
#  however, this may change at higher or lower elos

# more complex fit on player data
playerFit <- randomForest(winner ~ championId + champLevel + kills + largestKillingSpree + deaths + assists + 
                          totalDamageDelta + minionsKilled + neutralMinionsKilled + neutralMinionsKilledEnemyJungle + 
                          goldEarned + goldSpent + firstBlood + firstTower + firstInhibitor + wardsPlaced + 
                          wardsKilled + killingSprees,
                        data=complete.cases(playerStats), 
                        importance=TRUE, 
                        ntree=2000)
varImpPlot(playerFit)
# this will vary player by player

# todo:
#  implement SQL storage / cache / interface
#  display splitting values for constant vars (i.e. kills)
#  implement visualizations