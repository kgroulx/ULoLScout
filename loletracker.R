library(jsonlite)
library(curl)
library(httr)
library(plyr)

# make this entry based
sName <- "yourSummonerNameHere"
apiKey <- "yourApiKeyHere"
sID_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/",sName,"?api_key=",apiKey))
sID <- sID_json[[1]]$id
matchlist_json <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/matchlist/by-summoner/",sID,"?seasons=SEASON2016&api_key=",apiKey))

# clean up slightly
# assume all region and platform similarity
matchlist <- matchlist_json$matches[,-c(1,2)]
head(matchlist)
playerStats <- data.frame() # initialize data frame for player and team
teamStats <- data.frame()
# implement try/catch here with error 429 from server
for (i in 221:matchlist_json$totalGames){
#for (i in 1:1){
    status <- 0
    while (status != 200){
        #match <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",matchlist$matchId[i],"?includeTimeline=false&api_key=",apiKey))
        #match <- fromJSON(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",2340433904,"?includeTimeline=false&api_key=",apiKey))
        t <- GET(paste0("https://na.api.pvp.net/api/lol/na/v2.2/match/",matchlist$matchId[i],"?includeTimeline=false&api_key=",apiKey))
        status <- t$status_code
        if (status != 200){
            Sys.sleep(10) # figure out something better here
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

# todo:
#  do analysis on all of this
#   i.e. decision tree, random forest, k-means for scoring
#  tidy up
