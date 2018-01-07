library(twitteR)
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "CxCZ0N5PUr4EaBY5av2AQ"
consumerSecret <- "EFLfRL9SagxC1Rt5qxikvy2WHDzwmP0l4CUwhPOgTc"
options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
twitCred <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, requestURL=reqURL, accessURL=accessURL, authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

# List of users
users <- c("24_hour_news", "14stu14", "AaronHXover", "1joserodriguez", "10nihati", "7thsky_crkt", "21_Navy", "a0rtega", "1BadazzBrwnSkin", "971theticketxyt", "9374ashishpatel", "4freetechnology", "99thRangernick", "2chancemarz", "7Stevemcc", "AbdallaANasser", "AaronJFentress", "007_Debby", "4iHD", "AARPIndiana", "24_bambam", "17jrod", "69CLaY", "410Trugoodfella", "1_2_Brefo", "8InchiPad", "2stepsAhead__", "3rdsoft", "4PennyGirl", "10Blackberry", "1hsfball", "ab914", "aadulay33", "aaron1312", "407hoops", "AaronRogers89", "A99Alison", "1992Forever", "aaliyahjae", "247_Big_Ten", "1upfeed", "1WildCardWknd3")
for (i in 1:length(users)) {
	for (j in 1:length(users)) {
		if (i != j) {
			friends <- friendships(users[i], users[j])
			print(friends)
		}
	}
}
# Additional list (after 'zdnet') is extracted from Search.com's top searches of 2012
# topics = c("3ds", "adidas", "afghanistan", "afridi", "alabama football", "amazing race", "amazon", "andre drummond", "android", "apple store", "arkansas razorbacks", "armani", "assassins creed 3", "auburn football", "benghazi", "borderlands 2 dlc", "calvinklein", "cbsnews", "chrome", "chromebook", "cnet", "colin powell", "cricket", "dallas mavericks", "dancing with the stars", "david letterman", "deadlight", "delonte west", "detroit tigers", "dilshan", "dkny", "donald trump", "dropbox", "earthquake", "east coast storm", "ebay", "engadget", "farming simulator 2013", "firefox", "florida gators", "forza horizon", "fsu football", "galaxy note 2", "google tablet", "groupon", "gucci", "halloween", "high school playoffs", "himym", "houston rockets", "hp envy x2", "htc 8x", "hurricane sandy", "imac", "ios 6 update", "iowa football", "ipad", "ipad mini review", "ipod", "iran", "iraq", "galaxy s3", "jellybean", "kohli", "lattimore", "lebron james", "lenovo yoga", "levis", "libya", "libya attack", "lockergnome", "lucius", "lumia", "mac mini", "macbook air", "macbook pro 13 retina", "marcus lattimore", "medal of honor warfighter", "megaupload", "microsoft surface", "microsoft tablet", "minecraft", "mini ipad", "mitt romney", "ms surface", "nanny", "navy football", "nba injuries", "netflix", "new ipad", "new macbook", "nexus", "nfl weather", "nike", "nokia lumia 920", "obama", "odi", "office 2013", "oregon ducks football", "oregon football", "pakistan", "person of interest", "pietersen", "pinterest", "psvita", "psy gangnam style", "romney", "rutgers football", "safari", "sandy", "sandy hurricane", "sf giants", "skyrim dlc", "south carolina football", "spotify", "steelers", "stephen curry", "surface rt", "syria", "t mobile", "t20", "taylor swift", "techcrunch", "technobuffalo", "techradar", "tendulkar", "terrorism", "texas longhorns", "the big bang theory", "tim cook", "tsunami", "ubuntu", " ultrabook", "vampire diaries", "wii ", "windows phone 8", "windows store", "windows surface", "windows 8", "xbox", "zdnet", "zumba", "windows rt", " syria", " 3ds", " psvita", " amazon", " adidas", " libya", " google tablet", " iraz", " mini ipad", " windows 8", " nexus", " deadlight", " pakistan", " mac mini", " macbook air", " apple store", " android", " surface rt", "", " surface pro", " microsoft surface", " office 2013", " ipad", " dropbox", " earthquake", " iran", " t mobile", " verizon", " at&t", " jellybean")
for (i in 1:length (topics))
{
	# Search 1000 tweets for each topic (needs package twitteR)
	data = searchTwitter(topics[i], 500)
	# Collect required attributes
	fData = list()
	if (length (data) == 0)
	{
		next
	}
	for (j in 1:length (data))
	{
		name = data[[j]]$screenName
		text = data[[j]]$text
		date = data[[j]]$created
		fav = data[[j]]$favorited
		id = data[[j]]$id
		fData[[j]] = paste(name, "\t", date, "\t", id, "\t", text, "\t", fav)
	}
	# Write CSV
	write.table(fData, paste("d:/Datasets/Twitter", topics[i], ".csv"), sep="\n", row.names=FALSE, col.names=FALSE)
	print (paste (topics[i], " Done!"))
}
