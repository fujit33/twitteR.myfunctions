#=======================================================
#-------------------------------------------------------
#
# twUserData関数
#　
#----Kosuke Fujita--------------------------------------
#=======================================================

twUserData <- function(username,n=200){
# ------------------------------------------
# <概要>
# ユーザーのTwitter利用情報をまとめて取得しデータ
# フレームで返す関数
# <引数>
#   username : screenNameもしくはuserID
#   n : 分析に使うユーザーツイートの取得数
# <返り値> (データフレーム)
#   ユーザーid(@)・ユーザー名・自己紹介の文字数
#   最も使うクライアント・フォロー数・フォローワー数
#   両思い率・片思い率・片思われ率（フォローの方向性）
#   総ツイート数・一日あたりのツイート数・ツイートの平均文字数
# 　リプライの割合・RTの割合・urlを含む割合
#   １週間の平均ファボ数・１ツイートあたりのファボられ数
#   １ツイート当たりのRTされ数
#--------------------------------------------
  user <- getUser(username)  # ユーザ情報
  ut <- userTimeline(username,n,includeRts=T)  # ユーザータイムライン
  ut.df <- twListToDF(ut)

  # 最もよく使うクライアント mostSource
  source.df <- data.frame(table(ut.df[,"statusSource"]))
  maxid <- which.max(source.df[,2])
  rawsource <-  as.character(source.df[as.integer(maxid),1])
  cut.head <- regexpr(">",rawsource) + 1
  cut.tail <- nchar(rawsource) -4
  mostSource <- substr(rawsource,cut.head,cut.tail)

  # 両思い率、片思い率、片思われ率
  followers.ids <- user$getFollowerIDs()
  friends.ids<- user$getFriendIDs()
  couple.count <- sum(is.element(friends.ids,followers.ids)==T)  # 両思い数
  kataomoi.count <- sum(is.element(friends.ids,followers.ids)==F)  # 片思い数
  kataomoware.count <- sum(is.element(followers.ids,friends.ids)==F)  # 片思われ数
  sum.ff <- user$followersCount + user$friendsCount  # フォロワー+フォロー
  couple <- couple.count*2/sum.ff  # 両思い率
  kataomoi <- kataomoi.count/sum.ff  # 片思い率
  kataomoware <- kataomoware.count/sum.ff  # 片思われ率

  # 1日の平均ツイート数
  birth.ymd <- format(user$created,"%y/%m/%d")
  twhistory <- difftime(Sys.time(),birth.ymd,units="days")
  twhistory.num<- as.numeric(twhistory)
  twCountOnDay <- user$statusesCount / twhistory.num

  # ツイートの平均文字数
  text <- ut.df[,"text"]
  cleen.text <- removeSpecialStr(text)
  twchar <- nchar(cleen.text)
  twncharave <- mean(twchar)

  # ツイートのリプの割合
  reptable <- table(!is.na(ut.df[,"replyToSID"]))
  repcount <- reptable["TRUE"]
  if(is.na(repcount)){
    repRate <- 0
  }else{
    repRate <- repcount / length(ut)
  }
  # ツイートのRTの割合
  RTtable <- table(ut.df[,"isRetweet"])
  RTcount <- RTtable["TRUE"]
  if(is.na(RTcount)){
    RTRate <- 0
  }else{
    RTRate <- RTcount / length(ut)
  }
  # ツイートのURLつきの率
  urlTorF <- grepl("http",ut.df[,"text"])
  urlRate <- sum(urlTorF==T)/length(urlTorF)

  # 1週間あたりのファボ数
  favInWeek <- user$favoritesCount / twhistory.num * 7

  # 1ツイートあたりのファボられ数・RTされ数
  RTnum <- which(ut.df[,"isRetweet"])  # 他人のツイートのRTは抜く
  if(length(RTnum)==0){
    favedRate <- mean(ut.df[,"favoriteCount"])
    RTedRate <- mean(ut.df[,"retweetCount"])
  }else{
    favedRate <- mean(ut.df[-c(RTnum),"favoriteCount"])
    RTedRate <- mean(ut.df[-c(RTnum),"retweetCount"])
  }



  # 可視化
  cat("============================================","\n")
  cat("(",length(ut),"tweets",")","\n")
  cat(user$screenName,"\n")
  cat(user$name,"\n","\n")
  cat("自己紹介の文字数",nchar(user$description),"\n")
  cat("よく使うクライアント",mostSource,"\n")
  cat("フォロー数",user$friendsCount,"\n")
  cat("フォローワー数",user$followersCount,"\n")
  cat("両思い率",round(couple,2),"\n")
  cat("片思い率",round(kataomoi,2),"\n")
  cat("片思われ率",round(kataomoware,2),"\n","\n")
  cat("ツイート数",user$statusesCount,"\n")
  cat("1日平均ツイート数",round(twCountOnDay,2),"\n")
  cat("ツイートの平均文字数",round(twncharave,2),"\n")
  cat("リプライ率",round(repRate,2),"\n")
  cat("RT率",round(RTRate,2),"\n")
  cat("リンク（画像含む）つき率",round(urlRate,2),"\n")
  cat("週間平均ファボ数",round(favInWeek,2),"\n")
  cat("ファボられ数/１ツイート",round(favedRate,3),"\n")
  cat("RTされ数/1ツイート",round(RTedRate,3),"\n")

# データフレームに代入
  user.data <- c(user$screenName,
            user$name,
            nchar(user$description),
            mostsource,
            user$friendsCount,
            user$followersCount,
            round(couple,2),
            round(kataomoi,2),
            round(kataomoware,2),
            user$statusesCount,
            round(twCountOnDay,2),
            round(twncharave,2),
            round(repRate,2),
            round(RTRate,2),
            round(urlRate,2),
            round(favInWeek,2),
            round(favedRate,3),
            round(RTedRate,3)
            )
  df <- data.frame(rbind(user.data))
  colnames(df) <- c("screenName",
                    "name",
                    "ncharDes",
                    "mostSource",
                    "follow",
                    "follower",
                    "couple",
                    "kataomoi",
                    "kataomoware",
                    "twCount",
                    "twCountOnDay",
                    "twncharave",
                    "repRate",
                    "RTRate",
                    "urlRate",
                    "favInWeek",
                    "favoritedRate",
                    "RTedRate"
                    )
  invisible(df)
}


