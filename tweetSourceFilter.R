tweetSourceFilter <- function(tweetDF){
	# ＜概要＞
	# ツイートを普段人間が使うクライアントでフィルタリングする
	# ＜注意点＞
	# 利用できるのはtwListToDF等でデータフレーム化したデータのみ
	# データフレームにはクライアントを示す"statusSource"という変数があること

	# ツイートクライアントをURL表示からクライアント名表示に
	tweetDF[,"statusSource"] <- substr(tweetDF[,"statusSource"],
    regexpr(">",tweetDF[,"statusSource"])+1,
    nchar(tweetDF[,"statusSource"])-4)

	twDF.filter <- subset(tweetDF,statusSource=="Twitter for iPhone"|
							statusSource=="Twitter for Android"|
							statusSource=="Twitter Web Client"|
							statusSource=="TweetDeck"|
							statusSource=="Twitter for iPad"|
							statusSource=="Tweetbot for iΟS"
							)
	return(twDF.filter)
}


