#this method uses list of winners and runners up, finds position, and then creates df using 126 previous entries

#Winner Source: https://www.supersport.com/tennis/wimbledon/wimbledon-mens-champions
#https://www.supersport.com/tennis/wimbledon/wimbledon-womens-champions
atp_winners<-read.csv("mens.csv")
colnames(atp_winners)<-c("year","winner",'loser','score')
wta_winners<-read.csv("womens.csv")
colnames(wta_winners)<-c("year","winner",'loser','score')

atp_winners$winner<-gsub("\\s*\\([^\\)]+\\)","",as.character(atp_winners$winner)) #remove country in bracket next to name
atp_winners$loser<-gsub("\\s*\\([^\\)]+\\)","",as.character(atp_winners$loser))
wta_winners$winner<-gsub("\\s*\\([^\\)]+\\)","",as.character(wta_winners$winner))
wta_winners$loser<-gsub("\\s*\\([^\\)]+\\)","",as.character(wta_winners$loser))

