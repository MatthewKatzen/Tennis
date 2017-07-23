#this method uses list of winners and runners up, finds position, and then creates df using 126 previous entries
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
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

#create data frames for each gender and year
for (j in c('atp','wta')){
  for (i in c(2001:2016)){
    url<-paste0('https://raw.githubusercontent.com/JeffSackmann/tennis_',j,'/master/',j,'_matches_',i,'.csv')
    assign(paste0('data_',j,'_',i),read.csv(url,na.strings=c("","NA")))#creates df for entire year
    
    end<-eval(parse(text=paste0('which(data_',j,'_',i,'$winner_name==',j,'_winners$winner[',(2016+1-i),'] & data_',j,'_',i,'$loser_name==',j,'_winners$loser[',(2016+1-i),'] & data_',j,'_',i,"$tourney_name=='Wimbledon')")))
    list<-c((end-126):end)
    assign(paste0(j,'_',i),eval(parse(text=paste0('data_',j,'_',i,'[list,-c(1:8,10,12,17,18,20,22,27:49)]'))))#cuts down to interesting data
  }
}

atp_seed<-array(data=NA,dim=c(16,8,4),dimnames = list(c(2001:2016),c(1:8),c('QF','SF','F','W')))

wta_seed<-array(data=NA,dim=c(16,8,4),dimnames = list(c(2001:2016),c(1:8),c('QF','SF','F','W')))

for(i in c(2001:2016)){
  for (j in c('QF','SF','F','W')){
    if (j=='QF'){
      for (k in c(121:124)){
        w<-eval(parse(text=paste0('atp_',i,'$winner_seed[',k,']')))
        l<-eval(parse(text=paste0('atp_',i,'$loser_seed[',k,']')))
        atp_seed[as.character(i),2*(k-121)+1,'QF']<-as.numeric(as.character(w))
        atp_seed[as.character(i),2*(k-121)+2,'QF']<-as.numeric(as.character(l))
      }
    }
    if (j=='SF'){
      for (k in c(125:126)){
        w<-eval(parse(text=paste0('atp_',i,'$winner_seed[',k,']')))
        l<-eval(parse(text=paste0('atp_',i,'$loser_seed[',k,']')))
        atp_seed[as.character(i),2*(k-125)+1,'SF']<-as.numeric(as.character(w))
        atp_seed[as.character(i),2*(k-125)+2,'SF']<-as.numeric(as.character(l))
      }
    }
    if (j=='F'){
      w<-eval(parse(text=paste0('atp_',i,'$winner_seed[',127,']')))
      l<-eval(parse(text=paste0('atp_',i,'$loser_seed[',127,']')))
      atp_seed[as.character(i),1,'F']<-as.numeric(as.character(w))
      atp_seed[as.character(i),2,'F']<-as.numeric(as.character(l))
    }
    if (j=='W'){
      w<-eval(parse(text=paste0('atp_',i,'$winner_seed[',127,']')))
      atp_seed[as.character(i),1,'W']<-as.numeric(as.character(w))
    }
  }
}

#create 3d array for each men(atp) and women(wta), with dimensions year, round, and indiividual.
for(i in c(2001:2016)){
  for (j in c('QF','SF','F','W')){
    if (j=='QF'){
      for (k in c(121:124)){
        w<-eval(parse(text=paste0('wta_',i,'$winner_seed[',k,']')))
        l<-eval(parse(text=paste0('wta_',i,'$loser_seed[',k,']')))
        wta_seed[as.character(i),2*(k-121)+1,'QF']<-as.numeric(as.character(w))
        wta_seed[as.character(i),2*(k-121)+2,'QF']<-as.numeric(as.character(l))
      }
    }
    if (j=='SF'){
      for (k in c(125:126)){
        w<-eval(parse(text=paste0('wta_',i,'$winner_seed[',k,']')))
        l<-eval(parse(text=paste0('wta_',i,'$loser_seed[',k,']')))
        wta_seed[as.character(i),2*(k-125)+1,'SF']<-as.numeric(as.character(w))
        wta_seed[as.character(i),2*(k-125)+2,'SF']<-as.numeric(as.character(l))
      }
    }
    if (j=='F'){
      w<-eval(parse(text=paste0('wta_',i,'$winner_seed[',127,']')))
      l<-eval(parse(text=paste0('wta_',i,'$loser_seed[',127,']')))
      wta_seed[as.character(i),1,'F']<-as.numeric(as.character(w))
      wta_seed[as.character(i),2,'F']<-as.numeric(as.character(l))
    }
    if (j=='W'){
      w<-eval(parse(text=paste0('wta_',i,'$winner_seed[',127,']')))
      wta_seed[as.character(i),1,'W']<-as.numeric(as.character(w))
    }
  }
}

#create plots
plot_func<-function(array_m,array_f,title,round){
#set rows to observe in seed array
if(round=='W'){
  obs<-1:16
}
else if(round=='F'){
  obs<-1:32    
}
else if(round=='SF'){
  obs<-1:64
}
else{
  obs<-1:128
}
#create and clean data frame
data_m<-data.frame(array_m[,,round])
data_m$year<-rownames(data_m)
data_m<-melt(data_m,id.vars="year")[obs,-2]#remove empty portion of df and get rid of var X1

data_f<-data.frame(array_f[,,round])
data_f$year<-rownames(data_f)
data_f<-melt(data_f,id.vars="year")[obs,-2]#remove empty portion of df and get rid of var X1
  
#scatterplot NAs
  f_na_list<-c()
  m_na_list<-c()
  for(i in c(2001:2016)){
    temp<-sum(is.na(data_f$value[data_f$year==i]))
    if (temp==0){
      temp<-''
    }
    f_na_list[i-2000]<-temp
    assign(paste0('f_na_',(i-2000)),textGrob(f_na_list[i-2000], gp=gpar(fontsize=18)))
    
    temp<-sum(is.na(data_m$value[data_m$year==i]))
    if (temp==0){
      temp<-''
    }
    m_na_list[i-2000]<-temp
    assign(paste0('m_na_',(i-2000)),textGrob(m_na_list[i-2000], gp=gpar(fontsize=18)))
  }
  
#setting boxplot NA count
  if (sum(is.na(data_m$value))==0){
    NA_m_sum<-''
  }
  else{
    NA_m_sum<-sum(is.na(data_m$value))
  }
  
  if (sum(is.na(data_f$value))==0){
    NA_f_sum<-''
  }
  else{
    NA_f_sum<-sum(is.na(data_f$value))
  }
  NA_m_groob<-textGrob(NA_m_sum, gp=gpar(fontsize=18))
  NA_f_groob<-textGrob(NA_f_sum, gp=gpar(fontsize=18)) 
  
#scatterplots
  scatterplot_m<-ggplot(data_m, aes(x=year, y=value))+geom_point()+ylim(0,32)+ggtitle('Men') +labs(x='Year',y='Seed') +scale_y_discrete(breaks=1:34,limit=c(seq(1,33,5)),expand = c(0.1,0.1)) +coord_cartesian(ylim=c(2,33)) +theme(axis.text.x=element_text(angle=270,vjust=0.25),axis.text=element_text(size=18), axis.title=element_text(size=21),plot.title = element_text(size=24))  +annotation_custom(m_na_1,xmin=1,xmax=1,ymin=35,ymax=35) +annotation_custom(m_na_2,xmin=2,xmax=2,ymin=35,ymax=35) +annotation_custom(m_na_3,xmin=3,xmax=3,ymin=35,ymax=35) +annotation_custom(m_na_4,xmin=4,xmax=4,ymin=35,ymax=35) +annotation_custom(m_na_5,xmin=5,xmax=5,ymin=35,ymax=35) +annotation_custom(m_na_6,xmin=6,xmax=6,ymin=35,ymax=35) +annotation_custom(m_na_7,xmin=7,xmax=7,ymin=35,ymax=35) +annotation_custom(m_na_8,xmin=8,xmax=8,ymin=35,ymax=35) +annotation_custom(m_na_9,xmin=9,xmax=9,ymin=35,ymax=35) +annotation_custom(m_na_10,xmin=10,xmax=10,ymin=35,ymax=35) +annotation_custom(m_na_11,xmin=11,xmax=11,ymin=35,ymax=35) +annotation_custom(m_na_12,xmin=12,xmax=12,ymin=35,ymax=35) +annotation_custom(m_na_13,xmin=13,xmax=13,ymin=35,ymax=35) +annotation_custom(m_na_14,xmin=14,xmax=14,ymin=35,ymax=35) +annotation_custom(m_na_15,xmin=15,xmax=15,ymin=35,ymax=35) +annotation_custom(m_na_16,xmin=16,xmax=16,ymin=35,ymax=35) 
  scatterplot_f<-ggplot(data_f, aes(x=year, y=value))+geom_point()+ggtitle('Women') +labs(x='Year',y='Seed') +scale_y_discrete(breaks=1:34,limit=c(seq(1,33,5)),expand = c(0.1,0.1)) +coord_cartesian(ylim=c(2,33)) +theme(axis.text.x=element_text(angle=270,vjust=0.25),axis.text=element_text(size=18), axis.title=element_text(size=21),plot.title = element_text(size=24))  +annotation_custom(f_na_1,xmin=1,xmax=1,ymin=35,ymax=35) +annotation_custom(f_na_2,xmin=2,xmax=2,ymin=35,ymax=35) +annotation_custom(f_na_3,xmin=3,xmax=3,ymin=35,ymax=35) +annotation_custom(f_na_4,xmin=4,xmax=4,ymin=35,ymax=35) +annotation_custom(f_na_5,xmin=5,xmax=5,ymin=35,ymax=35) +annotation_custom(f_na_6,xmin=6,xmax=6,ymin=35,ymax=35) +annotation_custom(f_na_7,xmin=7,xmax=7,ymin=35,ymax=35) +annotation_custom(f_na_8,xmin=8,xmax=8,ymin=35,ymax=35) +annotation_custom(f_na_9,xmin=9,xmax=9,ymin=35,ymax=35) +annotation_custom(f_na_10,xmin=10,xmax=10,ymin=35,ymax=35) +annotation_custom(f_na_11,xmin=11,xmax=11,ymin=35,ymax=35) +annotation_custom(f_na_12,xmin=12,xmax=12,ymin=35,ymax=35) +annotation_custom(f_na_13,xmin=13,xmax=13,ymin=35,ymax=35) +annotation_custom(f_na_14,xmin=14,xmax=14,ymin=35,ymax=35) +annotation_custom(f_na_15,xmin=15,xmax=15,ymin=35,ymax=35) +annotation_custom(f_na_16,xmin=16,xmax=16,ymin=35,ymax=35)
  
#boxplots
  data_m$value[is.na(data_m$value)]<-128-33
  boxplot_m<-ggplot(data_m, aes(x="",y=value))+geom_boxplot(fatten = 3)+ggtitle('')  +scale_y_discrete(breaks=1:34,limit=c(seq(1,33,5)),expand = c(0.1,0.1)) +coord_cartesian(ylim=c(2,33)) +theme(axis.text.x=element_text(angle=270,vjust=0.25))  +theme(axis.text.x = element_text(color="white"),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),plot.title = element_text(size=24),axis.text=element_text(size=18)) +scale_x_discrete(labels='20  16')+xlab('') +annotation_custom(NA_m_groob,xmin=1,xmax=1,ymin=35,ymax=35)
  data_f$value[is.na(data_f$value)]<-128-33
  boxplot_f<-ggplot(data_f, aes(x="",y=value))+geom_boxplot(fatten = 3)+ggtitle('')  +scale_y_discrete(breaks=1:34,limit=c(seq(1,33,5)),expand = c(0.1,0.1)) +coord_cartesian(ylim=c(2,33)) +theme(axis.text.x=element_text(angle=270,vjust=0.25))   +theme(axis.text.x=element_text(color="white"),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),plot.title = element_text(size=24),axis.text=element_text(size=18))+scale_x_discrete(labels='20  16')+xlab('') +annotation_custom(NA_f_groob,xmin=1,xmax=1,ymin=35,ymax=35)
  
  plot<-grid.arrange(scatterplot_m,boxplot_m,scatterplot_f,boxplot_f,ncol=4,widths=c(1,0.15,1,0.15),top=textGrob(title,gp=gpar(fontsize=30,fontface=c('bold'))))
  return(plot)
}

ggsave(file='Winner.jpg',plot_func(atp_seed,wta_seed,"Winner",'W'),width=15)
ggsave(file='Final.jpg',plot_func(atp_seed,wta_seed,"Final",'F'),width=15)
ggsave(file='Semi.jpg',plot_func(atp_seed,wta_seed,"Semi Finals",'SF'),width=15)
ggsave(file='Quarter.jpg',plot_func(atp_seed,wta_seed,"Quarter Finals",'QF'),width=15)

#create summary table
summary<-as.data.frame(matrix(ncol = 8,nrow = 4))
rownames(summary)<-c('W','F','SF','QF')
colnames(summary)<-c('mean_m','mean_w','median_m','median_w','sd_m','sd_w','IQR_m','IQR_w')
for (round in c('W','F','SF','QF')){
  if(round=='W'){
  obs<-1
  }else if(round=='F'){
    obs<-1:2    
  }else if(round=='SF'){
    obs<-1:4
  }else{
    obs<-1:8
  }
  temp<-atp_seed[,obs,round]
  temp[is.na(temp)]<-128-33
  summary[round,'mean_m']<-round(mean(temp),2)
  summary[round,'median_m']<-round(median(temp),2)
  summary[round,'sd_m']<-round(sd(temp),2)
  summary[round,'IQR_m']<-round(IQR(temp),2)
  
  temp<-wta_seed[,obs,round]
  temp[is.na(temp)]<-128-33
  summary[round,'mean_w']<-round(mean(temp),2)
  summary[round,'median_w']<-round(median(temp),2)
  summary[round,'sd_w']<-round(sd(temp),2)
  summary[round,'IQR_w']<-round(IQR(temp),2)
}

p<-tableGrob(summary)
ggsave(file='summary.jpg',p,width=)
h = convertHeight(sum(p$heights), "in", TRUE)
w = convertWidth(sum(p$widths), "in", TRUE)
ggsave("summary.jpg", p, width=w, height=h)
