library("reshape", lib.loc="C:/Users/amclane/Documents/R/win-library/3.0")
library("reshape2", lib.loc="C:/Users/amclane/Documents/R/win-library/3.0")
require("ggplot2")
require("car")
require(scales)

##Multiplot Function
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#begin injesting ADX Data



adxquerydata<-read.csv("bidlandscape_3_14_2014.csv", stringsAsFactors=FALSE)

#=right function from excel
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

##clean up injested data
adxquerydata$Bid.ranges<-substrRight(adxquerydata$Bid.ranges,4)
adxquerydata$Bid.ranges<- as.numeric(adxquerydata$Bid.ranges)
adxquerydata$Bid.ranges<- adxquerydata$Bid.ranges/100
adxquerydata$winrate<- adxquerydata$Impressions.won/adxquerydata$Bids
#function for difference between close and winning bid CPM
adxquerydata$closebiddelta<-adxquerydata$Winning.bid.CPM-adxquerydata$Close.CPM
adxquerydata$revenuelost<-(adxquerydata$Impressions.won/1000)*adxquerydata$closebiddelta


adxquerydata[is.na(adxquerydata)]<-0


adxbidmelt<-melt(data=adxquerydata,id.vars=c("Bid.ranges","Advertisers"),measure.vars=c("Bids","Estimated.earnings.from.bids","Impressions.won"),na.rm=TRUE)
bidsbyadvertisers<- cast(adxbidmelt,Advertisers~variable,fun.aggregate=sum)
bidsbyadvertisers<- bidsbyadvertisers[with(bidsbyadvertisers, order(-Bids)), ]
bidsbyadvertisers$cpm<-(bidsbyadvertisers$Estimated.earnings.from.bids/bidsbyadvertisers$Impressions.won) * 1000
##SETS MINIMUM BIDS PER ADVERTISER
advertiserbidmin<- 500000
#DF of top Advertisers by BIDS
TopAdvertisersBids<-bidsbyadvertisers[bidsbyadvertisers$Bids>advertiserbidmin,]

#SETS MINIMUM IMPRESSIONS PER ADVERTISER
advertiserimpmin<- 10000
##DF of top advertisers by IMPRESSIONS
impsbyadvertisers<- bidsbyadvertisers[with(bidsbyadvertisers, order(-Impressions.won)), ]
TopAdvertisersImps<-bidsbyadvertisers[bidsbyadvertisers$Impressions.won>advertiserimpmin,]
TopAdvertisersImps<- TopAdvertisersImps[with(TopAdvertisersImps, order(-Impressions.won)), ]
##DF of Top Advertisers by REVENUE
TopAdvertisersRev<- TopAdvertisersImps[with(TopAdvertisersImps, order(-Estimated.earnings.from.bids)), ]

adbids<-function(x,xmin=0,xmax=50)

    {
 p<-ggplot(adxquerydata[adxquerydata$Advertisers==x,])
p1<- p + geom_bar(aes(x=Bid.ranges,y=Bids),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
p2<- p + geom_bar(aes(x=Bid.ranges,y=Impressions.won),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
p3<- p + geom_bar(aes(x=Bid.ranges,y=Estimated.earnings.from.bids),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
p4<- p + geom_bar(aes(x=Bid.ranges,y=winrate),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
p5<- p + geom_bar(aes(x=Bid.ranges,y=Close.CPM),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
p6<- p + geom_bar(aes(x=Bid.ranges,y=cumsum(revenuelost)),stat="identity") + ggtitle(x) + xlim(xmin,xmax)
multiplot(p1, p2, p3,p4,p5,p6,cols=3)

}
##CUMULATIVE SUM
newdf<-adxquerydata[adxquerydata$Advertisers=='ValueClick',]
cumsum(x=adxquerydata$revenuelost,)

adbids(TopAdvertisersBids[1,1],0,10)
adbids(TopAdvertisersBids[3,1],,5)
adbids(TopAdvertisersBids[4,1])
adbids(TopAdvertisersBids[5,1])
adbids(TopAdvertisersBids[6,1],37)

p<-ggplot(adxquerydata[adxquerydata$Advertisers=='Other advertisers',])
p1<- p + geom_bar(aes(x=Bid.ranges,y=Bids),stat="identity") + ggtitle('Other advertisers') #+ xlim(xmin,xmax)

p1
list(TopAdvertisersBids$Advertisers)

