library(tidyverse)
library(gridExtra)
library(viridis)
library(lubridate)

base = read.csv('C:/Users/MIC/Documents/MQM/R/steam_base.csv')
head(base)


# keep only paid games
base = base %>%
       filter(price>0)

# create dummy variables for genres
base$Action = as.integer(grepl(pattern='Action', x=base$steamspy_tags))
base$Strategy = as.integer(grepl(pattern='Strategy', x=base$steamspy_tags))
base$FPS = as.integer(grepl(pattern='FPS', x=base$steamspy_tags))
base$RPG = as.integer(grepl(pattern='RPG', x=base$steamspy_tags))
base$Casual = as.integer(grepl(pattern='Casual', x=base$steamspy_tags))
base$Indie = as.integer(grepl(pattern='Indie', x=base$steamspy_tags))
base$Adventure = as.integer(grepl(pattern='Adventure', x=base$steamspy_tags))
base$Simulation = as.integer(grepl(pattern='Simulation', x=base$steamspy_tags))
base$Puzzle = as.integer(grepl(pattern='Puzzle', x=base$steamspy_tags))
base$Racing = as.integer(grepl(pattern='Racing', x=base$steamspy_tags))
base$multiplayer = as.integer(grepl(pattern='Multi-player',x=base$categories))



base$month = month(ymd(base$release_date))
base$year = year(ymd(base$release_date))


base$wtf=as.vector(strsplit(as.character(base$release_date),'-'))
head(base$wtf)


list = c('a','b','c')
list[2]
# split owners range
base =  base %>%
        separate('owners',c('owners_low', 'owners_high'),'-')



# have to convert to numeric before able to make multiplication
base$owners_low = as.numeric(base$owners_low)
base$owners_high = as.numeric(base$owners_high)
base$price = as.numeric(base$price)


# Calculating revenue, taking the log to account for extreme values
base$revenue = log(base$price * (base$owners_high+base$owners_low)/2)


# positive_ratings_percetnage
base$pos_rate_perc = base$positive_ratings/(base$positive_ratings+base$negative_ratings)


# plot to show that revenues are skewed
ggplot(base, aes(x=revenue)) + geom_histogram()

# also include one without log to show the difference
# understand that most of the revenue in the industry is generated from outliers
base$revenue2 = base$price * (base$owners_high+base$owners_low)/2


# look at raw revenue numbers in $Millions, shows that a lot the highest ones added together doens't even meet a hit game revenue
skew2 = base %>%
        group_by(revenue2) %>%
        summarize(count=n(),total_rev = sum(revenue2)/1000000) %>%
        arrange(desc(total_rev))

colSums(skew2)
skew2$count_perc = skew2$count/24515
skew2$rev_perc = skew2$total_rev/8113

skew2



# plot to show unproportional difference between % of total number of games and % of total revenue
ggplot(skew2,aes(x=count_perc, y=rev_perc, size=total_rev)) +
                  geom_point()+ ggtitle('% Total Number vs % Total Revenue') +
                  xlab('% of Total Number') + ylab('% of Total Revenue') +
                  theme(plot.title = element_text(hjust = 0.5))





##to make a plot of boxplots showing relationship between revenue and the popular genres
p1 = ggplot(base,aes(x=as.factor(Strategy),y=revenue,fill="red"))+geom_boxplot()+theme(legend.position="none") + xlab("Strategy")+ylab("Log of Revenue")+ theme(axis.title.y=element_text(size = 15))+theme(axis.title.x=element_text(size = 15))
p3 = ggplot(base,aes(x=as.factor(FPS),y=revenue,fill="green"))+geom_boxplot() +theme(legend.position="none") + xlab("FPS")+scale_fill_brewer(palette="Pastel1")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("")+theme(axis.title.x=element_text(size = 15))
p4 = ggplot(base,aes(x=as.factor(Casual),y=revenue,fill="orange"))+geom_boxplot() + theme(legend.position="none") + xlab("Casual")+scale_fill_brewer(palette="Oranges")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("") + theme(axis.title.x=element_text(size = 15))
p5 = ggplot(base,aes(x=as.factor(Indie),y=revenue,fill="yellow"))+geom_boxplot() + theme(legend.position="none") + xlab("Indie")+scale_fill_brewer(palette="Spectral")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("") + theme(axis.title.x=element_text(size = 15))
p6 = ggplot(base,aes(x=as.factor(Simulation),y=revenue,fill="pink"))+geom_boxplot() + theme(legend.position="none") + xlab("Simulation")+ scale_fill_brewer(palette="Spectral")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("") + theme(axis.title.x=element_text(size = 15))
p7 = ggplot(base,aes(x=as.factor(Adventure),y=revenue,fill="black"))+geom_boxplot() + theme(legend.position="none") + xlab("Adventure")+ scale_fill_brewer(palette="YlGn")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("")+ theme(axis.title.x=element_text(size = 15))
p2=  ggplot(base,aes(x=as.factor(Action),y=revenue,fill="blue"))+geom_boxplot() + theme(legend.position="none") + xlab("Action")+ scale_fill_brewer(palette="Dark2")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())+ylab("")+ theme(axis.title.x=element_text(size = 15))
grid.arrange(p1,p2,p3,p4,p5,p7,ncol = 6)


# create table of count of publisher and organizing based on size
base = base %>%
  separate(publisher,c('pub1','pub2'),';')


pub = base %>%
      group_by(pub1) %>%
      summarize(count=n(),rev=(mean(revenue))) %>%
      mutate(size=cut(count,breaks=c(-Inf,10,50,Inf),labels=c('Small','Medium','Large'))) %>%
      arrange(desc(count))

# merge with original database
total = merge(base,pub,by='pub1')

## boxplot for revenue vs size pf publisher
ggplot(total,aes(x=size,y=revenue,fill=size))+ geom_boxplot()+ xlab('Size of Publisher') +
                                             ylab('Log of Revenue') +ggtitle('Revenue Group By Publisher Size') +theme(plot.title = element_text(hjust = 0.5)) +
                                             theme(axis.title.y=element_text(size = 15)) + theme(axis.title.x=element_text(size = 15))+ theme(axis.text.x =element_text(size=15))


# make correlation matrix
cor_column=select(total,revenue,Action,Strategy,FPS,RPG,Casual,Indie,Adventure,Simulation,Puzzle,Racing, multiplayer,size,pos_rate_perc)
str(cor_column)
M <- cor(cor_column)
print(M)
corrplot(M,method='circle')

# get the plot between number of owners and genre
genre_column=select(total,Action,Strategy,FPS,RPG,Casual,Indie,Adventure,Simulation, Puzzle,Racing,multiplayer)
total_number_owners=genre_column*total$average_owner
new_total_number_owners<-rbind(total_number_owners,c(colSums(total_number_owners)))


df<-tail(new_total_number_owners,1)
number<-c(929520000,342620000,385405000,301210000,223285000,584290000,358170000,217345000,119015000,57765000)/1000000
title<-c('Action','Strategy','FPS','RPG', 'Casual', 'Indie','Adventure','Simulation','Puzzle','Racing')
df2<-data.frame(title,number)


owner_plot = ggplot(df2,aes(x=title,y=number,fill=title))+geom_bar(stat='identity')+xlab('Genre')+ylab('Total Number of Owners')+
                                                                theme(axis.text.x =element_text(size=15,angle=45))+
                                                                theme(axis.title.x = element_text(size=20))+
                                                                theme(axis.title.y=element_text(size=20))+
                                                                ggtitle('The Number of Owners(in millions) for Each Genre')+
                                                                theme(plot.title = element_text(hjust = 0.5)) +
                                                                theme(legend.position = 'none')


# get the relationship between total_game with genre
count_column=select(total,Action,Strategy,FPS,RPG,Casual,Indie,Adventure,Simulation, Puzzle,Racing,multiplayer)
new_total_count_game=rbind(count_column,colSums(count_column))
tail(new_total_count_game)
number_game<-c(9684,3834,363,580,7684,15229,7330,3019,1126,722)
title_game<-c('Action','Strategy','FPS','RPG', 'Casual', 'Indie','Adventure','Simulation','Puzzle','Racing')
df3<-data.frame(title,number_game)


game_plot = ggplot(df3,aes(x=title,y=number_game,fill=title))+ geom_bar(stat='identity')+
      xlab('Genre')+ylab('Total Number of Games') + 
      theme(axis.text.x =element_text(size=15,angle=45))+
      theme(axis.title.x = element_text(size=20))+ 
      theme(axis.title.y=element_text(size=20))+
      ggtitle('The Number of Games for Each Genre')+ 
      theme(plot.title = element_text(hjust = 0.5)) + 
      theme(legend.position = 'none')

df4
df4 = merge(df2,df3,by='title')

# add a new calculation for number of owners per game 
df4$owners_per_game = df4$number/df4$number_game

# graph that shows number of owners per game 
# does it make sense to have close to a million owners per game???
ggplot(df4, aes(x=title, y=owners_per_game, fill=title)) + geom_bar(stat='identity') + 
  xlab('Genre')+ylab('Number of Owners per Game (in Millions)') +
  theme(axis.text.x =element_text(size=15,angle=45))+theme(axis.title.x = element_text(size=15))+
  theme(axis.title.y=element_text(size=15))+ggtitle('Average Number of Owners per Game')+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'none')

# note: don't use rev cuz that's the same for publisher, just use the original revenue
total = total %>%
        arrange(desc(size))


lm2 = lm(revenue~Action + Strategy + FPS + Casual +
                Indie + Adventure + Simulation +
                Racing + multiplayer + pos_rate_perc + as.factor(size) + year ,data=total)
summary(lm2)

plot(lm2)


ggplot(total,aes(x=as.factor(month),y=revenue)) + geom_boxplot()


head(total$revenue)

#resid plot
ggplot(data=lm2, aes(x=.fitted, y=.resid)) + geom_point(alpha=0.1) + geom_hline(yintercept=0) + xlab('Fitted Values') +
                                            ylab('Residual Values') + ggtitle('Residual Plot') + theme(plot.title = element_text(hjust = 0.5))


# Interaction between multiplayer & pos_rate_perc
lm3 = lm(revenue~Action + Strategy + FPS + Casual +
                Indie + Adventure + Simulation +
                Racing + multiplayer + pos_rate_perc + as.factor(size) + multiplayer*pos_rate_perc,data=total)
summary(lm3)

# Interaction between multiplayer & FPS
lm4 = lm(revenue~Action + Strategy + FPS + Casual +
                Indie + Adventure + Simulation +
                Racing + multiplayer + pos_rate_perc + as.factor(size) + multiplayer*FPS,data=total)
summary(lm4)
