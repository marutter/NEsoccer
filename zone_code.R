# Get the data

setwd("~/Dropbox/Stats/Soccer/Research")
shotData <- read.csv("shot_data_ne_2018.csv")
summary(shotData)

# Plot raw data

with(shotData,plot(X,Y,pch=as.numeric(Result),
                   xlim=c(0,200),ylim=c(0,100)))

# Transform data from right to left side

library(dplyr)
right <- shotData %>% filter(X>100) %>% mutate(X = 200-X,Y=100-Y)
left <- shotData %>% filter(X<=100)
shotData <- bind_rows(left,right)
with(shotData,plot(X,Y,pch=as.numeric(Result),
                   xlim=c(0,200),ylim=c(0,100)))

# Determine Locations

library(sp)
z1 <- Polygon(rbind(c(0,36.5),c(10.7,36.5),c(10.7,63.5),c(0,63.5)))
zone1 <- Polygons(list(z1),"zone1")
z2 <- Polygon(rbind(c(10.7,36.5),c(31.6,36.5),c(31.6,63.5),c(10.7,63.5)))
zone2 <- Polygons(list(z2),"zone2")
z3a <- Polygon(rbind(c(0,20.4),c(31.6,20.4),c(31.6,36.5),c(0,36.5)))
z3b <- Polygon(rbind(c(0,63.5),c(31.6,63.5),c(31.6,79.5),c(0,79.5)))
zone3 <- Polygons(list(z3a,z3b),"zone3")
z4 <- Polygon(rbind(c(31.6,20.4),c(44,20.4),c(44,79.5),c(31.6,79.5)))
zone4 <- Polygons(list(z4),"zone4")
z5 <- Polygon(rbind(c(44,20.4),c(100,20.4),c(100,79.5),c(44,79.5)))
zone5 <- Polygons(list(z5),"zone5")
z6a <- Polygon(rbind(c(0,0),c(100,0),c(100,20.4),c(0,20.4)))
z6b <- Polygon(rbind(c(0,79.5),c(100,79.5),c(100,100),c(0,100)))
zone6 <- Polygons(list(z6a,z6b),"zone6")
zones <- SpatialPolygons(list(zone1,zone2,zone3,zone4,zone5,zone6))
plot(zones)

shots <- SpatialPoints(shotData[,1:2])
plot(zones)
points(shots,pch=as.numeric(shotData$Result))
res <- over(shots,zones)
shotData$Zone <- res

# Percentages

shotsZones <- with(shotData,table(Zone,Result))
shotsZones[,1]/(shotsZones[,1]+shotsZones[,2])
zone.prob <- shotsZones[,1]/(shotsZones[,1]+shotsZones[,2])
mls <- c(.311,.177,.071,.053,.023,.035)


# Mixed-Model

shotData$Zonef <- factor(shotData$Zone)
shotData$TGf <- shotData$Team:factor(shotData$Game)

res.glm <- glm(Result~Zonef,family=binomial(link="logit"),data=shotData)
summary(res.glm)
round(1-predict(res.glm,newdata=data.frame(Zonef=levels(shotData$Zonef)),type="response"),3)
round(zone.prob,3)

library(arm)
res.bglm <- bayesglm(Result~Zonef-1,family=binomial(link="logit"),data=shotData)
summary(res.bglm)
round(1-predict(res.bglm,newdata=data.frame(Zonef=levels(shotData$Zonef)),type="response"),3)
round(zone.prob,3)

library(MASS)
res.mglm <- glmmPQL(Result~Zonef-1,random=~1|TGf,family=binomial(link="logit"),data=shotData)
summary(res.mglm)
round(1-predict(res.mglm,newdata=data.frame(Zonef=levels(shotData$Zonef)),type="response",level=0),3)
round(zone.prob,3)

zone.prob2 <- 1-predict(res.mglm,newdata=data.frame(Zonef=levels(shotData$Zonef)),type="response",level=0)


# Distance from center of goal

shotData$Dist <- round(sqrt((shotData$X*.57)^2+((shotData$Y-50)*.745)^2),0)
shotsDist <- with(shotData,table(Dist,Result))
shotsProb <- shotsDist[,1]/(shotsDist[,1]+shotsDist[,2])
plot(as.numeric(names(shotsProb)),shotsProb)
plot(log(as.numeric(names(shotsProb))),shotsProb)
shotData$Goal <- as.numeric(!(as.numeric(shotData$Result)-1))

res <- lm(Goal~log(Dist),data=shotData)
summary(res)
abline(res)

f1 <- function(x) res$coefficients[1]+res$coefficients[2]*log(x)

plot(as.numeric(names(shotsProb)),shotsProb)
curve(f1,from=0,to=60,add=TRUE,col="red")

res.glm <- glm(Goal~Dist,data=shotData,family="binomial")
summary(res.glm)
library(boot)
f2 <- function(x) inv.logit(res.glm$coefficients[1]+res.glm$coefficients[2]*x)

plot(as.numeric(names(shotsProb)),shotsProb)
curve(f2,from=0,to=60,col="red",add=TRUE)

lm(logit((shotsProb+.01)/.8) ~ as.numeric(names(shotsProb)))


res.nls <- nls(Goal ~ theta1/(1 + exp(-(theta2 + theta3*Dist))),
                          start=list(theta2 = .112, theta3 = -.1338,theta1=1.5),data=shotData,
                          trace=TRUE,nls.control(maxiter = 100))

f3 <- function(x) 1.5/(1 + exp(-(.112 + -.1338*x)))
plot(as.numeric(names(shotsProb)),shotsProb)
curve(f3,from=0,to=60,col="red",add=TRUE)

# One Game

shots.sel <- shotData %>% filter(Game==5) %>% filter(Team=="NE")
shots <- SpatialPoints(shots.sel[,1:2])
plot(zones)
points(shots,pch=as.numeric(shots.sel$Result))
res <- over(shots,zones)
shots.sel$Zone <- res

shotsZones <- with(shots.sel,table(Zone,Result))
shotsZones[,1]/(shotsZones[,1]+shotsZones[,2])

# All Shots

table(shotData$Result)
101/(101+579)
round(raw.prob <- 101/(101+579),3)
round(zone.prob,3)
round(zone.prob2,3)
# Expected vs. Actual 3,4,8,15,11,13

shots.sel <- shotData %>% filter(Game==11) %>% filter(Team!="NE")
table(shots.sel$Result) # Two goals on 36 shots
table(shots.sel$Zone) -> game.shots

game.shots
sum(game.shots)*raw.prob

sum(game.shots*zone.prob[as.numeric(names(game.shots))])
sum(game.shots*zone.prob2[as.numeric(names(game.shots))])

    shots.sel <- shotData %>% filter(Game==15) %>% filter(Team!="NE")
table(shots.sel$Result) # Two goals on 36 shots
table(shots.sel$Zone) -> game.shots

game.shots

sum(game.shots*zone.prob[as.numeric(names(game.shots))])


## ggsoccer
library(ggplot2)
library(ggsoccer)

shots.p <- as.data.frame(shotData)
shots.p$X2 <- shots.p$X*.55


ggplot(shots.p,aes(x=X2,y=Y)) +
  annotate_pitch(colour = "white",
                 fill = "chartreuse4") +
  geom_point(aes(fill = Result),
             size = 2, 
             pch = 21) +
  scale_fill_manual(values=c("black","lightyellow"))+
  theme_pitch() +
  coord_flip(xlim = c(0, 50),
             ylim = c(0, 100)) +
  ggtitle("All Shots: 2018")
