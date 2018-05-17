## Load pathmapping
library(pathmapping)

dat.raw <- read.csv("mappoints-all.csv")

##do lakes1-1 problem
given <- read.csv("Lakes1-1.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Lakes 1-1"
problem <- dat.raw[dat.raw$problem=="Lakes1"&dat.raw$datafile=="1.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output1 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output1$problem[i] <- prob
  output1$deviation[i] <- mapping1$deviation
  output1$overlap[i] <- PathOverlap(mapping)
  output1$PathDistA[i] <- PathDist(as.matrix(given))
  output1$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output1$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output1)

dev.off()






##do lakes1-2 problem
given <- read.csv("Lakes1-2.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

#plot(given,type="o",pch=16)

prob <- "Lakes 1-2"
problem <- dat.raw[dat.raw$problem=="Lakes1"&dat.raw$datafile=="2.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output2 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output2$problem[i] <- prob
  output2$deviation[i] <- mapping1$deviation
  output2$overlap[i] <- PathOverlap(mapping)
  output2$PathDistA[i] <- PathDist(as.matrix(given))
  output2$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output2$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output2)

dev.off()





##do lakes2-1 problem
given <- read.csv("Lakes2-1.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]
par(mfrow = c(3,3))
plot(given,lwd=2,type="o",pch=.5)
given <- data.frame(SimplifyPath(as.matrix(given), tolerance = 0.2, faster = T, verbose = F, plot = F))

prob <- "Lakes 2-1"
problem <- dat.raw[dat.raw$problem=="Lakes2"&dat.raw$datafile=="1.csv.txt",]

id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output3 <- data.frame(id=levels(as.factor(id)))

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  tmp <- data.frame(SimplifyPath(as.matrix(tmp[, c(9,10)]), tolerance = 0.2, faster = T, verbose = F, plot = F))
  colnames(tmp) <- c("x", "y")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output3$problem[i] <- prob
  output3$deviation[i] <- mapping1$deviation
  output3$overlap[i] <- PathOverlap(mapping)
  output3$PathDistA[i] <- PathDist(as.matrix(given))
  output3$PathDistB[i] <- PathDist(as.matrix(tmp))
  output3$MinLinkCost[i] <- mapping1$minlinkcost
}


print(output3)

dev.off()





##do lakes2-2 problem
given <- read.csv("Lakes2-2.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]
par(mfrow = c(3,3))
plot(given,lwd=2,type="o",pch=.5)
given <- data.frame(SimplifyPath(as.matrix(given), tolerance = 0.2, faster = T, verbose = F, plot = F))

prob <- "Lakes 2-2"
problem <- dat.raw[dat.raw$problem=="Lakes2"&dat.raw$datafile=="2.csv.txt",]

id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output4 <- data.frame(id=levels(as.factor(id)))

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  tmp <- data.frame(SimplifyPath(as.matrix(tmp[, c(9,10)]), tolerance = 0.2, faster = T, verbose = F, plot = F))
  colnames(tmp) <- c("x", "y")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output4$problem[i] <- prob
  output4$deviation[i] <- mapping1$deviation
  output4$overlap[i] <- PathOverlap(mapping)
  output4$PathDistA[i] <- PathDist(as.matrix(given))
  output4$PathDistB[i] <- PathDist(as.matrix(tmp))
  output4$MinLinkCost[i] <- mapping1$minlinkcost
}


print(output4)

dev.off()







##do Riley problem
given <- read.csv("Riley.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Riley"
problem <- dat.raw[dat.raw$problem=="Riley"&dat.raw$datafile=="2.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output5 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output5$problem[i] <- prob
  output5$deviation[i] <- mapping1$deviation
  output5$overlap[i] <- PathOverlap(mapping)
  output5$PathDistA[i] <- PathDist(as.matrix(given))
  output5$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output5$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output5)

dev.off()









##do Topo-1 problem
given <- read.csv("Topo-1.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Topo-1"
problem <- dat.raw[dat.raw$problem=="Topo"&dat.raw$datafile=="1.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output6 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output6$problem[i] <- prob
  output6$deviation[i] <- mapping1$deviation
  output6$overlap[i] <- PathOverlap(mapping)
  output6$PathDistA[i] <- PathDist(as.matrix(given))
  output6$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output6$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output6)

dev.off()








##do Topo-2 problem
given <- read.csv("Topo-2.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Topo-2"
problem <- dat.raw[dat.raw$problem=="Topo"&dat.raw$datafile=="2.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output7 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output7$problem[i] <- prob
  output7$deviation[i] <- mapping1$deviation
  output7$overlap[i] <- PathOverlap(mapping)
  output7$PathDistA[i] <- PathDist(as.matrix(given))
  output7$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output7$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output7)

dev.off()









##do Urban-1 problem
given <- read.csv("Urban-1.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Urban-1"
problem <- dat.raw[dat.raw$problem=="Urban"&dat.raw$datafile=="1.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output8 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output8$problem[i] <- prob
  output8$deviation[i] <- mapping1$deviation
  output8$overlap[i] <- PathOverlap(mapping)
  output8$PathDistA[i] <- PathDist(as.matrix(given))
  output8$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output8$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output8)

dev.off()










##do Urban-2 problem
given <- read.csv("Urban-2.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Urban-2"
problem <- dat.raw[dat.raw$problem=="Urban"&dat.raw$datafile=="2.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
output9 <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
  output9$problem[i] <- prob
  output9$deviation[i] <- mapping1$deviation
  output9$overlap[i] <- PathOverlap(mapping)
  output9$PathDistA[i] <- PathDist(as.matrix(given))
  output9$PathDistB[i] <- PathDist(as.matrix(tmp[, c(9,10)]))
  output9$MinLinkCost[i] <- mapping1$minlinkcost
}

print(output9)

dev.off()


output <- rbind(output1, output2, output3, output4, output5, output6, output7, output8, output9)

final.output <- output

final.output$sub <- NA
final.output$mopp <- NA
final.output$round <- NA

for(i in 1:nrow(final.output)) {
  final.output[[i, 8]] <- strsplit(as.character(final.output[[i, 1]]), "-")[[1]][1]
  final.output[[i, 9]] <- strsplit(as.character(final.output[[i, 1]]), "-")[[1]][2]
  final.output[[i, 10]] <- strsplit(as.character(final.output[[i, 1]]), "-")[[1]][3]
}


final.output <- final.output[, c(1, 8:10, 2:7)]

write.csv(final.output, file = "All.Values.csv")

model <- lm(deviation ~ mopp + problem, data = final.output)
summary(model)


final.output$sdv <- final.output$deviation/(final.output$PathDistA)
final.output$adjusted <- final.output$sdv/final.output$overlap
final.output$logadj <- log( final.output$adjusted)

model1 <- lm(sdv ~ mopp + problem, data = final.output)
model2 <- lm(adjusted ~ mopp + problem, data = final.output)
model3 <- lm(logadj ~ mopp + problem, data = final.output)
model4 <- lm(overlap ~ mopp + problem, data = final.output)

summary(model1)
summary(model2)
summary(model3)
summary(model4)







given <- read.csv("Lakes1-1.csv",header=F)
colnames(given)<-c("x","y","N")
given <- given[,-3]

prob <- "Lakes 1-1"
problem <- dat.raw[dat.raw$problem=="Lakes1"&dat.raw$datafile=="1.csv.txt",]
id <- paste(problem$sub,problem$mopp,problem$round, sep="-")
dat <- data.frame(id=levels(as.factor(id)))

par(mfrow = c(3,3))

plot(given,lwd=2,type="o",pch=.5)

#plot(summary$cumdist2,summary$dist,type="l",ylim=c(0,1000))
levs <-  levels(as.factor(id))
for(i in 1:length(levs))
{
  
  tmp <- problem[id==levs[i],]
  #        points(tmp$x,tmp$y,type="l")
  mapping <- CreateMap(cbind(tmp$x,tmp$y),given)
  mapping1 <- GetMinMap(mapping)
  PlotMap(mapping1)
  summary<-SummarizeMapping(mapping1)
  
  ##let's look at summary distance as a function of length along path.
  ##points(summary$cumdist2,summary$dist,type="l")
  print(mapping1$deviation)
  print(PathOverlap(mapping))
}

print(output1)

par(mfrow = c(4, 6))


