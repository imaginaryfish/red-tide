rm(list=ls())
setwd('~/Desktop/professional/projects/Postdoc_FL/data/lek')

d <- read.csv('lek_processed.csv')



# grouper affected ----------------------------------
table(d$Species.Affected=="")

d2 <- d[d$Species.Affected != "",]
d2$grouper <- 0

# d2$Species.Affected[grep("grouper", d2$Species.Affected)]
# d2$grouper[grep("grouper", d2$Species.Affected)] <- 1     
d2$Species.Affected[grep("[gG+][rRaA+][oOgG+][uU+]?[pP+]?[eE+]?[rR+]?", d2$Species.Affected)]
d2$grouper[grep("[gG+][rRaA+][oOgG+][uU+]?[pP+]?[eE+]?[rR+]?", d2$Species.Affected)] <- 1     


tab <- table(d2$event, d2$grouper)
tab <- tab[-c(1,2),]
tab1 <- tab/rowSums(tab)
tab
tab1

setwd('~/Desktop/professional/publications/2020/sedar_LEK_wp/figures')
png('evnt_spp_killed.png',width=7,height=6,units='in',res=300)
par(mar = c(5,4,1,1))
b <- barplot(t(tab1), beside = F, col = c(3,4), 
             ylim = c(0, 1.3), axes = F, names.arg = rownames(tab), las = 1,  
             args.legend = list(x = "top", horiz = T, bty = "n"), 
             legend.text = c("Fish species other than grouper", "Grouper species"), 
             ylab = "Proportion of species-specific fish kill mentions")
mtext(side = 1, line=2.5, "Red tide event")
axis(2, at=seq(0,1, 0.2), lab=seq(0,1, 0.2), las=2)
# abline(h=0)
text(b, 1.05, paste("n =", rowSums(tab)))
dev.off()


tab <- table(d2$SCALE, d2$grouper)
tab1 <- tab/rowSums(tab)


png('scale_spp_killed.png',width=7,height=6,units='in',res=300)
par(mar = c(5,4,1,1))
b <- barplot(t(tab1), beside = F, col = c(3,4), 
             ylim = c(0, 1.3), axes = F, names.arg = rownames(tab), las = 1,  
             args.legend = list(x = "top", horiz = T, bty = "n"), 
             legend.text = c("Fish species other than grouper", "Grouper species"), 
             ylab = "Proportion of species-specific fish kill mentions")
mtext(side = 1, line= 2.5, "Severity")
axis(2, at=seq(0,1, 0.2), lab=seq(0,1, 0.2), las=2)
# abline(h=0)
text(b, 1.05, paste("n =", rowSums(tab)))
dev.off()


# tabulate all species affected -----------------------
table(d$Species.Affected=="")

d2 <- d[d$Species.Affected != "",]
dim(d2)

splis <- unlist(strsplit(d2$Species.Affected, ";"))

for (i in 1:length(splis))  { 
  ### remove leading space
  if (substr(splis[i], 1, 1) == " ") { 
    splis[i] <- substr(splis[i], 2, (nchar(splis[i])))  }
  ### remove trailing space
  if (substr(splis[i], nchar(splis[i]), nchar(splis[i])) == " ") { 
    splis[i] <- substr(splis[i], 1, (nchar(splis[i])-1))  }
}

splis
table(splis)
sort(table(splis), decreasing = F)
head(sort(table(splis), decreasing = T), 94)

spnam <- names(sort(table(splis), decreasing = T)[1:94])
spnam

### not sure why gag and goliath are being removed
rem <- c("fish", "everything", "gag grouper", "bottom fish", "all species", "", "fishes", "reef fish", "silver trout", "thread herring", "goliath grouper", 
         "trash fish", "a lot of pinfish", "all types of fish", "goliath groupers", "golitath groupers", "mostly red grouper", "no major impacts on fish species", 
         "none", "small fish mainly", "small fish", "some fish kills on the beach", "threadfin herring", " baitfish")

spnam <- spnam[-which(spnam %in% rem)]
spnam[which(spnam == "baitfish")] <- "bait"

for (i in 1:length(spnam))  { 
  if (substr(spnam[i], nchar(spnam[i]), nchar(spnam[i])) == "s") { 
    spnam[i] <- substr(spnam[i], 1, (nchar(spnam[i])-1))  }
}

length(spnam)
spnam <- unique(spnam)
length(spnam)

### not sure why these are being added
add <- c("houndfish", "barracuda", "blennies", "lionfish", "sennet", "sea bream", "mussels", 
         "manta ray", "mojarra", "octopus", "pilchards", "scamp", "almaco", "oarfish", "seabass", 
         "cowfish", "conch", "whelk", "barnacles", "permit", "moray", "croaker", "pufferfish", "kingfish",
         "thread", "sea fan", "gorgonian", "pelican", "sea horse", "hammerhead", "snake fish", "spadefish")
spnam <- c(spnam, add)
spnam

spnam <- spnam[order(nchar(spnam))]
spnam

s1 <- unlist(strsplit(d2$Species.Affected, ";"))
s1c <- rep(NA, length(s1))

for (j in 1: length(spnam))  { 
  s1c[grep(spnam[j], s1)] <- spnam[j]
}

cbind(s1, s1c)
s1[is.na(s1c)] 

sort(table(s1c))


### by severity
smin <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Minor")], ";"))
smaj <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Major")], ";"))
sext <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Extreme")], ";"))
spp <- c(smin, smaj, sext)
rat <- c(rep("minor", length(smin)), rep("major", length(smaj)), rep("extreme", length(sext)))
dat <- data.frame(spp, rat, NA)

### by event
s05 <- unlist(strsplit(d2$Species.Affected[which(d2$event == 2005)], ";"))
s14 <- unlist(strsplit(d2$Species.Affected[which(d2$event == 2014)], ";"))
s18 <- unlist(strsplit(d2$Species.Affected[which(d2$event == 2018)], ";"))
spp <- c(s05, s14, s18)
evnt <- c(rep(2005, length(s05)), rep(2014, length(s14)), rep(2018, length(s18)))
dat <- data.frame(spp, evnt, NA)

### by event
soff <- unlist(strsplit(d2$Species.Affected[which(d2$Offshore.or.Inshore. == 'offshore')], ";"))
sin <- unlist(strsplit(d2$Species.Affected[which(d2$Offshore.or.Inshore. == 'inshore')], ";"))
sboth <- unlist(strsplit(d2$Species.Affected[which(d2$Offshore.or.Inshore. == 'both')], ";"))
spp <- c(soff, sin, sboth)
area <- c(rep('offshore', length(soff)), rep('inshore', length(sin)), rep('both', length(sboth)))
dat <- data.frame(spp, area, NA)


names(dat)[3] <- "label"
head(dat)

for (j in 1: length(spnam))  { 
  dat$label[grep(spnam[j], dat$spp)] <- spnam[j]
}

dat <- dat[!is.na(dat$label),]

sort(table(dat$label))

dat$label[which(dat$label == "porgie")] <- "porgy"
dat$label[which(dat$label == "bait")] <- "baitfish"
dat$label[which(dat$label == "\nblue crab")] <- "blue crab"
dat$label[which(dat$label == "seagras")] <- "seagrass" 
dat$label[which(dat$label == "pelican")] <- "bird" 
dat$label[which(dat$label == "seabass")] <- "grouper"
dat$label[which(dat$label == "porpoise")] <- "dolphin" 

dat$lab2 <- dat$label

jck <- c("almaco", "permit", "pompano")
mck <- c("kingfish")
shk <- c("spinner", "whale shark", "hammerhead", "black tip", "blacknose", "nurse shark", "sharpnose")
ben <- c("barnacles", "clam", "conch", "coral", "gorgonian", "mussels", "sponge", "whelk", "sea fan", "seagrass")
drm <- c("black drum", "croaker", "whiting", "redfish", "trout", "red drum", "sand trout")
grt <- c("pigfish", "tomtate")
eel <- c("moray", "sand eel", "sea snake")
bt <-  c("thread", "spanish sardine", "needlefish", "pilchards")
prg <- c("sea bream", "sheepshead")
ray <- c("manta ray", "skate", "stingray")
grp <- c("scamp", "gag", "goliath")

dat$lab2[which(dat$label %in% jck)] <- "jack"
dat$lab2[which(dat$label %in% mck)] <- "mackerel"
dat$lab2[which(dat$label %in% shk)] <- "shark" 
dat$lab2[which(dat$label %in% ben)] <- "benthic"
dat$lab2[which(dat$label %in% drm)] <- "drum" 
dat$lab2[which(dat$label %in% grt)] <- "grunt" 
dat$lab2[which(dat$label %in% eel)] <- "eel"
dat$lab2[which(dat$label %in% bt)] <- "baitfish"
dat$lab2[which(dat$label %in% prg)] <- "porgy"
dat$lab2[which(dat$label %in% ray)] <- "rays"
dat$lab2[which(dat$label %in% grp)] <- "grouper"

dat$lab2[grep("crab", dat$label)] <- "crab"
dat$lab2[grep("grouper", dat$label)] <- "grouper"
dat$lab2[grep("snapper", dat$label)] <- "snapper"

table(dat$lab2)

tab <- table(dat$lab2, dat$rat)
tab <- table(dat$lab2, dat$evnt)
tab <- table(dat$lab2, dat$area)

tab <- tab[order(rowSums(tab)),]
tab <- tail(tab, 20)
sort(rownames(tab))

setwd('~/Desktop/professional/publications/2020/sedar_LEK_wp/figures')
write.csv(tab,'spp_killed_evnt.csv')
write.csv(tab,'spp_killed_scale.csv')
write.csv(tab,'spp_killed_area.csv')

cols <- rainbow(20)
cols <- colorRampPalette(c('orangered','gold','chartreuse','deepskyblue','purple'))
cols <- colorRampPalette(c('slateblue3','dodgerblue2','chartreuse','gold','orangered','gray40'))
cols <- rev(cols(20))
# col1 <- colorRampPalette(c('dodgerblue3','orangered'))
# col2 <- colorRampPalette(c('green2','purple3'))
# cols <- c(col1(10),col2(10))
# cols <- rep(NA,20)
# cols[seq(1,20,2)] <- col1(10)
# cols[seq(2,20,2)] <- col2(10)


#pdf(file="spp_killed.pdf", width=6, height=5)

par(mar = c(4, 4, 1, 0.5))
b <- barplot(tab, beside = F, col = cols, axes = F, xlim = c(0, 4.5),
             args.legend = list(x = "right", horiz = F, bty = "n"),
             legend.text = rownames(tab),
             ylab = "number of species-specific fish kill mentions")
# b <- barplot(tab, beside = T, col = cols, axes = F,  
# args.legend = list(x = "right", horiz = F, bty = "n"), 
# legend.text = rownames(tab), 
# ylab = "number of species-specific fish kill mentions")
axis(2, las=2)
abline(h=0)

# tab1 <- cbind(tab[,1]/colSums(tab)[1], tab[,2]/colSums(tab)[2], tab[,3]/colSums(tab)[3])
tab1 <- t(t(tab)/colSums(tab))
colnames(tab1) <- colnames(tab)

setwd('~/Desktop/professional/publications/2020/sedar_LEK_wp/figures')
png('spp_killed_scale.png',width=7,height=6,units='in',res=300)
png('spp_killed_evnt.png',width=7,height=6,units='in',res=300)
png('spp_killed_area.png',width=7,height=6,units='in',res=300)
par(mar = c(4, 4, 1, 0.5))
b <- barplot(tab1, beside = F, col = cols, axes = F, xlim = c(0, 4.5),ylim=c(0,1.1),
             args.legend = list(x = "right", horiz = F, bty = "n"), 
             legend.text = rownames(tab), 
             ylab = "Proportion of species-specific fish kill mentions")
# mtext(side = 1, line= 2.5, "Severity")
# mtext(side = 1, line= 2.5, "Red Tide event")
mtext(side = 1, line= 2.5, "Region")
axis(2, las=2)
text(b, 1.05, paste("n =", colSums(tab)))
dev.off()


# inshore/offshore ----------------------------------

yr <- as.character(round(d$Year))
levels(yr) <- as.character(1930:2020)

### year versus inshore/offshore
# yr_region <- table(d$Year,d$Offshore.or.Inshore.)
yr_region <- table(yr,d$Offshore.or.Inshore.)
yr_region <- yr_region[,c(2,4,1)]
yr_sum <- apply(yr_region,1,sum,na.rm=T)
yr_region_ratio <- yr_region/yr_sum

barplot(t(yr_region_ratio),las=2,legend.text = c('inshore','offshore','both'))

# yr_region_scale <- table(d$Year,d$Offshore.or.Inshore.,d$SCALE)
yr_region_scale <- table(yr,d$Offshore.or.Inshore.,d$SCALE)
yr_region_scale <- yr_region_scale[,c(2,4,1),2] ### 1==Devastating, 2==Major, 3==Minor
yr_sum <- apply(yr_region_scale,1,sum,na.rm=T)
yr_region_ratio <- yr_region_scale/yr_sum
barplot(t(yr_region_ratio),las=2,legend.text = c('inshore','offshore','both'),args.legend = list(bty='n'))


region_scale <- table(d$Offshore.or.Inshore.,d$SCALE)
region_scale <- region_scale[-3,]
row_sum <- apply(region_scale,1,sum,na.rm=T)
region_ratio <- region_scale/row_sum
region_ratio2 <- t(region_scale)/colSums(region_scale)


# cols <- c("#FF000015", "#FF000050", "#FF000095")
cols <- colorRampPalette(c('darkseagreen1','deepskyblue4'))
cols <- cols(3)

setwd('~/Desktop/professional/publications/2020/sedar_LEK_wp/figures')
png('scale_region.png',width=7,height=6,units='in',res=300)
par(mar = c(5,4,1,1))
b <- barplot(t(region_ratio2),
             legend.text = c(rownames(region_ratio)),
             args.legend = list(x='top',horiz=T,bty='n'),
             col=cols,
             ylab = "Proportion of area affected mentions",
             ylim=c(0,1.2),yaxt='n',las=1)
mtext(side = 1, line= 2.5, "Severity")
axis(2,seq(0,1,.2),las=1)
text(b, 1.05, paste("n =", colSums(region_scale)))
dev.off()

### alternate figure
# b <- barplot(t(region_ratio),
#              legend.text = c(colnames(region_ratio)),
#              args.legend = list(x='top',horiz=T,bty='n'),
#              col=cols,
#              ylim=c(0,1.2),yaxt='n',las=1)
# axis(2,seq(0,1,.2),las=1)
# text(b, 1.05, paste("n =", rowSums(region_scale)))



region_scale <- table(d$Offshore.or.Inshore.,d$event)
region_scale <- region_scale[-3,-c(1,2)]
row_sum <- apply(region_scale,1,sum,na.rm=T)
region_ratio <- region_scale/row_sum
region_ratio2 <- t(region_scale)/colSums(region_scale)


# cols <- c("#FF000015", "#FF000050", "#FF000095")
cols <- colorRampPalette(c('darkseagreen1','deepskyblue4'))
cols <- cols(3)

setwd('~/Desktop/professional/publications/2020/sedar_LEK_wp/figures')
png('event_region.png',width=7,height=6,units='in',res=300)
par(mar = c(5,4,1,1))
b <- barplot(t(region_ratio2),
             legend.text = c(colnames(region_ratio2)),
             args.legend = list(x='top',horiz=T,bty='n'),
             col=cols,
             ylab = "Proportion of area affected mentions",
             ylim=c(0,1.2),yaxt='n',las=1)
mtext(side = 1, line= 2.5, "Red tide event")
axis(2,seq(0,1,.2),las=1)
text(b, 1.05, paste("n =", colSums(region_scale)))
dev.off()


