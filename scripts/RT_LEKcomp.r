################################################################################
############  M. Karnauskas May 2019                                ############
############  code for compiling red tide oral history information  ############
############  produces outputs for SEDAR 61 working paper           ############

# todo:   NA

rm(list=ls())
setwd('~/Desktop/professional/projects/Postdoc_FL/data/lek')

# import data -------------
d <- read.table("RT_Summary_Spreadsheet_July_corrected.csv", header=T, skip=0, sep=",", quote="\"", stringsAsFactors = F) 

head(d)
names(d)
d$Community[which(d$Community == "Evergaldes City")] <- "Everglades City"  # fix spelling

# cut out notes columns and check input -------------------------
d <- d[1:19]  
head(d)
names(d)

table(d$SEFSC.or.SERO, useNA="always")
table(d$Interview.Number, useNA="always")
length(unique(d$Interview.Number)) # number of interviews == 62
table(d$Interview.ID, useNA="always")
length(table(d$Interview.ID))
table(d$Interviewee, useNA="always")
length(unique(d$Interviewee)) # number of interviewees == 57
table(d$Date.Of.Interview, useNA="always")    
# table(d$RT.Event.Period, useNA="always")
table(d$Year, useNA="always")
table(d$Community, useNA="always")
table(d$Offshore.or.Inshore., useNA="always")       
table(d$Area.Fished, useNA="always")
table(d$SCALE, useNA="always")
table(d$Temporal.Extent, useNA="always")
table(d$Spatial.Extent.of.Red.Tide, useNA="always")
table(d$Map.Legend, useNA="always")
table(d$Areas.Fished.that.are.not.affected.by.RT, useNA="always")
table(d$Species.Targeted, useNA="always")
table(d$Species.Affected, useNA="always")
table(d$Species.NOT.affected, useNA="always")
table(d$Recovery.Time_Months, useNA="always")

# extract date of interview info -------------------------
d$Date.Of.Interview <- as.character((d$Date.Of.Interview))
for (i in 1:nrow(d)) {
  if (nchar(d$Date.Of.Interview[i])==7) { d$Date.Of.Interview[i] <- paste0(0, d$Date.Of.Interview[i]) }
}
sort(strptime(d$Date.Of.Interview, format="%m%d%Y")) # interviews from Nov 2018 to Dec 2019

# calculate number of interviews per community
tab <- table(d$Interview.ID, d$Community)
tab[which(tab>0)] <- 1
data.frame(colSums(tab))

# define counties --------------------------------------
d$County <- NA
d$County[which(d$Community %in% c("Clearwater", "Madeira Beach", "St. Petersberg", "Tarpon Springs"))]<- "Pinellas"
d$County[which(d$Community %in% c("Boca Grande", "Cape Haze", "Placida"))]                            <- "Charlotte"
d$County[which(d$Community %in% c("Chokoloskee",  "Everglades City", "Goodland", "Naples"))]          <- "Collier"
d$County[which(d$Community %in% c("Cortez"))]                                                         <- "Manatee"
d$County[which(d$Community %in% c("Fort Myers Beach", "Pine Island", "Plantation Island"))]           <- "Lee"
d$County[which(d$Community %in% c("Panama City"))]                                                    <- "Bay"
d$County[which(d$Community %in% c("Sarasota"))]                                                       <- "Sarasota"
d$County[which(d$Community %in% c("Steinhatchee"))]                                                   <- "Taylor"

table(d$Community, d$County)
which(rowSums(table(d$Community, d$County))==0)  # check for zeros

d$County <- factor(d$County, levels = c("Collier", "Lee", "Charlotte", "Sarasota", "Manatee", "Pinellas", "Taylor", "Bay"))
d$County
table(d$County, useNA = "always")

d$region <- NA 
d$region[which(d$County %in% c("Collier", "Lee", "Charlotte"))]     <- "SW FL"
d$region[which(d$County %in% c("Sarasota", "Manatee", "Pinellas"))] <- "W Central FL"
d$region[which(d$County %in% c("Taylor"))]                          <- "Big Bend"
d$region[which(d$County %in% c("Bay"))]                             <- "Panhandle"
d$region <- factor(d$region, levels = c("SW FL", "W Central FL", "Big Bend" , "Panhandle"))
d$region
table(d$region, d$County)
table(d$region, useNA = "always")

# convert scale to numerical ranking for plotting --------------- 
table(d$SCALE, useNA="always")
nrank <- 3    
table(d$SCALE, d$Year)
d$rat <- nrank + 1 - as.numeric(as.factor(d$SCALE))   
table(d$SCALE, d$rat)                        
table(d$rat, d$Year)

# condense events into major red tide periods --------------------

table(d$Year)

yr <- as.character(round(d$Year))
levels(yr) <- as.character(1930:2020)
plot(table(yr), las=2)                # look for event periods
plot(table(yr), las=2, xlim=c(1960, 2019), ylab = "number of mentions")
table(yr)

which(table(yr) > 1)                  # 2 or more mentions
which(table(yr) > 2)                  # 3 or more mentions
which(table(yr) > 9)

# decide how to split up - different methods for this 
# method 1: all years of data
# brks <- c(1930, 1950, seq(1960, 1990, 10), 2000, 2003, 2006, 2011, 2015, 2016, 2020)
# d$event <- cut(d$Year, breaks = brks)

# method 2: analyze only years with 10 or more obs
d$event <- NA
lis <- as.numeric(names(which(table(yr) > 9)))  # create list of year-specific events only
d$event[which(yr %in% lis)] <- yr[which(yr %in% lis)]

# method 3: analyze only years with 10 or more obs but include margin of error +/- 1 year
lis1 <- lis + 1
lis2 <- lis - 1
d$event[which(yr %in% lis1)] <- as.numeric(yr[which(yr %in% lis1)]) - 1
d$event[which(yr %in% lis2)] <- as.numeric(yr[which(yr %in% lis2)]) + 1

table(d$event, useNA = "always")
d$Year[which(is.na(d$event))]             # check NAs
table(d$event, d$Year)                    # check results

# define events -----------------------------------------------

d$SCALE[which(d$SCALE == "Devastating")] <- "Extreme"
d$SCALE <- factor(d$SCALE, levels = c("Minor", "Major", "Extreme"))

tab <- table(d$event, d$SCALE); tab
tab1 <- tab / rowSums(tab)
tab
tab1
# labels ---------
labs <- NA
st <- substr(rownames(tab), 2, 5)
en <- substr(rownames(tab), 7, 10)
l1 <- which((as.numeric(en) - as.numeric(st)) == 10)
labs[l1] <- paste0(st[l1], "s")
l2 <- which((as.numeric(en) - as.numeric(st)) != 10)
labs[l2] <- paste0(as.numeric(st[l2])+1, "-", en[l2])
l3 <- which((as.numeric(en) - as.numeric(st)) == 1)
labs[l3] <- paste0(en[l3])
labs

if (is.na(labs))  { labs <- rownames(tab) }
labs

cols <- c("#FF000015", "#FF000050", "#FF000095")

################################################################################
###############################    PLOTTING    #################################

# plot severity over time -------------------------------
#pdf(file="all_events.pdf", width=8, height=5)

# plotting by region instead of county b/c too many counties (produced illegible symbols)

par(mar=c(5,4,2,1))
plot(d$Year, d$rat, col="#FF000050", 
     pch = as.numeric(d$region)+1, cex = 2, 
     xlab = "year", ylab = "event severity rating", axes = F, ylim = c(0.8, 3.2))
axis(1)
axis(2, at = 1:3, lab = c("minor", "major", "extreme"))
box()
legend("topleft", names(table(d$region)), pt.cex=2, col="#FF000060", pch=2:5) 
#abline(v=c(2003.5, 2005.5, 2013.5, 2014.5, 2017.5, 2019), lty=2, col=8)

# dev.off()

# longevity of event -------------------------------------
d$tim <- as.numeric(as.character(d$Temporal.Extent.Months))  # converts descriptions to NAs - warning OK
cbind(d$tim, d$Temporal.Extent.Months)                       # check

tapply(d$tim, d$event, mean, na.rm=T)
tapply(d$tim, d$event, sd, na.rm=T)

#pdf(file="temp_extent.pdf", width=8, height=5)

max(d$tim, na.rm=T)                           # max y-axis
min(d$Year[which(!is.na(d$tim))], na.rm = T)  # min x-axis

par(mar=c(5,4,2,1))
plot(d$Year, d$tim, col = "#FF000070", 
     pch = as.numeric(d$region)+1, cex = 2, xlim = c(1955, 2020), ylim = c(0, 20), 
      xlab = "year", ylab = "temporal extent of event (months)", axes = F)
axis(1); axis(2, las=2); box()
legend("topleft", names(table(d$region)), pt.cex=2, col="#FF000070", pch=2:5) 

# dev.off()

###########################   recovery after event   ###########################
d$recov <- as.numeric(as.character(d$Recovery.Time_Months))

d$Recovery.Time_Months[grep("recover", d$Recovery.Time_Months)]   # check to see that all of these are "still recovering" or "not yet recovered"
d$recov[grep("recover", d$Recovery.Time_Months)] <- 70
data.frame(d$Recovery.Time_Months, d$recov)                       # check conversion

#pdf(file="recovery.pdf", width=8, height=5)

par(mar=c(5,5,1,1))
plot(d$Year, d$recov, col="#FF000070", 
     pch = as.numeric(d$region)+1, cex = 2, xlab = "year", ylab = "recovery time (years)         ", 
     axes=F, ylim=c(0,75))
axis(1, at=seq(1940, 2020,10))
axis(2, las=2, at=seq(0, 60, 12), lab=0:5)
axis(2, at=70, lab="still \nrecovering", las=2)
legend(1937, 60, names(table(d$region)), pt.cex=2, col="#FF000070", pch=2:5) 

# dev.off()

# by event -----------------------------------------------

d$SCALE[which(d$SCALE == "Devastating")] <- "Extreme"
d$SCALE <- factor(d$SCALE, levels = c("Minor", "Major", "Extreme"))

tab <- table(d$event, d$SCALE); tab
tab1 <- tab / rowSums(tab)
tab
tab1
# labels ---------
labs <- NA
st <- substr(rownames(tab), 2, 5)
en <- substr(rownames(tab), 7, 10)
l1 <- which((as.numeric(en) - as.numeric(st)) == 10)
labs[l1] <- paste0(st[l1], "s")
l2 <- which((as.numeric(en) - as.numeric(st)) != 10)
labs[l2] <- paste0(as.numeric(st[l2])+1, "-", en[l2])
l3 <- which((as.numeric(en) - as.numeric(st)) == 1)
labs[l3] <- paste0(en[l3])
labs

if (is.na(labs))  { labs <- rownames(tab) }
labs

cols <- c("#FF000015", "#FF000050", "#FF000095")

#pdf(file="by_event.pdf", width=6, height=5)

par(mar=c(5,4,1,0.5))

b <- barplot(t(tab1), beside = F, col = cols, ylim = c(0, 1.35), axes = F, 
    names.arg = labs, las = 1, ylab="proportion of ratings                ",
    args.legend = list(x = "top", col = cols, horiz = T),
                legend.text = c("minor", "major", "extreme"))
mtext(side = 1, line = 3, "red tide event")

tail(table(d$event, d$SCALE))  # check that legend is correct

axis(2, at = seq(0,1, 0.2), lab = seq(0,1, 0.2), las = 2)
text(b, 1.1, paste0(rowSums(tab)), las=2)
#text(b, 1.1, paste0("n=", rowSums(tab)), las=2)
abline(h = 0)

# dev.off()

# by area --------------------------------------

tab <- table(d$County, d$SCALE)
tab1 <- tab / rowSums(tab)
tab
tab1

chisq.test(tab)
chisq.test(tab1)

#pdf(file="by_area.pdf", width=6, height=5)

par(mar=c(5,4,1,0.5))
b <- barplot(t(tab1), beside = F, col = cols, ylim = c(0, 1.3), axes = F, 
             args.legend=list(x = "top", horiz=T), legend.text = colnames(tab),
             ylab="proportion of ratings                    ", 
             xlab="home county of interviewee")
axis(2, at = seq(0,1, 0.2), lab = seq(0,1, 0.2), las = 2)
abline(h=0)                                      
text(b, 1.05, paste("n =", rowSums(tab)))

# dev.off()

# by area and event --------------------------
#tab3 <- table(d$County, d$SCALE, d$event)
#tab <- rbind(tab3[,,4], rep(NA, 3), rep(NA, 3), tab3[,,1], rep(NA, 3), rep(NA, 3), tab3[,,2], rep(NA, 3), rep(NA, 3), tab3[,,3])
#tab1 <- tab / rowSums(tab)   
#tab
#tab1

#pdf(file="by_area_and_event.pdf", width=6, height=5)

#par(mar=c(6, 4, 1, 1))
#b <- barplot(t(tab1), beside=F, col=cols, ylim=c(0, 1.3), axes=F,  
#args.legend=list(x = "top", horiz=T), legend.text=colnames(tab1), las=2,
#ylab="proportion of ratings")
#mtext(side=1, line=5, "home county of interviewee")
#axis(2, at=seq(0,1, 0.2), lab=seq(0,1, 0.2), las=2)
#abline(h=0)                                     
#text(b, 1.05, paste(rowSums(tab)))
#text(b[seq(3, length(b)-2, length.out=4)], 1.05, unique(d$event)[c(1, 2, 4, 3)])
      
#dev.off()

# by zone ---------------------------------
 
d$zone <- d$Offshore.or.Inshore.
table(d$Offshore.or.Inshore., d$zone)          # check reclassification

tab <- table(d$zone, d$SCALE)
tab <- tab[c(2, 4, 1),]
tab1 <- tab / rowSums(tab)           
tab
tab1

#pdf(file="by_zone.pdf", width=5, height=5)

par(mar=c(5,4,1,0.5))
b <- barplot(t(tab1), beside = F, col = cols, ylim = c(0, 1.3), axes = F,  
             args.legend=list(x = "top", horiz=T), legend.text = colnames(tab1), 
             ylab="proportion of ratings", xlab="zone of fishing")
axis(2, at = seq(0,1, 0.2), lab = seq(0,1, 0.2), las = 2)
text(b, 1.05, paste("n =", rowSums(tab)))
abline(h=0)
    
# dev.off()

# number of interviewees -----------------------------------

#pdf(file="num_interviewees.pdf", width=6, height=4)

tab <- table(d$region, d$event)
tab
par(mar=c(3,4,1,0.5))
b <- barplot(tab, beside = T, col= 2:5, 
             args.legend = list(x = "topleft", bty = "n", title = "home location of interviewee"),
             names.arg = labs, las = 1,
             legend.text = rownames(tab), axes = F,
             ylab = "number of interviewees\ndescribing event", xlab = "")

axis(2, las=2)
abline(h=0)
         
# dev.off()

# grouper affected ----------------------------------
table(d$Species.Affected=="")

d2 <- d[d$Species.Affected != "",]
d2$grouper <- 0

# d2$Species.Affected[grep("grouper", d2$Species.Affected)]
# d2$grouper[grep("grouper", d2$Species.Affected)] <- 1     
d2$Species.Affected[grep("[gG+][rRaA+][oOgG+][uU+]?[pP+]?[eE+]?[rR+]?", d2$Species.Affected)]
d2$grouper[grep("[gG+][rRaA+][oOgG+][uU+]?[pP+]?[eE+]?[rR+]?", d2$Species.Affected)] <- 1     

d2$Species.Affected[grep("grouper", d2$Species.Affected)]
d2$Species.Affected[grep("black grouper", d2$Species.Affected)]
d2$Species.Affected[grep("red grouper", d2$Species.Affected)]
d2$Species.Affected[grep("goliath", d2$Species.Affected)]
d2$Species.Affected[grep("gag", d2$Species.Affected)]

tab <- table(d2$event, d2$grouper)
tab <- tab[-c(1,2),]
tab1 <- tab/rowSums(tab)
tab
tab1

#pdf(file="spp_killed.pdf", width=6, height=5)

par(mar = c(7,4,1,0.5))
b <- barplot(t(tab1), beside = F, col = c(3,4), 
             ylim = c(0, 1.3), axes = F, names.arg = rownames(tab), las = 1,  
             args.legend = list(x = "top", horiz = T, bty = "n"), 
             legend.text = c("fish species other than grouper", "grouper species"), 
             ylab = "proportion of species-specific fish kill mentions")
mtext(side = 1, line= 5.5, "red tide event")
axis(2, at=seq(0,1, 0.2), lab=seq(0,1, 0.2), las=2)
abline(h=0)
text(b, 1.05, paste("n =", rowSums(tab)))

# dev.off()


tab <- table(d2$SCALE, d2$grouper)
tab1 <- tab/rowSums(tab)
tab
tab1

#pdf(file="spp_killed.pdf", width=6, height=5)

par(mar = c(7,4,1,0.5))
b <- barplot(t(tab1), beside = F, col = c(3,4), 
             ylim = c(0, 1.3), axes = F, names.arg = rownames(tab), las = 1,  
             args.legend = list(x = "top", horiz = T, bty = "n"), 
             legend.text = c("fish species other than grouper", "grouper species"), 
             ylab = "proportion of species-specific fish kill mentions")
# mtext(side = 1, line= 5.5, "red tide event")
axis(2, at=seq(0,1, 0.2), lab=seq(0,1, 0.2), las=2)
# abline(h=0)
text(b, 1.05, paste("n =", rowSums(tab)))

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

smin <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Minor")], ";"))
smaj <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Major")], ";"))
sext <- unlist(strsplit(d2$Species.Affected[which(d2$SCALE == "Extreme")], ";"))
spp <- c(smin, smaj, sext)
rat <- c(rep("minor", length(smin)), rep("major", length(smaj)), rep("extreme", length(sext)))

dat <- data.frame(spp, rat, NA)
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
tab <- tab[order(rowSums(tab)),]
tab <- tail(tab, 20)

cols <- rainbow(20)

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

tab1 <- cbind(tab[,1]/colSums(tab)[1], tab[,2]/colSums(tab)[2], tab[,3]/colSums(tab)[3])
colnames(tab1) <- colnames(tab)

par(mar = c(4, 4, 1, 0.5))
b <- barplot(tab1, beside = F, col = cols, axes = F, xlim = c(0, 4.5),  
             args.legend = list(x = "right", horiz = F, bty = "n"), 
             legend.text = rownames(tab), 
             ylab = "proportion of species-specific fish kill mentions")
axis(2, las=2)
abline(h=0)

# dev.off()


##################################    END     ##################################
################################################################################







