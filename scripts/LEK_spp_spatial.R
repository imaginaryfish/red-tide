rm(list=ls())
setwd('~/Desktop/professional/projects/Postdoc_FL/data/lek')

# import data -------------
d <- read.table("RT_Summary_Spreadsheet_July_corrected.csv", header=T, skip=0, sep=",", quote="\"", stringsAsFactors = F) 

d$Community[which(d$Community == "Evergaldes City")] <- "Everglades City"  # fix spelling

# cut out notes columns and check input -------------------------
d <- d[1:19]  

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




##### new code

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


cols <- c("#FF000015", "#FF000050", "#FF000095")

b <- barplot(t(region_ratio),
             legend.text = c(colnames(region_ratio)),
             args.legend = list(x='top',horiz=T,bty='n'),
             col=cols,
             ylim=c(0,1.2),yaxt='n',las=1)
axis(2,seq(0,1,.2),las=1)
text(b, 1.05, paste("n =", rowSums(region_scale)))


region_scale <- table(d$Offshore.or.Inshore.,d$event)
region_scale <- region_scale[-3,-c(1,2)]
row_sum <- apply(region_scale,1,sum,na.rm=T)
region_ratio <- region_scale/row_sum
region_ratio2 <- t(region_scale)/colSums(region_scale)


cols <- c("#FF000015", "#FF000050", "#FF000095")

b <- barplot(t(region_ratio2),
             legend.text = c(colnames(region_ratio2)),
             args.legend = list(x='top',horiz=T,bty='n'),
             col=cols,
             ylim=c(0,1.2),yaxt='n',las=1)
axis(2,seq(0,1,.2),las=1)
text(b, 1.05, paste("n =", colSums(region_scale)))
