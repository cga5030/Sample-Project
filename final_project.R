rm(list=ls())
graphics.off()

library(ggplot2)
library(gridExtra)
library(ggpmisc)
# library(GGally)
# library(PerformanceAnalytics)

###############################################################
# A conflict-year dataset with information on armed conflict 
# where at least one party is the government of a state in 
# the time period 1946-2019.

load("data/ucdp-prio-acd-201.RData")

# Count by year
statedata <- ucdp_prio_acd_201
stateyears <- statedata$year
stateyears_unique <- sort(unique(statedata$year))
stateconflicts <- data.frame(matrix(999,ncol=2,nrow=length(stateyears_unique)))
stateconflicts[,1] <- stateyears_unique
names(stateconflicts) <- c("year","count")

for (n in 1:length(stateyears_unique)){
stateconflicts[n,2] <- sum(stateyears==stateyears_unique[n])
}

# # plot 
# plot(x=stateconflicts$year,y=stateconflicts$count,
#      ylab = "global armed conflicts involving a state government",
#      xlab = "year")

# ggplot
stateplot  <-  ggplot(data=stateconflicts, aes(x=year, y=count)) + 
                geom_point() +
                # theme(panel.grid.major = element_blank(), 
                #       panel.grid.minor = element_blank()) +
                ylab("Global armed conflicts (state)") + xlab("Year") +
                geom_smooth()
                # geom_smooth(aes(colour="LOESS"))+
                # scale_colour_manual(name="legend", values=c("red"))
print(stateplot)

###############################################################
# A conflict-year dataset containing information on communal 
# and organized armed conflict where none of the parties is 
# the government of a state. 

load("data/ucdp-nonstate-201.RData")

# Count by year
nonstatedata <- ucdp_nonstate_201
nonstateyears <- nonstatedata$year
nonstateyears_unique <- sort(unique(nonstatedata$year))
nonstateconflicts <- data.frame(matrix(999,ncol=2,nrow=length(nonstateyears_unique)))
nonstateconflicts[,1] <- nonstateyears_unique
names(nonstateconflicts) <- c("year","count")

for (n in 1:length(nonstateyears_unique)){
  nonstateconflicts[n,2] <- sum(nonstateyears==nonstateyears_unique[n])
}

# plot 
# plot(x=nonstateconflicts[,1],y=nonstateconflicts[,2],
#      ylab = "global armed conflicts involving no state government",
#      xlab = "year")

# ggplot
nonstateplot <- ggplot(data=nonstateconflicts, aes(x=year, y=count)) + 
                geom_point() +
                # theme(panel.grid.major = element_blank(), 
                #       panel.grid.minor = element_blank()) +
                ylab("Global armed conflicts (non-state)") + xlab("Year") +
                geom_smooth()
                # geom_smooth(aes(colour="LOESS"))+
                # scale_colour_manual(name="legend", values=c("red"))
print(nonstateplot)

##############################################################
# Global temperature anomaly from NASA

tempdata.in <- read.delim("data/temperature_anomaly.txt",sep="")

temps <- data.frame(matrix(999,ncol=2,nrow=length(tempdata.in$X1880)))
names(temps) <- c("year","temp")
temps[,1] <- tempdata.in$X1880
temps[,2] <- tempdata.in$X.0.16

# plot
# plot(x=temps[1,],y=temps[2,],
#      xlab = "year",
#      ylab = "mean temperature anomaly in deg C")

anomalyplot <-  ggplot(data=temps, aes(x=year, y=temp)) + 
                geom_point() +
                # theme(panel.grid.major = element_blank(), 
                #       panel.grid.minor = element_blank()) +
                ylab("Mean global temperature anomaly [deg C]") + xlab("Year") +
                geom_smooth()
                # geom_smooth(aes(colour="LOESS"))+
                # scale_colour_manual(name="legend", values=c("red"))
print(anomalyplot)

##############################################################
# Correlation between temperature anomaly and state conflicts

# line up the years
begyear <- stateconflicts$year[1]
endyear <- stateconflicts$year[length(stateconflicts$year)]
statetemps = subset(temps, year>=begyear&year<=endyear) # trim to desired years

statecor <- cor.test(x=stateconflicts$count,y=statetemps$temp
                     ,method="pearson", exact=FALSE)

# pearson's correlation
stateplotdata <- stateconflicts
stateplotdata$temps <- statetemps$temp
stateplotdata <- subset (stateplotdata, select = -year)


##############################################################
# Correlation between temperature anomaly and state conflicts

# line up the years
begyear <- nonstateconflicts$year[1]
endyear <- nonstateconflicts$year[length(nonstateconflicts$year)]
nonstatetemps = subset(temps, year>=begyear&year<=endyear) # trim to desired years

# pearson's correlation
nonstatecor <- cor.test(x=nonstateconflicts$count,y=nonstatetemps$temp
                     ,method="pearson", exact=FALSE)

nonstateplotdata <- nonstateconflicts
nonstateplotdata$temps <- nonstatetemps$temp
nonstateplotdata <- subset (nonstateplotdata, select = -year)

##############################################################
# linear regressions


# state
statecordata <- stateconflicts
statecordata[,3] <- statetemps$temp
names(statecordata) <- c("year","conflicts","temps")
linear_state <- lm(conflicts~temps,data=statecordata) # linear regression
summary(linear_state)
anova(linear_state)

my.formula <- y ~ x
p_linear <- ggplot(data = statecordata, aes(x = temps, y = conflicts)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  ylab("Global armed conflict (state)") +
  xlab("Mean Temperature Anomaly [deg C]") +
  geom_point()
print(p_linear)


# nonstate
nonstatecordata <- nonstateconflicts
nonstatecordata[,3] <- nonstatetemps$temp
names(nonstatecordata) <- c("year","conflicts","temps")
linear_nonstate <- lm(conflicts~temps,data=nonstatecordata) # linear regression
summary(linear_nonstate)
anova(linear_nonstate)

my.formula <- y ~ x
p_linear_nonstate <- ggplot(data = nonstatecordata, aes(x = temps, y = conflicts)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +   

  ylab("Global armed conflict (non-state)") + 
  xlab("Mean Temperature Anomaly [deg C]") +
  geom_point()
print(p_linear_nonstate)

##############################################################
# plots

# state
statecorplot <- ggplot(data=stateplotdata, aes(x=temps,y=count)) +
  geom_point() +
  ylab("Global armed conflict (state)") +
  xlab("Mean Temperature Anomaly [deg C]") +
  # geom_smooth()
  geom_abline(intercept = 26.400, slope = 25.249, color="red")
# geom_smooth(aes(colour="LOESS"))+ 
# scale_colour_manual(name="legend", values=c("red"))
print(statecorplot)

# non-state
nonstatecorplot <- ggplot(data=nonstateplotdata, aes(x=temps,y=count)) +
  geom_point() +
  ylab("Global armed conflict (non-state)") + 
  xlab("Mean Temperature Anomaly [deg C]") +
  geom_abline(intercept = 1.464, slope = 67.564, color="red")
# geom_smooth()
# geom_smooth(aes(colour="LOESS"))+
# scale_colour_manual(name="legend", values=c("red"))
print(nonstatecorplot)

##############################################################
# grid arrange for plots
# save plot to file without using ggsave

png("!grid1.png")
grid.arrange(stateplot,nonstateplot,anomalyplot,ncol=2,nrow=2)
dev.off()

png("!grid2.png")
grid.arrange(p_linear,p_linear_nonstate,ncol=2)
dev.off()
