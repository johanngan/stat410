library(car)
library(ggplot2)

# Make font size bigger
theme_set(theme_gray(base_size=18))


# Plots marginal confidence intervals of certain coefficients in an lm
# regModel is an lm, plotWhich is list of names of coefficients to plot
plotBetas = function(regModel, plotWhich)
{
    intervals = data.frame(regressor = plotWhich,
                           coefficient = regModel$coefficients[plotWhich],
                           mins = confint(regModel)[plotWhich,1],
                           maxs = confint(regModel)[plotWhich,2])
    ggplot(intervals, aes(x = regressor, y = coefficient)) + 
        geom_point(size = 2) + geom_errorbar(aes(ymax = maxs, ymin = mins), width=0.3) +
        labs(title = "95% Marginal Confidence Intervals") + 
        theme(panel.background = element_rect(fill = "gray"), 
              panel.grid.major = element_line(colour="black", size=0.2),
              panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90, hjust=1))
}


# Plots marginal confidence intervals of coefficients for the year categories
# and connects them with a line
plotTime = function(regModel)
{
    times = sprintf("year%02i", 6:17)
    
    intervals = data.frame(year = sprintf("20%02i", 5:17),
                           offset = c(0, regModel$coefficients[times]),
                           mins = c(0, confint(regModel)[times,1]),
                           maxs = c(0, confint(regModel)[times,2]))
    ggplot(intervals, aes(x = year, y = offset, group = rep(1, 1+length(times)))) + 
        geom_point(size = 2) + geom_errorbar(aes(ymax = maxs, ymin = mins), width=0.3) +
        labs(title = "Course Score Offset by Year (Relative to 2005)") + 
        theme(panel.background = element_rect(fill = "gray"), 
              panel.grid.major = element_line(colour="black", size=0.2),
              panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90, hjust=1)) +
        geom_line()
}


# Plots marginal confidence intervals of coefficients for the school categories
plotSchool = function(regModel)
{
    schools = c("Architecture", "Business", "Cross-departmental", "Engineering", "Humanities",
                "Music", "Natural Science", "Other", "Social Science")
    schl = sprintf("school%s", c("business", "cross-departmental", "engineering", "humanities", 
                                 "music", "natural_science", "other", "social_science"))
    
    intervals = data.frame(school = schools,
                           offset = c(0, regModel$coefficients[schl]),
                           mins = c(0, confint(regModel)[schl,1]),
                           maxs = c(0, confint(regModel)[schl,2]))
    ggplot(intervals, aes(x = school, y = offset)) + 
        geom_point(size = 2) + geom_errorbar(aes(ymax = maxs, ymin = mins), width=0.3) +
        labs(title = "Course Score Offset by School (Relative to Architecture)") + 
        theme(panel.background = element_rect(fill = "gray"), 
              panel.grid.major = element_line(colour="black", size=0.2),
              panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90, hjust=1))
}



# Load data
setwd('/Users/johanngan/Desktop/My Stuff/Rice/Courses/Fall 2017/STAT 410/Project/Data')
load("course_data.RData")
attach(data)


# Useful things
n = length(X13)
X = cbind(X10, X11, X12, X13, X14)

# Visualizations of response-regressor correlations
setwd('plots')
pdf("course_scatters.pdf")
plot(X8_avg, X13, main="X13 vs. X8_avg", xlab="X8_avg", ylab="X13")
for(i in c(1:3, 5))
{
    plot(X[,i], X13, main=sprintf("X13 vs. X%i", i+9), xlab=sprintf("X%i", i+9), ylab="X13")
}
plot(size, X13, main="X13 vs. size", xlab="Class size", ylab="X13")
plot(ninstructors, X13, main="X13 vs. ninstructors", xlab="Number of instructors", ylab="X13")
plot(nsections, X13, main="X13 vs. nsections", xlab="Number of sections", ylab="X13")
plot(year, X13, main="X13 by year", xlab="Year", ylab="X13")
plot(school, X13, main="X13 by academic school", xlab="School", ylab="X13")
plot(upperlower, X13, main="X13 for upper and lower level classes", xlab="Course level", ylab="X13")
plot(season, X13, main="X13 by season", xlab="Season", ylab="X13")
dev.off()


# Compare qq plots

# Non-log model
fullModel = lm(X13 ~ season + year + school + upperlower + size + ninstructors + nsections +
                   X8_avg + X10 + X11 + X12 + X14, data=data, weights=N13)
pdf("course_qq.pdf")
qqnorm(rstandard(fullModel), main="Standardized Residual Q-Q plot for Full Course Model")
abline(0, 1, col="red")

# Log model
fullLogModel = lm(log(X13) ~ season + year + school + upperlower + 
                      log(size) + log(ninstructors) + log(nsections) +
                      log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14), weights=N13)
qqnorm(rstandard(fullLogModel), main="Standardized Residual Q-Q plot for Log Course Model")
abline(0, 1, col="red")
dev.off()


# Stepwise selection
f_base = lm(log(X13) ~ 1, data=data)
forAIC_f = step(f_base, scope=list(lower=~1, upper=~season + year + school + upperlower + 
                                       log(size) + log(ninstructors) + log(nsections) +
                                       log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14)),
                direction="forward", data=data, weights=N13)
forBIC_f = step(f_base, scope=list(lower=~1, upper=~season + year + school + upperlower + 
                                       log(size) + log(ninstructors) + log(nsections) +
                                       log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14)),
                direction="forward", data=data, k=log(n), weights=N13)
backAIC_f = step(fullLogModel, direction="backward", data=data, weights=N13)
backBIC_f = step(fullLogModel, direction="backward", data=data, k=log(n), weights=N13)


# Reduced model (-season)
redModel = lm(log(X13) ~ year + school + upperlower + log(size) + log(ninstructors) + log(nsections) +
                log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14), data=data, weights=N13)


# Redo the qq plot
pdf("course_qq_reduced.pdf")
qqnorm(rstandard(redModel), main="Standardized Residual Q-Q plot for Reduced Course Model")
abline(0, 1, col="red")
dev.off()


# Residual vs. fitted value plots
pdf("course_resid_v_fit.pdf")
plot(redModel$fitted.values, resid(redModel), main="Residuals vs. fitted values")

# Cleaned plots (remove tiny classes)
redModelClean = lm(log(X13) ~ season + year + school + upperlower + log(size) + log(ninstructors) + 
                       log(nsections) + log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14), 
                   data=subset(data, N13 > 5), weights=N13)
plot(redModelClean$fitted.values, resid(redModelClean), main="Residuals vs. fitted values (N13 > 5)")

redModelClean2 = lm(log(X13) ~ season + year + school + upperlower + log(size) + log(ninstructors) + 
                       log(nsections) + log(X8_avg) + log(X10) + log(X11) + log(X12) + log(X14), 
                   data=subset(data, N13 > 10), weights=N13)
plot(redModelClean2$fitted.values, resid(redModelClean2), main="Residuals vs. fitted values (N13 > 10)")
dev.off()


# Resid vs. regressor val plots (cleaned model)
pdf("course_resid_vs_regressor.pdf")
X = cbind(X10, X11, X12, X13, X14)
X = X[N13 > 10,]

plot(log(X8_avg[N13 > 10]), resid(redModelClean2), main="Residuals vs. log(X8_avg)", 
     xlab="log(X8_avg)", ylab="Residuals")
for(i in c(1:3, 5))
{
    plot(log(X[,i]), resid(redModelClean2), 
         main=sprintf("Residuals vs. log(X%i)", i+9), 
         xlab=sprintf("log(X%i)", i+9), ylab="Residuals")}
plot(log(size[N13 > 10]), resid(redModelClean2), 
     main="Residuals vs. log(size)", xlab="log(Class size)", ylab="Residuals")
plot(log(ninstructors[N13 > 10]), resid(redModelClean2), 
     main="Residuals vs. log(ninstructors)", xlab="log(Number of instructors)", 
     ylab="Residuals")
plot(log(nsections[N13 > 10]), resid(redModelClean2), main="Residuals vs. log(nsections)", 
     xlab="log(Number of sections)", ylab="Residuals")
plot(year[N13 > 10], resid(redModelClean2), main="Residuals by year", 
     xlab="Year", ylab="Residuals")
plot(school[N13 > 10], resid(redModelClean2), main="Residuals by academic school", 
     xlab="School", ylab="Residuals")
plot(upperlower[N13 > 10], resid(redModelClean2), 
     main="Residuals for upper and lower level classes", 
     xlab="Course level", ylab="Residuals")
dev.off()

# Fitted vals vs. measured vals
pdf("course_reg_perf.pdf")
plot(log(X13), redModel$fitted.values, main="Fitted values vs. measured values",
     xlab="Measured value", ylab="Fitted value")
dev.off()





# Residual histogram
pdf("course_histo.pdf")
hist(resid(redModel), breaks=100, main="Residual Histogram for Course Model")
dev.off()

# Check multicollinearity
X = redModel$model[-13]
print(cor(X[sapply(X, is.numeric)]))
print(kappa(redModel))
print(vif(redModel))


# Based on the performance of the fit is already very good, there seems to be no 
# need to add interaction terms, which would make the model much more complicated.

# Plot relevant marginal confidence intervals
plotBetas(redModel, c("log(X8_avg)", sprintf("log(X%i)", c(10:12, 14))))
ggsave("course_CI.pdf")

plotTime(redModel)
ggsave("course_year.pdf")

plotSchool(redModel)
ggsave("course_school.pdf")


# Hypothesis testing

# Test 2016 vs 2017 offset
lht(redModel, c(rep(0, 11), 1, -1, rep(0, 17)))

# Test school differences
# (14-22), (busi, xdept, engi, huma, musi, nsci, other, soci)

# engi vs. nsci
lht(redModel, c(rep(0, 15), 1, 0, 0, -1, rep(0, 11)))

# engi vs. busi, engi vs. musi, nsci vs. busi, nsci vs. musi
lht(redModel, c(rep(0, 13), 1, 0, -1, rep(0, 14)))
lht(redModel, c(rep(0, 15), 1, 0, -1, rep(0, 12)))
lht(redModel, c(rep(0, 13), 1, 0, 0, 0, 0, -1, rep(0, 11)))
lht(redModel, c(rep(0, 17), 1, -1, rep(0, 11)))

# busi vs. xdept, busi vs. musi, busi vs. soci
lht(redModel, c(rep(0, 13), 1, -1, rep(0, 15)))
lht(redModel, c(rep(0, 13), 1, 0, 0, 0, -1, rep(0, 12)))
lht(redModel, c(rep(0, 13), 1, rep(0, 6), -1, rep(0, 9)))

# xdept vs. musi, xdept vs. soci
lht(redModel, c(rep(0, 14), 1, 0, 0, -1, rep(0, 12)))
lht(redModel, c(rep(0, 14), 1, rep(0, 5), -1, rep(0, 9)))

# musi vs. soci
lht(redModel, c(rep(0, 17), 1, 0, 0, -1, rep(0, 9)))

# other vs. soci, other vs. xdept, other vs. busi, other vs. musi
lht(redModel, c(rep(0, 19), 1, -1, rep(0, 9)))
lht(redModel, c(rep(0, 14), 1, rep(0, 4), -1, rep(0, 10)))
lht(redModel, c(rep(0, 13), 1, rep(0, 5), -1, rep(0, 10)))
lht(redModel, c(rep(0, 17), 1, 0, -1, rep(0, 10)))

# huma vs. other
lht(redModel, c(rep(0, 16), 1, 0, 0, -1, rep(0, 10)))

detach(data)
