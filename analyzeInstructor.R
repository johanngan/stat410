library(car)
library(leaps)
library(ggplot2)

# Make font size bigger
par(cex=1.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
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
        labs(title = "Instructor Score Offset by Year (Relative to 2005)") + 
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
        labs(title = "Instructor Score Offset by School (Relative to Architecture)") + 
        theme(panel.background = element_rect(fill = "gray"), 
              panel.grid.major = element_line(colour="black", size=0.2),
              panel.grid.minor = element_blank(), axis.text.x=element_text(angle=90, hjust=1))
}



# Load in the data
setwd('/Users/johanngan/Desktop/My Stuff/Rice/Courses/Fall 2017/STAT 410/Project/Data')
load("course_data.RData")
attach(data)

# Useful things
n = length(X8_avg)
X = cbind(X1_avg, X2_avg, X3_avg, X4_avg, X5_avg, X6_avg, X7_avg, X8_avg, X9_avg, 
          X10, X11, X12, X13, X14)

# Summary statistics
summary(data)
summary(data$school)
summary(data$year)
for(i in 1:14)
{
    print(sprintf("X%i sd: %f", i, sd(X[,i])))
}
print(sprintf("Size sd: %f", sd(data$size)))
print(sprintf("Ninstructors sd: %f", sd(data$ninstructors)))
print(sprintf("Nsections sd: %f", sd(data$nsections)))

# Visualizations of regressor-response correlations
setwd('plots')
pdf("instructor_scatters.pdf")
for(i in c(1:7, 9))
{
    plot(X[,i], X[,8], main=sprintf("X8_avg vs. X%i_avg", i), xlab=sprintf("X%i_avg", i), ylab="X8_avg")
}
plot(year, X8_avg, main="X8_avg by year", xlab="Year", ylab="X8_avg")
plot(school, X8_avg, main="X8_avg by academic school", xlab="School", ylab="X8_avg")
dev.off()


# Non-log regression
iModel = lm(X8_avg ~ year + school + X1_avg + X2_avg + X3_avg + X4_avg + X5_avg + X6_avg + X7_avg + X9_avg,
            data=data, weights=N8)

# Log regression
iLogModel = lm(log(X8_avg) ~ year + school + log(X1_avg) + log(X2_avg) + log(X3_avg) + log(X4_avg) +
                   log(X5_avg) + log(X6_avg) + log(X7_avg) + log(X9_avg), data=data, weights=N8)


# QQ plot comparison
pdf("instructor_qq.pdf")
qqnorm(rstandard(iModel), main="Standardized Residual Q-Q plot for Instructor Model")
abline(0, 1, col="red")

qqnorm(rstandard(iLogModel), main="Standardized Residual Q-Q plot for Log Instructor Model")
abline(0, 1, col="red")
dev.off()


# Further model checking:
pdf("instructor_histo.pdf")
hist(resid(iModel), breaks=100, main="Residual Histogram for Instructor Model")
dev.off()


# Check multicollinearity
X = iModel$model[,-12]
print(cor(X[sapply(X, is.numeric)]))
print(kappa(iModel))
print(vif(iModel))


# Residual vs. fitted value plot
pdf("instructor_resid_v_fit.pdf")
plot(iModel$fitted.values, resid(iModel), main="Residuals vs. fitted values")

# Cleaned residual vs. fitted value plots (cut out the very tiny classes)
iModelClean = lm(X8_avg ~ year + school + X1_avg + X2_avg + X3_avg + 
                    X4_avg + X5_avg + X6_avg + X7_avg + X9_avg,
            data=subset(data, N8 > 5), weights=N8)
plot(iModelClean$fitted.values, resid(iModelClean), main="Residuals vs. fitted values (N8 > 5)")
iModelClean2 = lm(X8_avg ~ year + school + X1_avg + X2_avg + X3_avg + 
                        X4_avg + X5_avg + X6_avg + X7_avg + X9_avg,
                    data=subset(data, N8 > 10), weights=N8)
plot(iModelClean2$fitted.values, resid(iModelClean2), main="Residuals vs. fitted values (N8 > 10)")

# Simulation for tapering effect with homogeneous error
m = 500
x = c()
y = c()
for(i in 1:m)
{
    x = c(x, rep(-log(i), i))
    y = c(y, rnorm(i))
}
plot(x, y, main="Homogeneous normal error", sub="Decreasing number of points in fitted value",
     xlab="Fitted value", ylab="residual")

dev.off()

# Resid vs. regressor val plots (cleaned model)
pdf("instructor_resid_vs_regressor.pdf")
X = cbind(X1_avg, X2_avg, X3_avg, X4_avg, X5_avg, X6_avg, X7_avg, X8_avg, X9_avg)
X = X[N8 > 10,]
for(i in c(1:7, 9))
{
    plot(X[,i], resid(iModelClean2), 
         main=sprintf("Residuals vs. X%i_avg", i), 
         xlab=sprintf("X%i_avg", i), ylab="Residuals")
}
plot(year[N8 > 10], resid(iModelClean2), main="Residuals by year", xlab="Year", ylab="Residuals")
plot(school[N8 > 10], resid(iModelClean2), main="Residuals by academic school", xlab="School", ylab="Residuals")
dev.off()

# Fitted vals vs. measured vals
pdf("instructor_reg_perf.pdf")
plot(X8_avg, iModel$fitted.values, main="Fitted values vs. measured values",
     xlab="Measured value", ylab="Fitted value")
dev.off()




# Stepwise model selection
i_base = lm(X8_avg ~ 1, data=data)
forAIC_i = step(i_base, scope=list(lower=~1, upper=~year + school + X1_avg + X2_avg + X3_avg + 
                                       X4_avg + X5_avg + X6_avg + X7_avg + X9_avg),
                direction="forward", data=data, weights=N8)
forBIC_i = step(i_base, scope=list(lower=~1, upper=~year + school + X1_avg + X2_avg + X3_avg + 
                                       X4_avg + X5_avg + X6_avg + X7_avg + X9_avg),
                direction="forward", data=data, k=log(n), weights=N8)

backAIC_i = step(iModel, direction="backward", data=data, weights=N8)
backBIC_i = step(iModel, direction="backward", data=data, k=log(n), weights=N8)


# Exhaustive subset selection (with the numerical regressors)
# FUNCTION ONLY WORKS FOR THE SCORE (NONCATEGORICAL) REGRESSORS
rss.out = regsubsets(X8_avg ~ X1_avg + X2_avg + X3_avg + 
                         X4_avg + X5_avg + X6_avg + X7_avg + X9_avg,
                            data=data, weights=N8, nvmax=20)
rss.summary = summary(rss.out)
print(rss.summary$adjr2)
print(rss.summary$bic)


# Plot the relevant coefficient confidence intervals
plotBetas(iModel, c(sprintf("X%i_avg", c(1:7, 9))))
ggsave("instructor_CI.pdf")

plotTime(iModel)
ggsave("instructor_year.pdf")

plotSchool(iModel)
ggsave("instructor_school.pdf")


# Linear hypothesis tests to check for ordering between coefficients for
# X6 (27), X5 (26), X1 (22)

# beta6 - beta5 == 0 vs. beta6 - beta5 != 0
lht(iModel, c(rep(0, 25), -1, 1, 0, 0))
                 
# beta5 - beta1 == 0 vs. beta5 - beta1 != 0
lht(iModel, c(rep(0, 21), -1, 0, 0, 0, 1, 0, 0, 0))

# beta6 - beta1 == 0 vs. beta6 - beta1 != 0
lht(iModel, c(rep(0, 21), -1, 0, 0, 0, 0, 1, 0, 0))



# Linear hypothesis tests to compare intercept offsets by department
# (14 - 22, busi, xdept, engi, huma, musi, nsci, other, soci)

# busi vs. musi
lht(iModel, c(rep(0, 13), 1, rep(0, 3), -1, rep(0, 11)))

# busi vs. xdept, musi vs. xdept
lht(iModel, c(rep(0, 13), 1, -1, rep(0, 14)))
lht(iModel, c(rep(0, 14), 1, 0, 0, -1, rep(0, 11)))

# xdept vs. huma
lht(iModel, c(rep(0, 14), 1, 0, -1, rep(0, 12)))

# huma vs. soci
lht(iModel, c(rep(0, 16), 1, 0, 0, 0, -1, rep(0, 8)))

# huma vs. engi, soci vs. engi
lht(iModel, c(rep(0, 15), 1, -1, rep(0, 12)))
lht(iModel, c(rep(0, 15), 1, 0, 0, 0, 0, -1, rep(0, 8)))

# engi vs. nsci, engi vs. other, nsci vs. other
lht(iModel, c(rep(0, 15), 1, 0, 0, -1, rep(0, 10)))
lht(iModel, c(rep(0, 15), 1, 0, 0, 0, -1, rep(0, 9)))
lht(iModel, c(rep(0, 18), 1, -1, rep(0, 9)))

detach(data)
