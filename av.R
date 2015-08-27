#! /usr/bin/env Rscript

########################
##### Read in data #####
########################

data <- read.csv("observations.csv", header=T)  
spend <- read.csv("spend_values.csv", header=T)

n_media <- 20   # number of media
n_str   <- 10   # number of structures

# Changing column names in "data" for convenience  
for (i in 1:n_media) {
    # column names associated with media: Media.#
    var = paste("Media.", i, sep="")
    names(data)[i+1] = var
}

for (i in 1:n_str) {
    # column names associated with structure: Str.#
    var = paste("Str.", i, sep="")
    names(data)[i+21] = var
}

###########################################
##### Calculating Advertising Adstock #####
###########################################

# Assuming a constant rate for adstock
adstock_rate = 0.35

# Creating a data frame "adstock" to store the results of adstock
adstock <- data.frame(matrix(0.0, nrow=nrow(data), ncol=n_media))

for (i in 1:n_media) {
    # Creating column names for data frame "adstick"
    var_name = paste("Media.",i,sep="")
    names(adstock)[i] = var_name

    # Calculating adstock using "filter" function
    var = eval(parse(text=paste("data$","Media.", i, sep="")))
    adstock[,i] = as.numeric(filter(x=var, filter=adstock_rate, method="recursive"))
}

################################################################
### Performing Marketing Mix Modeling with Linear Regression ###
################################################################

# Assuming linear relations
# Assuming a constant base
# Performing the fitting using "lm" function

yfit <- lm(data$Sales~adstock$Media.1+adstock$Media.2+adstock$Media.3+adstock$Media.4+adstock$Media.5+adstock$Media.6+adstock$Media.7+adstock$Media.8+adstock$Media.9+adstock$Media.10+adstock$Media.11+adstock$Media.12+adstock$Media.13+adstock$Media.14+adstock$Media.15+adstock$Media.16+adstock$Media.17+adstock$Media.18+adstock$Media.19+adstock$Media.20+data$Str.1+data$Str.2+data$Str.3+data$Str.4+data$Str.5+data$Str.6+data$Str.7+data$Str.8+data$Str.9+data$Str.10)

print(summary(yfit))    # R^2 = 0.945 (correlation coeficient)

############################################################
##### Calculating Relative Contribution of Each Media ######
############################################################

# Creating a data frame "contribution" and an array "rel_mean_contribution" to store results
contribution <- data.frame(matrix(0.0, nrow=nrow(data), ncol=n_media))
rel_mean_contribution <- rep(0.0, n_media)

for (i in 1:n_media) { 
    var = eval(parse(text=paste("adstock$","Media.", i, sep="")))
    # Calculating contribution: adstock * fitting coefficient
    contribution[,i] = var*coefficients(yfit)[i+1] 

    # Taking the mean contribution of each media and normalizing it with the realized sales  
    rel_mean_contribution[i] = mean(contribution[,i]) / data$Sales
}

# Contribution normalized with realized sales
rel_contribution <- contribution / data$Sales

# Print out the result in a decreasing order 
print(c("Relative Mean Contribution from each media is:", sort.int(mean_contribution, index.return=TRUE, decreasing=TRUE)))

###########################################
##### Impact of Structural Occurrence #####
###########################################

# Creating a data frame "str_impact" and an array "rel_mean_impact" to store results
str_impact <- data.frame(matrix(0.0, nrow=nrow(data), ncol=n_str))
rel_mean_impact <- rep(0.0, n_str)
for (i in 1:n_str) {
    var = eval(parse(text=paste("data$","Str.", i, sep="")))
    # Impact = occurrence * fitting coefficient
    str_impact[,i] = var*coefficients(yfit)[i+21] 

    # Taking the mean impact of each structure and normalizing it with realized sales
    rel_mean_impact[i] = mean(str_impact[,i]) / data$Sales
}

# Impact normalized with realized sales
rel_str_impact <- str_impact / data$Sales

# Print out the result in a decreasing order 
print(c("Mean Impact from each structural occurrence is:", sort.int(rel_mean_impact, index.return=TRUE, decreasing=TRUE)))

##################################
##### Calculating Efficiency #####
##################################

efficiency <- rep(0.0, n_media)
for (i in 1:n_media) {
    # efficiency: Cost / Attributed Sale
    efficiency[i] = as.numeric(spend[i] / sum(contribution[,i]))
}

# Print out the result in a decreasing order 
print(c("Efficiency:", sort.int(efficiency, index.return=TRUE, decreasing=TRUE)))

####################
##### Plotting #####
####################

x11(width=12, height=8)
par(mfrow=c(2,3), mar=c(4,4,2,2))

# Observed and Fitted Data
plot(sales, type='l', lwd=2, col="red", xlab="Time (Weeks)", ylab="Realized Sales ($)")
lines(fitted(yfit), lwd=1, col='blue')
legend(1,7.7e+06, c("Observed Data","Fitted Data"), lwd=c(2,1), col=c("red","blue"), cex=1.0)

# Residual
plot(residuals(yfit), type='l', lwd=2, col="black", xlab="Time (Weeks)", ylab="Residual Sales ($)")

# Structural Impact 
plot(rel_str_impact[,1], type='l', lwd=1, col=1, xlab="Time (Weeks)", ylab="Relative Impact from Structure", ylim=c(-0.06,0.25))
lines(rel_str_impact[,2], col=2)
lines(rel_str_impact[,3], col=3)
lines(rel_str_impact[,4], col=4)
lines(rel_str_impact[,5], col=5)
lines(rel_str_impact[,6], col=6)
lines(rel_str_impact[,7], col=7)
lines(rel_str_impact[,8], col=8)
lines(rel_str_impact[,9], col=9)
lines(rel_str_impact[,10], col=10)
legend(1,0.25, c("Str 1","Str 2","Str 3", "Str 4", "Str 5", "Str 6", "Str 7", "Str 8", "Str 9", "Str 10"), lwd=rep(1,10), col=rep(1:10), ncol=5, cex=0.7)

# Media Contribution (Media 1-10)
plot(rel_contribution[,1], type='l', lwd=1, col=1, xlab="Time (Weeks)", ylab="Relative Contribution from Media", ylim=c(0.01,0.2))
lines(rel_contribution[,2], col=2)
lines(rel_contribution[,3], col=3)
lines(rel_contribution[,4], col=4)
lines(rel_contribution[,5], col=5)
lines(rel_contribution[,6], col=6)
lines(rel_contribution[,7], col=7)
lines(rel_contribution[,8], col=8)
lines(rel_contribution[,9], col=9)
lines(rel_contribution[,10], col=10)
legend(1,0.2, c("Media 1","Media 2","Media 3", "Media 4", "Media 5", "Media 6", "Media 7", "Media 8", "Media 9", "Media 10"), lwd=rep(1,10), col=rep(1:10), ncol=5, cex=0.7)

# Media Contribution (Media 11-20)
plot(rel_contribution[,11], type='l', lwd=1, col=11, xlab="Time (Weeks)", ylab="Relative Contribution from Media", ylim=c(0.01,0.2))
lines(rel_contribution[,12], col=12)
lines(rel_contribution[,13], col=13)
lines(rel_contribution[,14], col=14)
lines(rel_contribution[,15], col=15)
lines(rel_contribution[,16], col=16)
lines(rel_contribution[,17], col=17)
lines(rel_contribution[,18], col=18)
lines(rel_contribution[,19], col=19)
lines(rel_contribution[,20], col=20)
legend(1,0.2, c("Media 11","Media 12","Media 13", "Media 14", "Media 15", "Media 16", "Media 17", "Media 18", "Media 19", "Media 20"), lwd=rep(1,10), col=rep(11:20), ncol=5, cex=0.7)

# Efficiency
plot(efficiency, type='o', lwd=2, col="blue", xlab="Company", ylab="Efficiency (Cost / Attributed Sale)")
