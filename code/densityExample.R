#Here we demonstrate viewing the bandwidth
#in a smoothed density plot as as an estimator.
#We can use the bootstrap
#to generate many different bandwidths.

#We'll consider the spotify dataset discussed in the paper:
spotify = read.csv("https://raw.githubusercontent.com/zjbranson/315Fall2022/main/spotify.csv")

#generate 1000 bootstrapped datasets
set.seed(123)
bootData = list(length = 1000)
for(i in 1:1000){
	bootData[[i]] = spotify[sample(1:nrow(spotify),
		size = nrow(spotify), replace = TRUE),]
}
#compute binwidth for each bootstrapped dataset
bootBands = vector(length = 1000)
for(i in 1:1000){
	bootBands[i] = density(bootData[[i]]$loudness)$bw
}

#95% "interval" of smoothed density plots,
#where we plot the smoothed density using the
#mean, 2.5% quantile, and 97.5% quantile of the bandwidths.
#This generates Figure 6 from the paper.
plot(density(spotify$loudness, bw = mean(bootBands)),
	ylim = c(0, 0.25),
	lwd = 2,
	xlab = "Loudness (dB)", main = "")
lines(density(spotify$loudness,
	bw = as.numeric(quantile(bootBands, prob = 0.025))),
	col = "red", lty = 2)
lines(density(spotify$loudness,
	bw = as.numeric(quantile(bootBands, prob = 0.975))),
	col = "blue", lty = 2)
legend("topleft",
	legend = c("Avg. Bandwidth",
		"2.5% Quantile Bandwidth",
		"97.5% Quantile Bandwidth"),
	lty = c(1,2,2), lwd = c(2,1,1),
	col = c("black", "red", "blue"))
