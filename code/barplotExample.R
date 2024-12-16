
#Here we demonstrate that graphics may look the same, but
#the statistical inference may look very different
#(largely because of sample size/uncertainty).

#function that displays a barplot with confidence intervals
#for any dataframe with (A, B, C) categories.
getBarplot = function(n,
	props = c(0.25, 0.35, 0.4),
	displayCIs = FALSE, alpha = 0.05){
	#first, check to make sure that
	#each n*p is a whole number
	#(such that it's a proper barplot)
	if( sum((n*props)%%1 == 0) < 3 ){
		return(print("n times trueProps must be whole numbers."))
	}
	if( sum(props) != 1 ){
		return(print("total props must equal 1."))
	}
	#proportions
	propA = props[1]; propB = props[2]; propC = props[3]
	#create dataset
	data = data.frame(cat = c(
		rep("A", n*propA),
		rep("B", n*propB),
		rep("C", n*propC)))
	#compute CIs
	ciQuant = qnorm(1-alpha/2)
	propA.ci = c(
		propA - ciQuant*sqrt( (propA*(1-propA))/n ),
		propA + ciQuant*sqrt( (propA*(1-propA))/n )   )
	propB.ci = c(
		propB - ciQuant*sqrt( (propB*(1-propB))/n ),
		propB + ciQuant*sqrt( (propB*(1-propB))/n )   )
	propC.ci = c(
		propC - ciQuant*sqrt( (propC*(1-propC))/n ),
		propC + ciQuant*sqrt( (propC*(1-propC))/n )   )
	
	#create barplot
	barplot(prop.table(table(data$cat)), col = "white",
		main = "",
		ylim = c(0, 0.65))
	#add text with sample size and chi-squared p-value
	text(x = 1, y = 0.6,
		paste0("chi-squared test \n p-value = ",
			round(chisq.test(table(data$cat))$p.value, digits = 2)),
		cex = 1.5)
	#display CIs?
	if(displayCIs){
		segments(x0 = 0.7, x1 = 0.7,
			y0 = propA.ci[1], y1 = propA.ci[2])
		segments(x0 = 0.4, x1 = 1,
			y0 = propA.ci[1], y1 = propA.ci[1])
		segments(x0 = 0.4, x1 = 1,
			y0 = propA.ci[2], y1 = propA.ci[2])

		segments(x0 = 1.9, x1 = 1.9,
			y0 = propB.ci[1], y1 = propB.ci[2])
		segments(x0 = 1.6, x1 = 2.2,
			y0 = propB.ci[1], y1 = propB.ci[1])
		segments(x0 = 1.6, x1 = 2.2,
			y0 = propB.ci[2], y1 = propB.ci[2])

		segments(x0 = 3.1, x1 = 3.1,
			y0 = propC.ci[1], y1 = propC.ci[2])
		segments(x0 = 2.8, x1 = 3.4,
			y0 = propC.ci[1], y1 = propC.ci[1])
		segments(x0 = 2.8, x1 = 3.4,
			y0 = propC.ci[2], y1 = propC.ci[2])
	}
}

#Now we'll compute the significance
#of chi-squared test as a function of sample size
n.vec = seq(20, 1000, by = 20)
chisq.pvalue = (length = length(n.vec))
#we'll also consider significance of a pairwise test
#between A and B, and A and C.
#Specifically, we'll compute whether 0 is
#inside the CI for the difference in proportions.
#Because these differences are always negative,
#we'll only compute the upper bound.
ciAB = (length = length(n.vec))
ciAC = (length = length(n.vec))
#We'll also compute Bonferroni-corrected CIs:
ciAB.bonf = (length = length(n.vec))
ciAC.bonf = (length = length(n.vec))
for(n in n.vec){
	props = c(0.25, 0.35, 0.4)
	#chi-squared test
	chisq.pvalue[n/n.vec[1]] = 
	chisq.test(c(n*props[1],n*props[2],n*props[3]),
		p = rep(1/3, 3))$p.value
	#proportions
	propA = props[1]; propB = props[2]; propC = props[3]
	#CI upper bound for difference between A and B.
	diffAB = propA - propB
	seAB = sqrt( propA*(1-propA)/n + propB*(1-propB)/n 
		+ 2*propA*propB/n 
		)
	ciAB[n/n.vec[1]] = diffAB + qnorm(1-0.05/2)*seAB
	ciAB.bonf[n/n.vec[1]] = diffAB + qnorm(1-(0.05/3)/2)*seAB
	#CI upper bound for difference between A and C.
	diffAC = propA - propC
	seAC = sqrt( propA*(1-propA)/n + propC*(1-propC)/n 
		+ 2*propA*propC/n 
		)
	ciAC[n/n.vec[1]] = diffAC + qnorm(1-0.05/2)*seAC
	ciAC.bonf[n/n.vec[1]] = diffAC + qnorm(1-(0.05/3)/2)*seAC
}

# CODE TO GENERATE PLOTS IN FIGURE 4 FROM PAPER
# 1) overall chi-squared test fails to reject,
#    and all CIs overlap
#(plot a in Figure 4)
getBarplot(n = 40, props = c(0.25, 0.35, 0.4),
	displayCIs = TRUE)
#same plot, corrected for multiple testing
#(plot d in Figure 4)
getBarplot(n = 40, props = c(0.25, 0.35, 0.4),
	displayCIs = TRUE, alpha = 0.05/3)
#note that this CI fails to reject regardless of whether
#one accounts for multiple testing
ciAC[40/20]
ciAC.bonf[40/20]

# 2) overall chi-squared test fails to reject,
#    all CIs overlap, and at least one pairwise test reject
#(plot b in Figure 4)
getBarplot(n = 120, props = c(0.25, 0.35, 0.4),
	displayCIs = TRUE)
#same plot, corrected for multiple testing
#(plot e in Figure 4)
getBarplot(n = 120, props = c(0.25, 0.35, 0.4),
	displayCIs = TRUE, alpha = 0.05/3)
#note that this CI rejects when NOT accounting for 
#multiple testing, but otherwise fails to reject.
ciAC[120/20]
ciAC.bonf[120/20]

# 3) overall chi-squared test rejects,
#    and at least one pairwise test rejects.
#(plot c in Figure 4)
getBarplot(n = 200,
	props = c(0.25, 0.35, 0.40),
	displayCIs = TRUE)
#the same plot, but corrected for multiple testing
#(plot f in Figure 4)
getBarplot(n = 200,
	props = c(0.25, 0.35, 0.40),
	displayCIs = TRUE,
	alpha = 0.05/3)
#note that this CI rejects regardless of whether
#one accounts for multiple testing
ciAC[200/20]
ciAC.bonf[200/20]


