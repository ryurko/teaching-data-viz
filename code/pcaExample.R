##Zach Branson

#From a past homework assignment in 36-315, we'll
#consider the spotify dataset:
spotify = read.csv("https://raw.githubusercontent.com/zjbranson/315Fall2022/main/spotify.csv")

#for ease of display, change "duration_ms" to just "duration"
names(spotify)[which(names(spotify) == "duration_ms")] = "duration"

#First, define principal components
#To do this, only focus on the 11 quantitative variables:
spotify.subset = subset(spotify, select = -c(year, decades))

#now run PCA
spotify.pca = prcomp(spotify.subset,
	center = TRUE, scale. = TRUE)
summary(spotify.pca)

#now make a biplot on the first two principal components
#(note that scale = 0 displays the actual principal components)
library(ggbiplot)
ggbiplot(spotify.pca,
	scale = 0,
	point.size = 1.5,
	alpha = 0.5,
	group = spotify$decades,
	varname.size = 5,
	varname.adjust = 1.5
	) +
	theme_light() +
	labs(color = "Decades",
		x = "First Principal Component (57.5% Variance Explained)",
		y = "Second Principal Component (18.8% Variance Explained)")

#elbow plot
ggscreeplot(spotify.pca) + 
	labs(x = "Principal Component Number",
		y = "Variance Explained") +
	theme_light() +
	coord_cartesian(ylim = c(0, 0.572)) +
	scale_x_continuous(breaks = 1:11, minor_breaks = 1:11)

#table of first two columns of rotation matrix
#(organized in alphabetical order)
library(knitr)

#get the "rotation" from PCA:
rotation.pca = as.matrix(spotify.pca$rotation)

kable(round(rotation.pca[
	c("acousticness", "danceability", "duration",
		"energy", "instrumentalness", "liveness",
		"loudness", "popularity",
		"speechiness", "tempo", "valence"),1:2], digits = 3),
"latex")
