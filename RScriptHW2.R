install.packages("data.table")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
install.packages("caret")
install.packages("factoextra")
install.packages("mlbench")
install.packages("gridExtra")
install.packages("devtools")
install.packages("plot3D")
install.packages("imager")
install.packages("jpeg")
library(data.table)
library(plot3D)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(caret)
library(factoextra)
library(imager)
library(jpeg)
library(grid)
getwd()

#Question 1
#Part 1A

muskdata = fread("Musk1.csv") 
scaledmuskdata = scale(muskdata[,3:168])
musk= cbind(scaledmuskdata,muskdata[,1:2])

length(findCorrelation(cor(musk), cutoff = 0.95,verbose=FALSE)) / ncol(musk)
#%23 of the attributes are highly correlated, more than 95%.
#These data can be excluded in order to have a meaningful PCA.

pcamusk = princomp(musk[,1:166], cor=TRUE)
#In fact, princomp includes scaling.

#Information about the components can be found by summary function.
summary(pcamusk)

eigenvalues=get_eigenvalue(pcamusk)
head(eigenvalues,10)
#First 7 elements of the dimensions are contributed %70 of the total variance. (Rule of thumb)
plot(eigenvalues$cumulative.variance.percent,x=1:166,type="b")
abline(v=8, col="red")
abline(h=70,col="red")

#Visualization of first two components and first three components are visualized below.

plot(pcamusk$scores[,1],pcamusk$scores[,2],col=musk$V1+3) 
plot(pcamusk$scores[,1],pcamusk$scores[,2],col=musk$V2+3)
scatter3D(pcamusk$scores[,1], pcamusk$scores[,2], pcamusk$scores[,3])

#As seen from the graphs above, first 2 and first 3 components are not enough to differentiate Bag classes.
#Both bag classes are spread through the graph. 
#It is recommended to use 7 components.

#MDS

DistanceMatrix = as.data.table(as.matrix(dist(scaledmuskdata, method = "euclidean")))
mdsmusk = cmdscale(DistanceMatrix, k = 2)
#k=3 could also be used.

mdsmusklast = cbind(mdsmusk,musk[,1:2])
x= data.frame(mdsmusklast[,1],mdsmusklast[,2])
plot(x, xlab="Dimension 1", ylab="Dimension 2",col= musk$V1+3, main="MDS k=2")

#MDS results in same manner, there is not a significant differentiability. Both bag classes are spread into graph.
#Moreover, if k=3 MDS is conducted and evaluated, same result will be observed.
#MDS and PCA resulted in poor conclusions, due to the reason that 2 dimensions are not capable of explaining at least 80% of the variance.

#Part 1b

#We should aggregate Musk data with respect to Bag IDs.

aggregatedmusk = aggregate(muskdata, by = list(muskdata$V2), FUN = mean)
aggregatedmusk = as.data.table(aggregatedmusk)
aggregatedmusk[,Group.1:=NULL]
#aggregatedmusk
scaledmuskdata2 = scale(aggregatedmusk[,3:168])
musk2= cbind(scaledmuskdata2,aggregatedmusk[,1:2])
#musk2


pcamusk2 = prcomp(musk2[,1:166])
eigenvalues2=get_eigenvalue(pcamusk2)
summary(pcamusk2)
head(eigenvalues2,10)

#First 4 elements of the dimensions are contributed %70 of the total variance. (Rule of thumb)
#Using the means of the observations in a bag, we can obtain the following graph

plot(eigenvalues2$cumulative.variance.percent,x=1:92,type="b")
abline(v=5, col="red")
abline(h=70,col="red")

#Applying MDS to aggreaged data

DistanceMatrix2 = as.data.table(as.matrix(dist(scaledmuskdata2, method = "euclidean")))
mdsmusk2 = cmdscale(DistanceMatrix2, k = 2)
#k=3 could also be used.
mdsmusklast2 = cbind(mdsmusk2,musk2[,1:2])

x2= data.frame(mdsmusklast2[,1],mdsmusklast2[,2])
plot(x2, xlab="Dimension 1", ylab="Dimension 2",col= musk2$V1 + 3, main="MDS with aggregated data k=2")

#Results of aggregated PCA and MDS is very similar to previous graphs.
#On the other hand, grouping instances seems to be more logical since difference between bags are more clear.

#Question 2
#Part 2a
dev.off()


ataturklisesi = load.image("C:/Users/Ekin/Desktop/AnkaraAtaturkLisesi.jpg")
str(ataturklisesi)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "",main = "Türkiye'nin en iyi lisesi")
rasterImage(ataturklisesi, 1, 1, 256, 256)

#Part 2b

ataturklisesired = copy(ataturklisesi)
ataturklisesiblue = copy(ataturklisesi)
ataturklisesigreen = copy(ataturklisesi)

B(ataturklisesired) = 0
G(ataturklisesired) = 0
R(ataturklisesiblue) = 0
G(ataturklisesiblue) = 0
R(ataturklisesigreen) = 0
B(ataturklisesigreen) = 0

par(mfrow = c(1,3))
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Red")
rasterImage(ataturklisesired, 0, 0, 256, 256, angle = 0, interpolate = FALSE)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Green")
rasterImage(ataturklisesigreen, 0, 0, 256, 256, angle = 0, interpolate = FALSE)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Blue")
rasterImage(ataturklisesiblue, 0, 0, 256, 256, angle = 0, interpolate = FALSE)

minimal = min(ataturklisesi)
maximal = max(ataturklisesi)*0.1
Noise1=matrix(runif(65536,min=minimal,max=maximal),256)
Noise2=matrix(runif(65536,min=minimal,max=maximal),256)
Noise3=matrix(runif(65536,min=minimal,max=maximal),256)

ataturklisesinoise = ataturklisesi

ataturklisesinoise[,,1] <- ataturklisesi[,,1] + Noise1
ataturklisesinoise [,,2] <- ataturklisesi [,,2] + Noise2
ataturklisesinoise [,,3] <- ataturklisesi [,,3] + Noise3

ataturklisesinoise [,,1][ataturklisesinoise [,,1]>1]=1
ataturklisesinoise [,,2][ataturklisesinoise [,,2]>1]=1
ataturklisesinoise [,,3][ataturklisesinoise [,,3]>1]=1

par(mfrow = c(1,2))
plot(c(1, 256), c(1, 256), axes = FALSE, col = 0, xlab = "",ylab = "", main = "Noised image")
rasterImage(ataturklisesinoise, 0,0,256,256, interpolate = FALSE)
plot(c(1, 256), c(1, 256), axes = FALSE ,xlab = "",ylab = "", col = 0, main = "Original image ")
rasterImage(ataturklisesi, 0,0,256,256, interpolate = FALSE)

ataturklisesinoisered = copy(ataturklisesinoise)
ataturklisesinoiseblue = copy(ataturklisesinoise)
ataturklisesinoisegreen = copy(ataturklisesinoise)

B(ataturklisesinoisered) = 0
G(ataturklisesinoisered) = 0
R(ataturklisesinoiseblue) = 0
G(ataturklisesinoiseblue) = 0
R(ataturklisesinoisegreen) = 0
B(ataturklisesinoisegreen) = 0


#Displaying each channel of noised image
par(mfrow = c(1, 4))
plot(c(1, 256), c(1, 256),type = "n", xlab = "", ylab = "",main = "Noised image")
rasterImage(ataturklisesinoise, 0,0,256,256, interpolate = FALSE)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Red")
rasterImage(ataturklisesinoisered, 0, 0, 256, 256, angle = 0, interpolate = FALSE)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Green")
rasterImage(ataturklisesinoisegreen, 0, 0, 256, 256, angle = 0, interpolate = FALSE)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Blue")
rasterImage(ataturklisesinoiseblue, 0, 0, 256, 256, angle = 0, interpolate = FALSE)


#Part 2c
dev.off()
ataturklisesigray = imager::grayscale(ataturklisesinoise)
plot(c(1, 256), c(1, 256), type = "n", xlab = "", ylab = "", main = "Atatürk Lisesi Noised Gray")
rasterImage(ataturklisesigray, 1, 1, 256, 256, angle = 0, interpolate = FALSE)

#It is desired to use 25x25 sized patches. Now we have 625 instances. 

#Therefore, there will be (256-25+1) x (256-25+1) many patches. +1 comes from the initial matrix created.
#-25 comes from the length of the patches. Simple math.


DataMatrix = matrix(, nrow = 232 * 232, ncol = 25*25)

#Since for loop first increases j, the matrix is filled with data from left to right first, then i increases and matrix goes below through y axis.
k = 1
for (i in 13:244) {
  for (j in 13:244) {
    featurevector = ataturklisesigray[(i - 12):(i + 12),(j - 12):(j + 12) ]
    featurevector = as.vector(featurevector)
    DataMatrix[k, ] = featurevector
    k = k + 1
  }
}

#Part 2c-a

ataturklisesigraypca = princomp(DataMatrix)
summary(ataturklisesigraypca)

eigenvalues2=get_eigenvalue(ataturklisesigraypca)
head(eigenvalues2,10)
plot(eigenvalues2$cumulative.variance.percent,x=1:625,type="b",xlab="Dimensions",ylab="Cumulative Variance Percent Explained")
abline(v=2, col="red")
abline(h=70,col="red")

#First component is enough to obtain more than 70% of the variance.

#Part 2c-b

Scoresofpca = as.data.table(ataturklisesigraypca$scores)
head(Scoresofpca)

Component1 = matrix(Scoresofpca$Comp.1, nrow = 232, ncol = 232 , byrow = TRUE)
Component2 = matrix(Scoresofpca$Comp.2, nrow = 232, ncol = 232 , byrow = TRUE)
Component3 = matrix(Scoresofpca$Comp.3, nrow = 232, ncol = 232 , byrow = TRUE)

par(mfrow = c(1, 3))
plot(c(0, 232), c(0, 232), type = "n", xlab = "", ylab = "",main = "Reconstructed image with Component 1")
rasterImage(as.cimg(Component1), 0, 0, 232, 232, angle = 0, interpolate = TRUE)
plot(c(0, 232), c(0, 232), type = "n", xlab = "", ylab = "",main = "Reconstructed image with Component 2")
rasterImage(as.cimg(Component2), 0, 0, 232, 232, angle = 0, interpolate = TRUE)
plot(c(0, 232), c(0, 232), type = "n", xlab = "", ylab = "",main = "Reconstructed image with Component 3")
rasterImage(as.cimg(Component3), 0, 0, 232, 232, angle = 0, interpolate = TRUE)

#First component is holding the variance more than other components. Therefore, as expected, the image reconstructed from component 1 is better than others.
#Second component is worse than first. Since components are reordered with respect to their contribution to total variance, it is expected to have worse images while proceding into higher index components.

dev.off()

FirstComponent = ataturklisesigraypca$loadings[,1]
FirstComponent = matrix(FirstComponent, nrow = 25, ncol = 25, byrow = TRUE)
SecondComponent = ataturklisesigraypca$loadings[,2]
SecondComponent = matrix(SecondComponent, nrow = 25, ncol = 25, byrow = TRUE)
ThirdComponent = ataturklisesigraypca$loadings[,3]
ThirdComponent = matrix(ThirdComponent, nrow = 25, ncol = 25, byrow = TRUE)

par(mfrow = c(1, 3))

plot(c(0, 25), c(0, 25), type = "n", xlab = "", ylab = "",main = "Image of the first component")
rasterImage(as.cimg(FirstComponent), 0, 0, 25, 25, angle = 0, interpolate = TRUE)
plot(c(0, 25), c(0, 25), type = "n", xlab = "", ylab = "",main = "Image of the second component")
rasterImage(as.cimg(SecondComponent), 0, 0, 25, 25, angle = 0, interpolate = TRUE)
plot(c(0, 25), c(0, 25), type = "n", xlab = "", ylab = "",main = "Image of the third component")
rasterImage(as.cimg(ThirdComponent), 0, 0, 25, 25, angle = 0, interpolate = TRUE)

dev.off()

#First component seems to explain the building at the center of the image well.
#As expected, first component resulted better in representing the original image.
#Second component created image at the left side of the square. 
#Third component is close to first component, however it is more dense near center and less dense across edges of the image through x axis.
#Combining components may help to reconstruct the image.

