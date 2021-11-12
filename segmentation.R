library(png)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)
options(digits=20)  # this allows R to use decimals up to 20 places



######## 1. small example in a 3x3 pixel image


v<- c(0.9360514,0.8375647,0.7753784,
      0.7413904,0.5348064,0.2113981,
      0.4964131,0.3649066,0.9998654,
      0.8374608,0.6660899,0.7857080,
      0.2113981,0.5348064,0.5656764)

# Take these vectors as input to the array.
result <- array(v,dim = c(3,3,3))
dim(result)
grid.raster(result,interpolate=FALSE)


col.interest <- ((result[,,1] == 0.21139810000000001) &
                   (result[,,2] == 0.56567639999999997) &
                   result[,,3] == 0.99986540000000002)

resultBlue<- result * replicate(dim(result)[3], col.interest)
grid.raster(resultBlue,interpolate = FALSE)


# Ex. Can you isolate the top right pixel?



## 2. Finding lesions in histology slides in people with Multiple Sclerosis

histslide = readPNG("/Users/arron/Documents/272_Introduction_to_DataScience/Week 9 - Image Processing/immunohistImageSegmentation/immunohist1.png")

dim(histslide)

grid.raster(histslide)

# 441 x 619 = 272979 pixels
# red,green, blue and ALPHA (transparency) channel
# all the values in the alpha channel are set to 1 i.e. not transparent
# so we only need to keep the red, green and blue channels

histslide<-histslide[,,1:3]

# reshape image into a data frame
df = data.frame(
  red = matrix(histslide[,,1], ncol=1),
  green = matrix(histslide[,,2], ncol=1),
  blue = matrix(histslide[,,3], ncol=1)
)

nrow(df)

# how many unique colours in this image?

nrow(distinct(df))


# We can't choose just one colour - lesions most likely consist of pixels containing 1000's of colours

# Let's vizualize the scale of the different colours

plot(dist.colours$red,dist.colours$green,   
     pch = 16,                                            
     col=rgb(dist.colours))


# Can we reduce the colour palette to make this task more manageable? 

### k-means clustering - this is an unsupervised machine learning technique
# Let's reduce to 3 colours only
set.seed(272)
K = kmeans(df,3)

# Attach cluster labels to our dataframe
df$label = K$cluster


# Replace the original colour for each pixel with the colour of the cluster

colours<-data.frame(K$centers,c(1:3))
colnames(colours) <- c("red.c","green.c","blue.c","label")
colours

# join each pixel to it;s corresponding cluster i.e. "inner join on label"

df = join(df, colours,by="label")

# Convert data frame back to array to show image
histslide.segmented = array(dim=dim(histslide))
histslide.segmented[,,1] = matrix(df$red.c, nrow=dim(histslide)[1])
histslide.segmented[,,2] = matrix(df$green.c, nrow=dim(histslide)[1])
histslide.segmented[,,3] = matrix(df$blue.c, nrow=dim(histslide)[1])
histslide.segmented[,,4] = 1

# View the result
dev.off()
grid.raster(histslide.segmented)
summary(as.factor(df$label)) / nrow(df)


# We chose 3 colours, but is there an optimum number?

# How much variance between 3 colours and the original 47313 colours?
K$tot.withinss

# Use a for loop to try different numbers of clusters
error<-vector()   # define an empty object to store the error for each cluster
for (i in 1:15){
  error[i]<-kmeans(df,i)$tot.withinss 
}

dev.off()

plot(error)

