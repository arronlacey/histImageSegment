library(png)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)
options(digits=20)  # this allows R to use decimals up to 20 places



######## 1. small example in a 3x3 pixel image


v <- c(0.9360514,0.8375647,0.7753784,
      0.7413904,0.5348064,0.2113981,
      0.4964131,0.3649066,0.9998654,
      0.8374608,0.6660899,0.7857080,
      0.2113981,0.5348064,0.5656764)

v

# turn vector into a 3x3x3 array (3x3 pixels), 3 channels - red,green,blue
result <- array(v,dim = c(3,3,3))

# show image
grid.raster(result,interpolate = FALSE)

# pick out the blue pixel
col.interest <- ((result[,,1] == 0.21139810000000001) &
                 (result[,,2] == 0.56567639999999997) &
                   (result[,,3] == 0.99986540000000002))

resultBlue <- result * replicate(dim(result)[3], col.interest)

grid.raster(resultBlue,interpolate = FALSE)


###### Read our immunohist slide into R


histslide = readPNG("/Users/arron/Documents/272_Introduction_to_DataScience/Week 9 - Image Processing/immunohistImageSegmentation/immunohist1.png")

dim(histslide)

grid.raster(histslide)


histslide<-histslide[,,1:3]

# convert the image array to a dataframe so we can use machine learning methods
df <- data.frame(
  red = matrix(histslide[,,1], ncol=1),
  green = matrix(histslide[,,2], ncol=1),
  blue = matrix(histslide[,,3], ncol=1)
)

head(df)
nrow(df)
dim(histslide)
# how many colours in our image? = 81,042 
nrow(distinct(df))

# vizualize all of the colours 

plot(df$red,df$green,   
     pch = 16,                                            
     col=rgb(df))

# Run kmeans with k = 3 to reduce all 81,042 colours to 3 unique colours
K <- kmeans(df,3)

# attach the cluster number to each row
df$label <- K$cluster
head(df)

colours <- cbind(data.frame(K$centers),c(1:3))
names(colours) <- c("red.c","green.c","blue.c","label")


df = join(df, colours,by="label")

# Convert data frame back to array to show image
histslide.segmented = array(dim=dim(histslide))
histslide.segmented[,,1] = df$red.c
histslide.segmented[,,2] = df$green.c
histslide.segmented[,,3] = df$blue.c
# View the result
dev.off()
grid.raster(histslide.segmented)

