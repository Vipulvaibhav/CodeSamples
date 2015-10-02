initialize <- function(filepath)
{
  #if(!("fields" %in% rownames(installed.packages())))
  #{
  #  require("fields")
  #}
  library("fields")
  mydata1 <- read.csv(filepath)
  menu(mydata1)
}
#--------(A)----Displaying the Lat and Long Co-ordinates------- 
menu <- function(mydata1)
{
  lat <- mydata1$lat
  lon <- mydata1$long
  coord <- cbind(lat,lon)
#--------(C)-------Correlation of the data----------  
  cor_dim <- cor(mydata1["lat"],mydata1["long"])
#--------(D)-------Mean of the Variables-------- 
  mean_lat <- mean(mydata1$lat)
  mean_long <- mean(mydata1$long)
  mat1 <- matrix(c(mydata1$long,mydata1$lat),nrow=length(mydata1$long))
  mat2 <- matrix(c(mean_long,mean_lat),nrow=1,ncol=2)
  colnames(mat2) <- c("long", "lat")
#-------------Distance measures-----------
  euclid <- rdist(mat2,mat1)
  covar <- cov(mat1,mat1)
  maha <- mahalanobis(mat1, mat2, covar, inverted = FALSE)
  cityblock <- matrix(c(0),nrow=length(mydata1$lat))
  for(i in 1:nrow(mat1))
  {
    cityblock[i] = abs(mat1[i,1] - mat2[1])+ abs(mat1[i,2] - mat2[2])
  }
  
  #dist(rbind(mat1,mat2), method = "minkowski", diag = FALSE, upper = FALSE, p = 3)
  minwok <- matrix(c(0),nrow=length(mydata1$lat))
  for(i in 1:nrow(mat1))
  {
    minwok[i] = ((abs(mat1[i,1] - mat2[1]))^3 + (abs(mat1[i,2] - mat2[2]))^3)^(1/3)
  }
  
  cheby <- matrix(c(0),nrow=length(mydata1$lat))
  for(i in 1:nrow(mat1))
  {
    cheby[i] = max(abs(mat1[i,1] - mat2[1]), abs(mat1[i,2] - mat2[2]))
  }
  
  library("lsa")
  cosi <- matrix(c(0),nrow=length(mydata1$lat))
  for(i in 1:nrow(mat1))
  {
    cosi[i] = 1 - ((mat2[1]*mat1[i,1]+mat2[2]*mat1[i,2])/ (sqrt(abs(mat1[i,1]^2)+abs(mat1[i,2]^2)) *sqrt(abs(mat2[1]^2)+abs(mat2[2]^2))))
  }
  
 #-------Marking the 10 closest Points------------ 
  euc_close <- which(euclid %in% sort(euclid)[1:10])
  maha_close <- which(maha %in% sort(maha)[1:10])
  city_close <- which(cityblock %in% sort(cityblock)[1:10])
  min_close <- which(minwok %in% sort(minwok)[1:10])
  cheby_close <- which(cheby %in% sort(cheby)[1:10]) 
  cos_close <- which(cosi %in% sort(cosi)[1:10])
  cat("\nEuclidean closest point indexes :\n")
  print(euc_close)
cat("\nMahalanobis closest point indexes :\n")
print(maha_close)
cat("\nCity block closest point indexes :\n")
print(city_close)
cat("\nMinkowski closest point indexes :\n")
print(min_close)
cat("\nChebyshev closest point indexes :\n")
print(cheby_close)
cat("\nCosine closest point indexes :\n")
print(cos_close)
  n <- readline(prompt="Enter the number for the action: \n1.lat-long values\n2.2D plot\n3.Correlation\n4.P\n5.Euclidean\n6.Mahalanobis\n7.City Block\n8.Minkowski\n9.Chebyshev\n10.Cosine\n11.Euclidean2D\n12.Mahalanobis2D\n13.City Block2D\n14.Minkowski2D\n15.Chebyshev2D\n16.Cosine2D\n")
  if(n==1)
  {
    cat("\nThe lat and long columns are :\n")
    print(coord)
    menu(mydata1)
  }
  else if(n==2)
  {
    plot(long~lat,data=mydata1,xlab="Latitude", ylab="Longitude",type="l")
    menu(mydata1)
  }
  else if(n==3)
  {
    cat("\nThe Correlation is :\n")
    print(cor_dim)
    menu(mydata1)
  }
  else if(n==4)
  {
    cat("\nThe Point P :\n")
    print(mat2)
    menu(mydata1)
  }
  else if(n==5)
  {
    cat("\nThe Euclidean Distances are :\n")
    print(euclid)
    menu(mydata1)
  }
  else if(n==6)
  {
    cat("\nThe Mahanalobis Distances are :\n")
    print(maha)
    menu(mydata1)
  }
  else if(n==7)
  {
    cat("\nThe City Block Distances are :\n")
    print(cityblock)
    menu(mydata1)
  }
  else if(n==8)
  {
    cat("\nThe Minkowski Distances are :\n")
    print(minwok)
    menu(mydata1)
  }
  else if(n==9)
  {
    cat("\nThe Chebyshev Distances are :\n")
    print(cheby)
    menu(mydata1)
  }
  else if(n==10)
  {
    cat("\nThe Cosine Distances are :\n")
    print(cosi)
    menu(mydata1)
  }
  else if(n==11)
  {
    plot_data <- matrix(c(0),nrow=length(euc_close),ncol=2)
    plot(mat2,xlim=c(-79.1,-78.1),ylim=c(37,37.62),xlab="Latitude", ylab="Longitude",main="Euclidean Distance",type="p",pch=4,col="blue")
    for(i in 1:length(euc_close))
    {
      plot_data[i,1] = mydata1[euc_close[i],2]
      plot_data[i,2] = mydata1[euc_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest Euclidean points are : \n")
    print(plot_data)
    menu(mydata1)
  }
  else if(n==12)
  {
    plot_data <- matrix(c(0),nrow=length(maha_close),ncol=2)
    plot(mat2,xlim=c(-78.2,-76.6),ylim=c(36.9,40.8),xlab="Latitude", ylab="Longitude",main="Mahalanobis Distance",type="p",pch=4,col="blue")
    for(i in 1:length(maha_close))
    {
      plot_data[i,1] = mydata1[maha_close[i],2]
      plot_data[i,2] = mydata1[maha_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest Mahalanobis points are : \n")
    print(plot_data)
    menu(mydata1)
  }
  else if(n==13)
  {
    plot_data <- matrix(c(0),nrow=length(city_close),ncol=2)
    plot(mat2,xlim=c(-79.2,-78.1),ylim=c(37,37.5),xlab="Latitude", ylab="Longitude",main="City Block Distance",type="p",pch=4,col="blue")
    for(i in 1:length(city_close))
    {
      plot_data[i,1] = mydata1[city_close[i],2]
      plot_data[i,2] = mydata1[city_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest City Block points are : \n")
    print(plot_data)
    menu(mydata1)
  }
  else if(n==14)
  {
    plot_data <- matrix(c(0),nrow=length(min_close),ncol=2)
    plot(mat2,xlim=c(-79.1,-78.1),ylim=c(37,37.62),xlab="Latitude", ylab="Longitude",main="Minkowski Distance",type="p",pch=4,col="blue")
    for(i in 1:length(min_close))
    {
      plot_data[i,1] = mydata1[min_close[i],2]
      plot_data[i,2] = mydata1[min_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest Minkowski points are : \n")
    print(plot_data)
    menu(mydata1)
  }
  else if(n==15)
  {
    plot_data <- matrix(c(0),nrow=length(cheby_close),ncol=2)
    plot(mat2,xlim=c(-79.1,-78.1),ylim=c(37,37.62),xlab="Latitude", ylab="Longitude",type="p",main="Chebyshev Distance",pch=4,col="blue")
    for(i in 1:length(cheby_close))
    {
      plot_data[i,1] = mydata1[cheby_close[i],2]
      plot_data[i,2] = mydata1[cheby_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest Chebyshev points are : \n")
    print(plot_data)
    menu(mydata1)
  }
  else if(n==16)
  {
    plot_data <- matrix(c(0),nrow=length(cos_close),ncol=2)
    plot(mat2,xlim=c(-79.1,-78.1),ylim=c(37,37.5),xlab="Latitude", ylab="Longitude",main="Cosine Distance",type="p",pch=4,col="blue")
    for(i in 1:length(cos_close))
    {
      plot_data[i,1] = mydata1[cos_close[i],2]
      plot_data[i,2] = mydata1[cos_close[i],3]
      points(plot_data[i,1],plot_data[i,2],xlab="Latitude", ylab="Longitude",type="p",col = "red", pch=1)
    }
    cat("\nThe 10 closest Cosine points are : \n")
    print(plot_data)
    menu(mydata1)
  }
}

