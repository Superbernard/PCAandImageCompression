library(jpeg)
pic<-readJPEG("C:\\Users\\Zhicong\\Desktop\\study\\R\\projects\\image compress\\ou2.jpeg") 
#replace with your directory here   
str(pic)
dim(pic)

rgb2gray<-function(rgb){
  #function that convert rgb array to 1 channel grayscale matrix
  x<- rgb[,,1]*0.21 + rgb[,,2]*0.71 + rgb[,,3]*0.07
  return(x)
}

if ( is.na( dim( pic ) [3] )==F ) { #if dimension of pic is 3
  pic_gray<-rgb2gray(pic) # convert it from rgb to gray scale
} else {    #if dimension of pic is 2
  pic_gray<-pic #no conversion
}

dim(pic_gray) #check the dimension again

rotate_pic<-function(array){
  #function to rotate the picture to the right orientation
  x1<-apply(array,2,rev)
  x2<-apply(x1,1,rev)
  return(x2)
}

x<-rotate_pic(pic_gray)
image(z=x, col  = gray((0:99)/100), xaxt="n", yaxt="n",xlab="Original") 
#display gray-scale image

size_or<-format(object.size(x),units="auto") 
#Image (matrix) size before compression
size_or  #size in MB
size_or<-object.size(x)  #size in bytes

windows()
layout(matrix(1:16,4,4,byrow=T))
par(mar=c(3, 2, 1, 2),mgp=c(2,0.5,0))

for (i in 1:16){ # check the distribution of the first 16 variables 
# (namely first 16 columns of pixel values)
  hist(x[,i],xlab=paste("x",i,sep=""),main=paste("Histgram of x",i,sep=""))
} 


#############pca from covariance matrix##########################

cova<-cov(x)

lambda<-eigen(cova)$values   #eigen values
eig_v<-eigen(cova)$vectors   #eigen vectors

graphics.off()
windows()

plot(y=lambda,x=1:length(lambda),log="x",las=1,ylab="Eigenvalues",
     xlab="Index (log scale)",main="Scree plot")  #scree plot of eigen values

cum_var<-cumsum(lambda)/sum(lambda)*100  #cumulative portion of variance 
#explained by the first n PCs

cum_var[c(5,10,20,60,100)]

windows()
plot(y=cum_var,x=1:length(lambda),log="x",las=1,ylab="Cumulative percentage of total variance",
     xlab="Index (log scale)",main="Cumulative variance")


k=100 #use the first k principal components

cum_var[100] #check the portion of variance explained by the first k PCs in 
#percentage

y<-matrix(nrow=k,ncol=dim(x)[1])
y<-t(eig_v[,1:k])%*%t(x)  #calculate principal components from eigen vectors


recon<-t(eig_v[,1:k]%*%y)  #reconstruct the data from compressed representation

dim(recon)  #check the dimension of the reconstructed data
dim(x)  #compare with reconstructed data

graphics.off()
image(z=recon, col  = gray((0:99)/100),xaxt="n", yaxt="n",xlab="Reconstructed")  
#display compressed image 



size_recon<-object.size(recon)  
#Image (matrix) size after reconstruction should be the same with the original image
size_recon

rmse <- function(y,z){   #function to calculate root mean square error
  e <- y - z
  return(sqrt(mean(e^2)))
}

size_comp<-object.size(eig_v[,1:k])+object.size(y)
#compressed size, storage of 2 matrices: eig_v[,1:k] (first k eigen vectors) and y

size_comp

error<-rmse(x,recon)  # check error

com_rat<-paste(round(size_or/size_comp,2),":",1)  #compressed ratio 
# defined as original size/ compressed size
com_rat


imcom<-function(filepath,k){
  #Function that takes the filepath of the picture and the number of pcs k 
  #to use to reconstruct the pic and output compression ratio and error
  pic<-readJPEG(filepath)
  
  if ( is.na( dim( pic ) [3] )==F ) { #if dimension of pic is 3
    pic_gray<-rgb2gray(pic) # convert it from rgb to gray scale
  } else {    #if dimension of pic is 2
    pic_gray<-pic #no conversion
  }
  
  x<-rotate_pic(pic_gray)
  
  if ( dim(x)[1]<dim(x)[2] ) {  # rotate if num of rows is larger than num of columns 
    x<-t(x)
  }
  
  size_or<-object.size(x)
  
  cova<-cov(x)  
  
  lambda<-eigen(cova)$values   #eigen values
  eig_v<-eigen(cova)$vectors   #eigen vectors
  
  y<-matrix(nrow=k,ncol=dim(x)[1])
  y<-t(eig_v[,1:k])%*%t(x)  #calculate principal components from first k eigen vectors
  
  size_comp<-object.size(eig_v[,1:k])+object.size(y)
  
  recon<-t(eig_v[,1:k]%*%y) #reconstruct the data from compressed representation
  
  comp_ratio_v<-as.numeric(round(size_or/size_comp,1))  
  #compression ratio value in 1 decimal place
  compress_ratio<-paste(round(size_or/size_comp,1),":",1) 
  #compression ratio value in the form of a : 1
  error<-rmse(recon,x)
  
  par(mar=c(0.2, 0.2, 0, 0),mgp=c(0.5,1,0))
  image(z=recon, col  = gray((0:99)/100), xaxt="n", yaxt="n")  
  #display compressed image
  text(x=0.17,y=0.06,labels=paste(k,"PCs"),col="white")
  text(x=0.84,y=0.06,labels=compress_ratio,col="white")
  
  list( compress_ratio = compress_ratio , error = error
        , comp_ratio_v =comp_ratio_v)
}


maxk<-dim(pic)[1]
maxk

ks1<-c(1:15,seq(20,30,5),seq(40,70,10),c(100,200,400))  #test different num of k

ks2<-c(  1, 20, 100, 200, 400) #test different num of k

file<-"C:\\Users\\Zhicong\\Desktop\\study\\R\\projects\\image compress\\ou2.jpeg"
#replace with your file path

ratio1<-numeric(length(ks1))  #empty vector to hold compression ratios
ratio2<-numeric(length(ks2))

e1<-character(length(ks1))   #empty vector to hold errors
e2<-character(length(ks2))


layout(matrix(1:length(ks1),5,5,byrow=T))   # display reconstructed images 
for (i in 1:length(ks1)){
  result<-imcom(file,ks1[i])
  ratio1[i]=result$comp_ratio_v
  e1[i]=result$error
  
}

r<-data.frame(ks1,ratio1)
colnames(r)<-c("number of PCs m","compression ratio in value")
write.csv(r,file="compression_ratio.csv",row.names=F) 
#write a table of compression ratio on number of PCs

windows()
plot(ks1,ratio1,log="x",xlab="number of PCs m",ylab="compression ratio") 
#plot of compression ratio on k

graphics.off()
layout(matrix(1:6,3,2,byrow=T))  # display reconstructed images 
for (i in 1:length(ks2)){
  result<-imcom(file,ks2[i])
  ratio2[i]=result$comp_ratio_v
  e2[i]=result$error
}
image(z=x, col  = gray((0:99)/100), xaxt="n", yaxt="n")   #display original image
text(x=0.15,y=0.06,labels="Original",col="white")



windows()
plot(ks1,e1,xlab="number of principal components"
     , ylab="RMSE",main="Error plot")   # plot of error on k




####################pca from correlation matrix###################

trans<-NA  %in% cor(x) #check if cor(x) is calculatable, 
#if standard deviation is zero

if ( trans==T  ) {    #if NA in cor matrix, transpose x
  x<-t(x)
}

r<-cor(x)   #start from the correlation matrix and repeat the procedures 

lambda_r<-eigen(r)$values
eigr_v<-eigen(r)$vectors

graphics.off()
windows()
plot(y=lambda_r,x=1:length(lambda_r),log="x",las=1,ylab="Eigenvalues",
     xlab="Index (log scale)",main="scree plot")
cumr_var<-cumsum(lambda_r)/sum(lambda_r)*100  
#cumulative portion of variance explained by the first n PCs

windows()
plot(y=cumr_var,x=1:length(lambda_r),log="x",las=1
     ,ylab="Cumulative percentage of total variance",
     xlab="Index (log scale)",main="Cumulative variance")




std<-function(x){      #function to calculate standard deviation by column(variable)
  output<-sqrt(var(x))
  return(output)
}

mu<-apply(x,2,mean)  #mean of each variable
stdev<-apply(x,2,std) #standard deviation of each variable
z<-(x-mu)/stdev

graphics.off()
image(z,col  = gray((0:99)/100),xaxt="n",yaxt="n",xlab="Image on z")  
#Buid the image based on z, see the difference with that from x

k=200 #use the first k principal components

yh<-matrix(nrow=k,ncol=dim(x)[1])
yr<-matrix(nrow=k,ncol=dim(x)[1])

yh<-t(eigr_v[,1:k])%*%t(z)
yr<-t(eigr_v[,1:k])%*%t(x) #calculate principal components from eigen vectors


rec_r<-t(eigr_v[,1:k]%*%yr) #reconstruct the data from compressed representation

dim(rec_r) #check the dimension of the reconstructed data
dim(x) #compare with reconstructed data

graphics.off()

if ( trans==T  ) {    #if x is transposed, transpose it back before display
  image(z=t(rec_r), col  = gray((0:99)/100),xaxt="n",yaxt="n"
        ,xlab="Reconstructed")    #display compressed image 
} else {
  image(z=rec_r, col  = gray((0:99)/100),xaxt="n"
        ,yaxt="n",xlab="Reconstructed")
}


imcom_r<-function(filepath,k){
  #Put the procedures into a function that takes directory of picture and k, and output
  #compression ratio and error. It also displays the reconstructed image
  
  pic<-readJPEG(filepath)
  
  if ( is.na( dim( pic ) [3] )==F ) { #if dimension of pic is 3
    pic_gray<-rgb2gray(pic) # convert it from rgb to gray scale
  } else {    #if dimension of pic is 2
    pic_gray<-pic #no conversion
  }
  
  x<-rotate_pic(pic_gray)
  
  if ( dim(x)[1]<dim(x)[2] ) {
    x<-t(x)
  }
  
  
  trans<-NA  %in% cor(x) #check if cor(x) is calculatable, 
  #if standard deviation is zero
  
  if ( trans==T  ) {    #if NA in cor matrix, transpose x
    x<-t(x)
  }
  
  size_or<-object.size(x)
  
  r<-cor(x)
  lambda_r<-eigen(r)$values
  eigr_v<-eigen(r)$vectors
  
  mu<-apply(x,2,mean)  #mean of each variable
  stdev<-apply(x,2,std) #standard deviation of each variable
  z<-(x-mu)/stdev
  
  yh<-matrix(nrow=k,ncol=dim(x)[1])
  yr<-matrix(nrow=k,ncol=dim(x)[1])
  yh<-t(eigr_v[,1:k])%*%t(z)
  yr<-t(eigr_v[,1:k])%*%(t(z*stdev+mu)) 
  #calculate principal components from eigen vectors
  
  
  rec_r<-t(eigr_v[,1:k]%*%yr) #reconstruct the data from compressed 
  #representation
  
  size_comp<-object.size(eigr_v[,1:k])+object.size(yr)
  
  comp_ratio_v<-as.numeric(round(size_or/size_comp,1))  
  #compression ratio value in 1 decimal place
  compress_ratio<-paste(round(size_or/size_comp,1),":",1) 
  #compression ratio value in the form of a : 1
  error<-rmse(rec_r,x)
  
  par(mar=c(0.2, 0.2, 0, 0),mgp=c(0.5,1,0))
  
  
  if ( trans==T  ) {    #if x is transposed, transpose it back before display
    image(z=t(rec_r), col  = gray((0:99)/100),xaxt="n"
          , yaxt="n", xlab=paste(k,"PCs","compression ratio",compress_ratio))  #display compressed image)  #display compressed image 
  } else {
    image(z=rec_r, col  = gray((0:99)/100), xaxt="n"
          , yaxt="n", xlab=paste(k,"PCs","compression ratio",compress_ratio))  #display compressed image)
  }

  text(x=0.15,y=0.06,labels=paste(k,"PCs"),col="white")
  text(x=0.84,y=0.06,labels=compress_ratio,col="white")
  
  
  list( compress_ratio = compress_ratio 
        , error = error, comp_ratio_v =comp_ratio_v)
}



graphics.off()
imcom_r(file,300)  #check function

maxk<-dim(pic)[1]
maxk

ks1<-c(1:15,seq(20,30,5),seq(40,70,10),c(100,200,400))
ks2<-c(  1, 20, 100, 200, 400)

file<-"C:\\Users\\Zhicong\\Desktop\\study\\R\\projects\\image compress\\ou2.jpeg"

ratio_r1<-character(length(ks1))
ratio_r2<-character(length(ks1))

er1<-character(length(ks1))
er2<-character(length(ks2))


layout(matrix(1:length(ks1),5,5,byrow=T))
for (i in 1:length(ks1)){
  result<-imcom_r(file,ks1[i])
  ratio_r1[i]=result$comp_ratio_v
  er1[i]=result$error
}


layout(matrix(1:6,3,2,byrow=T))
for (i in 1:length(ks2)){
  result<-imcom_r(file,ks2[i])
  ratio_r2[i]=result$comp_ratio_v
  er2[i]=result$error
}
image(z=x, col  = gray((0:99)/100), xaxt="n", yaxt="n")
text(x=0.15,y=0.06,labels="Original",col="white")


windows()
plot(ks1,er1,xlab="number of principal components", ylab="RSME",main="Error plot")





################test on another image############################


file2<-"C:\\Users\\Zhicong\\Desktop\\study\\R\\projects\\image compress\\ou1.jpg"  
#replace with your directory here 
pic<-readJPEG(file2) 

if ( is.na( dim( pic ) [3] )==F ) { #if dimension of pic is 3
  pic_gray<-rgb2gray(pic) # convert it from rgb to gray scale
} else {    #if dimension of pic is 2
  pic_gray<-pic #no conversion
}

x2<-rotate_pic(pic_gray)  #rotate image
graphics.off()
windows()
image(z=x2, col  = gray((0:99)/100), xaxt="n", yaxt="n",xlab="Original") 
#display original gray scale image

size_or2<-object.size(x2)  
#Image size before compression, storage of matrix x
size_or2


maxk<-dim(pic)[1]
maxk
ks<-c(10, 20, 50, 80, 100, 200, 300, maxk)  #different values in k

ratio<-character(length(ks))
windows()
layout(matrix(1:9,3,3,byrow=T))    # display reconstructed images
for (i in 1:length(ks)){
  ratio[i]=imcom(file2,ks[i])$compress_ratio
}
image(z=x2, col  = gray((0:99)/100), xaxt="n", yaxt="n")
text(x=0.15,y=0.06,labels="Original",col="white")

ratio
