## PCA Image Compression on RGB Pitures
#
# The aim of this piese of script is to implement principal component analysis (PCA) on three-channel RGB picture arrays. The 
# main function takes the compression ratio and the image file to be compressed as input variables. The number of how many 
# principal components to use will be calculated from the compression ratio. And The reconstructed image is then displayed to 
# make comparison with the original image.    
#
# Compression ratio R:
# R = So / Sc , with So represents the size of the original picture array and Sc represents the total size of the compressed
# picture arrays.   
#


library(jpeg)   #load packages
library(grid)

pic<-readJPEG("Thunder.jpeg")  #pic imported via readJPEG function is an array with each pixel vale ranging from 0 to 1
format(object.size(pic),units="auto")  #check size of pic 

grid.raster(pic)  #display picture
dev.off()

################## functions ##################

eigenpca<-function(x,k){
  
  #Function that takes a matrix and the number of principal components (pcs) k and 
  #output a list with first k principal components matrix and the corresponding eigne vector matrix
  #in the list
  
  cova<-cov(x)  #covariance matrix
  
  lambda<-eigen(cova)$values  #eigen values
  eig_v<-eigen(cova)$vectors  #eigen vectors
  
  eigen_k<-eig_v[,1:k]    #choose the first k pcs
  
  y<-matrix(nrow=k,ncol=dim(x)[1])
  
  y<-t(eigen_k)%*%t(x)  #calculate principal components from eigen vectors
  
  output<-list(PCs=y, eigen_k=eigen_k)    #output PCk and eigenVector_k matrices
  return(output)
}

kvalue<-function(com_ratio,dim1,dim2){ #function to determine k (number of PCs to use) 
  #based on compression ratio (original size / compressed size)
  
  #basic idea: size of an empty 3-dimentional array is 208 bytes (empty matrix 200 bytes). 
  #and One numeric element 8 bytes.  
  
  d1=dim1  #dimensions 
  d2=dim2
  
  k_exact=((208+3*8*d1*d2)/com_ratio-208*2)/(3*8*(d1+d2))  
  #formula to estimate k from compression ratio
  
  k_int=round(k_exact,digits=0)  #round k to the nearest integer   
  
  return(k_int)
}


JPEGcomp<-function(readpath,ratio,savepath_y,savepath_eig){
  #function to compress the pic array into 2 arrays with smaller size
  
  #library(jpeg)
  pic<-readJPEG(readpath)  #load picture
  
  dim1<-dim(pic)[1]     #get dimensions
  dim2<-dim(pic)[2]
  
  k<-kvalue(ratio,dim1,dim2)  #call function defined above to determine k from compression ratio
  
  list1<-eigenpca(pic[,,1],k)  #get the first k pcs of channel 1 (matrix 1) in RGB array
  list2<-eigenpca(pic[,,2],k)  #get the first k pcs of channel 2 (matrix 2) in RGB array
  list3<-eigenpca(pic[,,3],k)  #get the first k pcs of channel 3 (matrix 3) in RGB array
  
  
  y_array<-array(c(list1[[1]],list2[[1]],list3[[1]]),dim=c(k,dim1,3))
  #put the pc matrices of all 3 channels together to form a reorganized array
  
  eigen_array<-array(c(list1[[2]],list2[[2]],list3[[2]]),dim=c(dim2,k,3))
  #put the eigen vector matrices of all 3 channels together to form a reorganized array
  
  save( y_array, file = savepath_y)   #save the reorganized pc array
  save( eigen_array, file = savepath_eig)   #save the reorganized eigen vector array
  
}


rec<-function(y,eigen){    #reconstruct matrix from pcs and eigen vectors and also standardize values 
  #of the reconstrcuted matrix to the range 0 to 1
  
  rec_mtr<-t(eigen%*%y)  #matrix reconstrcution 
  
  if(min(rec_mtr)<0){    #if the minmum pixel value in the reconstructed array is negative
    rec_mtr<-rec_mtr-min(rec_mtr)   #conduct matrix transformation that each element x = x - min(x)
  } #all pixel values should become positive after this step 
  
  if (max(rec_mtr)>1){    #if the maxmum pixel value in the reconstructed array is larger than 1
    rec_mtr=rec_mtr/max(rec_mtr)    #conduct matrix transformation that each element x = x / max(x)
  } #all pixel values should fall into the range of 0 to 1 after this step 
}




recnstr<-function(PCs,Eigen_Mtr,loadfile=F){
  #function to reconstruct picture
  #input variables represent directory of pc matrices, directory of eigen vector 
  #matrices, whether to load pc matrices data from directory, pc matrices, eigen 
  #vector matrices
  
  if (loadfile==T){    #if load pc and eigen vetor matrices from directory
    
    PCs<-load(file.choose())  #load pc matrix
    PCs<-get(PCs)
    
    Eigen_Mtr<-load(file.choose())  #load eigen vetor matrix
    Eigen_Mtr<-get(Eigen_Mtr)
  }  
  
  r_pc<-PCs[,,1]  #pc matrix of channel 1 R
  g_pc<-PCs[,,2]  #pc matrix of channel 2 G
  b_pc<-PCs[,,3]  #pc matrix of channel 3 B
  
  eigen_r<-Eigen_Mtr[,,1]  #eigen vector matrix of channel 1 R
  eigen_g<-Eigen_Mtr[,,2]  #eigen vector matrix of channel 2 G
  eigen_b<-Eigen_Mtr[,,3]  #eigen vector matrix of channel 3 B
  
  recon_r<-rec(r_pc,eigen_r)  #reconstructed matrix of channel 1 R
  recon_g<-rec(g_pc,eigen_g)  #reconstructed matrix of channel 2 G
  recon_b<-rec(b_pc,eigen_b)  #reconstructed matrix of channel 3 B
  
  comp_data<-array(as.vector(c(recon_r,recon_g,recon_b)),
                   dim=c(dim(recon_r)[1],dim(recon_r)[2],3))  
  #assemble reconstructed matrices back into a 3 dimensional array (RGB pic array)
  
  comp_img<-grid.raster(comp_data)  #display the reconstructed picture
  
}



##############test functions########################

JPEGcomp("Thunder.jpeg",6.2,"Thunder_compressed1.Rda","Thunder_compressed2.Rda")
#compress the picture with a ratio of 6.2:1

thunder_comp1<-load("Thunder_compressed1.Rda")  #load pc matrix 
thunder_comp1<-get(thunder_comp1)
thunder_comp2<-load("Thunder_compressed2.Rda")  #load eigen vector matrix 
thunder_comp2<-get(thunder_comp2)

check_ratio<-as.numeric(object.size(pic)/(object.size(thunder_comp1)    #check actual compression ratio 
                                          +object.size(thunder_comp2)))  
check_ratio   #should be around 6.2 (compared with predetermined compression ratio in the input)

recnstr(NA, NA,loadfile=T) # show reconstructed pic
# arrays load from directory, set first 2 variables as NA since matrices are loaded from directory 
dev.off()
recnstr(thunder_comp1, thunder_comp2,loadfile=F) # show reconstructed pic
# arrays load from global envirenment
dev.off()
