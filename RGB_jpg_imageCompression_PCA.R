library(jpeg)
library(grid)

pic<-readJPEG("Thunder.jpeg")  #pic is an array with each pixel vale ranging from 0 to 1
format(object.size(pic),units="auto")

grid.raster(pic)  #display picture
dev.off()

################## functions ##################

eigenpca<-function(x,k){
  
  #Function that takes a matrix and the number of principal components (pcs) k and 
  #out put a list with first k principal components and corresponding eigne vector matrix
  #in the list
  
  cova<-cov(x)  #covariance matrix
  
  lambda<-eigen(cova)$values  #eigen values
  eig_v<-eigen(cova)$vectors  #eigen vectors
  
  eigen_k<-eig_v[,1:k]    #choose the first k pcs
  
  y<-matrix(nrow=k,ncol=dim(x)[1])
  
  y<-t(eigen_k)%*%t(x)  #calculate principal components from eigen vectors
  
  output<-list(PCs=y, eigen_k=eigen_k)
  return(output)
}

kvalue<-function(com_ratio,dim1,dim2){ #function to determine k (number of PCs to use) 
#based on compress ratio (original size / compressed size)
  
  #basic idea: size of an empty 3-dimentional array is 208 bytes (empty matrix 200 bytes). 
  #and One numeric element 8 bytes.  
  
  d1=dim1  #dimensions 
  d2=dim2
  
  k_exact=((208+3*8*d1*d2)/com_ratio-208*2)/(3*8*(d1+d2))  
  #formula to estimate k from compression ratio
  
  k_int=round(k_exact,digits=0)  
  
  return(k_int)
}


JPEGcomp<-function(readpath,ratio,savepath_y,savepath_eig){
  #function to compress the pic array into 2 arrays with smaller size
  
  library(jpeg)
  pic<-readJPEG(readpath)  #load picture
  
  dim1<-dim(pic)[1]     #get dimensions
  dim2<-dim(pic)[2]
  
  k<-kvalue(ratio,dim1,dim2)  #determine k from compression ratio
  
  list1<-eigenpca(pic[,,1],k)  #get the first k pcs of channel 1 (matrix 1) in RGB array
  list2<-eigenpca(pic[,,2],k)  #get the first k pcs of channel 2 (matrix 2) in RGB array
  list3<-eigenpca(pic[,,3],k)  #get the first k pcs of channel 3 (matrix 3) in RGB array
  
  
  y_array<-array(c(list1[[1]],list2[[1]],list3[[1]]),dim=c(k,dim1,3))
  # put the pc matrices of all 3 channels together
  
  eigen_array<-array(c(list1[[2]],list2[[2]],list3[[2]]),dim=c(dim2,k,3))
  # put the eigen vector matrices of all 3 channels together
  
  save( y_array, file = savepath_y)
  save( eigen_array, file = savepath_eig)
  
}


rec<-function(y,eigen){    #reconstruct matrix from pcs and eigen vectors 
  # also standardize values of the reconstrcuted matrix to the range 0 to 1
  
  rec_mtr<-t(eigen%*%y)  #matrix reconstrcution 
  
  if(min(rec_mtr)<0){
    rec_mtr<-rec_mtr-min(rec_mtr)
  }
  
  if (max(rec_mtr)>1){
    rec_mtr=rec_mtr/max(rec_mtr)
  } 
}




recnstr<-function(loadpath_PCs,loadpath_Eigen_Mtr,loadfile=T,PCs,Eigen_Mtr){
  #function to reconstruct picture
  #input variables represent directory of pc matrices, directory of eigen vector 
  #matrices, whether to load pc matrices data from directory, pc matrices, eigen 
  #vector matrices
  
  if (loadfile==F){    #load pc and eigen vetor matrices from environment 
    
    r_pc<-PCs[,,1]  #pc matrix of channel 1 R
    g_pc<-PCs[,,2]  #pc matrix of channel 2 G
    b_pc<-PCs[,,3]  #pc matrix of channel 3 B
    
    eigen_r<-PCs[,,1]   #eigen vector matrix of channel 1 R
    eigen_g<-PCs[,,2]   #eigen vector matrix of channel 2 G
    eigen_b<-PCs[,,3]   #eigen vector matrix of channel 3 B
    
    recon_r<-rec(r_pc,eigen_r)  #reconstructed matrix of channel 1 R
    recon_g<-rec(g_pc,eigen_g)  #reconstructed matrix of channel 2 G
    recon_b<-rec(b_pc,eigen_b)  #reconstructed matrix of channel 3 B
    
  } else{   #load pc and eigen vetor matrices from directory
    
    PCs<-load(loadpath_PCs)
    PCs<-get(PCs)
    
    Eigen_Mtr<-load(loadpath_Eigen_Mtr)
    Eigen_Mtr<-get(Eigen_Mtr)
    
    r_pc<-PCs[,,1]
    g_pc<-PCs[,,2]
    b_pc<-PCs[,,3]
    
    eigen_r<-Eigen_Mtr[,,1]
    eigen_g<-Eigen_Mtr[,,2]
    eigen_b<-Eigen_Mtr[,,3]
    
    recon_r<-rec(r_pc,eigen_r)
    recon_g<-rec(g_pc,eigen_g)
    recon_b<-rec(b_pc,eigen_b)
  }  
  
  comp_data<-array(as.vector(c(recon_r,recon_g,recon_b)),
                   dim=c(dim(recon_r)[1],dim(recon_r)[2],3))  
  #combine reconstructed matrices back into a 3 dimensional array (RGB pic array)
  
  comp_img<-grid.raster(comp_data)  #display the reconstructed picture
  
}



##############test functions########################

JPEGcomp("Thunder.jpeg",6.2,"Thunder_compressed1.Rda","Thunder_compressed2.Rda")
#compress the picture with a ratio of 6.2:1

thunder_comp1<-load("Thunder_compressed1.Rda")  #load pc matrix 
thunder_comp1<-get(thunder_comp1)
thunder_comp2<-load("Thunder_compressed2.Rda")  #load eigen vector matrix 
thunder_comp2<-get(thunder_comp2)

check_ratio<-as.numeric(object.size(pic)/(object.size(thunder_comp1)
                                          +object.size(thunder_comp2)))  
#check compression ratio
check_ratio   #should be around 6.2

recnstr("Thunder_compressed1.Rda","Thunder_compressed2.Rda") # show reconstructed pic
dev.off()

