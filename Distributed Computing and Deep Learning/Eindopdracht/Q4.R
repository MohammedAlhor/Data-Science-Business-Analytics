#install.packages("magick")
library(magick)

#
im1 <- image_read("~/Data-Science-Business-Analytics/Data/triang.png")
print(im1)


imdat1 <- image_data(im1, channels= "rgb") %>% as.integer
# imdat1 %>% dim %>% print
N <- NROW(imdat1)

imdat_pad <- cbind(rep(255, N), imdat1[,,1])
imdat_pad <- rbind(rep(255, (N+1)), imdat_pad)
imdat_pad <- cbind(imdat_pad, rep(255, (N+1)) )
imdat_pad <- rbind(imdat_pad, rep(255, (N+2)) )

dim(imdat_pad)

Prewitt <- matrix(c(0,1,1,-1,0,1,-1,-1,0), nrow= 3, ncol= 3, byrow= T)

kk <- NROW(imdat_pad)
filterr <- matrix(nrow= (kk-2), ncol= (kk-2))
for (i in 1:(kk-2)){
    for (j in 1:(kk-2)){
        filterr[i,j] <- sum( Prewitt * imdat_pad[i:(i+2), j:(j+2) ] )
    } }

# dim(filterr)

# Arrange as array
filterr_array <- array(filterr, dim= c(NROW(filterr), NCOL(filterr), 3) )
im_filter <- magick::image_read(filterr_array / 255)
im_filter
