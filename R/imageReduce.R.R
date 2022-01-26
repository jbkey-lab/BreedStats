#library(magick)
#library(openxlsx)
#library(purrr)

imageReduce = function(dp,ndp){
  #dp="C:/Users/jake.lamkey/Desktop/Inbreds/"
  #ndp="C:/Users/jake.lamkey/Desktop/New folder/"
  dp = dp
  ndp = ndp

  image.paths = openxlsx::read.xlsx("R:/Research General/Marshalltown_IA/Inbred Pictures/Images.Path.Inbredspics.xlsx",1)

  image.paths$Inbred = gsub(image.paths$Path, pattern = dp ,replacement="")

  index.inbreds = image.paths$Inbred

  index.inbreds = index.inbreds[-1]

  for (i in index.inbreds) {
    capturas <- list.files(paste0(dp,i), pattern = "*.JPG", recursive=T, ignore.case=T)


    # Remove list element with !=
    setwd(paste0(dp,i))

    images <- purrr::map(capturas, magick::image_read)
    images <- magick::image_join(images)


    #img <- image_read(paste0(dp, i, "*.jpg"))
    resize = function(x=images){magick::image_scale(magick::image_scale(x,"640"),"480")} # percent down is smaller picture

    img_resized=lapply(images, resize)

    #create new folders
    dir.create(paste0(ndp,i))

    #write the resized images to the folder


    #resize_write()
    for(j in 1:length(img_resized)){

      magick::image_write(img_resized[[j]], path=paste0(ndp, i,"/",capturas[[j]]), format="jpg" )
    }

    rm(images,img_resized, resize,capturas,j)
    invisible(gc())

  }
}
