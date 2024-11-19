library(rsvg)
library(magick)
library(purrr)

path2images <- "/Users/carlostoruno/OneDrive - World Justice Project/EU Subnational/EU-S Data/reports/eu-thematic-reports/html/"

svgs <- c(
  "R1" = "R1F67",
  "R1" = "R1F68",
  "R1" = "R1F79",
  "R2" = "R2F31",
  "R2" = "R2F35",
  "R2" = "R2F39",
  "R2" = "R2F48"
)

imap(
  svgs,
  function(file, report){
    source <- paste0(
      path2images,
      report,"/assets/",file,".svg"
    )
    destination <- paste0(
      path2images,
      report,"/assets/",file,".png"
    )
    png <- image_read_svg(source)
    image_write(png, 
                path = destination, 
                format = "png",
                quality = 100)
  }
)
