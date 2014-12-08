rm(list=ls())

require(plyr)
require(reshape2)
require(stringr)


files_to_grab <- dir("data/messy/e0_per/E0per/")

fn <- function(filename, dir){
  out <- read.table(
    paste0(
      dir, filename
      ),
    skip=1, header=T
    )
  
  country_code_part <- str_extract(filename, "^[^\\.]*") 
  # regex: 
  # ^ : start of line
  # [^\\.]*  : anything but '.', 1 or more times
  
  out$country_code <- tolower(country_code_part)
  names(out) <- tolower(names(out))
  
  return(out)
}

tidied_data <- ldply(
  files_to_grab,
  fn,
  dir="data/messy/e0_per/E0per/"
  )

write.csv(tidied_data, file="data/tidy/e0_per.csv", row.names=FALSE)


files_to_grab <- dir("data/messy/e0_coh/E0coh/")

tidied_data <- ldply(
  files_to_grab,
  fn,
  dir="data/messy/e0_coh/E0coh/"
)

write.csv(tidied_data, file="data/tidy/e0_coh.csv", row.names=FALSE)
