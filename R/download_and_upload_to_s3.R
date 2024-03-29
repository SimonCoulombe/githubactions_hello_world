# This is a small script thats:
#   -downloads a zip file
#   - unzip it to a temp directory, then read the csv from that directory
#   - write the csv to amazon s3

library(dplyr)
library(aws.s3)

# file.edit(".Renviron")
# AWS_ACCESS_KEY_ID = "mykey"
# AWS_SECRET_ACCESS_KEY = "mysecret"
# AWS_DEFAULT_REGION = "us-east-1"



# download the wells zip  to a temporary file
url <- "https://s3.ca-central-1.amazonaws.com/gwells-export/export/v2/gwells.zip"
temp_zip <- tempfile()
download.file(url, destfile = temp_zip)
temp_dir <- tempdir()
utils::unzip(temp_zip, files =  "well.csv", exdir = temp_dir)

# create fake  "yesterday" wells file, dropping a thousand rows.
if(FALSE){
  utils::unzip(temp_zip, files =  "well.csv")
  system("head -n 120000  well.csv   >  first120000well.csv")
  aws.s3::put_object(
    #file = "~/git/schedule_github_actions_to_save_csv_to_amazon_s3/well.csv",
    file = "first120000well.csv",
    object = paste0("well/well_20211130.csv" ),
    bucket = "blogsimoncoulombe",
    acl = "public-read" # ,
    # headers=list("Content-Type" = "image/png")
  )
}


# unzip well.csv  to a temporary directory then upload as well_YYYYMMDD.csv to s3
if (FALSE){
  temp_dir <- tempdir()
  utils::unzip(temp_zip, files =  "well.csv", exdir = temp_dir)
  
  # upload the well.csv to  well_YYYYMMDD.csv on the bucket (50MB .. long)
  aws.s3::put_object(
    #file = "~/git/schedule_github_actions_to_save_csv_to_amazon_s3/well.csv",
    file = paste0(temp_dir, "/well.csv"),
    object = paste0("well/well_", format(Sys.Date(), "%Y%m%d"), ".csv" ),
    bucket = "blogsimoncoulombe",
    acl = "public-read",
    verbose = TRUE,
    multipart = TRUE # ,
    # headers=list("Content-Type" = "image/png")
  )
}
