read_all <- function(fs, dirPath, ...){
  UseMethod("read_all")
}

read_all.default <- function(fs, dirPath, ...){
  warning("Unrecognized filesystem, invoking read/writeLines...")
  if(dir.exists(dirPath))
    stop("File already exists: ", dirPath)

}

#' @rdname concat_file
#' @method concat_file webhdfs
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R get_webhdfs_home.R
#' @export
read_all.webhdfs <- function(fs, dirPath, ...){
  #Check path is non empty
  if(!nzchar(dirPath))
    stop("Directory Path must be non-empty")
  
  response <- dir_stat(fs,dirPath)
  dataset <- as.data.frame(response)
  file_name <- dataset[,1]
  all_file <- ""
				 
  if(!identical("/",substr(dirPath,start=nchar(dirPath),stop=nchar(dirPath))){
	path <- paste(dirPath,"/",sep="") 
  } else {
	path <- dirPath
  }
  for(name in file_name){
	path <- paste(path,name,sep="")
	data <- read_file(hdfs,path)
	all_file <- paste(all_file,data,sep="")
  }
  return(all_file)
}
