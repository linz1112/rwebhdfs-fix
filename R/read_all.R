#' Read all Files in a directory on a FileSystem
#'
#' @return a character vector that contains the contents of all Files
#' @title read_all: Read all Files in a directory on a FileSystem
#' @param fs FileSystem object
#' @param path a character vector that contains the path of files to read
#' @param ... other arguments
#' @rdname read_all
#' @export read_all
read_all <- function(fs, dirPath, ...){
  UseMethod("read_all")
}
#' @rdname read_all
#' @method read_all default
#' @export
read_all.default <- function(fs, dirPath, ...){
  warning("Unrecognized filesystem, invoking read/writeLines...")

}

#' @title Read all Files in a directory on a FileSystem
#' @rdname read_all.webhdfs
#' @method read_all webhdfs
#' @export
#' @param fs HDFS FileSystem object
#' @param path a character vector that contains the path of files to read
#' @param offset The starting byte position
#' @param length The number of bytes to be processed
#' @param buffersize used in transferring data
#' @param ... additional arguments passed to \code{\link{curl_webhdfs}}
#' @return a character vector that contains the contents of all Files
#' @references \href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}{WebHDFS}
#' \href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFSFileSystem}
#' @importFrom RCurl basicHeaderGatherer
#' @include curl_webhdfs.R
read_all.webhdfs <- function(fs, dirPath, ...){

  #Check path is non empty
  if(!nzchar(dirPath))
    stop("Directory Path must be non-empty")

  response <- dir_stat(fs,dirPath)
  dataset <- as.data.frame(response)
  file_name <- dataset[,1]
  all_file <- ""

  if(!identical("/",substr(dirPath,start=nchar(dirPath),stop=nchar(dirPath)))){
	dirpath <- paste(dirPath,"/",sep="")
  } else {
	dirpath <- dirPath
  }
  for(name in file_name){
	path <- paste(dirpath,name,sep="")
	dataset <- read_file(hdfs,path)
	all_file <- paste(all_file,dataset,sep="")
  }
  
  all_file
}
