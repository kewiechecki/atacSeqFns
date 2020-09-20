#' Creates a file path from a name, directory, and extension, then recursively creates each directory in the path.
#' By default, the path is placed in a directory with today's date.
#' @param filename The output file name.
#' @param ext The file extension.
#' @param path The path to the output file.
#' @param append.date Whether to write the output to a directory with today's date.
#' @return The output file path.
#' @examples
#' mkdate("example","txt",path="path/to")
mkdate <- function(filename,ext,path='.',append.date=T){
  if(append.date){
    if(grepl('^[~/\\.]',path)) path <- paste(path, Sys.Date(), sep = '/')
    else path <- paste(Sys.Date(), path, sep = '/')
  }
  filename <- paste0(path,'/' ,filename)
  path <- sub('(^.*\\/).*',"\\1",filename)
  if(!dir.exists(path)) dir.create(path,recursive = T)
  if(ext!='') filename <- paste0(filename,'.',ext)
  return(filename)
}

#' Concatenates a path, filename, and file extension into an output path, then writes data x to the output path using function fn.
#' Any folders in the path that do not exist are created.
#' @param x Data to be written to filename.
#' @param fn Function that accepts x as its first argument and a file path as its second argument.
#' @param filename The output file name.
#' @param ext The file extension.
#' @param path The path to the output file.
#' @param ... Additional arguments to fn.
#' @param append.date Whether to write the output to a directory with today's date.
#' @return The resulting value of fn.
#' @examples
#' dir.out(iris, write.table,"example","txt",path="path/to")
dir.out <- function(x,fn,filename,ext='txt',path='.',...,append.date=T){
  filename <- mkdate(filename,ext,path,append.date)
  fn(x,filename,...)
}

#' Concatenates a path, filename, and file extension into an output path, then opens a connection to the output path using function fn.
#' Any folders in the path that do not exist are created.
#' @param filename The output file name.
#' @param fn Function that accepts x as its first argument and a file path as its second argument.
#' @param ext The file extension.
#' @param path The path to the output file.
#' @param ... Additional arguments to fn.
#' @param append.date Whether to write the output to a directory with today's date.
#' @return The resulting value of fn.
#' @examples
#' dir.img(pdf,"example","pdf",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.img <- function(filename, fn,ext, path = '.', ...,append.date=T){
  filename <- mkdate(filename,ext,path,append.date)
  fn(filename, ...)
}

#' Concatenates a path, filename, and file extension into an output path, then writes data x to the output path as a tab-separated file.
#' Any folders in the path that do not exist are created.
#' @param x Data to be written to filename.
#' @param filename The output file name.
#' @param ext The file extension.
#' @param path The path to the output file.
#' @param ... Additional arguments to write.table.
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.out}}, \code{\link{utils::write.table}}
#' @examples
#' dir.tab(iris, "example", path="path/to")
dir.tab <- function(x,filename, path = '.',ext='txt',quote=F,...,append.date=T){
  dir.out(x,write.table,filename,ext,path,sep='\t', quote=quote,...,append.date=append.date)
}

#' Concatenates a path, filename, and file extension into an output path, then writes data x to the output path as a csv.
#' Any folders in the path that do not exist are created.
#' @param x Data to be written to filename.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param ... Additional arguments to write.csv.
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.out}}, \code{\link{utils::write.csv}}
#' @examples
#' dir.csv(iris, "example", path="path/to")
dir.csv <- function(x,filename, path = '.', summary=F,quote=T,...,append.date=T){
  dir.out(x,write.csv,filename,'csv',path,quote=quote,...,append.date=append.date)
}

#' Concatenates a path, filename, and file extension into an output path, then exports data x to the output path in a specified format.
#' Any folders in the path that do not exist are created.
#' @param x Data to be written to filename.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param format An appropriate format for rtracklayer::export.
#' @param ... Additional arguments to rtracklayer::export. 
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.out}}, \code{\link{rtracklayer::export}}
#' @examples
dir.export <- function(x,filename,path='.',format='bed',...,append.date=T){ 
  require(rtracklayer)
  dir.out(x,export,filename,ext=format,path,format,...,append.date=append.date)
}

#' Concatenates a path, filename, and file extension into an output path, then opens a png connection at the output path.
#' Any folders in the path that do not exist are created.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param res The nominal resolution in ppi.
#' @param width The width of the device.
#' @param height The height of the device.
#' @param ... Additional arguments to png. 
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.img}}, \code{\link{grDevices::png}}
#' @examples
#' dir.png("example",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.png <- function(filename, path = '.', ...,append.date=T) dir.img(
  filename,png,'png',path,res=300,width=2000,height=2000,...,append.date=append.date
)

#' Concatenates a path, filename, and file extension into an output path, then opens a pdf connection at the output path.
#' Any folders in the path that do not exist are created.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param ... Additional arguments to pdf. 
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.img}}, \code{\link{grDevices::pdf}}
#' @examples
#' dir.pdf("example",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.pdf <- function(filename, path = '.', ...,append.date=T) dir.img(
  filename,pdf,'pdf',path,...,append.date=append.date
)

#' Concatenates a path, filename, and file extension into an output path, then opens a svg connection at the output path.
#' Any folders in the path that do not exist are created.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param ... Additional arguments to svg. 
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.img}}, \code{\link{grDevices::svg}}
#' @examples
#' dir.svg("example",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.svg <- function(filename, path = '.', ...,append.date=T) dir.img(
  filename,svg,'svg',path,...,append.date=append.date
)

#' Concatenates a path, filename, and file extension into an output path, then opens an eps connection at the output path.
#' Any folders in the path that do not exist are created.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param ... Additional arguments to postscript. 
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.img}}, \code{\link{grDevices::postscript}}
#' @examples
#' dir.eps("example",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.eps <- function(filename,path='.',...,append.date=T) {
  setEPS()
  dir.img(filename,postscript,'eps',path,...,append.date=append.date)
}

#' Concatenates a path, filename, and file extension into an output path, then writes a plot to the output path.
#' Any folders in the path that do not exist are created.
#' @param x A plot object.
#' @param filename The output file name.
#' @param path The path to the output file.
#' @param ext The file extension.
#' @param ... Additional arguments to ggplot2::ggsave.
#' @param append.date Whether to write the output to a directory with today's date.
#' @seealso \code{\link{dir.img}}, \code{\link{ggplot2::ggsave}}
#' @examples
#' library(ggplot2)
#' x <- ggplot2(iris)
#' dir.gg(x,"example",path="path/to")
#' plot(1:5,1:5)
#' dev.off()
dir.gg <- function(x,filename,path='.',ext='pdf',...,append.date=T) {
  require(ggplot2)
  filename <- mkdate(filename,ext,path,append.date)
  ggsave(filename,x,ext,...)
}

#' Wrapper for applying a write function to each element of list x and using names(x) as the filenames.
#' @param x A list.
#' @param fn The function to be applied to each element of x.
#' @param ... Additional arguments to fn.
#' @return A list.
#' @seealso \code{\link{dir.tab}}, \code{\link{base::lapply}}
#' @examples
#' x <- split(iris,iris$Species)
#' dir.apply(x, dir.csv, path="path/to")
dir.apply <- function(x,path,fn=dir.tab,...) sapply(names(x),function(y) fn(x[[y]],y,path,...))

#' Applies a function to all file names in a specified directory matching a given pattern.
#' @param dir The traget directory.
#' @param fn The function to be applied to each directory name.
#' @param pattern The regular expression matching desired files.
#' @param ... Additional arguments to fn.
#' @return A list.
#' @seealso \code{\link{dir.img}}, \code{\link{base::list.files}}
#' @examples
#' dir.csv(iris, "example", path="path/to")
#' dir.tab(iris, "example", path="path/to")
#' lrtab("path/to", read.csv, "\\.csv")
lrtab <- function(dir,fn=read.table,pattern=NULL,...) {
  res <- lapply(
    list.files(dir,pattern,full.names = T),
    function(x) if(file.size(x)>0) fn(x,...) else NULL)
  names(res) <- sub('\\.[A-Za-z0-9]*$','',list.files(dir,pattern))
  return(res)
}
