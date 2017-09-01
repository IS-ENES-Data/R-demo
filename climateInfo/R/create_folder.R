#function to ceate a directory where images and data will be saved
# user is requested to put a path to directory
#seperated by '/'

create_folder <- function (file, var, t, reg ) {
  path <- readline("enter path to where folder to save images should be created: ")
  nc<-ncdf4::nc_open(file)
  projectId <- ncdf4::ncatt_get(nc,0, "project_id")$value
  modelId <- ncdf4::ncatt_get (nc, 0,"model_id")$value
  freq<- ncdf4::ncatt_get (nc, 0,"frequency")$value
  ncdf4::nc_close(nc)

  if (!(is.null(t))){
    t.id<- paste(as.character(t[1]), as.character(t[2]), sep="-")
  }
  else {t.id <-NULL}

  if (!(is.null(reg))){
    reg.id<- paste0(as.character(reg[1]), as.character(reg[2]),as.character(reg[3]),as.character(reg[4]) )
  }
  else{reg.id <- NULL}

  name <- paste(var,freq,projectId,modelId,t.id,reg.id, sep=".")
  dir <- paste0(path,"/",name)
  dir.create(dir)
  return(dir)
}
