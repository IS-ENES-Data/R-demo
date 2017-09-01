#function to save data in csv format

saveData <- function(bSub, dir){
  val <- getValues(bSub)
  coord<-xyFromCell(bSub, c(1:ncell(bSub)))
  table<-cbind(coord, val)
  # ";"-separeted csv file
  write.csv2(table, paste0(dir,"/data.csv"))
}
