getGSFun <- function(pathToFiles, pathToDataBase) {
  if(missing(pathToDataBase)) {
    dataframe <- data.frame()
    analys <- c()
  } else {
    dataframe <- read.delim(pathToDataBase, header = TRUE, sep = ":")
    analys <- dataframe$AnalysisID
    dataframe$X <- NULL
    
    colnames(dataframe) <- namescol
  }
  swear <- 0
  separator <- ":"
  firstCellValue <-  "2500"
  options(scipen = 999)
  cruises <- data.frame(ru = c("ÀÈ", "ÀÍÑ", "ÀÌÊ", "ÍÂ"), en = c("AI", "ANS", "AMK", "NV"))
  files <- list.files(pathToFiles, pattern = "\\.dat")
  l <- length(files)
  for (i in 1:l) {
    rawdf <- readLines(paste(pathToFiles, files[i], sep = "\\"))
    skipn <- grep(paste(firstCellValue, separator, sep = ""), rawdf) - 1
    tick <- 1
    df <- read.delim(paste(pathToFiles, files[i], sep = "\\"), header = FALSE, sep = separator, skip = skipn)
    namescol1 <- c("AnalysisID", "Site", "Interval", "DateTime", 
                   "Equipment", "Analysis", "Description")
    namescol2 <- c(df[,1])
    namescol <- c(namescol1, namescol2)
    m1 <- gregexec("\\d{1,3}\\-\\d{1,3}", files[i])
    depth <- regmatches(files[i], m1)
    depth <- unlist(depth)[2]
    m1 <- unlist(m1)[2]
    mm1 <- which(m1!=-1, arr.ind = TRUE)
    if (mm1 == -1) {
      depth <- readline(paste0("No depth data in", sep = " ", files[i], sep = "", ". Please enter depth parameters:"))
    } else if (length(unlist(depth)) < 1) {
      depth <- readline(paste0("No depth data in", sep = " ", files[i], sep = "", ". Please enter depth parameters:"))
    }
    m2 <- regexpr("bulk|rem", files[i])
    typean <- regmatches(files[i], m2)
    mm2 <- which(m2!=-1, arr.ind = TRUE)
    if (mm2 == -1) {
      typean <- readline(paste0("No analysis type in", sep = " ", files[i], sep = "", ". Please enter the type of analysis"))
    }
    m3 <- regexpr("[[:alpha:]]{3,}\\_\\d{1,}", files[i])
    try <- regmatches(files[i], m3)
    m3 <- regexpr("\\d{1,}", try)
    try <- regmatches(try, m3)
    mm3 <- which(m3!=-1, arr.ind = TRUE)
    if (mm3 == -1) {
      typean <- readline(paste0("No itteration number in", sep = " ", files[i], sep = "", ". Please enter the itteration number:"))
    }
    m4 <- regexpr("\\d{1,}\\/\\d{1,}\\/\\d{1,}\\:\\d{1,}\\:\\d{1,}:\\d{1,}\\(\\+\\d{1,}\\)", rawdf)
    date <- regmatches(rawdf, m4)
    mm4 <- which(m4!=-1, arr.ind = TRUE)
    if (mm4 == -1) {
      date <- readline(paste0("No date in ", sep = " ", files[i], sep = "", ". Please enter the date:"))
    }
    m5 <- regexpr("[[:alpha:]]{2,}\\-\\d{1,}", rawdf)
    equipment <- regmatches(rawdf, m5)
    mm5 <- which(m5!=-1, arr.ind = TRUE)
    if (mm5 == -1) {
      equipment <- readline(paste0("No equipment type in", sep = " ", files[i], sep = "", ". Please enter the equipment type:"))
    }
    m6 <- regexpr("^.{1,3}", files[i])
    cruise <- regmatches(files[i], m6)
    mm6 <- which(m5!=-1, arr.ind = TRUE)
    if(is.na(match(cruise, cruises$en))) {
      swear <- swear + 1
      cruise <- cruises[match(cruise, cruises$ru),2]
    } 
    if (mm6 == -1) {
      cruise <- readline(paste0("No cruise name in", sep = " ", files[i], sep = "", ". Please enter cruise name:"))
    }
    m7 <- regexpr("\\-\\d+\\-", files[i])
    cruisenum <- regmatches(files[i], m7)
    m7 <- regexpr("\\d+", cruisenum)
    cruisenum <- regmatches(cruisenum, m7)
    mm7 <- which(m7!=-1, arr.ind = TRUE)
    if (mm7 == -1) {
      cruisenum <- readline(paste0("No cruise number in", sep = " ", files[i], sep = "", ". Please enter cruise number:"))
    }
    m8 <- regexpr("\\-\\d+\\_", files[i])
    station <- regmatches(files[i], m8)
    m8 <- regexpr("\\d+", station)
    station <- regmatches(station, m8)
    mm8 <- which(m8!=-1, arr.ind = TRUE)
    if (mm8 == -1) {
      station <- readline(paste0("No station number in", sep = " ", files[i], sep = "", ". Please enter station number:"))
    }
    pattern <- paste0(cruise, sep = "", cruisenum, sep = "", sep = "-", station, sep = "_", depth, sep = "_", typean, sep = "_", try)
    m9 <- regexpr(pattern, analys)
    simil <- regmatches(analys, m9)
    mm9 <- which(m9!=-1, arr.ind = TRUE)
    if(length(mm9) > 1) {
      tick <- length(mm9)
    } else {
      tick <- 1
    }
    if ((floor(log10(abs(tick)))) + 1 == 1){
      pr <- "00000"
    } else if ((floor(log10(abs(tick)))) + 1 == 2){
      pr <- "0000"
    } else if ((floor(log10(abs(tick)))) + 1 == 3){
      pr <- "000"
    } else if ((floor(log10(abs(tick)))) + 1 == 4){
      pr <- "00"
    } else if ((floor(log10(abs(tick)))) + 1 == 5){
      pr <- "0"
    } else {
      pr <- ""
    }
    AnalysisID <- paste0(paste("GS", pr, tick, sep = ""), sep = "/", cruise, sep = "", cruisenum, 
                         sep = "", sep = "-", station, sep = "_", 
                         depth, sep = "_", typean, sep = "_", try)
    Site <- cruise
    Interval <- depth
    DateTime <- date
    Equipment <- equipment
    Analysis <- typean
    Description <- ""
    res <- data.frame(AnalysisID, Site, Interval, DateTime, Equipment, Analysis, Description)
    nqdata <- nrow(df)
    qdata <- matrix(df$V3, ncol = nqdata)
    res <- cbind(res, qdata)
    colnames(res) <- namescol
    dataframe <- rbind(dataframe, res)
  }
  if (swear > 0) {
    print(paste0("Some cruise names are written in Russian. It can cause an error"))
  }
  Sys.sleep(1)
  print("Table created sucessfully")
  Sys.sleep(1)
  print("Stay cheeky breaky")
  Sys.sleep(1)
  View(dataframe)
  write.table(dataframe, paste0(pathToFiles, sep = "\\", "dataframe", sep = ".", "txt"), sep = ":")
}