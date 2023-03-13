library (tabulizer)
library(dplyr)
library(stringr)
options(java.parameters = "- Xmx1024m")
#first extravtion
pdfd <- extract_tables("E://AI-60.pdf", pages = 214)
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
long_deg <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{1,2}")
  long_deg <- c(long_deg, l)
}
print(long_deg)
lat_deg <- c()
for (i in 1:d) {
  l <- str_extract(df[i,3], "\\d{1,2}")
  lat_deg <- c(lat_deg, l)
}
print(lat_deg)
long_min <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{1,2}\\.\\d{1,2}")
  long_min <- c(long_min, l)
}
print(long_min)
lat_min <- c()
for (i in 1:d) {
  l <- str_extract(df[i,3], "\\d{1,2}\\.\\d{1,2}")
  lat_min <- c(lat_min, l)
}
print(lat_min)
long_p <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "[:upper:]")
  long_p <- c(long_p, l)
}
print(long_p)
lat_p <- c()
for (i in 1:d) {
  l <- str_extract(df[i,3], "[:upper:]")
  lat_p <- c(lat_p, l)
}
print(lat_p)
long_deg <- as.numeric(long_deg)
lat_deg <- as.numeric(lat_deg)
long_min <- as.numeric(long_min)
lat_min <- as.numeric(lat_min)
long <- c()
lat <- c()
for (i in 1:d) {
  l <- paste0(if (long_p[i]=="S") {
    paste ("-", sep = "")
  } else if (long_p[i]=="N"){
    paste("", sep = "")
  }, paste((long_min[i]/60) + long_deg[i], sep = "."))
    long <- c(long, l)
}
print(long)
for (i in 1:d) {
  l <- paste0(if (lat_p[i]=="E") {
    paste ("-", sep = "")
  } else if (long_p[i]=="W"){
    paste("", sep = "")
  }, paste((lat_min[i]/60) + lat_deg[i], sep = "."))
  lat <- c(lat, l)
}
print(lat)
series <- data.frame()
for (i in 1:d) {
ser <- tibble(
  id_id = i,
  site = df[i,1],
  longitude = long[i],
  latitude = lat[i],
  longitude_deg = long_deg[i],
  longitude_min = long_min[i],
  ew = long_p[i],
  latitude_deg = lat_deg[i],
  latitude_min = lat_min[i],
  ns = lat_p[i],
  depth = df[i,4]
)
series <- rbind(series, ser)
}
#second extraction
pdfd <- extract_tables("E://AI-60.pdf", pages = 200, method = "lattice")
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
stat <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{4}")
  stat <- c(stat, l)
}
print(stat)
dev <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "-\\s\\w+")
  dev <- c(dev, l)
}
print(dev)
df2 <- data.frame(stat, dev)
df2 <- na.omit(df2)
pdfd <- extract_tables("E://AI-60.pdf", pages = 201, method = "lattice")
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
stat <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{4}")
  stat <- c(stat, l)
}
print(stat)
dev <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "-\\s\\w+")
  dev <- c(dev, l)
}
print(dev)
df3 <- data.frame(stat, dev)
df3 <- na.omit(df3)
df2 <- rbind(df2, df3)
pdfd <- extract_tables("E://AI-60.pdf", pages = 202, method = "lattice")
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
stat <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{4}")
  stat <- c(stat, l)
}
print(stat)
dev <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "-\\s\\w+")
  dev <- c(dev, l)
}
print(dev)
df3 <- data.frame(stat, dev)
df3 <- na.omit(df3)
df2 <- rbind(df2, df3)
pdfd <- extract_tables("E://AI-60.pdf", pages = 203, method = "lattice")
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
stat <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{4}")
  stat <- c(stat, l)
}
print(stat)
dev <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "-\\s\\w+")
  dev <- c(dev, l)
}
print(dev)
df3 <- data.frame(stat, dev)
df3 <- na.omit(df3)
df2 <- rbind(df2, df3)
pdfd <- extract_tables("E://AI-60.pdf", pages = 204, method = "lattice")
df <- pdfd[[1]]
df <- df[-1,]
d <- nrow(df)
stat <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "\\d{4}")
  stat <- c(stat, l)
}
print(stat)
dev <- c()
for (i in 1:d) {
  l <- str_extract(df[i,2], "-\\s\\w+")
  dev <- c(dev, l)
}
print(dev)
df3 <- data.frame(stat, dev)
df3 <- na.omit(df3)
df2 <- rbind(df2, df3)
#third extraction
pdfd <- extract_tables("E://AI-60.pdf", pages = 108, method = "lattice")
df4 <- pdfd[[1]]
df4 <- df4[-1,]
pdfd <- extract_tables("E://AI-60.pdf", pages = 109, method = "lattice")
df5 <- pdfd[[1]]
df4 <- rbind(df4, df5)
pdfd <- extract_tables("E://AI-60.pdf", pages = 110, method = "lattice")
df5 <- pdfd[[1]]
df4 <- rbind(df4, df5)
pdfd <- extract_tables("E://AI-60.pdf", pages = 111, method = "lattice")
df5 <- pdfd[[1]]
df4 <- rbind(df4, df5)
d <- nrow(df4)
#series2
long_deg <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,3], "\\d{1,2}")
  long_deg <- c(long_deg, l)
}
print(long_deg)
lat_deg <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,5], "\\d{1,2}")
  lat_deg <- c(lat_deg, l)
}
print(lat_deg)
long_min <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,3], "\\d{1,2}\\.\\d{1,2}")
  long_min <- c(long_min, l)
}
print(long_min)
lat_min <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,5], "\\d{1,2}\\.\\d{1,2}")
  lat_min <- c(lat_min, l)
}
print(lat_min)
device <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,2], "\\(\\w+\\)")
  device <- c(device, l)
}
print(device)
site <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,2], "\\d{1,4}")
  site <- c(site, l)
}
print(site)
long_deg <- as.numeric(long_deg)
lat_deg <- as.numeric(lat_deg)
long_min <- as.numeric(long_min)
lat_min <- as.numeric(lat_min)
long <- c()
lat <- c()
for (i in 1:d) {
  l <- paste0(paste((long_min[i]/60) + long_deg[i], sep = ""))
  long <- c(long, l)
}
print(long)
for (i in 1:d) {
  l <- paste0("-", sep = "", paste((lat_min[i]/60) + lat_deg[i], sep = ""))
  lat <- c(lat, l)
}
print(lat)
series2 <- data.frame()
for (i in 1:d) {
  ser2 <- tibble(
    id_id = i,
    site = paste("AI", site[i], sep = "-"),
    longitude = long[i],
    latitude = lat[i],
    longitude_deg = long_deg[i],
    longitude_min = long_min[i],
    ns = paste("N"),
    latitude_deg = lat_deg[i],
    latitude_min = lat_min[i],
    ew = paste("E"),
    depth = df4[i,7],
    length = df4[i,9],
    device = if(device[i]=="(Ä×)") {
      paste("grab")
    } else if (device[i]=="(ÒÁÄ)") {
      paste("gravity corer")
    },
    cruise = paste("AI-60")
  )
  series2 <- rbind(series2, ser2)
}
series2$length <-str_extract(series2$length, "~?\\d{1,3}")
#fourth extraction
pdfd <- extract_tables("E://ans45.pdf", pages = 128, method = "lattice")
df4 <- pdfd[[1]]
df4 <- df4[-1,]
pdfd <- extract_tables("E://ans45.pdf", pages = 129, method = "lattice")
df5 <- pdfd[[1]]
df4 <- rbind(df4, df5)
pdfd <- extract_tables("E://ans45.pdf", pages = 130, method = "lattice")
df5 <- pdfd[[1]]
df4 <- rbind(df4, df5)
d <- nrow(df4)
#series3
long_deg <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,5], "\\d{1,2}")
  long_deg <- c(long_deg, l)
}
print(long_deg)
lat_deg <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,6], "\\d{1,2}")
  lat_deg <- c(lat_deg, l)
}
print(lat_deg)
long_min <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,5], "\\d{1,2}\\.\\d{1,2}")
  long_min <- c(long_min, l)
}
print(long_min)
lat_min <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,6], "\\d{1,2}\\.\\d{1,2}")
  lat_min <- c(lat_min, l)
}
print(lat_min)
device <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,1], "\\w+\\*?$")
  l <- str_extract(l, "\\w+")
  device <- c(device, l)
}
print(device)
site <- c()
for (i in 1:d) {
  l <- str_extract(df4[i,1], "\\d{1,4}\\-\\d{1,4}")
  site <- c(site, l)
}
print(site)

site2 <- c()
site3 <- c()
for (i in 1:d) {
  l <- str_extract(site[i], "\\d{1,2}")
  l2 <- str_extract(site[i], "\\d{1,2}$")
  site2 <- c(site2, l)
  site3 <- c(site3, l2)
}
print(site2)
print(site3)

long_deg <- as.numeric(long_deg)
lat_deg <- as.numeric(lat_deg)
long_min <- as.numeric(long_min)
lat_min <- as.numeric(lat_min)
long <- c()
lat <- c()
for (i in 1:d) {
  l <- paste0(paste((long_min[i]/60) + long_deg[i], sep = ""))
  long <- c(long, l)
}
print(long)
for (i in 1:d) {
  l <- paste0("-", sep = "", paste((lat_min[i]/60) + lat_deg[i], sep = ""))
  lat <- c(lat, l)
}
print(lat)
series3 <- data.frame()
for (i in 1:d) {
  ser3 <- tibble(
    id_id = i,
    site = paste0("ANS", sep = "-", site2[i], paste(site3[i], sep = "")),
    longitude = long[i],
    latitude = lat[i],
    longitude_deg = long_deg[i],
    longitude_min = long_min[i],
    ns = paste("N"),
    latitude_deg = lat_deg[i],
    latitude_min = lat_min[i],
    ew = paste("E"),
    depth = df4[i,7],
    length = df4[i,8],
    device = if(device[i]=="ä÷") {
      paste("grab")
    } else if (device[i]=="òáä") {
      paste("gravity corer")
    },
    cruise = paste("ANS-45")
  )
  series3 <- rbind(series3, ser3)
}



a <- AMK_79
d <- nrow(AMK_79)
long_deg <- c()
for (i in 1:d) {
  l <- str_extract(a[i,3], "\\d{1,2}")
  long_deg <- c(long_deg, l)
}
print(long_deg)
lat_deg <- c()
for (i in 1:d) {
  l <- str_extract(a[i,4], "\\d{1,2}")
  lat_deg <- c(lat_deg, l)
}
print(lat_deg)
long_min <- c()
for (i in 1:d) {
  l <- paste(0, str_extract(a[i,3], "\\.\\d{1,5}"), sep = "")
  l <- as.numeric(l) * 60
  long_min <- c(long_min, l)
}
print(long_min)
lat_min <- c()
for (i in 1:d) {
  l <- paste(0, str_extract(a[i,4], "\\.\\d{1,5}"), sep = "")
  l <- as.numeric(l) * 60
  lat_min <- c(lat_min, l)
}
print(lat_min)

device <- c()
for (i in 1:d) {
  l <- str_extract(a[i,7], "\\w{1,2}")
  device <- c(device, l)
}
print(device)
for (i in 1:d) {
  if(device[i]=="Ä÷") {
  device[i] <- paste("grab")
} else if (device[i]=="Ò") {
  device[i] <- paste("gravity corer")
} else if (device[i]=="Ò1") {
  device[i] <- paste("gravity corer")
} else if (device[i]=="Ò2") {
  device[i] <- paste("gravity corer")
} else if (device[i]=="ò") {
  device[i] <- paste("gravity corer")
} else {
  device[i] <- paste()
}
}

print(device)



length <- c()
for (i in 1:d) {
  l <- str_extract(a[i,6], "\\-\\d{1,2}")
  l <- str_extract(l, "\\d{1,2}")
  length <- c(length, l)
}
print(length)







amk <- data.frame()
for (i in 1:72) {
  aa <- tibble(
    site = paste("AMK", sep = "-", a[i,2]),
    longitude = paste(round(a[i,3], 4), sep = ""),
    latitude = paste(round(a[i,4], 4), sep = ""),
    longitude_deg = long_deg[i],
    longitude_min = long_min[i],
    ns = paste("S"),
    latitude_deg = lat_deg[i],
    latitude_min = lat_min[i],
    ew = paste("E"),
    depth = paste(a[i,5]),
    length = length[i],
    device = device[i],
    cruise = paste("AMK-79")
  )
  amk <- rbind(amk, aa)
}