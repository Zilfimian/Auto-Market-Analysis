library(rvest)
library(xml2)
library(XML)
library(stringr)


fileName <- "Page2.txt"
data.read <- function(fileName){
  
  text1 <- readChar(fileName, file.info(fileName)$size)
  # First 2 variables
  dt1 <- str_split(text1, pattern = '\n<div id=\"ad-[0-9]*\"', simplify = T) 
  dt1 <- t(dt1) %>% as.data.frame()
  split1.data <- str_split(str_split(dt1$V1, pattern  = '<a\\shref="/offer/', simplify = T)[,2], pattern = "\\s+", simplify = T)%>% as.data.frame()
  # Content info
  
  dt2 <- str_split(text1, pattern = 'grey-text', simplify = T) %>% t() %>% as.data.frame()
  dt2 <- str_split(dt2$V1, pattern = '\\s+', simplify = T)  %>% as.data.frame()
  dt2 <- dt2[seq(2,nrow(dt2), by=2),]
  
  
  dt3 <- str_split(text1, pattern = '<div class=\"ad-mob-price bold grey-text\">\r\n', simplify = T) %>% t() %>% as.data.frame()
  dt3 <- str_split(dt3$V1, pattern = '\\s{2,}', simplify = T)  %>% as.data.frame()
  dt3 <-dt3[-1,]
  
  
  autoam <- data.frame(offer=1:(nrow(dt3)+1))
  autoam$offer <- split1.data$V1
  autoam$image <- split1.data$V3
  autoam <- autoam[-1,]
  
  autoam$year <- str_extract_all(dt2$V1, pattern = "[0-9]{4}", simplify = T)[,1]
  autoam$Model <- dt2$V2
  autoam$Model2 <- dt2$V3
  autoam$Model3 <- dt2$V4
  autoam$Price <- dt3$V2
  autoam$Milage <-dt3$V8
  
  
  v9.info <- str_split(dt3$V10, pattern = "Sedan,|Coupe\\s+?,|Truck,|Hatchback,|Wagon,|Minibus,|Van,", simplify = T)
  autoam$Model4 <- trimws(v9.info[,1])
  dt4 <- str_split(v9.info[,2], pattern = ",\\s", simplify = T) %>% as.data.frame()
  
  autoam$Autom <- dt4$V1
  autoam$rul <- dt4$V2
  autoam$gas <- dt4$V3
  autoam$color <- dt4$V4
  autoam$Other <- dt4$V5
  autoam$View <-  str_extract_all(dt3$V10, pattern = "Sedan,|Coupe\\s+?,|Truck,|Hatchback,|Wagon,|Minibus,|Van,", simplify = T)[,1]
  autoam$Date <- str_extract_all(string = dt3$V13,pattern = "[0-9]*\\.[0-9]*\\.[0-9]*" ,simplify = T)[,1]
  
  
  autoam
}

getwd()

auto.am.data <- rbind(data.read("Page1.txt"), data.read("Page2.txt"), data.read("Page3.txt"), 
                      data.read("Page4.txt"), data.read("Page5.txt"), #data.read("Page6.txt"), 
                      data.read("Page7.txt"), data.read("Page8.txt"),data.read("Page9.txt")
                      )


save(file = "auto.am.data.rda", auto.am.data)
auto.am.data %>% View()
