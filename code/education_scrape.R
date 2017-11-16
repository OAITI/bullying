###
### Required Libraries
### 
library(rvest)
library(dplyr)
library(readxl)

###
### Student Enrollment
###
url <- "https://www.educateiowa.gov/education-statistics"
my_nodes <- url %>%
    read_html() %>%
    html_nodes(css = "ul")
my_links <- my_nodes[[9]] %>%
    html_node("li") %>%
    html_nodes("a") %>%
    html_attr("href")
my_links <- paste0("https:", my_links)

if (!file.exists(file.path("data", "enrollment"))) dir.create(file.path("data", "enrollment"), recursive = TRUE)
lapply(my_links, function(thelink) {
    myhref <- thelink %>%
        read_html() %>%
        html_nodes(css = ".file a")
    myfile <- myhref %>%
        html_attr("href")
    myname <- myhref %>%
        html_text
    
    download.file(myfile, file.path("data", "enrollment", myname), mode = "wb")
})

enrollment_2016 <- read_excel("data/enrollment/2015-2016 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender.xlsx", skip = 4) %>%
    select(`COUNTY NAME`, `DISTRICT NUMBER`, `Total K12`)
enrollment_2015 <- read_excel("data/enrollment/2015-2016 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender.xlsx", skip = 4) %>%
    select(`COUNTY NAME`, `DISTRICT NUMBER`, `Total K12`)
enrollment_2014 <- read_excel("data/enrollment/2015-2016 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender.xlsx", skip = 4) %>%
    select(`COUNTY NAME`, `DISTRICT NUMBER`, `Total K12`)
enrollment_2013 <- read_excel("data/enrollment/2015-2016 Iowa Public School District PreK-12 Enrollments by District, Grade, Race and Gender.xlsx", skip = 4) %>%
    select(`COUNTY NAME`, `DISTRICT NUMBER`, `Total K12`)
enrollment <- rbind(enrollment_2013, enrollment_2014, enrollment_2015, enrollment_2016)
enrollment$Year <- rep(2013:2016, times = c(nrow(enrollment_2013), nrow(enrollment_2014), nrow(enrollment_2015), nrow(enrollment_2016)))
enrollment$`COUNTY NAME`[enrollment$`COUNTY NAME` == "Pottawattam"] <- "Pottawattamie"

###
### Bullying
###
my_links <- url %>%
    read_html() %>%
    html_nodes(css = "#BullyingData~ a") %>%
    html_attr("href")
my_links <- paste0("https:", my_links)

if (!file.exists(file.path("data", "bullying"))) dir.create(file.path("data", "bullying"), recursive = TRUE)
lapply(my_links, function(thelink) {
    myhref <- thelink %>%
        read_html() %>%
        html_nodes(css = ".file a")
    myfile <- myhref %>%
        html_attr("href")
    myname <- myhref %>%
        html_text
    
    download.file(myfile, file.path("data", "bullying", myname), mode = "wb")
})

###
### Joining Bullying with County
###
bullying_2016 <- read_excel("data/bullying/2015-2016BullingData.xlsx", sheet = 2, skip = 10)
names(bullying_2016)[2] <- "District Name"
names(bullying_2016)[3] <- "Founded Incidents"

bullying_2015 <- read_excel("data/bullying/2014-2015BullingData.xlsx", sheet = 2, skip = 10)
names(bullying_2015)[2] <- "District Name"
names(bullying_2015)[3] <- "Founded Incidents"

bullying_2014 <- read_excel("data/bullying/2013-2014BullyingData.xlsx", sheet = 2, skip = 9)
names(bullying_2014)[2] <- "District Name"
names(bullying_2014)[3] <- "Founded Incidents"
bullying_2014$`Bully - Other` <- NA

bullying_2013 <- read_excel("data/bullying/2012-2013BullyingData.xlsx", sheet = 2, skip = 9)
names(bullying_2013)[2] <- "District Name"
names(bullying_2013)[43] <- "Bully - Other"

bullying_all <- rbind(bullying_2016, bullying_2015, bullying_2014, bullying_2013)
bullying_all$Year <- rep(2016:2013, times = c(nrow(bullying_2016), nrow(bullying_2015), nrow(bullying_2014), nrow(bullying_2013)))
#bullying_all$`District Name`[bullying_all$`District Name` == "Decorah Community"] <- "Decorah"

bullying_join <- bullying_all %>%
    mutate(District = as.numeric(District)) %>%
    left_join(enrollment, by = c("District" = "DISTRICT NUMBER", "Year" = "Year")) %>%
    filter(!duplicated(interaction(District, Year)),
           `District Name` != "State") %>%
    select(District:Other, Year, County = `COUNTY NAME`, Enrollment = `Total K12`)
#bullying_join[is.na(bullying_join)] <- 0
#bullying_join$`Founded Incidents`[bullying_join$`Founded Incidents` == "<10" & !is.na(bullying_join$`Founded Incidents`)] <- sample(1:9, size = sum(bullying_join$`Founded Incidents` == "<10", na.rm = TRUE), replace = TRUE)
bullying_join[bullying_join == "<10"] <- NA
write.csv(bullying_join, file = "data/bullying/bullying_clean.csv", row.names = FALSE)

