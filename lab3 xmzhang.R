#-----------------
# lab3
#-----------------
library(tidyverse)
readxl::excel_sheets("./Spreadsheets/FileOne.xlsx")
## deal with dat1 sheet

d1_sheet <- NULL
d2_sheet <- NULL
for (i in 1:4){
d1_sheet[[i]] <- readxl::read_excel("Spreadsheets/FileOne.xlsx",sheet=i) 
d2_sheet[[i]] <- readxl::read_excel("Spreadsheets/FileTwo.xlsx",sheet=i) 
colnames(d1_sheet[[i]])[1:2] <- c("ID","Time")  # fill empty colnames
colnames(d2_sheet[[i]])[1:2] <- c("ID","Time")
d2_sheet[[i]] <-d2_sheet[[i]] %>% mutate(test=2)
d1_sheet[[i]] <-d1_sheet[[i]] %>% mutate(test=1)  ## specify the test for each file
} 

## left join by same id for two files
sem=NULL
for (i in 1:4){
sem[[i]]= left_join(d1_sheet[[i]],d2_sheet[[i]],by="ID") %>% 
  mutate(sem=i) ## specify the semester
sem[[i]]=sem[[i]][names(sem[[i]])!=""] ## remove empty columns
}
### sem1 miss the column "MTH 3" 
names(sem[[2]])[names(sem[[2]])!=names(sem[[1]])][1]
sem[[1]] = sem[[1]] %>% mutate(`MTH 3`=NA)  # add an empty colunm named "MTH 3"

## merge all data from two files
dat=rbind(sem[[2]],sem[[3]],sem[[4]],sem[[1]])
str(dat)  ## 636 * 60
#------------------
#fill in NAs
dat %>% group_by(ID) %>% gather(key=sem)


#---------------------------------------
#check consistence
#---------------------------------------
names(dat)
dat %>% group_by(ID) %>% select(`GENDER.x`,`GENDER.y`) %>% ## compare gender in two files
  gather(key=key,value=gender,`GENDER.x`:`GENDER.y`) %>% 
  summarise(gender=unique(gender))
## something wrong here!! Error: expecting a single value
dat %>% group_by(ID) %>% select(`GENDER.x`,`GENDER.y`) %>%
  gather(key=key,value=gender,`GENDER.x`:`GENDER.y`) %>% 
  summarise(gender=length(unique(gender))) %>% arrange(desc(gender)) %>% filter(gender!=2)
## 3 means 2 conflict gender records plus NA, 1 means missing gender
#----------------------
dat %>% group_by(ID) %>% select(`Characteristic.x`,`Characteristic.y`) %>%
  gather(key=key,value=Characteristic,`Characteristic.x`:`Characteristic.y`) %>% 
  summarise(Characteristic=unique(Characteristic))
## something wrong here!! Error: expecting a single value
dat %>% group_by(ID) %>% select(`Characteristic.x`,`Characteristic.y`) %>%
  gather(key=key,value=Characteristic,`Characteristic.x`:`Characteristic.y`) %>% 
  summarise(Characteristic=length(unique(Characteristic)))%>% 
  arrange(desc(Characteristic)) %>% filter(Characteristic!=2)

test = dat %>% filter(ID==30030)  ## ID 30030 with different gender and characteristic

#-----------------------------
dat %>% group_by(ID,test,sem) %>% filter(Time=="Pre") %>% 
  select(1,`GENDER`:`Treatment Part 2 `)

#-----------test total and normalized changes
#split key for file1 sem1
student = d1_sheet[[1]] %>% filter(Time=="Pre") %>% 
  select(1,`GENDER`:`Treatment Part 2 `)
 
dat1.1 = d1_sheet[[1]] %>% select(1:12) %>%
 gather(key=answer,value=score,3:12)

## check rederive total and normalized changes
sumtotal=dat1.1 %>% group_by(ID,Time) %>% summarise(sum.total=sum(score)) # re-derive total
total=d1_sheet1 %>% select(ID,Time,Total) %>% arrange(ID) ## original data


# re-derive normalized changes (post-pre)/(100−pre) if post>pre
## (post-pre)/pre if post<pre
newchanges=sumtotal %>% spread(key=Time,value=sum.total) %>% 
  mutate(
    diff=Post-Pre, 
    normalize=100-Pre, 
    normalize=replace(normalize,diff<0,Pre),
    changes = diff/normalize)

d1_sheet1 %>% select(`Normalized Changes`) %>% filter(!is.na(`Normalized Changes`)) # original data
  
####----------------------------

