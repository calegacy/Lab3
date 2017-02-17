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
d2_sheet[[i]] <-d2_sheet[[i]] %>% mutate(test=2,sem=i)
d2_sheet[[i]] = d2_sheet[[i]][names(d2_sheet[[i]])!=""] ## remove empty columns
d1_sheet[[i]] <-d1_sheet[[i]] %>% mutate(test=1,sem=i) 
d1_sheet[[i]] = d1_sheet[[i]][names(d1_sheet[[i]])!=""]
## specify the test for each file
} 

#------deal with file 1--I do this first because all data in the same file have similar structure

d1=rbind(d1_sheet[[1]],d1_sheet[[2]],d1_sheet[[3]],d1_sheet[[4]])
d1=d1 %>% mutate(Time=replace(Time,Time=="PRE","Pre"),  ## unify upper/lower cases
              Time=replace(Time,Time=="POST","Post"))

#split key for file1 
student1 = d1 %>% filter(Time=="Pre") %>%      ## semester/gender/charactor..whose key is only ID
  select(1,`Normalized Changes`:`Treatment Part 2 `,sem)
 
dat1 =d1 %>% select(1:`Total`,test) %>%            ## data: answer1-10...whose key is ID Time group
  gather(key=answer ,value=score,3:12) %>% 
  separate(answer,into=c("type","group"),sep=" ") 

#-----test total and normalized changes by re-derive them--------------------

## check rederive total and normalized changes
sumtotal=dat1 %>% group_by(ID,Time) %>% summarise(sum.total=sum(score)) # re-derive total
dat1.1=full_join(dat1, sumtotal, by=c("ID","Time")) 
dat1.1 %>% filter(Total!=sum.total)  ## all right!

# re-derive normalized changes (post-pre)/(100−pre) if post>pre
## (post-pre)/pre if post<pre
newchanges=sumtotal %>% spread(key=Time,value=sum.total) %>% 
  mutate(
    diff=Post-Pre, 
    normalize=100-Pre, 
    normalize=replace(normalize,diff<0,Pre),
    changes = diff/normalize) %>%
  select(ID,changes)

student1.1 = full_join(student1,newchanges,by="ID")
student1.1 %>% filter(`Normalized Changes`!=changes) ## all right!
####----------------------------

#------deal with file 2----------------------
### File2 sem1 miss the column "MTH 3" 
d2_sheet[[1]] = d2_sheet[[1]] %>% mutate(`MTH 3`=NA)  # add an empty colunm named "MTH 3"
d2=rbind(d2_sheet[[2]],d2_sheet[[1]],d2_sheet[[3]],d2_sheet[[4]])
d2=d2 %>% mutate(Time=replace(Time,Time=="PRE","Pre"),  ## unify upper/lower cases
                 Time=replace(Time,Time=="POST","Post"))

#split key for file1 
student2 = d2 %>% filter(Time=="Pre") %>%      ## semester/gender/charactor..whose key is only ID
  select(1,`Normalized Change`:`Treatment Part 2`,sem)

dat2 =d2 %>% select(1:`Total`,test) %>%            ## data: answer1-10...whose key is ID Time group
  gather(key=answer ,value=score,3:`DI 4`) %>% 
  separate(answer,into=c("type","group"),sep=" ") %>%
  mutate(score=parse_number(score))

#test total and normalized changes by re-derive them

## check rederive total and normalized changes
sumtotal2=dat2 %>% group_by(ID,Time) %>% summarise(sum.total=sum(score,na.rm=T)) # re-derive total
dat2.1=full_join(dat2, sumtotal2, by=c("ID","Time")) 
dat2.1 %>% filter(Total!=sum.total)  ## total.score match !!!


# re-derive normalized changes (post-pre)/(100−pre+60) if post>pre
## (post-pre)/pre if post<pre
newchanges2=sumtotal2 %>% spread(key=Time,value=sum.total) %>% 
  mutate(
    diff=Post-Pre, 
    normalize=100-Pre+60, ### a guess from the data
    normalize=replace(normalize,diff<0,Pre),
    changes = diff/normalize) %>%
  select(ID,changes)

student2.1 = full_join(student2,newchanges2,by="ID")
student2.1 %>% filter(`Normalized Change`!=changes) ## all right!

#---------------------------------------
#check consistence for gender/characteristic between two files
#---------------------------------------
check=full_join(student1,student2,
          by=c("ID","sem","Treatment Part 1","Treatment Part 2 "="Treatment Part 2"))
check %>% select(1,3:4,9:10) %>% filter(GENDER.x!=GENDER.y) 
## genders of 10 IDs do not match !! inconsistent

check %>% select(1,3:4,9:10) %>% filter(Characteristic.x!=Characteristic.y)
## characteristics of 13 IDs do not match !! inconsistent

### drop these inconsistences or change the value ??
# try change them in file 2 and keep information from file 1
mergekey=check %>% group_by(ID)%>% 
  mutate(
  gender=GENDER.x,
  gender=replace(gender,is.na(gender),GENDER.y),
  Characteristic = Characteristic.x,
  Characteristic = replace(Characteristic,is.na(Characteristic),Characteristic.y)
  ) %>%
  select(-c(3:4,9:10)) %>% gather(key=test,value=Normalized.change,c(2,6)) %>%
  mutate(
    test=replace(test,test=="Normalized Changes",1),
    test=replace(test,test=="Normalized Change",2),
    test=parse_number(test)
  )

mergedat <- rbind(dat1,dat2) ## merge data in two files
alldata = full_join(mergedat,mergekey,by=c("ID","test"))  ## all information form two files
#-----------------------------
#visual summary
#-----------------------------

x=alldata %>% group_by(ID,Time,test) %>% summarise(total=unique(Total)) %>%
  spread(key = Time, value = total) 
full_join(x,mergekey,by=c("ID","test")) %>% mutate(term=paste("semester",sem)) %>%
  ggplot(aes(x=Pre,y=Post,colour=gender))+geom_point()+facet_wrap(~term)
