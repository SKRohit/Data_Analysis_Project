#Following code reads school's data and calculates total year wise and class wise enrollment, Enrollment Ratio of 7 to 1 class students,
#total enrollment yearwise and district wise, total enrollment of girls yearwise and district wise, retention rates for 5 and 7 class year and district wise,
#calculate GPI yearwise and districtwise and performs some data manipulation tasks.

# Different elementS of school data are: 
# 1.Basic_Data
# 2.DisabledEnrollment
# 3.Facility_Data
# 4.General_Data
# 5.OBCEnrollment_Data
# 6.Repeaters_Data
# 7.SCEnrollment_Data
# 8.STEnrollment_Data
# 9.TotalEnrollment_Data
# 10.RTE_Data
# 11.Teachers_Data

# Code for different districts are:
# 1.Kachchh(MDDS CODE 468) 2.Banas Kantha(MDDS CODE 469) 3.Patan(MDDS CODE 470) 4.Mahesana(MDDS CODE 471) 5.Sabar Kantha(MDDS CODE 472) 6.Gandhinagar(MDDS CODE 473) 7.Ahmadabad(MDDS CODE 474) 
# 8.Surendranagar(MDDS CODE 475) 9.Rajkot(MDDS CODE 476) 10.Jamnagar(MDDS CODE 477) 11.Porbandar(MDDS CODE 478) 12.Junagadh(MDDS CODE 479) 13.Amreli(MDDS CODE 480) 14.Bhavnagar(MDDS CODE 481)
# 15.Anand(MDDS CODE 482) 16. Kheda(MDDS CODE 483) 17.Panch Mahals(MDDS CODE 484) 18.Dohad(MDDS CODE 485) 19.Vadodara(MDDS CODE 486) 20.Narmada(MDDS CODE 487) 21.Bharuch(MDDS CODE 488)
# 22.The Dangs(MDDS CODE 489) 23.Navsari(MDDS CODE 490) 24.Valsad(MDDS CODE 491) 25.Surat(MDDS CODE 492) 26. Tapi(MDDS CODE 493) 27.Aravalli 28.Botad 29.Devbhoomi Dwarka 
# 30.Gir Somnath 31.Mahisagar 32.ChhotaUdepur 33.Morbi 
## Districts from 27-33 have not been given MDDS code 



# creating a list of list to hold ten years of school data
DISEGUJ = list(list(), list(), list(), list(), list(), list(), list(), list(), list(), list())

#loop to read school data
for(i in 1:10){
  #for years 2005-2010 there are only 9 features and next years 2010-2015 there are 11 features
  if(i<=5)          
    for (j in 1:9) {      
      
      DISEGUJ[[i]][[j]] = read.csv(file.choose(), header = T)
      
    }
  else
    for(j in 1:11){
      
      
      DISEGUJ[[i]][[j]] = read.csv(file.choose(), header = T)
      
    }
}


# To add columns in different data frames
# Code between this line to line no 80 doesn't run 

for(i in 1:10) {

if(i<=5)
  for(j in 2:9){
    
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][1])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][5])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][6])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][7])
  }

else
  for(j in 2:11){
    
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][1])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][5])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][6])
    DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][7])
  }
}


for(i in 1:10) {
  
  if(i<=5)
    for(j in 2:9){
      
      DISEGUJ[[i]][[j]]$District_Name = DISEGUJ[[i]][[1]][1]
      DISEGUJ[[i]][[j]]$Block_Name = DISEGUJ[[i]][[1]][5]
      DISEGUJ[[i]][[j]]$Cluster_Name = DISEGUJ[[i]][[1]][6]
      DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][7])
    }
  
  else
    for(j in 2:11){
      
      DISEGUJ[[i]][[j]]$District_Name = DISEGUJ[[i]][[1]][1]
      DISEGUJ[[i]][[j]]$Block_Name = DISEGUJ[[i]][[1]][5]
      DISEGUJ[[i]][[j]]$Cluster_Name = DISEGUJ[[i]][[1]][6]
      DISEGUJ[[i]][[j]] = DISEGUJ[[i]][[1]] %>% mutate( District_Name = DISEGUJ[[i]][[1]][7])
    }
  
}


# Total year wise and class wise enrollment
Total_Class_Enrollment = data.frame()
Total_Boys_Enrollment = data.frame()
Total_Girls_Enrollment = data.frame()
Enrollment_7to1 = data.frame()
EnrollmentB_7to1 = data.frame()
EnrollmentG_7to1 = data.frame()

for(i in 1:10) {
  
  for(j in 1:8){
    
    Total_Class_Enrollment[i,j] = colSums(DISEGUJ[[i]][[9]][j+2]) + colSums(DISEGUJ[[i]][[9]][j+10])
    Total_Boys_Enrollment[i,j] = DISEGUJ[[i]][[9]][j+2]
    Total_Girls_Enrollment[i,j] = DISEGUJ[[i]][[9]][j+10]
    
  }
}


# Enrollment Ratio of 7 to 1 class students
for(i in 1:33){
  
  for(j in 1:10){
    
    Enrollment_7to1[i,j] = DISEGUJ[[j]][[9]]%>%filter(State_Code == i)%>%((DISEGUJ[[j]][[9]][3])+DISEGUJ[[j]][[9]][11])/
      (DISEGUJ[[j]][[9]][9]+DISEGUJ[[j]][[9]][17]))
   }
}

# Loading enrollment data in R to calculate enrollment and repeater measures

for( i in 1:10){
  
  DISEGUJ[[i]][[1]] = read.csv(file.choose(), header = T)
  DISEGUJ[[i]][[9]] = read.csv(file.choose(), header = T)
  DISEGUJ[[i]][[6]] = read.csv(file.choose(), header = T)
}
#

# to change the column names in enrollment and repeaters data

library(dplyr)

for(i in 1:8) {
  
  DISEGUJ[[i]][[9]] = rename(DISEGUJ[[i]][[9]], C1_TOTB = Class1_Total_Enr_Boys, C2_TOTB = Class2_Total_Enr_Boys, C3_TOTB = Class3_Total_Enr_Boys, 
                             C4_TOTB = Class4_Total_Enr_Boys, C5_TOTB = Class5_Total_Enr_Boys, C6_TOTB = Class6_Total_Enr_Boys, C7_TOTB = Class7_Total_Enr_Boys, C8_TOTB = Class8_Total_Enr_Boys, C1_TOTG =  Class1_Total_Enr_Girls,
                             C2_TOTG = Class2_Total_Enr_Girls, C3_TOTG = Class3_Total_Enr_Girls,C4_TOTG = Class4_Total_Enr_Girls, C5_TOTG = Class5_Total_Enr_Girls, C6_TOTG = Class6_Total_Enr_Girls,
                             C7_TOTG = Class7_Total_Enr_Girls,C8_TOTG = Class8_Total_Enr_Girls)
  
}


## To insert basic data for all the years, making the column names same and adding column District_Code in basic data

for(i in 1:10) {
  
  DISEGUJ[[i]][[1]] = read.csv(file.choose(), header = T)
  
}

# for 2013-14 and 2014-15 only because rest are same
DISEGUJ[[10]][[1]] = rename(DISEGUJ[[10]][[1]], District_Name = DISTNAME, School_Code = SCHOOL_CODE, School_Name = SCHOOL_NAME, Block_Name = BLOCK_NAME, Cluster_Name = CLUSTER_NAME, Village_Name = VILLAGE_NAME, Pincode = PINCODE)
DISEGUJ[[9]][[1]] = rename(DISEGUJ[[9]][[1]],School_Code = SCHOOL_CODE, School_Name = SCHOOL_NAME, Block_Name = BLOCK_NAME, Cluster_Name = CLUSTER_NAME, Village_Name = VILLAGE_NAME, Pincode = PINCODE)


# To add column District_Code in basic data
for(i in 1:10){
  
  DISEGUJ[[i]][[1]]$District_Code = as.numeric(sapply(DISEGUJ[[i]][[9]]$School_Code, function(x) substring(x, first = 3, last= 4)))
  
}
##

# To add District_Code in Total Enrollment data
for(i in 1:10){
  
  DISEGUJ[[i]][[9]]$District_Code = as.numeric(sapply(DISEGUJ[[i]][[9]]$School_Code, function(x) substring(x, first = 3, last= 4)))
  
}

# to calculate yearwise and district wise enrollment ratio
tapply(DISEGUJ[[1]][[9]][,c("C1_TOTB, C1_TOTG")], DISEGUJ[[1]][[9]]$District_Code, sum)

mylist = list()
enrollment_mat = matrix(mylist, nrow = 10, ncol = 33)
for( i in 1:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c("C1_TOTB", "C1_TOTG", "C7_TOTB", "C7_TOTG")]))
  enrollment = (apply(mat[c(3:4),], 2,sum))/(apply(mat[c(1:2),], 2,sum))
  mylist[[i]] = enrollment
  
}

# to convert enrollment list in a matrix (in presentable form)
for(i in 1:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {enrollment_mat[i,j] = mylist[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {enrollment_mat[i,j] = mylist[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {enrollment_mat[i,j] = mylist[[i]][j]}
    
  }
}

# to rename columns and rows
rownames(enrollment_mat) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(enrollment_mat) = c("KACHCHH", "BANAS KANTHA", "PATAN", "MAHESANA", "SABAR KANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                             "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                             "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")


# to find and represent enrollment ratio of girls
mylist2 = list()
enrollment_matg = matrix(mylist2, nrow = 10, ncol = 33)
for( i in 1:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c("C1_TOTG", "C7_TOTG")]))
  enrollment_girls = mat[2,]/mat[1,]
  mylist2[[i]] = enrollment_girls
  
}

# to convert enrollment list in a matrix (in presentable form)
for(i in 1:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {enrollment_matg[i,j] = mylist2[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {enrollment_matg[i,j] = mylist2[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {enrollment_matg[i,j] = mylist2[[i]][j]}
    
  }
}

# to rename columns and rows
rownames(enrollment_matg) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(enrollment_matg) = c("KACHCHH", "BANAS KANTHA", "PATAN", "MAHESANA", "SABAR KANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                              "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                              "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")



# to find out total enrollment yearwise and district wise
mylist3 = list()
enrollment_mattot = matrix(mylist3, nrow = 10, ncol = 33)
for( i in 1:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c(3:18)]))
  enrollment_total = apply(mat, 2, sum)
  mylist3[[i]] = enrollment_total
  
}

# to convert enrollment list in a matrix (in presentable form)
for(i in 1:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {enrollment_mattot[i,j] = mylist3[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {enrollment_mattot[i,j] = mylist3[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {enrollment_mattot[i,j] = mylist3[[i]][j]}
    
  }
}

# to rename columns and rows
rownames(enrollment_mattot) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(enrollment_mattot) = c("KACHCHH", "BANAS KANTHA", "PATAN", "MAHESANA", "SABAR KANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                                "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                                "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")


# to find out total enrollment of girls yearwise and district wise
mylist4 = list()
enrollment_mattotG = matrix(mylist4, nrow = 10, ncol = 33)
for( i in 1:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c("C1_TOTG","C2_TOTG","C3_TOTG","C4_TOTG","C5_TOTG","C6_TOTG","C7_TOTG","C8_TOTG")]))
  enrollment_totalg = apply(mat, 2, sum)
  mylist4[[i]] = enrollment_totalg
  
}

# to convert enrollment list in a matrix (in presentable form)
for(i in 1:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {enrollment_mattotG[i,j] = mylist4[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {enrollment_mattotG[i,j] = mylist4[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {enrollment_mattotG[i,j] = mylist4[[i]][j]}
    
  }
}

# to rename columns and rows
rownames(enrollment_mattotG) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(enrollment_mattotG) = c("KACHCHH", "BANAS KANTHA", "PATAN", "MAHESANA", "SABAR KANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                                 "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                                 "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")




# to load and format census 2011 data
CENSUS_EDU_2011 = read.csv(file.choose(), header = T)
CENSUS_EDU_2011 = CENSUS_EDU_2011[-c(1:6),]
colnames(CENSUS_EDU_2011) = c("Table_Name", "State_Code", "District_Code", "Area_Name", "Total/Rural/Urban", "Age_Group", "TOT_P", "TOT_M","TOT_F", "ATNDEDU_P", "ATNDEDU_M", "ATNDEDU_F", "SCH_P","SCH_M",
                              "SCH_F","CLG_P","CLG_M","CLG_F","VOC_P","VOC_M","VOC_F","SPDISBLD_P","SPDISBLD_M","SPDISBLD_F","LITC_P","LITC_M","LITC_F","OTH_P","OTH_M","OTH_F","NATNDG_ATND_BFR_P",
                              "NATNDG_ATND_BFR_M","NATNDG_ATND_BFR_F","NVRATND_P","NVRATND_M","NVRATND_F")

Gross_ER_2011 = (sum(enrollment_mattot[7,], na.rm = T))/(sum(CENSUS_EDU_2011[c(10:18), c("TOT_P")]))

# to make column names of repeater data uniform
for (i in 1:8) {
  
  DISEGUJ[[i]][[6]] = rename(DISEGUJ[[i]][[6]], AC_YEAR = acyear,FAIL1B = Repeaters_C1_Boys,FAIL2B = Repeaters_C2_Boys,FAIL3B = Repeaters_C3_Boys,FAIL4B = Repeaters_C4_Boys,FAIL5B = Repeaters_C5_Boys,FAIL6B = Repeaters_C6_Boys,
                             FAIL6B = Repeaters_C6_Boys,FAIL7B = Repeaters_C7_Boys,FAIL8B = Repeaters_C8_Boys, FAIL1G = Repeaters_C1_Girls, FAIL2G = Repeaters_C2_Girls,FAIL3G = Repeaters_C3_Girls,
                             FAIL4G = Repeaters_C4_Girls,FAIL5G = Repeaters_C5_Girls,FAIL6G = Repeaters_C6_Girls,FAIL7G = Repeaters_C7_Girls,FAIL8G = Repeaters_C8_Girls )
  
}

for(i in 9:10){
  
  DISEGUJ[[i]][[6]] = rename(DISEGUJ[[i]][[6]], School_Code = SCHCD)
  
}


# to find retention rates for 5 and 7 class year and district wise
mylist5 = list()
Retent_Rate5 = matrix(mylist5, nrow = 10, ncol = 33)
for( i in 5:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c("C5_TOTG","C5_TOTB")]))
  s2 = split(DISEGUJ[[i]][[9]], DISEGUJ[[i-4]][[9]]$District_Code)
  mat2 = sapply(s, function(x) colSums(x[,c("C1_TOTG","C1_TOTB")]))
  retention = (apply(mat, 2, sum))/(apply(mat2, 2, sum))
  mylist5[[i]] = retention
  
}

# to convert retention rate list in a matrix (in presentable form)
for(i in 5:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {Retent_Rate5[i,j] = mylist5[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {Retent_Rate5[i,j] = mylist5[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {Retent_Rate5[i,j] = mylist5[[i]][j]}
    
  }
}

# to rename columns and rows
rownames(Retent_Rate5) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(Retent_Rate5) = c("KACHCHH", "BANAS KANTHA", "PATAN", "MAHESANA", "SABAR KANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                           "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                           "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")



# to change row order
enrollmentrowname = rownames(t(enrollment_mat))
bubble_sort = function(array) {
 count = 0
 while(1) {
     count_swaps = 0
     for (j in 1 : (length(array) - 1 - count)) {
        if (array[j] > array[j + 1]) {
          s = array[j]
          array[j] = array[j+1]
          array[j+1] = s
          count_swaps = count_swaps + 1
          }
     }
     count = count + 1
     if(count_swaps == 0) break
   }
 }
 
bubble_sort(enrollmentrowname)

# to plot enrollment ratios as line with districts in x axis and ratios in y axis
transenrol_dat = as.data.frame(t(enrollment_mat))
transenrol_dat$districtname = C(1:33)

# to calculate GPI yearwise and districtwise
mylist6 = list()
egpi.mat = matrix(mylist6, nrow = 10, ncol = 33)
for( i in 1:10){
  
  s = split(DISEGUJ[[i]][[9]], DISEGUJ[[i]][[9]]$District_Code)
  mat = sapply(s, function(x) colSums(x[,c("C1_TOTB", "C2_TOTB","C3_TOTB","C4_TOTB","C5_TOTB","C6_TOTB","C7_TOTB","C1_TOTG","C2_TOTG","C3_TOTG","C4_TOTG","C5_TOTG","C6_TOTG","C7_TOTG")]))
  enrollment = (apply(mat[c(7:14),], 2,sum))/(apply(mat[c(1:7),], 2,sum))
  mylist6[[i]] = enrollment
  
}

# to convert enrollment list in a matrix (in presentable form)
for(i in 1:10){
  
  if(i <=3)
  {
    for(j in 1:25)
    {egpi.mat[i,j] = mylist6[[i]][j]} 
  }
  
  else if(i>=4 && i<=9)
  {
    for(j in 1:26)
    {egpi.mat[i,j] = mylist6[[i]][j]} 
    
  }
  
  else
  {
    for(j in 1:33)
    {egpi.mat[i,j] = mylist6[[i]][j]}
    
  }
}


# to rename columns and rows
rownames(egpi.mat) = c("2005_06", "2006_07", "2007_08", "2008_09", "2009_10", "2010_11", "2011_12", "2012_13","2013_14", "2014_15")
colnames(egpi.mat) = c("KACHCHH", "BANASKANTHA", "PATAN", "MAHESANA", "SABARKANTHA", "GANDHINAGAR", "AHMADABAD", "SURENDRANAGAR", 
                       "RAJKOT", "JAMNAGAR", "PORBANDAR", "JUNAGADH", "AMRELI", "BHAVNAGAR", "ANAND", "KHEDA", "PANCH MAHALS", "DOHAD", "VADODARA",
                       "NARMADA", "BHARUCH", "THE DANGS", "NAVSARI", "VALSAD", "SURAT", "TAPI", "ARAVALLI", "BOTAD", "DEVBHOOMI DWARKA", "GIR SOMNATH", "MAHISAGGER", "CHHOTAUDEPUR", "MORBI")