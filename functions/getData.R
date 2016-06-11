getData<-function(id) {
  setwd("/Users/ensheng/OneDrive/stat500/Data/")
  if (id=="104") {
    # 104 male_felony
    # count(realData[,1]==1)=6514
    # count(realData[,1]==0)=14859
    # IR = 14859/6514 = 2.28
    readFile<-read.csv("male_felony.csv",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
  } else if(id=="105") {
    # 105 Breast_Cancer
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/breast-cancer-wisconsin.data")
    realData<-readFile[,c(11,2:10)]
    realData[,1]<-ifelse(realData[,1]==4,1,0)
    realData[,7]<-as.numeric(realData[,7])      # 241/(698+241)=0.256656
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 1.896266
  } else if(id=="106") {
    # 106 glass1   
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 1.815789
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/glass1.dat",header = F)
    realData<-readFile[,c(10,1:9)]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="107") {
    # 107 ecoli-0_vs_1
    # http://sci2s.ugr.es/keel/dataset.php?cod=980
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 1.857143
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/ecoli-0_vs_1.dat",header = F)
    realData<-readFile[,c(8,1:7)][,-5] # V4 is a constant
    realData[,1]<-ifelse(realData[,1]==" negative",1,0)
  } else if(id=="108") {
    # 108	wisconsin
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/wisconsin.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="109") {
    # 109 pima
    # http://sci2s.ugr.es/keel/dataset.php?cod=155
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 1.865672
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/pima.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="110") {
    # 110 iris0
    readFile<-read.csv("iris0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="111") {
    # 111 glass0
    readFile<-read.csv("glass0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="112") {
    # 112 yeast1
    readFile<-read.csv("yeast1.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="113") {
    # 113 haberman
    readFile<-read.csv("haberman.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="114") {
    # 114 vehicle2
    readFile<-read.csv("vehicle2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive ",1,0)
  } else if(id=="115") {
    # 115 vehicle1
    # readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/vehicle1.dat",header = F)
    realData<-readFile[,c(19,1:18)]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="116") {
    # 116 vehicle3
    readFile<-read.csv("vehicle3.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="117") {
    # 117 glass-0-1-2-3_vs_4-5-6
    readFile<-read.csv("glass-0-1-2-3_vs_4-5-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="118") {
    # 118 vehicle0
    readFile<-read.csv("vehicle0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="119") {
    # 119 ecoli1
    readFile<-read.csv("ecoli1.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="120") {
    # 120 new-thyroid1
    readFile<-read.csv("new-thyroid1.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="121") {
    # 121 ecoli2
    readFile<-read.csv("ecoli2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="122") {
    # 122 segment0
    readFile<-read.csv("segment0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="123") {
    # 123 glass6
    readFile<-read.csv("glass6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="124") {
    # 124 yeast3
    readFile<-read.csv("yeast3.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="125") {
    # 125 ecoli3
    readFile<-read.csv("ecoli3.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="126") {
    # 126 page-blocks0
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("page-blocks0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="201") {
    # 201	yeast-2_vs_4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-2_vs_4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="202") {
    # 202	yeast-0-5-6-7-9_vs_4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-0-5-6-7-9_vs_4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="203") {
    # 203	vowel0
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("vowel0.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="204") {
    # 204	glass-0-1-6_vs_2
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-1-6_vs_2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="205") {
    # 205	glass2
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="206") {
    # 206	shuttle-c0-vs-c4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("shuttle-c0-vs-c4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="207") {
    # 207	yeast-1_vs_7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-1_vs_7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="208") {
    # 208	glass4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="209") {
    # 209	ecoli4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="210") {
    # 210	page-blocks-1-3_vs_4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("page-blocks-1-3_vs_4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="211") {
    # 211	abalone9-18
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone9-18.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
    realData[,2]<-ifelse(realData[,2]=="F",1,0)
  } else if(id=="212") {
    # 212	glass-0-1-6_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-1-6_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="213") {
    # 213 shuttle-c2-vs-c4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 20.5
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/shuttle-c2-vs-c4.dat",header = F)
    realData<-readFile[,c(10,1:9)]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="214") {
    # 214	yeast-1-4-5-8_vs_7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-1-4-5-8_vs_7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="215") {
    # 215	glass5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="216") {
    # 216	yeast-2_vs_8
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-2_vs_8.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="217") {
    # 217	yeast4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="218") {
    # 218	yeast-1-2-8-9_vs_7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-1-2-8-9_vs_7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="219") {
    # 219	yeast5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="220") {
    # 220	ecoli-0-1-3-7_vs_2-6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-1-3-7_vs_2-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="221") {
    # 221 yeast6
    # http://sci2s.ugr.es/keel/dataset.php?cod=135
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 41.4
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/yeast6.dat",header = F)
    realData<-readFile[,c(9,1:8)]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="222") {
    # 222	abalone19
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone19.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
    realData[,2]<-ifelse(realData[,2]=="F",1,0)
  } else if(id=="301") {
    # 301	ecoli-0-3-4_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-3-4_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="302") {
    # 302	ecoli-0-6-7_vs_3-5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-6-7_vs_3-5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="303") {
    # 303	ecoli-0-2-3-4_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-2-3-4_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="304") {
    # 304	glass-0-1-5_vs_2
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-1-5_vs_2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="305") {
    # 305	yeast-0-3-5-9_vs_7-8
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-0-3-5-9_vs_7-8.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="306") {
    # 306	yeast-0-2-5-7-9_vs_3-6-8
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-0-2-5-7-9_vs_3-6-8.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="307") {
    # 307	yeast-0-2-5-6_vs_3-7-8-9
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast-0-2-5-6_vs_3-7-8-9.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="308") {
    # 308	ecoli-0-4-6_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-4-6_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="309") {
    # 309	ecoli-0-1_vs_2-3-5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-1_vs_2-3-5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="310") {
    # 310	ecoli-0-2-6-7_vs_3-5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-2-6-7_vs_3-5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="311") {
    # 311	glass-0-4_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-4_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="312") {
    # 312	ecoli-0-3-4-6_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-3-4-6_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="313") {
    # 313	ecoli-0-3-4-7_vs_5-6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-3-4-7_vs_5-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="314") {
    # 314	ecoli-0-6-7_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-6-7_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="315") {
    # 315	ecoli-0-1-4-7_vs_2-3-5-6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-1-4-7_vs_2-3-5-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="316") {
    # 316	led7digit-0-2-4-5-6-7-8-9_vs_1
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("led7digit-0-2-4-5-6-7-8-9_vs_1.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="317") {
    # 317	glass-0-6_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-6_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="318") {
    # 318	ecoli-0-1_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-1_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="319") {
    # 319	glass-0-1-4-6_vs_2
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass-0-1-4-6_vs_2.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="320") {
    # 320	ecoli-0-1-4-7_vs_5-6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli-0-1-4-7_vs_5-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="321") {
    # 321 cleveland-0_vs_4
    # http://sci2s.ugr.es/keel/dataset.php?cod=980
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 12.30769
    readFile<-read.csv("cleveland-0_vs_4.dat",header = F)
    realData<-readFile[,c(14,1:13)]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="322") {
    # 322 ecoli-0-1-4-6_vs_5
    # http://sci2s.ugr.es/keel/dataset.php?cod=981
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 13
    # variable 3 appears to be constant within groups
    readFile<-read.csv("ecoli-0-1-4-6_vs_5.dat",header = F)
    realData<-readFile[,c(7,1:6)]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="401") {
    # 401	dermatology-6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("dermatology-6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="402") {
    # 402	zoo-3
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("zoo-3.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="403") {
    # 403	shuttle-6_vs_2-3
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("shuttle-6_vs_2-3.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="404") {
    # 404	lymphography-normal-fibrosis
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("lymphography-normal-fibrosis.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="405") {
    # 405	flare-F
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("flare-F.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="406") {
    # 406	car-good
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("car-good.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="407") {
    # 407	car-vgood
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("car-vgood.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="408") {
    # 408	kr-vs-k-zero-one_vs_draw
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kr-vs-k-zero-one_vs_draw.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="409") {
    # 409	kr-vs-k-one_vs_fifteen
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kr-vs-k-one_vs_fifteen.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="410") {
    # 410	winequality-red-4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-red-4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="411") {
    # 411	poker-9_vs_7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("poker-9_vs_7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="412") {
    # 412	kddcup-guess_passwd_vs_satan
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kddcup-guess_passwd_vs_satan.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="413") {
    # 413	abalone-3_vs_11
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone-3_vs_11.dat",header = F)
    realData<-readFile[,c(ncol(readFile),2:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="414") {
    # 414	winequality-white-9_vs_4
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-white-9_vs_4.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="415") {
    # 415	kr-vs-k-three_vs_eleven
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kr-vs-k-three_vs_eleven.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="416") {
    # 416	winequality-red-8_vs_6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-red-8_vs_6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="417") {
    # 417	abalone-17_vs_7-8-9-10
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone-17_vs_7-8-9-10.dat",header = F)
    realData<-readFile[,c(ncol(readFile),2:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="418") {
    # 418	abalone-21_vs_8
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone-21_vs_8.dat",header = F)
    realData<-readFile[,c(ncol(readFile),2:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="419") {
    # 419	winequality-white-3_vs_7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-white-3_vs_7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="420") {
    # 420	winequality-red-8_vs_6-7
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-red-8_vs_6-7.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="421") {
    # 421	kddcup-land_vs_portsweep
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kddcup-land_vs_portsweep.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="422") {
    # 422	abalone-19_vs_10-11-12-13
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone-19_vs_10-11-12-13.dat",header = F)
    realData<-readFile[,c(ncol(readFile),2:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="423") {
    # 423	kr-vs-k-zero_vs_eight
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kr-vs-k-zero_vs_eight.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="424") {
    # 424	winequality-white-3-9_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-white-3-9_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="425") {
    # 425	poker-8-9_vs_6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("poker-8-9_vs_6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="426") {
    # 426	shuttle-2_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("shuttle-2_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="427") {
    # 427	winequality-red-3_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("winequality-red-3_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="428") {
    # 428	abalone-20_vs_8-9-10
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("abalone-20_vs_8-9-10.dat",header = F)
    realData<-readFile[,c(ncol(readFile),2:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]==" positive",1,0)
  } else if(id=="429") {
    # 429	kddcup-buffer_overflow_vs_back
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kddcup-buffer_overflow_vs_back.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="430") {
    # 430	kddcup-land_vs_satan
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kddcup-land_vs_satan.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="431") {
    # 431	kr-vs-k-zero_vs_fifteen
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kr-vs-k-zero_vs_fifteen.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="432") {
    # 432	poker-8-9_vs_5
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("poker-8-9_vs_5.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="433") {
    # 433	poker-8_vs_6
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("poker-8_vs_6.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="434") {
    # 434	kddcup-rootkit-imap_vs_back
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("kddcup-rootkit-imap_vs_back.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="504") {
    # 504 wine.dat
    # http://sci2s.ugr.es/keel/dataset.php?cod=1077
    # count(realData[,1]==1)=59
    # count(realData[,1]==2)=71
    # count(realData[,1]==3)=48
    # IR = 71/48 = 1.5
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/wine.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData0<-realData[realData[,1]==1,][,-1]
    realData1<-realData[realData[,1]==3,][,-1]
    realData2<-realData[realData[,1]==2,][,-1]
  } else if(id=="505") {
    # 505	hayes-roth
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("hayes-roth.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="506") {
    # 506	contraceptive
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("contraceptive.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="507") {
    # 507	penbased
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("penbased.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="508") {
    # 508	new-thyroid
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("new-thyroid.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="509") {
    # 509	dermatology
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("dermatology.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="510") {
    # 510 balance.dat
    # http://sci2s.ugr.es/keel/dataset.php?cod=1065
    # count(realData[,1]==" L")=288
    # count(realData[,1]==" B")=49
    # count(realData[,1]==" R")=288
    # IR = 288/49 = 5.88
    readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/balance.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData0<-realData[realData[,1]==" L",][,-1]
    realData1<-realData[realData[,1]==" B",][,-1]
    realData2<-realData[realData[,1]==" R",][,-1]
  } else if(id=="511") {
    # 511	glass
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("glass.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="512") {
    # 512	autos
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("autos.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="513") {
    # 513	yeast
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("yeast.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="514") {
    # 514	thyroid
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("thyroid.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="515") {
    # 515	lymphography
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("lymphography.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="516") {
    # 516	ecoli
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("ecoli.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="517") {
    # 517	pageblocks
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("pageblocks.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  } else if(id=="518") {
    # 518	shuttle
    # IR = (nrow(realData)-sum(realData[,1]))/sum(realData[,1]) = 
    readFile<-read.csv("shuttle.dat",header = F)
    realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
    realData[,1]<-ifelse(realData[,1]=="positive",1,0)
  }
  
  return(realData)
}