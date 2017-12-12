#### requires netmeta
library(netmeta)

### loading the data
library(xlsx)
rm(list = ls())
setwd("C:/Users/efthimiou/Desktop/post doc/PROJECT/SNF/paper 2-NMA/data")
dataMH= read.xlsx("example7.xlsx", sheetIndex = 1, stringsAsFactors=FALSE, colClasses=NA)

###load data by DONG
dong <- read.csv("C:/Users/efthimiou/Desktop/post doc/PROJECT/SNF/paper 2-NMA/data/Dong.csv", sep=";", stringsAsFactors=FALSE)
colnames(dong)=c("year","studyid","id","treatment","total","events")
dataMH=data.frame(dong$id,dong$treatment,dong$total,dong$events,  stringsAsFactors = F)
colnames(dataMH)=c("studyid","treatment","total","events")



######################################################################################################
# MH NMA 
# NOTE: to remove netgraph from the return list!
MHnma=function(data,studyid,treatment,events,total){
  
  #######################################################################################################  
  #############                                                                             #############
  #############              FIRST STAGE: SET UP THE DATA                                   #############
  #############                                                                             #############
  #######################################################################################################   

### set up a data frame
  studyid = eval(substitute(studyid), data)
  treatment = eval(substitute(treatment), data)
  events = eval(substitute(events), data)
  total = eval(substitute(total), data)  
  dataMH=data.frame(studyid, treatment, events,total,stringsAsFactors = F)
colnames(dataMH)=c("studyid","treatment","events","total")  
  
  
### pool studies with multiple arms of the same treatment
treatsbyarmmat = table(dataMH$studyid, dataMH$treatment)
pr=apply(treatsbyarmmat >1, 1, sum) > 0
studies.with.problem =unique(names(pr)[as.vector(pr)==T])
dataMH$clean = is.na(match(dataMH$studyid, studies.with.problem))

allr = alln = allt = allid =  c()
for (i in studies.with.problem) {
  nonproblematicarm = colnames(treatsbyarmmat)[treatsbyarmmat[i,] == 1]
  problematicarm = colnames(treatsbyarmmat)[treatsbyarmmat[i,] > 1]
  
  rnew=c()
  nnew=c()
  for(j in problematicarm){
    dataMH$select=c(dataMH$studyid == i & dataMH$treatment == j)
    rnew = c(rnew,sum(dataMH$events[dataMH$select]))
    nnew = c(nnew,sum(dataMH$total[dataMH$select]))}
  
  rold=c()
  nold=c()
  for (k in nonproblematicarm) {
  rold = c(rold, dataMH$events[dataMH$studyid == i & dataMH$treatment == k])
  nold = c(nold, dataMH$total[dataMH$studyid == i & dataMH$treatment == k]) }
  allr = c(allr, rnew, rold)
  alln = c(alln, nnew, nold)
 
  
  allt = c(allt, problematicarm, nonproblematicarm)
  allid = c(allid, rep(i,times=length(problematicarm)), rep(i,length(nonproblematicarm)))  
  
}

dataMH = cbind.data.frame(studyid = c(dataMH$studyid[dataMH$clean], allid), treatment=c(dataMH$treatment[dataMH$clean], allt),
                           events=c(dataMH$events[dataMH$clean], allr), total=c(dataMH$total[dataMH$clean], alln), stringsAsFactors=FALSE)



###remove single arms studies from dataset
nr.of.arms=table(dataMH$studyid)
oxo=names(table(dataMH$studyid))[table(dataMH$studyid)<2]
if(length(oxo>0)){
for (i in 1:length(oxo)){print(paste0("Study ", oxo[i], " was excluded from the network because it compares the same treatments"))  }
delete.arms=match(oxo,dataMH$studyid)
dataMH=dataMH[-delete.arms,]
}

 ### recode studies
  s1=unique(dataMH$studyid)
  for (i in 1:length(dataMH$studyid))
  {
    for (j in 1:length(s1))
         {
      if (dataMH$studyid[i]==s1[j]){dataMH$study[i]=j}
    }
  }
  
   ###sort study, treatment
dataMH=dataMH[order(dataMH$study, dataMH$treatment),]  
  
 #### exclude all-zero studies  
total.events=c(rep(0,length(unique(dataMH$study))))
for (i in 1:length(unique(dataMH$study))){
    total.events[i]=sum(dataMH$events[which(dataMH$study==i)])
    if(total.events[i]==0)  {
      print(paste0("Study ", s1[i], " was excluded from the network (all-zero study)"))
      dataMH=dataMH[dataMH$study!=i,]} 
}
rm(total.events)

 #### group studies by design
design=c(rep("",length(unique(dataMH$study))))
for (i in 1:length(unique(dataMH$study)))
  {
  for (j in 1:length(dataMH$study)){
    
    if (dataMH$study[j]==unique(dataMH$study)[i]){design[i]=paste(design[i],dataMH$treatment[j], sep=":")}
  }
  }
design=substring(design,2,)
for (i in 1:length(unique(dataMH$study))){
  for (j in 1:length(dataMH$study)){
    if (dataMH$study[j]==unique(dataMH$study)[i]){dataMH$Design[j]=design[i]}  }  }
   

 ### for each design drop treatments with no events
by.design=list()
for (i in 1:length(unique(design))){
  by.design[[i]]= subset(dataMH, Design== unique(design)[i])
}
for (i in 1:length(by.design)){
  events.tr=c(rep(0,length(unique(by.design[[i]]$treatment))))
  tt1=unique(by.design[[i]]$treatment)
  
  for (j in 1:length(unique(by.design[[i]]$treatment))){
    
events.tr[j]=sum(by.design[[i]]$events[which(by.design[[i]]$treatment==unique(by.design[[i]]$treatment)[j])])
}
  for (j in 1:length(unique(by.design[[i]]$treatment))){
if(events.tr[j]==0){by.design[[i]]=by.design[[i]][by.design[[i]]$treatment!=tt1[j],]}
    }
}


 ### remove designs with only one arm
N.remove=0
for (i in 1:length(by.design)){
  by.design[[i]]$remove=0
  
if(length(unique(by.design[[i]]$treatment))==1){
  print(paste0("Design ", by.design[[i]]$Design[1], " was excluded from the network (no events were reported in all treatments except one)"))
  by.design[[i]]$remove=1
 # N.remove=N.remove+1
}
}
by.design2=Filter(function(x){x$remove[1]==0},by.design)
by.design=by.design2
rm(by.design2)

 ### calculate some useful quantities
N.designs= length(by.design)     #number of designs
data.new=by.design[[1]] 


if (N.designs>1){for (i in 2:N.designs){data.new=rbind(data.new,by.design[[i]])} } #new dataset
N.studies=length(unique(data.new$studyid))
N.treat=length(unique(data.new$treatment))   #available treatments in the dataset
data.new=data.new[order(data.new$studyid,data.new$treatment),]

 ### to be used for the network graph
data.new1=data.new
data.new1$events=1 #### only to be used for drawing the network
DATApairsBin=pairwise(treat=data.new1$treatment,event=data.new1$events,n=data.new1$total, studlab = data.new1$study, sm= "OR")
connection=netconnection(treat1=DATApairsBin$treat1, treat2=DATApairsBin$treat2)
if (connection$n.subnets!=1){
stop(paste("Network is not connected. Number of disconnected networks:", connection$n.subnets),".") }
net1=netmeta(DATApairsBin,sm="OR",r=data.new1$treatment[1],comb.fixed =T, comb.random = F, tol.multiarm=0.0001)


#######################################################################################################  
#############                                                                             #############
#############       SECOND STAGE: PERFORM MH META-ANALYSIS IN EACH DESIGN                 #############
#############                                                                             #############
#######################################################################################################   

### List of available designs 
design.list=unique(data.new$Design)

### number of treatments in each design
NT.d=c(0,rep(N.designs))
for (i in 1:N.designs){
  NT.d[i]=length(unique(by.design[[i]]$treatment))
}


### number of studies in each design
NS.d=c(0,rep(N.designs))
for (i in 1:N.designs){
  NS.d[i]=length(unique(by.design[[i]]$study))
} 

### define non-events
for (i in 1:N.designs){
  by.design[[i]]$non.events=by.design[[i]]$total-by.design[[i]]$events
} 

### list of treatments in each design
treat.per.design=list()
for (i in 1:N.designs){
  treat.per.design[[i]]=unique(by.design[[i]]$treatment)
} 

### list of studies in each design 
studies.in.design=list()  
for (i in 1:N.designs){studies.in.design[[i]]=unique(by.design[[i]]$study)}

### recode the studies in each design
for (i in 1:N.designs){
    for (j in 1:length(by.design[[i]]$study)){
        for (k in 1:NS.d[i]) {
    if (by.design[[i]]$study[j]==studies.in.design[[i]][k]){by.design[[i]]$st.id[j]=k}
  }
}
}


### recode the treatments in each design
for (i in 1:N.designs){
  for (j in 1:length(by.design[[i]]$study)){
    for (k in 1:NT.d[i]) {
      if (by.design[[i]]$treatment[j]==treat.per.design[[i]][k]){by.design[[i]]$tr.id[j]=k}
    }  }}


### calculate total patients per studies
for (i in 1:N.designs){
  for (j in 1:length(by.design[[i]]$studyid)){
    by.design[[i]]$total.per.study[j]=sum(by.design[[i]]$total[which(by.design[[i]]$study==by.design[[i]]$study[j])])}
  }

  


### define c.xy and d.xy
c.xy=list()
d.xy=list()
for (i in 1:N.designs){c.xy[[i]]=array(rep(0,NS.d[i]*NT.d[i]*NT.d[i]), dim=c(NT.d[i],NT.d[i],NS.d[i]))}
for (i in 1:N.designs){d.xy[[i]]=array(rep(0,NS.d[i]*NT.d[i]*NT.d[i]), dim=c(NT.d[i],NT.d[i],NS.d[i]))}

for (i in 1:N.designs){  
  for (st in 1:NS.d[i]){
    for (t1 in 1:NT.d[i]){
      for (t2 in 1:NT.d[i]){
        
        c.xy[[i]][t1,t2,st]=sum(by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)])*
          sum(by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)])/
          by.design[[i]]$total.per.study[by.design[[i]]$st.id==st][1]
        
        d.xy[[i]][t1,t2,st]=(sum(by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)])+
          sum(by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)]))/
          by.design[[i]]$total.per.study[by.design[[i]]$st.id==st][1]
      }  }  }}

### define C_xy
C_xy=list()
for (i in 1:N.designs){
C_xy[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]), nrow=NT.d[i])
for (j in 1:NT.d[i]){
  for (k in 1:NT.d[i]){
    C_xy[[i]][j,k]= sum(c.xy[[i]][j,k,])
  }}}

### define L.xy
L.xy=list()
for (i in 1:N.designs){
  L.xy[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]), nrow=NT.d[i])
  for (j in 1:NT.d[i]){
    for (k in 1:NT.d[i]){
      L.xy[[i]][j,k]= log(C_xy[[i]][j,k]/C_xy[[i]][k,j])
    }}}
### calculate the variance of L.xy, dimensions NT.d[i]*NT.d[i]
U.xyy=list()
for (i in 1:N.designs){
  U.xyy[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]), nrow=NT.d[i])
  
  for (j in 1:NT.d[i]){
    for (k in 1:NT.d[i]){
      U.xyy[[i]][j,k]= sum(c.xy[[i]][j,k,]*d.xy[[i]][j,k,])/(2*C_xy[[i]][j,k]^2)+
                        sum(c.xy[[i]][j,k,]*d.xy[[i]][k,j,]+c.xy[[i]][k,j,]*d.xy[[i]][j,k,])/(2*C_xy[[i]][j,k]*C_xy[[i]][k,j])+
                      sum(c.xy[[i]][k,j,]*d.xy[[i]][k,j,])/(2*C_xy[[i]][k,j]^2)
    }}}




##### calculate the covariance matrix U.xyz 
t.pl=list()
for (i in 1:N.designs){t.pl[[i]]=array(rep(0,NS.d[i]*NT.d[i]*NT.d[i]), dim=c(NT.d[i],NT.d[i],NS.d[i]))}
for (i in 1:N.designs){
  for (j in 1:NS.d[i]){
  t.pl[[i]][,,j]=by.design[[i]]$total.per.study[by.design[[i]]$st.id==j][1]
  }}

U.xyz=list()
for (i in 1:N.designs){
  U.xyz[[i]]=array(rep(0,NT.d[i]*NT.d[i]*NT.d[i]), dim=c(NT.d[i],NT.d[i],NT.d[i]))
           }

per.study1=list()
per.study2=list()
per.study3=list()
per.study4=list()
for (i in 1:N.designs){ 
  per.study1[[i]]=c(rep(0,NS.d[i]))
  per.study2[[i]]=c(rep(0,NS.d[i]))
  per.study3[[i]]=c(rep(0,NS.d[i]))
  per.study4[[i]]=c(rep(0,NS.d[i]))
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      for (t3 in 1:NT.d[i]){
        for (st in 1:NS.d[i]){
per.study1[[i]][st]=by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)]/(t.pl[[i]][1,1,st])^2*
                  by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)]*
                  by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t3)]*
                    (t1!=t2)*(t1!=t3)*(t2!=t3)
                  
per.study2[[i]][st]=by.design[[i]]$total[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)]/(t.pl[[i]][1,1,st])^2*
  by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)]*
  by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t3)]*
  (t1!=t2)*(t1!=t3)*(t2!=t3)

per.study3[[i]][st]=by.design[[i]]$total[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)]/(t.pl[[i]][1,1,st])^2*
  by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)]*
  by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t3)]*
  (t1!=t2)*(t1!=t3)*(t2!=t3)


per.study4[[i]][st]=by.design[[i]]$non.events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t1)]/(t.pl[[i]][1,1,st])^2*
  by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t2)]*
  by.design[[i]]$events[which(by.design[[i]]$st.id==st&by.design[[i]]$tr.id==t3)]*
  (t1!=t2)*(t1!=t3)*(t2!=t3)

        }
U.xyz[[i]][t1,t2,t3]=sum(per.study1[[i]][])/(3*C_xy[[i]][t1,t2]*C_xy[[i]][t1,t3])+
                     sum(per.study2[[i]][])/(3*C_xy[[i]][t1,t2]*C_xy[[i]][t3,t1])+
                     sum(per.study3[[i]][])/(3*C_xy[[i]][t2,t1]*C_xy[[i]][t1,t3])+
                     sum(per.study4[[i]][])/(3*C_xy[[i]][t2,t1]*C_xy[[i]][t3,t1])
        }}}}



### calculate L.bar.xy
L.bar.xy=list()
for (i in 1:N.designs){
  
  L.bar.xy[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]), nrow=NT.d[i])
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      L.bar.xy[[i]][t1,t2]= (sum(L.xy[[i]][t1,])-sum(L.xy[[i]][t2,]))/NT.d[i]
    }}}





### calculate U.plus.xx
for (i in 1:N.designs){
  for (t1 in 1:NT.d[i]){
U.xyy[[i]][t1,t1]=0}}
U.plus.xx=list()
for (i in 1:N.designs){
  U.plus.xx[[i]]=c(rep(0,NT.d[i]))
  for (t1 in 1:NT.d[i]){

        U.plus.xx[[i]][t1]= sum(U.xyy[[i]][t1,1:NT.d[i]])+sum(U.xyz[[i]][t1,,])
    }
}


### calculate U.plus.xy
U.new=list()

  for (i in 1:N.designs){
    U.new[[i]]=array(rep(0,NT.d[i]*NT.d[i]*NT.d[i]), dim=c(NT.d[i],NT.d[i],NT.d[i]))
  }
for (i in 1:N.designs){
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      for (t3 in 1:NT.d[i]){
        
U.new[[i]][t1,t2,t3]= (t1!=t2)*(t1!=t3)*(t2!=t3)*U.xyz[[i]][t1,t2,t3]+
                     (t1!=t2)*(t2==t3)*U.xyy[[i]][t1,t2]
      }}}}

U.plus.xy=list()
for (i in 1:N.designs){ U.plus.xy[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]),nrow = NT.d[i])}

for (i in 1:N.designs){ 
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      
      U.plus.xy[[i]][t1,t2]= (sum(U.new[[i]][1:NT.d[i],t1,t2])-sum(U.new[[i]][t1,t2,1:NT.d[i]])-sum(U.new[[i]][t2,t1,1:NT.d[i]]))+U.new[[i]][t1,t2,t2]
    }
  }}

### variance of L.bar.xy
Var.Lbar=list()
for (i in 1:N.designs){
  
  Var.Lbar[[i]]=matrix(rep(0,NT.d[i]*NT.d[i]), nrow=NT.d[i])
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      Var.Lbar[[i]][t1,t2]= (U.plus.xx[[i]][t1]-2*U.plus.xy[[i]][t1,t2]+U.plus.xx[[i]][t2])/NT.d[i]^2
    }}}

### Covariance of L.bar.xy .Only a subset of covariances are calculate here, i.e. cov(L.bar_(1,t1), L.bar_(1,t2)) 
CoVar.Lbar=list()
for (i in 1:N.designs){
  
  CoVar.Lbar[[i]]=matrix(rep(0,NT.d[i]^2), nrow=NT.d[i])
  for (t1 in 1:NT.d[i]){
    for (t2 in 1:NT.d[i]){
      CoVar.Lbar[[i]][t1,t2]= (U.plus.xx[[i]][1]-U.plus.xy[[i]][1,t2]-U.plus.xy[[i]][t1,1]+U.plus.xy[[i]][t1,t2])/NT.d[i]^2
    }}}


#######################################################################################################  
#############                                                                             #############
#############                         THIRD STAGE: NMA                                    #############
#############                                                                             #############
#######################################################################################################   


### create y, the vector of treatment effects from each study. Only effects vs the first treatment are needed. y is coded as OR 1vsX 

Dim.y=sum(NT.d[]-1)
y=c(rep(0,Dim.y))
counter=0
for (i in 1:N.designs){

  for(j in 1:(NT.d[i]-1)){
    y[j+counter]=L.bar.xy[[i]][1,j+1]
    
  }
  counter=counter+NT.d[i] -1 
}



### create V, the matrix of covariances of treatment effects from each study. Only covariances of effects vs the first treatment are needed.
V1=matrix(rep(0,Dim.y*Dim.y),nrow = Dim.y)
V2=matrix(rep(0,Dim.y*Dim.y),nrow = Dim.y)
counter=0
for (i in 1:N.designs){
  
for (j in 1:(NT.d[i]-1)){
  for (k in 1:(NT.d[i]-1)){
    
    V1[j+counter,k+counter]=(k==j)*Var.Lbar[[i]][1,j+1]
                       }
  
}
counter=counter+NT.d[i]-1
}

counter=0
for (i in 1:N.designs){
  
  for (j in 2:(NT.d[i])){
    for (k in 2:(NT.d[i])){
      
      V2[j+counter-1,k+counter-1]=(k!=j)*CoVar.Lbar[[i]][j,k]
    }
    
  }
  counter=counter+NT.d[i]-1
}
V=V1+V2
  
### define H matrix
H=matrix(rep(0, N.treat*((N.treat-1)/2)*(N.treat-1)), nrow=N.treat*((N.treat-1)/2))

for (i in 1:N.treat-1){
  H[i,i]=1
}

if(N.treat>2){
t1=c()
t2=c()
for (i in 2:(N.treat-1)){
  for (j in (i+1):(N.treat)){
t1=rbind(t1,i)
  t2=rbind(t2,j)
}
}
h1=matrix(c(t1,t2),nrow=nrow(t1))



for (i in 1:((N.treat-1)*(N.treat-2)/2)){
  for(j in 1:(N.treat-1)){
  
   H[i+N.treat-1,j]=-(h1[i,1]==j+1)+(h1[i,2]==j+1)
 }}
}


### define matrix X
X=matrix(rep(0,Dim.y*(N.treat-1)), nrow=Dim.y)

treat.list=sort(unique(data.new$treatment))
for (i in 1:N.designs){
  for (j in 1:length(by.design[[i]]$studyid)){
    for (k in 1:length(treat.list)){
     if(by.design[[i]]$treatment[j]==treat.list[k]){by.design[[i]]$tr.id2[j]=k} 
    }
      }
}

list1=matrix(rep(0,Dim.y*2), nrow=Dim.y)

N.j=c(rep(0,N.designs))
if(N.designs>1){for (i in 2:N.designs){N.j[i]=N.j[i-1]+NT.d[i-1]-1}}

for (i in 1:N.designs){
  for (j in 1:(NT.d[i]-1)){
 list1[N.j[i]+j,1]=by.design[[i]]$tr.id2[[1]]

 list1[N.j[i]+j,2]=by.design[[i]]$tr.id2[[j+1]]

  }      }

basic.contrasts=c(2:N.treat)
for (i in 1:Dim.y){
  for (k in 1:(N.treat-1)){
  if (list1[i,1]==basic.contrasts[k]){X[i,k]=-1}
  
    if (list1[i,2]==basic.contrasts[k]){X[i,k]=1}
}}

### estimate NMA 
W=solve(V)
d.basic <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y  # basic parameters
d.hat=H%*%d.basic  # all estimates in the network
cov.d.hat=H%*%solve(t(X) %*% W %*% X)%*%t(H)

### league table
league.table=matrix(rep(0,N.treat*N.treat),nrow=N.treat)
league.table[lower.tri(league.table, diag=FALSE)]=exp(d.hat) 
league.table=t(league.table)
league.table[lower.tri(league.table, diag=FALSE)]=t(league.table)[lower.tri(league.table)] 
league.table[lower.tri(league.table, diag=FALSE)]=1/league.table[lower.tri(league.table, diag=FALSE)]
league.table=round(league.table,2)
diag(league.table)=sort(treat.list)

Lower95CI=matrix(rep(0,N.treat*N.treat),nrow=N.treat)
Lower95CI[lower.tri(Lower95CI, diag=FALSE)]=exp(d.hat-1.96*sqrt(diag(cov.d.hat)))
Lower95CI=t(Lower95CI)
Lower95CI[lower.tri(Lower95CI, diag=FALSE)]=t(Lower95CI)[lower.tri(Lower95CI)] 
Lower95CI[lower.tri(Lower95CI, diag=FALSE)]=1/Lower95CI[lower.tri(Lower95CI, diag=FALSE)]
Lower95CI=round(Lower95CI,2)

Upper95CI=matrix(rep(0,N.treat*N.treat),nrow=N.treat)
Upper95CI[lower.tri(Upper95CI, diag=FALSE)]=exp(d.hat+1.96*sqrt(diag(cov.d.hat)))
Upper95CI=t(Upper95CI)
Upper95CI[lower.tri(Upper95CI, diag=FALSE)]=t(Upper95CI)[lower.tri(Upper95CI)] 
Upper95CI[lower.tri(Upper95CI, diag=FALSE)]=1/Upper95CI[lower.tri(Upper95CI, diag=FALSE)]
Upper95CI=round(Upper95CI,2)

league.table1=matrix(paste(league.table,"[",Lower95CI,";",Upper95CI,"]", sep=""),ncol=ncol(league.table))
league.table2=matrix(paste(league.table,"[",Upper95CI,";",Lower95CI,"]", sep=""),ncol=ncol(league.table))
league.table3=matrix(rep(0,N.treat*N.treat),nrow=N.treat)
league.table3[upper.tri(league.table3, diag=FALSE)]=league.table1[upper.tri(league.table1)]
league.table3[lower.tri(league.table3, diag=FALSE)]=league.table2[lower.tri(league.table2)]
diag(league.table3)=treat.list
league.table=data.frame(league.table3)
colnames(league.table)=NULL


#inconsistency global
Q.net=t(y-X%*%d.basic)%*%solve(V)%*%(y-X%*%d.basic)
df.net=sum(NT.d-1)-N.treat+1
pvalue.Q.net=1-pchisq(Q.net,df=df.net)
Q.inconsistency=matrix(rep(0,3),ncol=3)
Q.inconsistency[,1]=round(Q.net,2)
Q.inconsistency[,2]=df.net
Q.inconsistency[,3]=round(pvalue.Q.net,3)
Q.inconsistency=data.frame(Q.inconsistency, row.names = "")
colnames(Q.inconsistency)=c("Q","df","p-value")



#inconsistency local per design
X1=list()
counter=0
for (i in 1:N.designs){
  counter1=0
    X1[[i]]=X
  for (j in 1:(NT.d[[i]]-1)){
   r1=c(rep(0,counter+counter1))
   r2=c(rep(0,(nrow(X)-counter-counter1-1)))
     X1[[i]]=cbind(X1[[i]],c(r1,c(1),r2))
     counter1=counter1+1
      }
 counter=counter+NT.d[i] -1 
}
d.basic1=list()
R1=list()
Q.inc.split=c()
df.detach=c()
detach.des=c()
for (i in 1:N.designs)
{
  if (abs(det(t(X1[[i]]) %*% W %*% X1[[i]]))>10e-8){
    d.basic1[[i]]=solve(t(X1[[i]]) %*% W %*% X1[[i]]) %*% t(X1[[i]]) %*% W %*% y
    R1[[i]]=y-X1[[i]]%*%d.basic1[[i]]
    Q.inc.split[i]=t(R1[[i]])%*%W%*%(R1[[i]])
    }
  if (abs(det(t(X1[[i]]) %*% W %*% X1[[i]]))<10e-8){Q.inc.split[i]=c(0) }
  df.detach[i]=max(0,df.net-NT.d[i]+1)
  detach.des[i]=by.design[[i]]$Design[1]
  
}

Q.detach=matrix(rep(0,length(Q.inc.split)*4),ncol=4)
Q.detach[,1]=detach.des
Q.detach[,2]=round(Q.inc.split,2)
Q.detach[,3]=df.detach
for(i in 1:length(Q.inc.split)){if(Q.inc.split[i]<1e-8){Q.detach[i,4]="--"} }
for(i in 1:length(Q.inc.split)){if(Q.inc.split[i]>1e-8&(1-pchisq(Q.inc.split[i],df=df.detach[i])<0.001)){Q.detach[i,4]="< 0.001"} }
for(i in 1:length(Q.inc.split)){if(Q.inc.split[i]>1e-8&(1-pchisq(Q.inc.split[i],df=df.detach[i])>0.001)){Q.detach[i,4]=round(1-pchisq(Q.inc.split[i],df=df.detach[i]),3)} }
Q.detach=data.frame(Q.detach)
colnames(Q.detach)=c("design","Q","df","p-value")




### return list
result.list=list("N.studies"=N.studies,"N.designs"=N.designs, "Design.list"=design.list,"league.table"=league.table, "Q.net"=Q.net,
                "df.net"=df.net,"pvalue.Q.net"=pvalue.Q.net, "Q.inconsistency"=Q.inconsistency, "Q.detach"=Q.detach,"y"=y,"V"=V,"H"=H,"X"=X,
                "d.hat"=d.hat,"cov.d.hat", "netgraph"=net1 )  
return(result.list)
  
####### DESCRIPTION OF RETURN LIST
# N.studies: number of studies in the network (after possibly removing all-zero studies)
# N.designs: number of different designs in the network
# design.list: list of different designs
# league.table: table of odds-ratios and 95% CIs, for all treatment comparisons in the network. In the upper triangle, numbers
#               correspond to the odds of treatment defining the row vs treatment defining the column of each cell. The 
#               opposite holds for the lower triangle of the table. Eg. in cell (row=2, column=3) there is the odds ratio of
#               treatment 2 vs. 3, while in cell (row=3,column=2) is the odds ratio 3 vs. 2
# Q.inconsistency: summary table with the global inconsistency test
# Q.net: Overall inconsistency statistic of the network
# df.net: degrees of freedom for the test of overall inconsistency
# pvalue.Q.net: p-value for the test of overall inconsistency
# Q.detach: table with results from tests for local inconsistency (per design)
# y: vector of design-specific log-odd ratios 
# V: matrix of variance-covariance matrix of y
# H: a matrix with elements 1, 0 and -1, H maps the basic parameters into all possible treatment comparisons in the network
# X: a matrix which describes the treatments  being compared in each design and maps the corresponding comparisons into the basic parameters
# d.hat: a vector of NMA estimates of all treatment comparisons (log-odds ratios)
# cov.d.hat: variance-covariance matrix of d.hat
# netgraph: an object of the class netmeta. This should be used only for plotting the network.
##function's end
} #### remove netgraph!
######################################################################################################



 nma1=MHnma(dataMH,studyid = studyid,treatment=treatment,events = events,total = total) 
 nma1$league.table

 nma1$Q.inconsistency
 nma1$Q.detach
 netgraph(nma1$netgraph, plastic=F, thickness="number.of.studies", multiarm = F, points=T, cex.points=5, col=1)  


 
 
 #### example from stefan
 setwd("C:/Users/efthimiou/Desktop/post doc/PROJECT/APPLIED PROJECTS/mortality schizo/data NEW2")
 aa= read.xlsx2("dataA1.xlsx", sheetIndex = 1, stringsAsFactors=FALSE, colClasses=NA)
dataMH=data.frame(aa$Study_name,aa$Drug_name_SAE,round(aa$N_arm_total_stapf),round(aa$TotalNrDeathOverall),stringsAsFactors = F)
colnames(dataMH)=c("studyid","treatment","total","events")
 
 nma2=MHnma(aa,studyid = Study_name,treat=Drug_name_SAE,events = TotalNrDeathOverall,total = N_arm_total_stapf) 
 nma2$league.table
