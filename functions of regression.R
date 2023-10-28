# opreation for multi modle 
singler<<-function(m){
  class(try(solve(m),silent = T))=="matrix"
}

Multiple_Fitted<-function(x,y){
  k<<-ncol(x)-1
  w<<-k+1
  N<<-nrow(x)
  x_t_y<<-t(x)%*%y
  y_bar<<-x_t_y[1,1]/N
  test_sing<-t(x)%*%x
  if(singler(test_sing)){
    B<<-solve(t(x)%*%x)%*%t(x)%*%y
    sst<<-t(y)%*%y-N*y_bar^2
    ssr<<-t(B)%*%t(x)%*%y-N*y_bar^2 
    sse<<-sst-ssr
    msr<<-ssr/k
    mse<<-sse/(N-w)
    mse_root=sqrt(mse)
    mse_root=as.double(mse_root)
    y_hat_forall<<-x%*%B
    ei<<- y-y_hat_forall
    di<<-ei/mse_root
    
    #y_hat<<-x%*%B
    #z=(y_hat-y)/(as.double(sqrt(mse)))
    B_=c()
    value=c()
    for(c in 1:length(B)){
      B_[c]=paste0("B",c-1)
      value[c]=B[c]
    }
    newdf<<-data.frame(B_,value)
    newdf
  }else{
    paste0("there is feature depend on other feature")
  }
  
}
anova_m<-function(){
  
  sourc=c("Regression","Residual","Total")
  ss=c(ssr,sse,sst)
  Df=c(k,N-w,N-1)
  ms=c(msr,mse," ")
  f_value=c(" ",msr/mse," ")
  data.frame(sourc,ss,Df,ms,f_value)
}
f_test_m<-function(confiadnc_level){
  
  confiadnc_level=1-confiadnc_level
  Fc=msr/mse
  Ft= qf(p=confiadnc_level, df1=k, df2=N-w, lower.tail=FALSE)
  g=data.frame(feat)
  if(Fc>Ft){
    paste0("we concluded that  at least on  of your features  effect on predicted feature ")
    
  }else paste0("we concluded that  no features effect on predicted feature ")
}
Confidance_Interval_B<- function(confiadnc_level,x,y){
  confiadnc_level=1-confiadnc_level
  mse=as.double(mse)
  variance_covariance_M<<-solve(t(x)%*%x)
  var=diag(variance_covariance_M)
  var_matrix=as.matrix(var)
  lower_B=B-qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*sqrt(mse*var_matrix)
  upper_B=B+qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*sqrt(mse*var_matrix)
  ci <- cbind(lower_B,upper_B)
  colnames(ci)<-c("Lower","Upper")
  newdf=data.frame(newdf,ci)
  data.frame(newdf)
}
mean_response_m<-function(x,y,x0,confiadnc_level){
  confiadnc_level=1-confiadnc_level
  x0=as.matrix(c(x0))
  x0=rbind(1,x0)
  mse=as.double(mse)
  y_new=t(x0)%*%B
  y_lower=y_new-qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*(sqrt(mse*(t(x0)%*%solve(t(x)%*%x)%*%x0)))
  y_upper=y_new+qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*(sqrt(mse*(t(x0)%*%solve(t(x)%*%x)%*%x0)))
  ci <- cbind(y_lower,y_upper)
  colnames(ci)<-c("Lower","Upper")
  ci=data.frame(ci)
  ci
}
New_observation_m<-function(x,y,x0,confiadnc_level){
  confiadnc_level=1-confiadnc_level
  x0=as.matrix(c(x0))
  x0=rbind(1,x0)
  mse=as.double(mse)
  y_new=t(x0)%*%B
  y_lower=y_new-qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*(sqrt(mse*(1+(t(x0)%*%solve(t(x)%*%x)%*%x0))))
  y_upper=y_new+qt(p=confiadnc_level/2, df=N-w, lower.tail=F)*(sqrt(mse*(1+(t(x0)%*%solve(t(x)%*%x)%*%x0))))
  ci <- cbind(y_lower,y_upper)
  colnames(ci)<-c("Lower","Upper")
  ci=data.frame(ci)
  ci
}


# operation for single  modle 
Simple_Fitted<-function(x,y){
  #this function from scratch 
  n<<-length(x)
  x_bar<<-sum(x)/n
  y_bar<<-sum(y)/n
  sxx<<-sum(x^2)-(n*(x_bar)^2)
  syy<<-sum(y^2)-(n*(y_bar)^2)
  sxy<<-sum(x*y)-(n*(x_bar)*(y_bar))
  B1<<-sxy/sxx
  B1<<-round(B1, digits = 4)
  B0<<-mean(y)-B1*mean(x)
  B0<<-round(B0, digits = 4)
  dF<<-n-2
  sst<<-syy
  ssr<<-sxy*B1
  sse<<-sst-ssr
  mse<<-sse/(n-2)
  msr<<-ssr
  y_hat_forall<<-B0+B1*x
  ei<<- y-y_hat_forall
  
  paste0("B0: ", B0,"   "," B1: ",B1)
  #x0=strtoi(x0)
  #y_hat<<-B0+B1*x0
  #y_=B0+B1*x
  #erorr<<-(y_-y)/sqrt(mse)
  #if(length(x0)==n){
  #  plot(x,y)
  #  lines(x,y_hat)
  #  segments(x,y_hat,x,y)
  #}else{
  #  d = density(erorr) # returns the density data
  #  plot(d) # plots the results
  #  #plot(x,y)
  #  #abline(a=B0,b=B1)
  
  #cat("the predicted value of ",feature,"=",y_hat,"\n")
  
}
confiadnce_Meanrespons<-function(x0,confiadnc_level){
  confiadnc_level=1-confiadnc_level
  y_hat<<-B0+B1*x0
  #Simple_Fitted(x,y,x0)
  y_lower=y_hat-qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*((1/n)+((x0-x_bar)^2)/sxx)))
  y_upper=y_hat+qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*((1/n)+((x0-x_bar)^2)/sxx)))
  paste0("CI for the mean response is:  ",y_lower,"< y <",y_upper)
} # confidence interval for Mean respons
confiadnce_newobsevation<-function(x0,confiadnc_level){
  y_hat<<-B0+B1*x0
  confiadnc_level=1-confiadnc_level
  #Simple_Fitted(x,y,x0)
  #mean response
  y_lower=y_hat-qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*(1+(1/n)+((x0-x_bar)^2)/sxx)))
  y_upper=y_hat+qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*(1+(1/n)+((x0-x_bar)^2)/sxx)))
  paste0("CI for the new observation:  ",y_lower,"< y <",y_upper)
  
}# confidence interval for new observation 
confiadnce_B0<-function(confiadnc_level){
  confiadnc_level=1-confiadnc_level
  B0_lower<<-B0-qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*(1/n+x_bar^2/sxx)))
  B0_upper<<-B0+qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse*(1/n+x_bar^2/sxx)))
  paste0("CI for B0:  ",B0_lower,"< B0 <",B0_upper)
  #fitvalu_L=B0_lower+B1*x
  #y_=B0+B1*x
  #fitvalu_U=B0_upper+B1*x
  #plot(x,y)
  #lines(x,fitvalu_L,col='red')
  #lines(x,y_)
  #lines(x,fitvalu_U,col="blue")
}
confiadnce_B1<-function(confiadnc_level){
  confiadnc_level=1-confiadnc_level
  B1_lower<<-B1-qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse/sxx))
  B1_upper=B1+qt(p=confiadnc_level/2, df=dF, lower.tail=F)*(sqrt(mse/sxx))
  paste0("CI for B1:  ",B1_lower,"< B1 <",B1_upper)
  
  #fitvalu_L=B0_lower+B1_lower*x
  # y_=B0+B1*x
  # fitvalu_U=B0_upper+B1_upper*x
  #plot(x,y)
  #lines(x,fitvalu_L,col="red")
  #lines(x,y_)
  #lines(x,fitvalu_U,col="blue")
  
  
  
  
}
anova=function(){
  sourc=c("Regression","Residual","syy")
  ss=c(ssr,sse,syy)
  Df=c(1,n-2,n-1)
  ms=c(msr,mse," ")
  f_value=c(" ",msr/mse," ")
  datfr=data.frame(sourc,ss,Df,ms,f_value)
  datfr
}
f_test=function(confiadnc_level){
  confiadnc_level=1-confiadnc_level
  Fc=msr/mse
  Ft<<- qf(p=confiadnc_level, df1=1, df2=dF, lower.tail=FALSE)
  if(Fc>Ft){
    paste0("we concluded that the ",feat," effect on " ,pfeat)
    
  }else paste0("we concluded that the ",feat," don't effect on " ,pfeat)
}



feature_selection<-function(x,y,confiadnc_level){
  confiadnc_level=1-confiadnc_level
  sxx_m<<- numeric(ncol(x))
  for (i in 2:ncol(x)) {                    
      sxx_m[i] <<- sum((x[,i]^2)) - N*mean(x[,i])^2
    
  }
  sxx_m<<-as.matrix(sxx_m)
  sxx_m<<-t(sxx_m)
  B_sq=B^2
  ssr_n<<- numeric(ncol(x))
  for (i in 2:ncol(sxx_m)) {
    ssr_n[i] = sxx_m[1,i]*B_sq[i,1]
  }
  ssr_n=as.matrix(ssr_n)
  Fc<<- numeric(ncol(x))
  
  for (i in 2:nrow(ssr_n)) {
    Fc[i]=(as.double(ssr)- ssr_n[i,1])/mse
  }
  Fc=as.matrix(Fc)
  Ft= qf(p=confiadnc_level, df1=1, df2=N-w, lower.tail=FALSE)
  names=rownames(B)
  names=as.matrix(names)
  for(i in 2:nrow(Fc)){
    if(Fc[i,1]>Ft){
      print(paste("F_calculated for  ",names[i,1],"=",Fc[i,1],"And","F0= ",Ft))
      print(paste("Then ",Fc[i,1]," > ",Ft))
      print(paste("After test we conclude that feature",names[i,1], "has an effect in model"))
    }
    else {
      print(paste("After test we conclude that feature",names[i,1], "has no effect in model"))
    }
    
  }
  
}

feature_selection_m<-function(x,y,confiadnc_level){
  confiadnc_level=1-confiadnc_level
  sxx_m<<- numeric(ncol(x))
  for (i in 2:ncol(x)) {                    
    sxx_m[i] <<- sum((x[,i]^2)) - N*mean(x[,i])^2
    
  }
  sxx_m<<-as.matrix(sxx_m)
  sxx_m<<-t(sxx_m)
  B_sq=B^2
  ssr_n<<- numeric(ncol(x))
  for (i in 2:ncol(sxx_m)) {
    ssr_n[i] = sxx_m[1,i]*B_sq[i,1]
  }
  ssr_n=as.matrix(ssr_n)
  Fc<<- numeric(ncol(x))
  
  for (i in 2:nrow(ssr_n)) {
    Fc[i]=(as.double(ssr)- ssr_n[i,1])/mse
  }
  Fc=as.matrix(Fc)
  Ft= qf(p=confiadnc_level, df1=1, df2=N-w, lower.tail=FALSE)
  for(i in 2:nrow(Fc)){
    if(Fc[i,1]>Ft){
      print(paste("F_calculated for  ","Feature",i-1,"=",Fc[i,1],"And","F0= ",Ft))
      print(paste("After test we conclude that feature","Feature",i-1, "has an effect in model"))
    }
    else {
      print(paste("F_calculated for  ","Feature",i-1,"=",Fc[i,1],"And","F0= ",Ft))
      print(paste("After test we conclude that feature","Feature",i-1, "has no effect in model"))
    }
    
  }
}
