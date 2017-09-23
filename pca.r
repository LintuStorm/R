get_pca2 <- function(data){
model <- prcomp(data)    
summary<-summary(model)
vars <- apply(model$x, 2, var)  
props <- vars / sum(vars)
sum<-0 
for (i in 1:length(props)){ 
sum<-sum+props[i] 
if (sum>=0.9){ 
break 
} 
}
selected<-summary$x[,c(1:i)]
rownames(selected)<-NULL
new<-cbind(data,selected)
return(new)
}