data2 <- read.csv('/Users/xudawei/Documents/video/assiette.csv', header = T,sep= ';')  
data2
std_data=scale(data2[2:8])
rownames(std_data)=data2[[1]]
class(std_data)
df=as.data.frame(std_data)
df
df.pr=princomp(df,cor=TRUE)
summary(df.pr,loadings=TRUE)
cor(df)
y=eigen(cor(df))
y$values
sum(y$values[1:4])/sum(y$values)
df.pr$loadings[,1:4]
screeplot(df.pr,type='lines')
biplot(df.pr)
s=df.pr$scores[,1:4]
scores=0.0
for (i in 1:4)
scores=(y$values[i]*s[,i])/(sum(y$values[1:4]))+scores
cbind(s,scores)



