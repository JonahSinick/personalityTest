source("~/Desktop/personalityTest/aux.R")
df = read.csv("~/Desktop/personalityTest/cleaned.csv")
df = df[-2:0]
n = names(df)

nums = n[sapply(df, function(x) {is.numeric(x)})]
cats = n[!(n %in% nums)]
vs = nums[grep("VCL", nums)]
gs = nums[grep("GS", nums)]
es = nums[grep("E", nums)][1:10]
ns = nums[grep("N", nums)][1:10]
as = nums[grep("A", nums)][1:10]
cs = nums[grep("^C", nums)][1:10]
os = nums[grep("O", nums)][1:10]
p = prcomp(df[c(gs, es, as, cs, os, ns)])

arr = c(gs, es, as, cs, os, ns)
slice = df[df$married %in% c("Married", "Divorced"),]
slice$married = factor(slice$married, levels= c("Married", "Divorced"))
rf = randomForest(married ~ . , slice)
df$atheist = factor(rowSums(dummy(df$religion, levels = c("Atheist", "Agnostic"))))
df[c("Rural", "Urban")] = as.data.frame(dummy(df$urban, c("Rural", "Urban")))
df[c("badVoc", "voc")] = scale(df[c("badVoc", "voc")])
m0 = glm(atheist ~ 1, df[c("atheist","badVoc","voc","gender","age","Rural","Urban", arr)], family= "binomial")
m = glm(df[c("atheist","badVoc","voc", "gender","age","Rural", "Urban", arr)], family= "binomial")

s = step(m0, formula(m), direction = "both", step = 6)
summary(s)
auc(df$atheist,fitted(s))



summary(m)
m = glm(df[c("atheist","badVoc","voc", "gender","age", arr)], family= "binomial")
summary(s)
names(df)

table(slice$married)/nrow(slice)
rf
df[arr]
lm(E2 ~ age,df)
cor2(df[arr], df["age"])
summary(lm(df[c("age",arr)]))
cor2(df[nums],p$x[,1:5])
agged = aggregate(df[c("age", vs, gs, es,as,cs,os)], df["age"], FUN = mean)
agged = agged[agged$age < 60,]
cor2(agged, agged["age"])
p = prcomp(agged[names(agged) != "age"])
cor2(agged["age"],p$x[,1:5])
summary(p)
p$rotation[,1:2]
ggplot(agged) + geom_point(aes(age, GS5))
summary(lm(df[c("O", cs, ns, as, es, gs)]))
m0 = lm(E ~ 1 , df[c("E", cs, ns, as, os, gs)])
m = lm(E ~ . , df[c("E", cs, ns, as, os, gs)])
s = step(m0, formula(m), direction = "both")
summary(s)
cor2(df[ns])
cor2(df[c("A10", "A2", "N10", "O10", "A3", "A7", "O9", "N3", "O5", "N8", "GS10", "C3")])
p = prcomp(df[c("A10", "A2", "N10", "O10", "A3", "A7")])
cdf = cbind(df["E"], as.data.frame(scale(p$x)))
cor2(df[nums],cdf)
summary(lm(cdf[c(1,2,4)]))
df[c("E", "O", "C", "N", "A", "G")] = scale(df[c("E", "O", "C", "N", "A", "G")])
summary(lm(O ~ .,df[c("E", "O", "C", "N", "A", "G")]))
slice = df[nums[1:85]]
slice$race = factor(slice$race)
slice$education = factor(slice$education)
rf = randomForest(df[n[29:40]])
s = slice[c("married","age","urban","engnat","hand", "religion", "orientation", "voted", "familysize",gs, os, as, ns, es, cs)]
rf = randomForest(s)
qplot(df) + geom_smooth(aes(age, E1))
cor2(df[nums], df["age"])
agged = aggregate(df, df["age"], FUN = mean)
library(ggplot2)
