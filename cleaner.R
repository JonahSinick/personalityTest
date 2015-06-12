source("~/Desktop/personalityTest/aux.R")

#Dataset her: http://personality-testing.info/_rawdata/duckworth-grit-scale-data.zip
df = read.csv("~/Desktop/personalityTest/duckworth.csv")

#Remove extraneous variables
n = names(df)
drops = n[grep("operatingsystem|browser|screen|elapse|X",n)]
df = drop(df, drops)



#Turn factor into numeric
df$O10 = as.numeric(as.character(factor(df$O10, levels = 1:5)))

#Remove missing values
n = names(df)
vcls = n[grep("VCL",n)]
notVCLs = n[!(n %in% c("country", vcls))]
df[notVCLs] = as.data.frame(sapply(df[notVCLs], 
                                   function(x) {ifelse(x == 0 |is.na(x), sample(x[x != 0], 1), x)}
                                   )
                            )

#Add factor names

addNames = function(df, name, h){
  arr = h[[name]]
  df[[name]] = factor(df[[name]], levels = seq_len(length(arr)))  
  df[[name]] = sapply(df[[name]],function(i){arr[i]})
  nonNAs = df[[name]][!is.na(df[[name]])]
  df[[name]] = factor(ifelse(is.na(df[[name]]), sample(nonNAs, 1), df[[name]]))
  return(df)
}

h = hash()
h[["religion"]] = c("Agnostic", "Atheist", "Buddhist", "Catholic", 
              "Mormon", "Protestant", "oChristian", "Hindu", 
              "Jewish", "Muslim", "Sikh", "Other")
h[["urban"]] = c("Rural", "Suburban", "Urban")
h[["gender"]] = c("Male", "Female", "Other")
h[["engnat"]] = c("English", "Other")
h[["orientation"]] = c("Hetero", "Bi", "Homo", "A", "Other")
h[["voted"]] = c("Yes", "No")
h[["married"]] = c("Never", "Married", "Divorced")
h[["hand"]] = c("Left", "Right", "Ambi")

for(key in keys(h)){ df = addNames(df, key, h)}

#Remove outliers
df$age = ifelse(df$age %in% 13:78, df$age, median(df$age))
df$familysize = ifelse(df$familysize < 10, df$familysize, 10)

#Add fake vocabulary variable
p = prcomp(df[vcls[c(6,9,12)]])
df$badVoc = p$x[,1]

#Add real vocabulary variable
p = prcomp(df[vcls[-c(6,9,12)]])
df$voc = -p$x[,1]



write.csv(df , "~/Desktop/personalityTest/cleaned.csv")
