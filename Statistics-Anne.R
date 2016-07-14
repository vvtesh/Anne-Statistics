titles <- read.csv("c:/temp/workdir/titles-java-all.txt")
samples <- sample(titles$titles, 2336, replace=FALSE, prob=NULL)
write.table(samples, "c:/temp/workdir/titles-java-10percent.txt")

titles <- read.csv("c:/temp/workdir/titles-cpp-all.txt")
samples <- sample(titles$titles, 1653, replace=FALSE, prob=NULL)
write.table(samples, "c:/temp/workdir/titles-cpp-10percent.txt")

Anne.Full <- read.csv("C:/data/svn/iiitdsvn/entity/src/Anne-Full-Update1.csv")
head(Anne.Full)
%hist(Anne.Full[which(Type=='Tagged'),"Time"])
attach(Anne.Full)
fit = aov(P ~ Type , data = Anne.Full[which(Task=='Enum'),])
fit = aov(P ~ Type , data = Anne.Full)
fit = aov(R ~ Type , data = Anne.Full)
summary(fit)

%***************
Anne.Full <- read.csv("C:/data/svn/iiitdsvn/entity/src/Completeness-R.csv")



%Get Mean, Median, SD.
mean(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
mean(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
mean(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
mean(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])


median(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
median(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
median(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
median(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])

sd(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
sd(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
sd(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
sd(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])

%***********************

attach(Anne.Full)

mean(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Time"])
mean(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"Time"])
mean(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"Time"])
mean(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"Time"])


median(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Time"])
median(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"Time"])
median(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"Time"])
median(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"Time"])

sd(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Time"])
sd(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"Time"])
sd(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"Time"])
sd(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"Time"])

%------------------
mean(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
mean(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
mean(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
mean(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])


median(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
median(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
median(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
median(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])

sd(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"R"])
sd(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"R"])
sd(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"R"])
sd(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"R"])

sd(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"P"])
sd(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"P"])
%------------------

fit = aov(Time ~ Type + Task, data = Anne.Full)
summary(fit)

fit = aov(R ~ Type + Task, data = Anne.Full)
summary(fit)

shapiro.test(R)
shapiro.test(Anne.Full[,"Time"])
?shapiro.test

shapiro.test(Anne.Full[which(Type=='Untagged'),"Time"])
shapiro.test(Anne.Full[which(Type=='Tagged'),"Time"])
shapiro.test(Anne.Full[which(Type=='Untagged'),"R"])
shapiro.test(Anne.Full[which(Type=='Tagged'),"R"])

shapiro.test(Anne.Full[which(Type=='Untagged' & Time != 1200),"Ntime"])
shapiro.test(Anne.Full[which(Time != 1200),"Time"])

fit <- aov(Anne.Full[which(Type=='Tagged'), "Ntime2"] ~ Anne.Full[which(Type=='Untagged'),"Ntime2"])
summary(fit)
?aov
fit = aov(R ~ Type + Task, data = Anne.Full)
fit = aov(Time ~ Type + Task, data = Anne.Full)
summary(fit)
cohen.d(Time~Type)
hist(Anne.Full[,"Ntime"])
plot(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Ntime"])
hist(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Ntime"])

# Kernel density estimation is a non-parametric method of estimating the 
# probability density function (PDF) of a continuous random variable. 
plot(density(Anne.Full[,"Time"]))
plot(density(Anne.Full[which(Type=='Tagged'),"Time"]))
plot(density(Anne.Full[which(Type=='Untagged'),"Time"]))

plot(density(Anne.Full[which(Type=='Tagged' & Task=='Enum'),"Ntime"]))
plot(density(Anne.Full[which(Type=='Tagged' & Task=='IncDec'),"Ntime"]))
plot(density(Anne.Full[which(Type=='Untagged' & Task=='Enum'),"Ntime"]))
plot(density(Anne.Full[which(Type=='Untagged' & Task=='IncDec'),"Ntime"]))
plot(density(Anne.Full[which(Type=='Untagged' & Time != 1200),"Ntime"]))

hist(Anne.Full[which(Type=='Tagged'),"Time"])
hist(Anne.Full[,"Time"])

# Plot for Enum only and IncDec only
boxplot(Time~Type,data=Anne.Full[which(Task=='Enum'),],las=2)
boxplot(Time~Type,data=Anne.Full[which(Task=='IncDec'),],las=2)
axis(2, seq(100,2000, 50), seq(100, 2000, 50), las=2)

mean(Anne.Full[which(Type=='Tagged'),"Time"])
mean(Anne.Full[which(Type=='Untagged'),"Time"])

median(Anne.Full[which(Type=='Tagged'),"Time"])
median(Anne.Full[which(Type=='Untagged'),"Time"])

869

boxplot(Anne.Full[which(Time != 1200),"Ntime"]~Type, data=Anne.Full)
boxplot(Time~Type)
boxplot(Ntime~Task,data=Anne.Full[which(Type=='Tagged'), ])

interaction.plot(Type, Task, Time)

par(mfrow=c(1,4))
boxplot(Time/60~Type, ylab ="Time (in minutes)", xlab ="Type")
boxplot(Time/60~Task, ylab ="Time (in minutes)", xlab ="Task")
boxplot(R~Type, ylab ="Completeness (in %)", xlab ="Type")
boxplot(R~Task, ylab ="Completeness (in %)", xlab ="Task")



?boxplot


boxplot(Ntime~Task,data=Anne.Full[which(Type=='Untagged'), ])

  ?wilcox.test
t.test(Ntime~Type,data=Anne.Full[which(Task=='Enum'), ])

?t.test
#t.test(Anne.Full[which(Type=='Tagged'), "Ntime"] ~ Anne.Full[which(Type=='Untagged' & Time != 1200),"Ntime"])

wilcox.test(Time~Type,data=Anne.Full)
wilcox.test(Time~Task,data=Anne.Full)
wilcox.test(R~Type,data=Anne.Full)
wilcox.test(Time~Type,data=Anne.Full[which(Task=='Enum'),])


detach(Anne.Full)


t.test(Time~Task,data=Anne.Full[which(Type=='Tagged'), ])
t.test(Time~Task,data=Anne.Full[which(Type=='Untagged'), ])
t.test(R~Type,data=Anne.Full[which(Task=='IncDec'), ])


require(effsize)
cohen.d(R~Type)
cohen.d(Type~Time)
cohen.d(R~Type,data=Anne.Full[which(Task=='IncDec'), ])
cohen.d(Time~Type,data=Anne.Full[which(Task=='IncDec'), ])



full <- read.csv("C:/temp/workdir/EntityEstimates.csv")
x=1:312
full[head(sample(x),20),"term"]

