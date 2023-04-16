library(rjags)
police <- read.csv("ca_san_francisco_2020_04_01-cleaned_dataset.csv", header=TRUE)
police_sub <- subset(police, district != 'N/A')
police_sub$incidents <- police_sub$arrest_made | police_sub$citation_issued
df <- data.frame(matrix(ncol = 4, nrow = 0))
names(df) <- c("race", "sex", "incidents", "stops")
for (race in unique(police_sub$subject_race)) {
  rows = police_sub[(police_sub$subject_race == race), ]
  stops = nrow(as.matrix(rows))
 
  rows = police_sub[(police_sub$subject_race == race &
                       police_sub$incidents == TRUE), ]
  incidents = nrow(as.matrix(rows))
 
  row = data.frame(race = race,
                   incidents = incidents,
                   stops = stops)
  df = rbind(df, row)
}
d1 <- list(stops=df$stops, incidents=df$incidents,
           race=unclass(factor(df$race)))
inits1 <- list(list(betarace=c(10,10,10,10,10)),
               list(betarace=c(10,-10,10,-10,10)),
               list(betarace=c(-10,10,-10,10,-10)),
               list(betarace=c(-10,-10,-10,-10,-10)))
m1<-jags.model("model.bug",d1,inits1,n.chains=4,n.adapt=1000)
update(m1, 1000)
x0 <- coda.samples(m1, c("betarace"), n.iter=8000, thin=4)
print(gelman.diag(x0, autoburnin = FALSE))
plot(x0)
print("======================")
print(mean(as.matrix(x0[,"betarace[2]"]) > as.matrix(x0[,"betarace[1]"])))
print("======================")
x1 <- coda.samples(m1, c("prob", "incidents"), n.iter=8000)
probs <- as.matrix(x1)[, paste("prob[", 1:nrow(df),"]", sep="")]
incidents <- as.matrix(x1)[, paste("incidents[", 1:nrow(df),"]", sep="")]
Tchi <- numeric(nrow(incidents))
Tchirep <- numeric(nrow(incidents))
for (s in 1:nrow(incidents)) {
  Tchi[s] <- sum((df$incidents     - df$stops*probs[s,])^2/(df$stops*probs[s,]*(1-probs[s,])))
  Tchirep[s] <- sum((incidents[s,] - df$stops*probs[s,])^2/(df$stops*probs[s,]*(1-probs[s,])))
}
print(mean(Tchirep))
print(mean(Tchi))
