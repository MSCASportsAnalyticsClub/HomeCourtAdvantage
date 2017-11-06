homecourt <- read.csv("C:/Users/brian/Downloads/Sports Analytics/game_log_1987_2017_clean.csv")
M <- matrix(data = NA, nrow = length(1987:2017), ncol = 5)
M[,1] <- 1987:2017
x <- 0
games <- 0
for (j in 1987:2017){
  for (i in 1:dim(homecourt)[1]){
    if (homecourt[i,2] == j){
      if(homecourt[i,1] == "RegularSeason"){
        if(homecourt[i,6] == 1){
          if(homecourt[i,8] == "W"){
            x <- x + 1
          }
        games <- games + 1
        }
      }  
    }
  }
M[(j-1986),2] <- x
M[(j-1986),3] <- games
M[(j-1986),4] <- x/games
M[(j-1986),5] <- binom.test(x,games,p = .5)$p.value
x <- 0
games <- 0
}
plot(x = M[,1], y = M[,4], type = "l")


colnames(homecourt)

means <- c()
for(i in 1987:2017){
  means[i-1986] <- mean(subset(homecourt, homecourt$Season == i)$Tm.PF)
}
