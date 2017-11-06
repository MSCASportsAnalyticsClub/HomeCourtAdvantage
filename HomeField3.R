# Read in game logs

df <- read.csv("C:/Users/JohntheGreat/Documents/MSCA/SportsClub/team_game_logs.csv", header=TRUE, sep=",")
head(df)
str(df)
colnames(df)
#Create empty dataframe
dat <- data.frame(Year=numeric(), Team=character(), W=numeric(), L=numeric(),
                  homeW=numeric(), homeL=numeric(),roadW=numeric(),roadL=numeric(),
                  homeFGP=numeric(), roadFGP=numeric(), OppHFGP=numeric(), OppRFGP=numeric(),
                  home3FGP=numeric(), road3FGP=numeric(), OppH3FGP=numeric(), OppR3FGP=numeric(),
                  homeFTP=numeric(), roadFTP=numeric(), OppHFTP=numeric(), OppRFTP=numeric(),
                  homePF=numeric(), roadPF=numeric(), OppHPF=numeric(), OppRPF=numeric(), 
                  stringsAsFactors = FALSE)

for (year in 1997:2016) {
  # Subset team = LAL and year = 1997 for entire df of game logs
  LAL.year <- subset(df, df$Team == "LAL" & df$Season == year)
  # Check LAL.year
  #print(tail(LAL.year))
  
  # Remove rows with no data
  row.to.keep <- LAL.year$G>0
  #print(row.to.keep)
  LAL.year <- LAL.year[row.to.keep, ]
  # Check the number of rows equals 82 games
  #print(nrow(LAL.year))
  
  # Store Year and team
  dat[year,1] <-  year
  dat[year,2] =  "LAL"
  
  # print the season Win loss record
  dat[year,3] = sum(LAL.year$W.L == 'W')
  dat[year,4] = sum(LAL.year$W.L == 'L')
  
  #separate home and road games
  LAL.year.road <- LAL.year[LAL.year$Â.== '@', ]
  LAL.year.home <- LAL.year[LAL.year$Â. != '@',]
  
  # Sum home wins and losses
  dat[year,5] = sum(LAL.year.home$W.L == 'W')
  dat[year,6] = sum(LAL.year.home$W.L == 'L')
  
  # Sum road wins and losses
  dat[year,7] = sum(LAL.year.road$W.L == 'W')
  dat[year,8] = sum(LAL.year.road$W.L == 'L')
  
  # Average Field Goal Percentage
  dat[year,9] = mean(LAL.year.home$FG_Perc, trim=2)
  dat[year,10] = mean(LAL.year.road$FG_Perc, trim=2)
  
  # Average Opponent Field Goal Percentage
  dat[year,11] = mean(LAL.year.home$FG_Perc.1, trim=2)
  dat[year,12] = mean(LAL.year.road$FG_Perc.1, trim=2)
  
  # Average 3 point Field Goal Percentage
  dat[year,13] = mean(LAL.year.home$X3P_Perc, trim=2)
  dat[year,14] = mean(LAL.year.road$X3P_Perc, trim=2)
  
  # Average Opponent 3 point Field Goal Percentage
  dat[year,15] = mean(LAL.year.home$X3P_Perc.1, trim=2)
  dat[year,16] = mean(LAL.year.road$X3P_Perc.1, trim=2)
  
  # Average Free throw Percentage
  dat[year,17] = mean(LAL.year.home$FT_Perc, trim=2)
  dat[year,18] = mean(LAL.year.road$FT_Perc, trim=2)
  
  # Average Opponent Free throw Percentage
  dat[year,19] = mean(LAL.year.home$FT_Perc.1, trim=2)
  dat[year,20] = mean(LAL.year.road$FT_Perc.1, trim=2)
  
  # Average Personal fouls
  dat[year,21] = mean(LAL.year.home$PF, trim=2)
  dat[year,22] = mean(LAL.year.road$PF, trim=2)
  
  #Average Opponent Personal fouls
  dat[year,23] = mean(LAL.year.home$PF.1, trim=2)
  dat[year,24] = mean(LAL.year.road$PF.1, trim=2)
}


# Read in game logs

df <- read.csv("C:/Users/JohntheGreat/Documents/MSCA/SportsClub/team_game_logs.csv", header=TRUE, sep=",")
dim(df)

# Remove rows with 0's
df.removezero <- df[df$Date!=0, ]
dim(df.removezero)

# Sort data by date
df_date <- df.removezero[order(df.removezero$Date), ]
head(df_date)

# Create a temporary vector to store points scored per team, 
# initialize first value
temp_vec = c(96)

# Create a dataframe to hold the date and total for the game
# total is the double the average of the points for all games that day
totals <- as.data.frame(matrix(0, nrow=1, ncol=2))  

# for loop starts in row 2, compares previous date value
# stores into temp_vec, then when the date changes, it prints the 
# average total for that day, and clears out temp_vec
for (i in 2:50){
  if (df_date$Date[i] == df_date$Date[i-1]){
      temp_vec <- c(temp_vec, df_date$Tm[i])
      print(temp_vec)
  }  else {
      print(2 * mean(temp_vec))
      #rbind(totals, c(df_date$Date[i-1], (2 * mean(temp_vec))))
      temp_vec = c()
      temp_vec <- c(temp_vec, df_date$Tm[i])
  }
}

