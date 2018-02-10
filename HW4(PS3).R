# Applied Statistical Programing 
# Homework 4 - Problem Set 3
# Gangyi Sun (441748)


# Part 1: using S3 class system. 
# Function that creates objects of class door
createDoor<-function(x){
  if (x==1|x==2|x==3){
    class(x)<-"door"
    return(x)
  } else {
    print("invalid door")
    return(NA)
  }
}

# Create method PlayGame
PlayGame<-function(x){
  UseMethod('PlayGame',x)     
}
PlayGame.door<-function(x){
  playerDoor<-as.numeric(x)
  carDoor<-sample(1:3,size=1)
  print(paste("The car was hidden behind door ", carDoor, ""))
  if (playerDoor==carDoor){
    print("Congratulations! You choose the correct door!")
  } else {
    print ("I'm sorry, you didn't choose the correct door :(")
  }
}

# To test if S3 methods work
door1<-createDoor(3)
door2<-createDoor(4)
PlayGame(door1)












