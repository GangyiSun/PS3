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
  print(paste("You chose door ", playerDoor, ""))
  print(paste("The car was hidden behind door ", carDoor, ""))
  if (playerDoor==carDoor){
    print("Congratulations! You chose the correct door!")
  } else {
    print ("I'm sorry, you didn't choose the correct door :(")
  }
}

# To test if S3 methods work
door1S3<-createDoor(3)
door2S3<-createDoor(4)
PlayGame(door1S3)



# Part 2: using S4 class system. 
# Fuctions to define the S4 class door and the create S4 door objects. 
setClass(Class="door",
         representation=representation(DoorChoice="integer"), 
         prototype=prototype(DoorChoice=c())
         )
setMethod("initialize", "door",  function(.Object, ...){
  value = callNextMethod()
  return(value)
  }
  )

# Validation function 
setValidity("door", function(object){
  test<-is.integer(object@DoorChoice)
  if(test!=TRUE){return("@DoorChoice is not a valid value")}
  }
  )

# PlayGame method
setGeneric("PlayGame", function(object="door"){
             standardGeneric("PlayGame")
           }
           )
setMethod("PlayGame", "door",
          function(object){
            playerDoor<-object@DoorChoice
            carDoor<-sample(1:3,size=1)
            print(paste("You chose door ", playerDoor, ""))
            print(paste("The car was hidden behind door ", carDoor, ""))
            if (playerDoor==carDoor){
              print("Congratulations! You chose the correct door!")
            } else {
              print ("I'm sorry, you didn't choose the correct door :(")
            }
          }
          )

# To test if S4 methods work
door1S4<-new("door", DoorChoice=as.integer(2))  # explain why as.integer is needed
PlayGame(door1S4)












