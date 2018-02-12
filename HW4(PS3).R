# Applied Statistical Programing 
# Homework 4 - Problem Set 3
# Gangyi Sun (441748)


# Part 1: using S3 class system. 
# Function that creates objects of class door
createDoor<-function(x){
  if (x==1|x==2|x==3){    # ensures that numeric value is 1, 2 or 3
    class(x)<-"door"      # defines object as class 'door'
    return(x)
  } else {
    print("invalid door")
    return(NA)
  }
}

# Create method PlayGame
PlayGame<-function(x){
  UseMethod('PlayGame',x)       # creates generic PlayGame
}
PlayGame.door<-function(x){     # creates method for class door 
  playerDoor<-as.numeric(x)     # obtains numeric vale stored in 'door' object 
  carDoor<-sample(1:3,size=1)   # randomly chooses door behind which the car is hidden 
  print(paste("You chose door ", playerDoor, ""))
  print(paste("The car was hidden behind door ", carDoor, ""))
  if (playerDoor==carDoor){     # player chose correct door, congratulate her
    print("Congratulations! You chose the correct door!")
  } else {                      # player chose wrong door, express sympathy 
    print ("I'm sorry, you didn't choose the correct door :(")
  }
}

# To test if S3 methods work
door1S3<-createDoor(3)
door2S3<-createDoor(4)
PlayGame(door1S3)



# Part 2: using S4 class system. 
# Fuctions to define the S4 class door and the create S4 door objects. 
# Begin by defining the class 'door'. Objects of class door contain an integer called DoorChoice. 
setClass(Class="door",
         representation=representation(DoorChoice="integer"), 
         prototype=prototype(DoorChoice=c())
         )
# Then, write the construction function that allows the user to create a door object 
setMethod("initialize", "door",  function(.Object, ...){
  value = callNextMethod()
  return(value)
  }
  )

# Validation function that checks whether the value stored in door is an integer 
setValidity("door", function(object){
  test<-is.integer(object@DoorChoice)
  if(test!=TRUE){return("@DoorChoice is not a valid value")}
  }
  )

# PlayGame method
# creates generic PlayGame
setGeneric("PlayGame", function(object="door"){   
             standardGeneric("PlayGame")
           }
           )
# creates method for class door 
setMethod("PlayGame", "door",
          function(object){
            playerDoor<-object@DoorChoice     # obtains integer vale stored in 'door' object 
            carDoor<-sample(1:3,size=1)       # randomly chooses door behind which the car is hidden 
            print(paste("You chose door ", playerDoor, ""))
            print(paste("The car was hidden behind door ", carDoor, ""))
            if (playerDoor==carDoor){         # player chose correct door, congratulate her
              print("Congratulations! You chose the correct door!")
            } else {                          # player chose wrong door, express sympathy 
              print ("I'm sorry, you didn't choose the correct door :(")
            }
          }
          )

# To test if S4 methods work
door1S4<-new("door", DoorChoice=as.integer(2))  # By default, R takes a number (such as the value 2 in 
                                                # this case) as a numeric. Thus, because the construction
                                                # function of a S4 object door requires an integer as its
                                                # content, as.integer() is needed to cast an entered numeric
                                                # value (2 in this case) as a integer. 
PlayGame(door1S4)












