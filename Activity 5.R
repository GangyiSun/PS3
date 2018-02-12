# Applied Statistical Programing 
# Activity 5
# Gangyi Sun (441748)

# Part 1. Create function to create student. 
# The function takes in the argument 'name', which is the student's name. 
# Create an object of class 'student', which contains the name of the student and four values 
# for the attributes courage, ambition, intelligence and effort. Each attribute takes a randomly 
# choosen value between 1-100. 
createStudent<-function(name){
  courage<-sample(1:100,size=1)
  ambition<-sample(1:100,size=1)
  intelligence<-sample(1:100,size=1)
  effort<-sample(1:100,size=1)
  student<-list(name,courage,ambition,intelligence,effort)
  names(student)<-c('name','courage','ambition','intelligence','effort')
  class(student)<-'Student' 
  return(student)
}

# Part 2&3. Creates a method for the generic 'sort', add house to student's class.
# The sort method takes in arguments x, decreasing, and matrix. It sorts the student into a house based on 
# the student's 4 attributes and the matrix matrix. 
# x is an object of class student, it is the student to be sorted. The object is required by the generic sort.
# decreasing is a logical. By default it is set to FALSE. The logical is required by the generic sort. 
# matrix is the 4x4 matrix X necessitated by the question. 
sort.Student<-function(x,decreasing=FALSE,matrix){
  a<-c(x$courage,x$ambition,x$intelligence,x$effort)    # vector of student's 4 attributes
  result<-t(matrix)%*%a                                 # calculates transpose(matrix) multiplied by a
  i<-which.max(result)                                  
  responses<-c('GRYFFINDOR!','SLYTHERIN!','RAVENCLAW!','HUFFLEPUFF!')
  houses<-c('Gryffindor','Slytherin','Ravenclaw','Hufflepuff')
  print(responses[i])                         # The largest element in the result vector determines the house
                                              # this student is sorted into 
  class(x)<-c(houses[i],'Student')            # the student's house is assigned as the student's second class
  return(x)
}

# Part 4. 
# creates the four required environments. 
Gryffindor_Tower<-new.env()
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()

# creates the generic 'curfew'
curfew<-function(x){
  UseMethod('curfew',x)     
}
# creates curfew methods for each house 
# the curfew method takes a student as input x, and changes the student's environemnt 
# to the appropriate dorm based on their house. 
curfew.Gryffindor<-function(x){
  Gryffindor_Tower$a<-x             # adds student x to Gryffindor Tower environment, 
                                    # given temporary name a
  toRM<-c(x$name)
  rm(list=toRM, pos=".GlobalEnv")    # removes student x from global environemnt 
  with(Gryffindor_Tower, nameStudent<-a$name)     
  with(Gryffindor_Tower, assign(paste(nameStudent),a))    # rename a to actual name of student 
  with(Gryffindor_Tower, rm(a,nameStudent))   # removes temporary object a from Gryffindor Tower environment. 
}
# the above function is recreated for the other 3 houses 
curfew.Slytherin<-function(x){
  Black_Lake$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Black_Lake, nameStudent<-a$name)
  with(Black_Lake, assign(paste(nameStudent),a))
  with(Black_Lake, rm(a,nameStudent))
}
curfew.Ravenclaw<-function(x){
  Ravenclaw_Tower$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Ravenclaw_Tower, nameStudent<-a$name)
  with(Ravenclaw_Tower, assign(paste(nameStudent),a))
  with(Ravenclaw_Tower, rm(a,nameStudent))
}
curfew.Hufflepuff<-function(x){
  Basement$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Basement, nameStudent<-a$name)
  with(Basement, assign(paste(nameStudent),a))
  with(Basement, rm(a,nameStudent))
}

# To test that code works. 
Harry<-createStudent('Harry')
Ron<-createStudent("Ron")
Hermione<-createStudent("Hermione")
Harry<-sort(Harry,F,diag(4))
Ron<-sort(Ron,F,diag(4))
Hermione<-sort(Hermione,F,diag(4))
curfew(Harry)
curfew(Ron)
curfew(Hermione)
