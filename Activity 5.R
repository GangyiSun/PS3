# Applied Statistical Programing 
# Activity 5
# Gangyi Sun (441748)

# Part 1. Create function to create student. 
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
sort.Student<-function(x,decreasing=FALSE,matrix){
  a<-c(x$courage,x$ambition,x$intelligence,x$effort)
  result<-t(matrix)%*%a
  i<-which.max(result)
  responses<-c('GRYFFINDOR!','SLYTHERIN!','RAVENCLAW!','HUFFLEPUFF!')
  houses<-c('Gryffindor','Slytherin','Ravenclaw','Hufflepuff')
  print(responses[i])
  class(x)<-c(houses[i],'Student')
  return(x)
}

# Part 4. 
Gryffindor_Tower<-new.env()
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()

curfew<-function(x){
  UseMethod('curfew',x)     
}
curfew.Gryffindor<-function(x){
  house<-class(x)[1]
  Gryffindor_Tower$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Gryffindor_Tower, nameStudent<-a$name)
  with(Gryffindor_Tower, assign(paste(nameStudent),a))
  with(Gryffindor_Tower, rm(a,nameStudent))
}
curfew.Slytherin<-function(x){
  house<-class(x)[1]
  Black_Lake$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Black_Lake, nameStudent<-a$name)
  with(Black_Lake, assign(paste(nameStudent),a))
  with(Black_Lake, rm(a,nameStudent))
}
curfew.Ravenclaw<-function(x){
  house<-class(x)[1]
  Ravenclaw_Tower$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Ravenclaw_Tower, nameStudent<-a$name)
  with(Ravenclaw_Tower, assign(paste(nameStudent),a))
  with(Ravenclaw_Tower, rm(a,nameStudent))
}
curfew.Hufflepuff<-function(x){
  house<-class(x)[1]
  Basement$a<-x
  toRM<-c(x$name)
  rm(list=toRM,pos=".GlobalEnv")
  with(Basement, nameStudent<-a$name)
  with(Basement, assign(paste(nameStudent),a))
  with(Basement, rm(a,nameStudent))
}

Harry<-createStudent('Harry')
Ron<-createStudent("Ron")
Hermione<-createStudent("Hermione")
Harry<-sort(Harry,F,diag(4))
Ron<-sort(Ron,F,diag(4))
Hermione<-sort(Hermione,F,diag(4))
curfew(Harry)
curfew(Ron)
curfew(Hermione)
