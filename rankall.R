rankall <- function(outcome, num="best"){
  
  measuresFile <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")
  
  ##check if the outcome exist, if not stop and return a message
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% validOutcomes){
    stop("invalid outcome")
  }
    
  f <- data.frame()
  f <- measuresFile[,c(2,7,11,17,23)]
  lstates <- unique(f[,2])
  lstates <- lstates[order()]
  lstates <- as.character(levels(lstates))
  
  f[,1] <- as.character(f[,1])
  for (i in 3:5){
    f[,i] <- suppressWarnings(as.numeric(levels(f[,i]))[f[,i]])
  }
  
  ## find the column which is related to the requested outcome  
  if (outcome=="heart attack"){measureCol <- 3}
  else if (outcome =="heart failure"){measureCol <- 4}
  else if (outcome == "pneumonia"){measureCol <- 5}
    
  stateData<- split(f, f$"State")
  
  ## number of states
  ns <- length (stateData)
  
  
  result <- matrix(nrow=54, ncol=2)
  
  if (num=="best") {num=1}
  
  for (i in 1:ns){
    
    sd <- stateData[[i]]
    orderedSD <- sd[order(sd[,measureCol], sd[,1], na.last=NA),]
    nr <- nrow(orderedSD)
    
    result[i,2] <- as.character(levels(orderedSD[1,2]))[orderedSD[1,2]]
    
   
   if (num=="worst"){result[i,1] <- orderedSD[nr,1]}
        
   else if (num>nr){result[i,1]=NA}
    else if (num<=nr) {result[i,1] <- orderedSD[num,1]} 
            
    }
  
 result <- data.frame(result)
 colnames(result) <- c("hospital", "state")
 row.names(result) <- lstates
 result
  
}