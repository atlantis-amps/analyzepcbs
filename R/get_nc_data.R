get_nc_data <- function(eachgroup,thisncfile, congener, fg.list, outputfrequency){

  print(paste("Analyzing this group",eachgroup))

  this.sprow <- fg.list %>%
    filter(name==eachgroup)

  print(this.sprow)

  if (this.sprow$NumCohorts==1){
    group.ages = eachgroup
  }else{
    group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")
  }


  print(group.ages)

  #make names for nc variables

  varlist <- c(paste0("_PCB", congener))

  var.listdata <- list()

  for(eachvar in 1:length(varlist)){

    eachvarlist <- varlist[eachvar]

    print(eachvarlist)

    name.var <- paste(group.ages,eachvarlist,sep="")

    variable.type <- gsub("_","",eachvarlist)

    for(eachage in 1:length(name.var)) {

      eachvariable <- name.var[eachage]
      print(eachvariable)

      thisData <- var.get.nc(thisncfile, eachvariable)
      thisData[thisData==0]<-NA  # Replace 0's with NA
      print(dim(thisData))
      if (length(dim(thisData)) == 3){
     #   thisData <- thisData[1:7,2:89,]
       thisDataNums<-apply(thisData,3,mean,na.rm = TRUE)#Get nums over time, summing over depth and location
    #    thisDataNums<-apply(thisData,2,mean,na.rm = TRUE)#Get nums over time, summing over depth

      }else{
        if(length(dim(thisData)) == 3)
          thisData <- thisData[,2:89]
       thisDataNums<-apply(thisData,2,mean,na.rm = TRUE)#Get nums over time, summing over depth and location
     #   thisDataNums<-apply(thisData,1,mean,na.rm = TRUE)#Get nums over time, summing over depth

      }
      thisY <-tibble(variable=thisDataNums) %>%  # Normalize by initial value
        mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)
      var.listdata[[eachvariable]] <- thisY

    }

  }


  thissp.data <- var.listdata %>%
    bind_rows() %>%
    mutate(code = this.sprow$Code, longname = this.sprow$longname) %>%
    dplyr::rename(atlantis_group = group) %>%
    mutate(Year=(time*outputfrequency)/365)


  print(paste("Done with group",eachgroup))


  return(thissp.data)
}
