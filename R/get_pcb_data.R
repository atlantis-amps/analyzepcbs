get_pbc_data <- function(fg.list, folder.paths, scenario.names, outputfrequency){

  used.groups <- fg.list %>%
    filter(IsTurnedOn==1) %>%
    dplyr::select(name) %>% .$name

pcb.data <- list()

  for(thisfolder in 1:length(folder.paths)) {

    eachscenario <- scenario.names[thisfolder]
    eachfolder <- folder.paths[thisfolder]
    print(eachfolder)

    this.output.nc <- "AMPS_OUT.nc"

    nc <- open.nc(paste0(eachfolder,"/",this.output.nc))
    nc.data <- read.nc(nc)

    print.nc(nc)

    group.atlantis.data <- lapply(used.groups, get_nc_data, thisncfile = nc, congener = "118", fg.list, outputfrequency) %>%
      bind_rows() %>%
      dplyr::mutate(scenario=eachscenario)

    pcb.data[[thisfolder]] <- group.atlantis.data

  }

  return(pcb.data)
}
