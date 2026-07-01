make_pcb_map <- function(thisvariabletype, group.atlantis.data, outfolder) {
  thisdataset <- group.atlantis.data %>%
    filter(!code %in% c("DIN", "DL")) %>%
    group_by(var) %>%
    filter(!all(is.na(valeur))) %>%
    ungroup() %>%
    left_join(boxes, by = "box_id") %>%
    st_as_sf()

  write_csv(st_drop_geometry(thisdataset),
            file = file.path(outfolder, paste0(thisvariabletype, ".csv")))

  pdf(file = file.path(outfolder, paste0("SpatialPCB_", thisvariabletype, "_individual.pdf")),
      width = 21/2.54, height = 29/2.54)

  for (eachvar in unique(thisdataset$var)) {
    df <- filter(thisdataset, var == eachvar)
    pplot <- ggplot(df) +
      geom_sf(aes(fill = valeur)) +
      scale_fill_viridis_c() +
      theme_minimal() +
      ggtitle(eachvar)
    print(pplot)
  }
  dev.off()

  pdf(file = file.path(outfolder, paste0("bar_PCB_", thisvariabletype, "_individual.pdf")),
      width = 21/2.54, height = 29/2.54)

  for (eachvar in unique(thisdataset$var)) {
    df <- filter(thisdataset, var == eachvar)
    pplot <- ggplot(df) +
      geom_bar(aes(x=box_id, y=valeur, fill = valeur), stat = "identity") +
      #     scale_fill_viridis_c() +
      theme_minimal() +
      ggtitle(eachvar)
    print(pplot)
  }
  dev.off()
}

