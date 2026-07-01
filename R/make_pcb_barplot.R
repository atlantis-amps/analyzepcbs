make_pcb_barplot <- function(pcb.data.conc, group.atlantis.data, fg.list){

  #plot vertebrates for all timesteps

 nums.plot.data <- pcb.data.conc %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(fg.list) %>%
  dplyr::filter(GroupType %in% c("FISH","SHARK","BIRD","MAMMAL")) %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  dplyr::mutate(variable=dplyr::if_else(is.nan(variable),0,variable))

  # Without transparency (left)
nums.plot.base <- nums.plot.data %>%
  ggplot2::ggplot(aes(x=time, y=variable, group=scenario, color=scenario, fill = scenario)) +
#  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_line() +
  ggplot2::labs(title="Vertebrates", x= "Age class", y = "mean PCB concentration mg.m3 N", color = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")

#   ggplot2::theme_ipsum() +

# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = pg)

  ggplot2::ggsave(
    filename = paste0("pcb_data_verts", pg, ".jpg"),
    plot     = nums.plot,
    device   = "jpeg",
    path = here::here("output"),
    width    = 14,
    height   = 10,
    units    = "in",
    dpi      = 300
  )
}


#average across last time steps for vertebrates

nums.plot.data.mean <- pcb.data.conc %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(fg.list) %>%
  dplyr::filter(GroupType %in% c("FISH","SHARK","BIRD","MAMMAL")) %>%
  dplyr::filter(time>912) %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  dplyr::mutate(variable=dplyr::if_else(is.nan(variable),0,variable)) %>%
  dplyr::summarise(mean_pcb = mean(variable), .by=c("scenario","longname","age"))

# Without transparency (left)
nums.plot.base.mean <- nums.plot.data.mean %>%
  ggplot2::ggplot(aes(x=age, y=mean_pcb, group=scenario, color=scenario, fill = scenario)) +
  #  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_line() +
  ggplot2::labs(title="Vertebrates", x= "Age class", y = "mean PCB concentration mg.m3 N", color = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")

#   ggplot2::theme_ipsum() +

# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base.mean +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base.mean +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = pg)

  ggplot2::ggsave(
    filename = paste0("pcb_data_verts_mean", pg, ".jpg"),
    plot     = nums.plot,
    device   = "jpeg",
    path = here::here("output"),
    width    = 14,
    height   = 10,
    units    = "in",
    dpi      = 300
  )
}

#average across last time steps for invertebrates

nums.plot.data.mean <- pcb.data.conc %>%
  dplyr::bind_rows() %>%
  dplyr::left_join(fg.list) %>%
  dplyr::filter(!GroupType %in% c("FISH","SHARK","BIRD","MAMMAL")) %>%
  #dplyr::filter(time>912) %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  dplyr::mutate(variable=dplyr::if_else(is.nan(variable),0,variable))

# Without transparency (left)

nums.plot.base.mean <- nums.plot.data.mean %>%
  ggplot2::ggplot(aes(x=time, y=variable, group=scenario, color= scenario, fill = scenario)) +
  ggplot2::geom_bar(stat="identity") +
  #ggplot2::geom_line() +
  ggplot2::labs(title="Invertebrates", x= "Group", y = "mean PCB concentration mg.m3 N", color = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")

#   ggplot2::theme_ipsum() +

# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base.mean +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base.mean +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 4, nrow = 3, page = pg)

  ggplot2::ggsave(
    filename = paste0("pcb_data_inverts_mean", pg, ".jpg"),
    plot     = nums.plot,
    device   = "jpeg",
    path = here::here("output"),
    width    = 14,
    height   = 10,
    units    = "in",
    dpi      = 300
  )
}


invisible(nums.plot)

}
