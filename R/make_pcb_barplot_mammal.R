make_pcb_barplot_mammal <- function(pcb.data.conc, group.atlantis.data, fg.list){

  mammal.groups <- fg.list %>%
    dplyr::filter(GroupType=="MAMMAL" | GroupType == "BIRD") %>%
    dplyr::pull(longname)


nums.plot.data <- pcb.data.conc %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  dplyr::mutate(variable=dplyr::if_else(is.nan(variable),0,variable)) %>%
  dplyr::summarise(mean_var = mean(variable), .by=c("scenario","longname","age")) %>%
  dplyr::filter(longname %in% mammal.groups)
  # Without transparency (left)
nums.plot.base <- nums.plot.data %>%
  ggplot2::ggplot(aes(x=age, y=mean_var, group=scenario, color=scenario, fill = scenario)) +
#  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_line() +
  ggplot2::labs(x= "Age class", y = "mean PCB concentration mg.m3 N", color = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")

#   ggplot2::theme_ipsum() +

# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = pg)

  ggplot2::ggsave(
    filename = paste0("pcb_data", pg, ".jpg"),
    plot     = nums.plot,
    device   = "jpeg",
    path = here::here("output"),
    width    = 14,
    height   = 10,
    units    = "in",
    dpi      = 300
  )
}


#average across last time steps

nums.plot.data <- pcb.data.conc %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(variable=dplyr::if_else(is.nan(variable),0,variable)) %>%
  dplyr::filter(time>912) %>%
  dplyr::summarise(mean_pcb = mean(variable), .by=c("scenario","longname","age")) %>%
  #dplyr::filter(age>1) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  dplyr::filter(longname %in% mammal.groups)


# Without transparency (left)
nums.plot.base <- nums.plot.data %>%
  ggplot2::ggplot(aes(x=age, y=mean_pcb, group=scenario, color=scenario)) +
  #  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_line() +
  ggplot2::labs(x= "Age", y = "Mean PCB concentration mg.m3 N", color = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")

#   ggplot2::theme_ipsum() +

# Without transparency (left)
nums.plot.base <- nums.plot.data %>%
  ggplot2::ggplot(aes(x=age, y=mean_pcb, group=scenario, fill=scenario)) +
  #  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(x= "Age", y = "Mean PCB concentration mg.m3 N", fill = "Scenario") +
  ggplot2::facet_wrap(~longname, scales = "free")



# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = pg)

  ggplot2::ggsave(
    filename = paste0("pcb_data_mean", pg, ".jpg"),
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
