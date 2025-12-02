full_results2 <- pixel_kappa_array
full_results2$Type <- "Pixel-based"
full_results2<- rbind(full_results2, data.frame(pixel_mat_kappa_array, 
                                              Type = "Mature Pixel-based"))
full_results2<- rbind(full_results2, data.frame(kappas_array[-(which(kappas_array$treecov == "NTM")),], 
                                              Type = "Parcel-based"))
full_results2<- rbind(full_results2, data.frame(kappas_mat_array[-(which(kappas_mat_array$treecov == "NTM")),], 
                                              Type = "Mature Parcel-based"))
ggplot(full_results2, aes(x = landcov, y = treecov, fill = kappa)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~Type, nrow =2, ncol = 2)+
  geom_text(aes(label = sprintf("%.2f", kappa)), color = "black", size = 3) +
  scale_fill_viridis_c(limits = c(-1,1), option = "plasma") +
  theme_minimal(base_size = 12) +
  labs(
    x = "Land Cover Map",
    y = "Tree Cover Source",
    fill = "Kappa"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_text(size = 12, face="bold")
  )

ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/figures/kappa.jpeg",
       width = 21, height = 21, units = "cm", dpi = 600)

#mcc
full_results3 <- pixel_mcc_array
full_results3$Type <- "Pixel-based"
full_results3<- rbind(full_results3, data.frame(pixel_mat_mcc_array, 
                                                Type = "Mature Pixel-based"))
full_results3<- rbind(full_results3, data.frame(mcc_array[-(which(mcc_array$treecov == "NTM")),], 
                                                Type = "Parcel-based"))
full_results3<- rbind(full_results3, data.frame(mcc_mat_array[-(which(mcc_mat_array$treecov == "NTM")),], 
                                                Type = "Mature Parcel-based"))

ggplot(full_results3, aes(x = landcov, y = treecov, fill = mcc)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~Type, nrow =2, ncol = 2)+
  geom_text(aes(label = sprintf("%.2f", mcc)), color = "black", size = 3) +
  scale_fill_viridis_c(limits = c(-1,1), option = "plasma") +
  theme_minimal(base_size = 12) +
  labs(
    x = "Land Cover Map",
    y = "Tree Cover Source",
    fill = "MCC"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.text.x = element_text(size = 12, face="bold")
  )

ggsave(filename = "C:/Users/am1355/OneDrive - University of Leicester/Publications/Thesis/ch2/figures/MCC.jpeg",
       width = 21, height = 21, units = "cm", dpi = 600)
