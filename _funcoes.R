library(showtext)


# Load font from Google Fonts
font_add_google("EB Garamond", "EB Garamond")
showtext_auto()


# Create a custom ggplot2 theme
tema <- function(base_size = 12){
  
  theme_bw(base_family = "EB Garamond", base_size = base_size) +
    theme(panel.border = element_rect(color = "black"),
          panel.grid = element_blank(),
          axis.text = element_text(color = "black", size = base_size - 1),
          axis.title.y = element_text(margin = margin(r = 15)),
          axis.title.x = element_text(margin = margin(t = 15)),
          strip.background = element_blank(),
          strip.text = element_text(size = base_size),
          plot.title = element_text(size = base_size + 1, hjust = 0.5),
          legend.position = "top"
    )
}

# Define colors
purple <- "#8f2a43"
paleta <- c(purple, "#F98A9C", "#00C9B4", "#005B4E")
