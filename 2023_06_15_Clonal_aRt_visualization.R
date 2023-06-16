###############################################################
## Project:Clonal aRt visualization
## Script purpose: Visual artsy representation of B-cell clonal repertoires using R.
##                 Name of the image: Ab field of dandelions
## Date:15/06/2023
## Version:1
## Author:Rodrigo Garcia (r.garciavaliente@amsterdamumc.nl)
## The radial plots are inspired and based on https://ijeamakaanyene.github.io/aRt_ggplot/index.html
## The used repertoire dataset subset from Laserson and Vigneault et al, 2014. is part of the Immcantation pipeline
## The combination of plots over the same grid is done using the cowplot package
## Characteristics and position of the dandelions change according to their (clonal) size
###############################################################

library(ggplot2)
library(tidyr) 
library(dplyr)
library(cowplot)
library(alakazam)  

### 0. Set-up
path="/home/rgarcia/Escritorio/Imaging/"
seed=2026
set.seed(seed)

ab_theme=  theme(
  plot.background = element_rect(
    fill = "transparent",
    linewidth=0,
    linetype="blank",
    color=NULL),
  panel.background = element_rect(
    fill = "transparent",
    linewidth=0,
    linetype="blank",
    color=NULL),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  plot.caption = element_text(
    family = "Open Sans",
    size = 6,
    color = "white"),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
)



### 1. Creating the plot
B_repertoire=ExampleDb
B_repertoire=B_repertoire[which(B_repertoire$sample_id == "+7d"),]
B_repertoire$clone_count=sapply(B_repertoire$clone_id, function(z) length(which(B_repertoire$clone_id == z)) )
B_repertoire$clone_order=sapply(B_repertoire$clone_count, function(z) which(sort(unique(B_repertoire$clone_count),decreasing = T)==z))


canvas=ggdraw( xlim = c(0, 100), ylim = c(0, 142)) 

for (clone in unique(B_repertoire$clone_id[order(B_repertoire$clone_order, decreasing = T)])) {

  size_factor=B_repertoire$clone_count[which(B_repertoire$clone_id == clone)[1]]/max(B_repertoire$clone_order)
  
  
  ##based on https://ijeamakaanyene.github.io/aRt_ggplot/index.html
  center_circle = tibble(
    x = seq(0, length(which(B_repertoire$clone_id == clone)), by = 1),
    xend = x,
    y = rep(0, length(x)),
    yend = rep(6, length(x)))
  
  outer_circle = center_circle %>%
    mutate(y = yend,
           yend = yend + 1)
  
  main_dandelion=ggplot() +
    geom_segment(data = center_circle,
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 size = 0.5,
                 color = "white") +
    geom_segment(data = outer_circle,
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 size = 1.5,
                 color = "white") +
    ylim(-2, 10) + 
    coord_polar()+ab_theme
  
  center_circle_as = tibble(
    x = seq(0, 100, by = 1),
    xend = x,
    y = rep(0, length(x)),
    yend = rep(6, length(x)))
  
  
  singleton_dandelion=ggplot() +
    geom_segment(data = center_circle_as,
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 size = 0.5,
                 color = "white") +
    ylim(-2, 10) + 
    coord_polar()+ab_theme
  
  stem_dandelion=ggplot() +
    ylim(-2, 100) + 
    coord_polar()+ab_theme+geom_vline(xintercept = 0,color = "white")
  
  
  inner_dandelion=ggplot() +
    geom_segment(data = center_circle,
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 size = 0.5,
                 color = "white")+
    ylim(-2, 100) + 
    coord_polar()+ab_theme
  
  
  data <- data.frame(
    value=c(1),
    group=as.factor("Center")
  )
  
  inner_color_dandelion=ggplot(data, aes(x="", y=value, fill=group)) +
    geom_bar(stat="identity", width=1) + scale_fill_manual(values=c("#840032")) +
    coord_polar("y", start=0) + ab_theme
  
  
  if (B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]] == 1) { ##So far this works only with one top clone, not with several in the #1 position
    position_x=50
    position_y=100
    
  } else if (B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]] == max(B_repertoire$clone_order)) {
    position_x=runif(1, min=0, max=142)
    position_y=runif(1, min=0, max=142)
    while(position_x<77 && position_x>23 && position_y<125 && position_y>75) {
      position_x=runif(1, min=0, max=142)
      position_y=runif(1, min=0, max=142)
    }
    
  } else {
    position_x=runif(1, min=2, max=98)
    
    position_y=if(B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]] >= (max(B_repertoire$clone_order)-7)){ c(1:9)[(max(B_repertoire$clone_order)-B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]] )]  }else{max(10, 75- (5*B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]]))}
    
    while(position_x<77 && position_x>23 && position_y>65) {
      while(position_x<55 && position_x>45) {
        position_x=runif(1, min=2, max=98)
      }
    }
  }
  
  if (B_repertoire$clone_order[which(B_repertoire$clone_id==clone)[1]] == max(B_repertoire$clone_order)) {
    canvas=canvas + draw_plot(singleton_dandelion, x=position_x,y=position_y,scale=3)
  } else {
    canvas=canvas +  draw_plot(main_dandelion, x=position_x,y=position_y,scale=25*size_factor) +
      draw_plot(stem_dandelion, x=position_x,y=position_y,scale=1000) +
      draw_plot(inner_dandelion, x=position_x,y=position_y,scale=90*min(1,size_factor)) +
      draw_plot(inner_color_dandelion, x=position_x,y=position_y,scale=max(1, 4*min(1,size_factor)))
  }
  
}

canvas=canvas +   theme(plot.background = element_rect(fill="#840032", color = NA))
ggsave(paste(path, seed, ".png", sep=""),canvas,dpi = 600, width = 28, height = 40, units = "cm")