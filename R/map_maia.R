
#' Plota o mapa com países pequenos representados por pontos
#'
#'
#'
#' @param df Data frame a ser plotado
#'
#' @param region Coluna compactível com o "region" do map_data
#'
#' @param fill Coluna com os valores a serem plotados
#'
#' @return none, plots map
#'
#' @author Gabriel Maia
#'
#' @import tidyverse
#'
#'


map_maia  <- function(df, region, fill, factor=T, ncolors=7, is_small="is_small") {


  df %>% rename("is_small2"=!!as.name(is_small)) %>%
    rename("region"=!!as.name(region)) %>%
    rename("fill"=!!as.name(fill))->df




  df %>% filter(is_small2=="Yes")->smol




    if(factor==T){
      map_data("world") %>%
        mutate(region=ifelse(region=="French Guiana",
                             "France", ifelse(region=="Greenland", "Denmark",region))) %>%
        left_join(df, by= c("region")) %>%  filter(is.na(`is_small2`)) %>%
        ggplot() +
        geom_polygon(aes(x=long, y=lat, group=group,fill=fill, label=region), col="gray14")+
        geom_point(data=smol, aes(x=as.numeric(ponto_lon), y=as.numeric(ponto_lat),fill=fill),
                   color="gray12",shape=21, position = "dodge")+
        ggalt::coord_proj()+
    scale_fill_manual(values =  (colorRampPalette(RColorBrewer::brewer.pal(4, "YlOrRd"))(ncolors)), drop=F)}else{
    map_data("world") %>%
      mutate(region=ifelse(region=="French Guiana",
                           "France", ifelse(region=="Greenland", "Denmark",region))) %>%
      left_join(df, by= c("region")) %>%  filter(is.na(is_small)) %>%
      ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group,fill=fill, label=region), col="gray14")+
      geom_point(data=smol, aes(x=as.numeric(ponto_lon), y=as.numeric(ponto_lat),fill=fill),
                 color="gray12",shape=21, position = "dodge")+
      ggalt::coord_proj()+
    scale_fill_continuous()
  }


}


