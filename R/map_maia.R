
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
#' @param join Booleano, se T sera automaticamente carregado o dataset countries
#'
#' @param factor Booleano, T se fill for Factor
#'
#' @param ncolors Numero de cores a ser usado se factor==T
#'
#' @param is_small Coluna com a tag "is_small". Defaults to "is_small"
#'
#' @return none, plots map
#'
#' @author Gabriel Maia
#'
#' @import tidyverse
#'
#' @export
#'


map_maia  <- function(df, region="region", fill=="n", join_countries=T, is_factor=T, ncolors=7,
                      is_small="is_small") {


  if(isTRUE(join_countries)){
    df %>%rename("region"=!!as.name(region)) %>%
      left_join(MaiaTools::countries2, region)->df}


  df %>% rename("is_small2"=!!as.name(is_small)) %>%
    rename("region"=!!as.name(region)) %>%
    rename("fill"=!!as.name(fill))->df




  df %>% filter(is_small2=="Yes")->smol




    if(isTRUE(is_factor)){
      map_data("world") %>%
        mutate(region=ifelse(region=="French Guiana",
                             "France", ifelse(region=="Greenland", "Denmark",region))) %>%
        left_join(df, by= c("region")) %>%  filter(is.na(`is_small2`)) %>%
        ggplot() +
        geom_polygon(aes(x=long, y=lat, group=group,fill=fill, label=region), col="gray14")+
        geom_point(data=smol, aes(x=as.numeric(ponto_lon), y=as.numeric(ponto_lat),fill=fill),
                   color="gray12",shape=21, position = "dodge")+
        ggalt::coord_proj()+
    scale_fill_manual(values =  (colorRampPalette(RColorBrewer::brewer.pal(4, "YlOrRd"))(ncolors)),
                      drop=F)}else{
    map_data("world") %>%
      mutate(region=ifelse(region=="French Guiana",
                           "France", ifelse(region=="Greenland", "Denmark",region))) %>%
      left_join(df, by= c("region")) %>%  filter(is.na(is_small)) %>%
      ggplot() +
      geom_polygon(aes(x=long, y=lat, group=group,fill=fill, label=region), col="gray14")+
      geom_point(data=smol, aes(x=as.numeric(ponto_lon), y=as.numeric(ponto_lat),fill=fill),
                 color="gray12",shape=21, position = "dodge")+
      ggalt::coord_proj()+
    scale_fill_continuous()  }


}


