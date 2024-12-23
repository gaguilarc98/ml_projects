agrupar <- function(x, vstep, vgrupo, vagre, pct=5, tms=100, last= 1){
  ult <- x %>% distinct({{vstep}}) %>% arrange(desc({{vstep}})) %>% 
    dplyr::filter(row_number()==last) %>% pull
  y <- x %>% 
    group_by({{vstep}}) %>% 
    mutate(rat = {{vagre}}/sum({{vagre}}, na.rm = T)*tms) %>% 
    #VARIANTE1
    group_by({{vstep}},{{vgrupo}}) %>%
    summarise(tot = sum({{vagre}}, na.rm = T),rat = sum(rat, na.rm = T)) %>%
    ungroup() %>%
    mutate(ORDEN = ifelse({{vgrupo}} %in% {{vgrupo}}[rat>=pct & {{vstep}} == ult],
                          {{vgrupo}}, "Otros")) %>%
    mutate(ORDEN = fct_reorder(ORDEN,rat)) %>%
    #VARIANTE2
    # group_by({{vstep}},{{vgrupo}}) %>% 
    # summarise(tot = sum({{vagre}}, na.rm = T),rat = sum(rat, na.rm = T)) %>% 
    # mutate(ORDEN = ifelse({{vgrupo}} %in% {{vgrupo}}[rat>=pct & {{vstep}} == ult],
    #                       {{vgrupo}}, "Otros")) %>% 
    # mutate(ORDEN = fct_reorder(ORDEN,rat)) %>% 
    # ungroup() %>% 
    #
    group_by({{vstep}},ORDEN) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T)) %>% 
    ungroup()
  z <- y %>%
    group_by({{vstep}}) %>% 
    summarise(tot = sum(tot,na.rm = T),rat=sum(rat,na.rm = T))
  result <- list(y=y,z=z)
  return(result)
}

plt <- agrupar(gph,vstep = year, vgrupo = tipoCred, vagre = saldoCast, pct=8)
plt$y <- plt$y %>% 
  mutate(lab = paste0(round(rat),'% (',comma(round(tot/1e3,1)),' M USD)'))
ggplot(plt$y,aes(x=year,y=tot,fill=ORDEN))+
  geom_bar(stat="identity")+
  geom_label(aes(label=lab),size=3,color="white",show.legend = F,
             position=position_stack(vjust=0.5))+
  annotate(geom = "label",x=plt$z$year,y=plt$z$tot*1.05,
           label=comma(round(plt$z$tot,0)),size=3.5,color=paleta(12)[7])+
  labs(x="",y="Castigos (en M de USD)",
       fill="Tipo de Crédito")+
  scale_fill_manual(values=paleta(8))+
  scale_x_continuous(breaks = seq(2015, 2022, 1))+
  scale_y_continuous(labels = comma)+
  theme_minimal()+
  theme(legend.position = "bottom")