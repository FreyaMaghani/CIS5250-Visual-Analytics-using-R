grouped_bar_chart <- function(main_data, category1,category2, measure1){

  
  tibble1<- main_data %>% group_by({{category1}}, {{category2}}) %>%  
    summarise(group_sum_QTY =sum({{measure1}}))
  
  ggplot(tibble1)+ geom_col(mapping=aes(x= {{category2}}, y= group_sum_QTY,
                                        fill = {{category1}}),
                            position = "dodge")+ coord_flip()+
    theme(panel.background = element_blank()) +
   guides(fill = guide_legend(reverse = T))
}

wide_to_long_tibble<- function(sourceTibble,gathered_key,gathered_value,gatherCol){
  longTibble <- gather(data=sourceTibble,{{gathered_key}},{{gathered_value}},
                       colnames(sourceTibble[{{gatherCol}}]))
  return(longTibble)
}
