tidy_HR <-
function(){
HR_clean_data <- HR_raw_data %>% 
  janitor::clean_names() %>% 
  dplyr::select(employee_id:x1st_interview_outcome) %>% 
  dplyr::select_all(~str_replace(.,"x1st_","")) %>% 
  dplyr::mutate(interview="1st") %>% 
  dplyr::bind_rows(HR_raw_data %>% 
              janitor::clean_names() %>% 
              dplyr::select(employee_id:consultation_end_date,contains("x2nd")) %>% 
              dplyr::select_all(~str_replace(.,"x2nd_","")) %>% 
              dplyr::mutate(interview="2nd")
            )
}
NPS_lolipop_viz <-
function(wrap_category=employee_region,y_category=consultation_name,y_label="Metrics") {
  
  # Prepare the data for the visualization
  NPS_intro <- HR_clean_data %>%
    dplyr::filter(interview=="2nd") %>%
    dplyr::group_by({{wrap_category}}, {{y_category}},interview_outcome) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(values_from = n,names_from=interview_outcome,values_fill=0) %>%
    dplyr::mutate(all_respondents=Happy+Neutral+Unhappy,
                  NPS=round((Happy/all_respondents-Unhappy/all_respondents)*100),0) %>%
    dplyr::group_by({{wrap_category}}) %>%
    dplyr::mutate(NPS_mean=sum(NPS)/nrow(.)) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(y_names=tidytext::reorder_within({{y_category}},NPS,{{wrap_category}}))
  
  
  
  # Visualization of the data
  
  NPS_intro %>%
    #Set up Geoms
    ggplot2::ggplot(aes(NPS,y_names)) +
    ggplot2::geom_vline(xintercept = 0,color="gray69", linetype='dashed')+
    ggplot2::geom_vline(aes(xintercept = NPS_mean),color="cornflowerblue")+
    ggplot2::geom_segment( aes(x=0, xend=NPS, y=y_names, yend=y_names), color="black") +
    ggplot2::geom_point( aes(colour=NPS),
                size=2,
                # Issue with the color of the circle - did not manage to do it black
                #colour="black",
                #alpha=0.6
    )+
    
    #ISSUE IN MY FACET _ DID NOT MANAGE TO TIDYEVAL IT
    #facet_wrap(~consultation_name,scales = "free_y",nrow = 1)
    
    #Set up formatting and labs
    tidytext::scale_y_reordered()+
    scale_color+
    ggplot2::theme(
      panel.grid.major.y=element_blank(),
      panel.grid.major.x=element_line(colour = "grey93")
    )+
    ggplot2::labs(title = "NPS results by Region and Metrics",
         x = "NPS Score",
         y = y_label)
  
  
}
likert_visualization <-
function(filter_region="all"){

  
  # Filter the data set based on user selection
  if (filter_region != "all") {
    
    HR_clean_data <- HR_clean_data %>%
      dyplyr::filter(employee_region==filter_region) 
    
  }


likert_frame <- HR_clean_data %>% 
  dplyr::filter(interview=="2nd") %>%
  dplyr::select(#employee_region,
                employee_id,
                consultation_name,interview_outcome)%>% 
  tidyr::pivot_wider(names_from = consultation_name,values_from=interview_outcome) %>% 
  janitor::clean_names() %>% 
  dplyr::select(-employee_id) %>% 
  # mutate(across(working_hours:safety,as.factor)
  #        # ,
  #        # across(working_hours:safety, fct_relevel("Unhappy","Neutral","Happy"))
  #        ) %>% 
  base::as.data.frame()


  
  likert_frame$working_hours %<>% as_factor() %>% fct_relevel("Unhappy","Neutral","Happy")
  likert_frame$pay_policy %<>% as_factor() %>% fct_relevel("Unhappy","Neutral","Happy")
  likert_frame$health %<>% as_factor() %>% fct_relevel("Unhappy","Neutral","Happy")
  likert_frame$performance %<>% as_factor() %>% fct_relevel("Unhappy","Neutral","Happy")
  likert_frame$safety %<>% as_factor() %>% fct_relevel("Unhappy","Neutral","Happy")
  
  likert_summary <- likert(likert_frame)
  plot(likert_summary, center=2,low.color= light_coral,mid.color=cadet_grey, high.color=carolina_blue)
  plot(likert_summary, center=2,colors=colors_likert)


}
