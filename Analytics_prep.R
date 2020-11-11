library(tidyverse)
library(janitor)
library(tidytext)
library(scales)

theme_set(theme_light())


scale_color <- scale_color_gradient2(midpoint=0, 
                                     low="red",
                                     mid="white",
                                     high="blue", 
                                     space ="Lab" )



  HR_raw_data <- readxl::read_xlsx("HR_Data.xlsx")

HR_raw_data %>% glimpse()

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

HR_clean_data %>% view()

NPS_intro <- HR_clean_data %>% 
  dplyr::filter(interview=="2nd") %>% 
  group_by(employee_region, consultation_name,interview_outcome) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  pivot_wider(values_from = n,names_from=interview_outcome,values_fill=0) %>% 
  mutate(all_respondents=Happy+Neutral+Unhappy,
         NPS=Happy/all_respondents-Unhappy/all_respondents) %>% 
  group_by(employee_region) %>% 
  mutate(NPS_mean=sum(NPS)/nrow(.)) %>% 
  ungroup()


NPS_intro_viz<- function(wrap_category=employee_region,y_category=consultation_name) {

NPS_intro %>% 
  mutate(consultation_name=tidytext::reorder_within({{y_category}},NPS,{{wrap_category}})) %>% 
  ggplot(aes(NPS,{{y_category}})) +
  geom_vline(xintercept = 0,color="gray69", linetype='dashed')+
  geom_vline(aes(xintercept = NPS_mean),color="cornflowerblue")+
  geom_segment( aes(x=0, xend=NPS, y={{y_category}}, yend={{y_category}}), color="black") +
  geom_point( aes(colour=NPS), 
              size=2, 
              #colour="black",
              #alpha=0.6
              )+

  facet_wrap(~employee_region,scales = "free_y",nrow = 1)+
  scale_y_reordered()+  
  scale_color+
  theme(
    panel.grid.major.y=element_blank()+
    panel.grid.major.x=element_line(colour = "grey93")
  )
    
  
}


NPS_intro_viz()

  NPS_intro %>% 
  mutate(consultation_name=tidytext::reorder_within(consultation_name,NPS,employee_region)) %>% 
  ggplot(aes(NPS,consultation_name)) +
  geom_segment( aes(x=0, xend=NPS, y=consultation_name, yend=consultation_name), color="skyblue") +
  geom_point( color="blue", size=2, alpha=0.6)+
  facet_wrap(~employee_region,scales = "free_y",nrow = 1)+
  scale_y_reordered()
  
  
    
  

  

HR_clean_data %>% glimpse()      

