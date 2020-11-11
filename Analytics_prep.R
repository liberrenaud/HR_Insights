library(tidyverse)
library(janitor)
library(tidytext)
library(scales)

install.packages("likert")
install.packages("pisaitems")

theme_set(theme_light())

# Set scale for the lolipop graph
scale_color <- scale_color_gradient2(midpoint=0, 
                                     low="red",
                                     mid="white",
                                     high="blue", 
                                     space ="Lab" )


# Import Data
HR_raw_data <- readxl::read_xlsx("HR_Data.xlsx")


# Clean the data
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

# Function - Intro Lollipop chart
NPS_intro_viz<- function(wrap_category=employee_region,y_category=consultation_name) {
  
  
  NPS_intro <- HR_clean_data %>% 
    dplyr::filter(interview=="2nd") %>% 
    group_by({{wrap_category}}, {{y_category}},interview_outcome) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    pivot_wider(values_from = n,names_from=interview_outcome,values_fill=0) %>% 
    mutate(all_respondents=Happy+Neutral+Unhappy,
           NPS=Happy/all_respondents-Unhappy/all_respondents) %>% 
    group_by({{wrap_category}}) %>% 
    mutate(NPS_mean=sum(NPS)/nrow(.)) %>% 
    ungroup()

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
  
    #ISSUE IN MY FACET _ DID NOT MANAGE TO TIDYEVAL IT
  
    scale_y_reordered()+  
    scale_color+
    theme(
      panel.grid.major.y=element_blank(),
      panel.grid.major.x=element_line(colour = "grey93")
    )
    
  
}





NPS_intro_viz()+
facet_wrap(~employee_region,scales = "free_y",nrow = 1)


NPS_intro_viz(wrap_category=consultation_name,y_category=employee_region,y_label="Regions")+
  facet_wrap(~consultation_name,scales = "free_y",nrow = 1)

  
  


  

  

HR_clean_data %>% glimpse()      





BACK UP
# 
# 
# NPS_intro_viz<- function(wrap_category=employee_region,y_category=consultation_name,y_label="Metrics") {
#   
#   # Prepare the data for the visualization
#   NPS_intro <- HR_clean_data %>% 
#     dplyr::filter(interview=="2nd") %>% 
#     dplyr::group_by({{wrap_category}}, {{y_category}},interview_outcome) %>% 
#     dplyr::summarise(n=n()) %>% 
#     dplyr::ungroup() %>% 
#     tidyr::pivot_wider(values_from = n,names_from=interview_outcome,values_fill=0) %>% 
#     dplyr::mutate(all_respondents=Happy+Neutral+Unhappy,
#                   NPS=round((Happy/all_respondents-Unhappy/all_respondents)*100),0) %>% 
#     dplyr::group_by({{wrap_category}}) %>% 
#     dplyr::mutate(NPS_mean=sum(NPS)/nrow(.)) %>% 
#     dplyr::ungroup()%>% 
#     dplyr::mutate(y_names=tidytext::reorder_within({{y_category}},NPS,{{wrap_category}}))
#   
#   # Visualization of the data
#   NPS_intro %>% 
#     #Set up Geoms
#     ggplot(aes(NPS,y_names)) +
#     geom_vline(xintercept = 0,color="gray69", linetype='dashed')+
#     geom_vline(aes(xintercept = NPS_mean),color="cornflowerblue")+
#     geom_segment( aes(x=0, xend=NPS, y=y_names, yend=y_names), color="black") +
#     geom_point( aes(colour=NPS), 
#                 size=2, 
#                 #colour="black",
#                 #alpha=0.6
#     )+
#     
#     #ISSUE IN MY FACET _ DID NOT MANAGE TO TIDYEVAL IT
#     #facet_wrap(~consultation_name,scales = "free_y",nrow = 1)
#     
#     #Set up formatting and labs
#     scale_y_reordered()+  
#     scale_color+
#     theme(
#       panel.grid.major.y=element_blank(),
#       panel.grid.major.x=element_line(colour = "grey93")
#     )+
#     labs(title = "NPS results by Region and Metrics",
#          x = "NPS Score",
#          y = y_label)
#   
#   
# }
