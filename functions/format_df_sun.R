format_df_sun <- function(
           id = id,
           data = data,
           p_col = p_col,
           c_col = c_col,
           parent_val = ""){
  
  require("dplyr")
  require("rlang")
  
  parent_col <- enquo(p_col)
  child_col <- enquo(c_col)
  id <- enquo(id)
  
    parent_data <- data %>%
    group_by(!!parent_col) %>%
    mutate(count_p = length(!!id)) %>%
    select(-!!id) %>%
    mutate(labels = paste(first(!!parent_col))) %>%
    mutate(parents = parent_val) %>%
    pivot_longer(-c(parents, 
                    labels,
                    !!parent_col, 
                    !!child_col),
                 "Count", "value") %>%
      ungroup() %>%
      select(!!parent_col, parents, labels, value) %>%
      unique()
    
   child_data <- data %>%
    group_by(!!child_col) %>%
    unique() %>%
    mutate(count_c = length(unique(!!id))) %>%
    select(!!-id) %>%
    mutate(parents = paste(first(!!parent_col)),
           labels = paste(first(!!child_col))) %>%
    pivot_longer(-c(parents, 
                    labels, 
                    !!parent_col, 
                    !!child_col),
                 "Count", "value") %>%
    ungroup() %>%
    select(!!parent_col, parents, labels, value) %>%
    unique()
   
   truedat <- data %>%
     select(!!parent_col, !!id) %>%
     unique() %>%
     group_by(!!parent_col) %>%
     mutate(count_true = length(unique(!!id))) %>%
     mutate(parents = parent_val) %>%
     mutate(ids = paste(parents, "-", !!parent_col)) %>%
     ungroup() %>%
     select(count_true, parents, ids) %>%
     unique()
   
   row <- c(sum(truedat$count_true), "", parent_val)
   
   truedat<-rbind(truedat, row)
   
   sun <- rbind.data.frame(child_data, parent_data)

  sun  <- sun %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    mutate(ids = ifelse(is.na(parents), paste(labels), paste(parents, "-", labels))) %>%
    unique()

  row <- c("", "", parent_val, sum(sun$value), parent_val)
  
  sun<-rbind(sun, row)

  sun$value <- as.numeric(as.integer(sun$value))

  sun<-sun[order(-sun$value, sun$parents),]
  
  sun <- merge(sun, truedat, by=c("parents", "ids"), all.x=T)

  sun <- sun %>%
    mutate(count_true = ifelse(is.na(count_true), value, count_true)) 
  
  sun <- unique(sun)

  return(sun)
  
  }
  
    
  