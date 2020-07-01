
reactive_rob_by_click <- function(
  RoB_data = RoB_data,
  AD_data = AD_data,
  SourceSunPlot = SourceSunPlot,
  SunPlot = SunPlot,
  RoBFilter = RoBFilter,
  subsetinput = subsetinput,
  ParentFilter = ParentFilter)
  {
  
  require("dplyr")
  require("rlang")

  
  RoBFilter <- enquo(RoBFilter)
  ParentFilter <- enquo(ParentFilter)
  
  
  rob <- subset(RoB_data, subset = RoB_data$Year %in% c(subsetinput[1]:subsetinput[2]))
  rob <- droplevels(rob)
  
  rob <- rob %>%
  select(RecordID, Regex, RegexResult) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  unique()

eventdata <<- event_data("plotly_click", source = SourceSunPlot)
datapoint <<- (as.numeric(eventdata$pointNumber) + 1)

selectedchunk <- subset(SunPlot, subset = rownames(SunPlot) %in% datapoint)
selectedchunk <- droplevels(selectedchunk)

selectedchunk <- selectedchunk %>%
select(labels) %>%
dplyr::mutate_if(is.factor, as.character) %>%
unique()

AD_data2 <- AD_data %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  rename(labels = !!RoBFilter) 

selectedchunk <- merge(AD_data2, selectedchunk, by="labels")

selectedchunk <- selectedchunk %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  select(labels, RecordID) %>%
  unique()

fin <- merge(rob, selectedchunk, by="RecordID")

fin <- fin %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  select(Regex, RegexResult, RecordID) %>%
  unique() %>%
  group_by(Regex) %>%
  mutate(Ntotal = length(unique(RecordID))) %>%
  filter(RegexResult > 0) %>%
  mutate(count = length(unique(RecordID))) %>%
  mutate(percent = round((count/Ntotal)*100,2)) %>%
  select(Regex, count, percent, Ntotal) %>%
  unique()

if(is.null(eventdata$pointNumber)){
  
  AD_data3 <- AD_data %>%
    dplyr::select(!!ParentFilter, RecordID) %>%
    unique()
  
  df <- merge(rob, AD_data3, by="RecordID")
  
  df$RegexResult<-as.numeric(df$RegexResult) 
  
  df <- df %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    select(!!ParentFilter, Regex, RegexResult, RecordID) %>%
    unique() %>%
    mutate(Parent = !!ParentFilter) %>%
    group_by(Parent, Regex) %>%
    mutate(Ntotal = length(unique(RecordID))) %>%
    mutate(count = length(RecordID[which(RegexResult>0)])) %>%
    ungroup() %>%
    select(Regex, Parent, Ntotal, count) %>%
    filter(!Ntotal < 10) %>%
    unique()
  
  
  df %>%
    mutate(percent = round((count/Ntotal)*100,2)) %>%
    select(Parent, Regex, count, percent, Ntotal) %>%
    unique() %>%
    plot_ly(x = ~Parent,
            y= ~percent,
            color = ~Regex,
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('</br><b>% publications reporting item: </b>', percent,
                          '</br><b>N publications reporting item: </b>', count,'/',Ntotal)) %>%
    layout(
      xaxis =
        list(
          title = ""
        ),
      yaxis = list(
        title = "% Reporting",
        range = c(0,100)))
  
  }

else if(eventdata$pointNumber ==0){
  
  AD_data3 <- AD_data %>%
    dplyr::select(!!ParentFilter, RecordID) %>%
    unique()
  
  df <- merge(rob, AD_data3, by="RecordID")
  
  df$RegexResult<-as.numeric(df$RegexResult) 
  
  df <- df %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    select(!!ParentFilter, Regex, RegexResult, RecordID) %>%
    unique() %>%
    mutate(Parent = !!ParentFilter) %>%
    group_by(Parent, Regex) %>%
    mutate(Ntotal = length(unique(RecordID))) %>%
    mutate(count = length(RecordID[which(RegexResult>0)])) %>%
    ungroup() %>%
    select(Regex, Parent, Ntotal, count) %>%
    filter(!Ntotal < 10) %>%
    unique()

  
  df %>%
    mutate(percent = round((count/Ntotal)*100,2)) %>%
    select(Parent, Regex, count, percent, Ntotal) %>%
    unique() %>%
    plot_ly(x = ~Parent,
            y= ~percent,
            color = ~Regex,
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('</br><b>% publications reporting item: </b>', percent,
                          '</br><b>N publications reporting item: </b>', count,'/',Ntotal)) %>%
    layout(
      xaxis =
        list(
          title = ""
        ),
      yaxis = list(
        title = "% Reporting",
        range = c(0,100)))
  
}

else{
  
  fin %>%
    plot_ly(x = ~Regex,
            y= ~percent,
            color = ~Regex,
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('</br><b>% publications reporting item: </b>', percent,
                          '</br><b>N publications reporting item: </b>', count,'/',Ntotal)) %>%
    layout(
      xaxis =
        list(
          title = ""
        ),
      yaxis = list(
        title = "% Reporting",
        range = c(0,100)))
}
}
