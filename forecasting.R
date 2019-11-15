library(tidyquant)
library(h2o)

df<-read.csv('monthly-milk-production-pounds-p.csv',nrows = 168) %>% `colnames<-`(c('Date','Count'))


df$Date<-as.Date(paste0(as.character(df$Date), "-01")) # Datanın formatı = Gün və aydır, biz hər bir aya
# əlavə olaraq gün əlavə edirik.


#Vizuallaşdırma
ggplot(df, aes(Date,Count)) + geom_line(color='red') + 
  scale_x_date(date_breaks = "1 year") +theme_bw()

library(timetk) 

df %>% tk_augment_timeseries_signature() -> cleaned # Datanı transformasiya etmək


# Datanı təmizləmək
cleaned <- cleaned %>%
  select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)


# Train, valid və test setlərə bölmək
train_tbl <- cleaned %>% filter(year < 1969)
valid_tbl <- cleaned %>% filter(year >= 1969, year < 1973)
test_tbl  <- cleaned %>% filter(year >= 1973)


h2o.init()    

train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

y <- "Count" # Proqnoz etdiyimiz sütun
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  #leaderboard_frame = test_h2o, 
  max_runtime_secs = 720, 
  stopping_metric = "deviance",exclude_algos = c("DRF", "GBM","GLM",'XGBoost')) # yalnız DEEP-LEARNİNG
# alqoritmi Time series ilə yaxşı nəticə göstərir !


automl_models_h2o@leaderboard %>% as.tibble() %>% head(20) # liderlərin siyahısı


automl_leader <- automl_models_h2o@leader # LİDER ALQORİTM 
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o) # Test datsı üçün proqnoz etmək
h2o.performance(automl_leader, newdata = test_h2o) # Testdə alınan göstəricilərə baxmaq

error_tbl <- df %>% 
  filter(lubridate::year(Date) >= 1973) %>%   # Date üçün filtr etmə üsulu
  add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = Count) %>% 
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl
error_tbl %>%
  summarise(
    me   = mean(error),          # mean error -  ortalama səhvi göstərir (nəticədə mənfi və müsbət ədədlər var)
    mae  = mean(abs(error)),     # mean absolute error - ortalamanı modul ilə çıxarıb göstərir
    mape = mean(abs(error_pct)), # abs ədədləri modul ilə çıxarıb faiz nisbətini tapırıq
    mpe  = mean(error_pct)       # modulsuz (mənfi+mübət ədədlər) faizi tapırıq
  ) %>%
  glimpse()


library(highcharter)

# Vizuallaşdırma
p1<-highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line',color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line',color='green', name='Predicted') %>% 
  hc_title(text='H2O')

#-------------------------------------------------------ARIMA-----------------------------------------------
library("forecast")

df[1:120,] ->train_forecast 

df[121:168,] -> for_testing

train_forecast_arima<-tk_ts(train_forecast$Count, start = 1962, freq = 12) # Datanı ARİMA üçün transform edək

for_testing_arima<-tk_ts(for_testing$Count, start = 1972, freq = 12)

# 95 % əminliklə proqnoz edək növbəti 48 / 12 = 4 il üçün
forecast(ets(train_forecast_arima), h = 48, level = 95) %>% as.tibble() ->comparison

# Actual ve predicted-lərə baxa bilərik
tk_tbl(for_testing_arima) %>% mutate(predicted=comparison$`Point Forecast`) %>% 
  mutate(
    error     = value - predicted,
    error_pct = error / value
  ) ->errors
errors

mae<-mean(abs(errors$error), na.rm=TRUE)
mape<-mean(abs(errors$error_pct), na.rm=TRUE)
me<-mean(errors$error, na.rm=TRUE)
mpe<-mean(errors$error_pct, na.rm=TRUE)
tibble(me,mae, mape,mpe) %>% glimpse()

p2<-highchart() %>% 
  hc_xAxis(categories = df[121:168,]$Date) %>% 
  hc_add_series(data=errors$value, type='line',color='red', name='Actual') %>% 
  hc_add_series(data=errors$predicted, type='line',color='green', name='Predicted') %>% 
  hc_title(text='AUTO-ARIMA')

library(htmltools)
hw_grid(p1,p2) %>% browsable()



#----------------------------------------------Generate new predictions for H2O------------------------------------


seq(as.Date("1976/01/01"), as.Date("1980/12/01"), "months") %>% as.data.frame() %>% 
  add_column(Count=c(0))->validation # Yeni, yəni növbəti 4 il üçün aylar üzrə proqnoz edək

colnames(validation)<-c('Date','Count') # sütun adlarını dəyişək

validation %>% tk_augment_timeseries_signature()->validation

validation %>%
  select_if(~ !is.Date(.)) %>% # Date formatda sütun olmamması üçün
  select_if(~ !any(is.na(.))) %>% # NA- boş xanaları silmək üçün
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)->validation  # əlifba ardıcılığına görə sıralama

validation %>% as.h2o()->validation # h2o formatına datanı çevirmək

# növbəti 4 il üçün yeni datanın yaradılması
seq(as.Date("1976/01/01"), as.Date("1980/12/01"), "months") %>% as.data.frame() %>% 
  add_column(Count=c(0))->validation2 

# h2o modeli ilə yeni satışların proqnoz edilməsi
h2o.predict(automl_leader, newdata = validation) %>% as.tibble() %>% 
  add_column(Date=validation2$.) %>% select(Date,predict) %>% 
  `colnames<-`(c('Date','Count'))->new_predictions

# Vizuallaşdırmaq üçün köhnə datanı yeni ilə birləşdirmək
bind_rows(df,new_predictions) %>%as.tibble() %>% 
  
  # rep - təkrarlamaq deməkdir. Yəni 168 dəfə Actual, 60 dəfə isə Predicted təkrarla, çünki
  # 168 sətr artıq var idi, yeni 60 sətridə əlavə etdik.
  mutate(colors=c(rep('Actual',168),rep('Predicted',60)))->new_predictions


# Actual və Predicted üçün qruplaşdırıb qırmızı və yaşıl xətlə göstəririk
p3<-hchart(new_predictions, "line", hcaes(Date, Count, group = new_predictions$colors)) %>% 
  hc_title(text='H2O') %>% hc_colors(colors = c('red','green'))


#----------------------------------------------Generate new predictions for ARİMA-----------------------------
df_arima<-tk_ts(df$Count, start = 1962, freq = 12)

# Növbəri 4 il üçün proqnoz etmək
p4 <- forecast(ets(df_arima), h = 48, level = 90) %>% hchart() %>% hc_colors(colors = c('red','green'))


# Plotları birləşdirmək
hw_grid(p3,p4) %>% browsable()
















