library(tidyquant)
library(ggridges)
library(h2o)

fraud <- read_delim('creditcard.csv',delim = ',')

fraud$Class %>% table() %>% prop.table() * 100 # disproporsiya

fraud %>%
  gather(variable, value, -Class) %>% #datanı 3 sütuna yığmaq üçün
  ggplot(aes(y = as.factor(variable), # character olan sütünu factor etmək
             fill = as.factor(Class), # 2 qrupa ayırmaq və rəngləmək
             x = percent_rank(value))) + # faiz aralığında data paylanmasının göstərilməsi
  geom_density_ridges() + theme_tq() + #bütün sütunların paylanmasının vizuallaşdırmada əks etdirilməsi
  labs(y = 'Variable', x = 'Percent rank') + # x və y xətlərinə yeni adların verilməsi
  guides(fill=guide_legend(title="Class")) # Legend-in adını dəyişmək üçün

fraud[sample(nrow(fraud)),]->df #datanı sətrlər üzrə qarışdırmaq

h2o.init(max_mem_size = '8g') # clusteri açmaq


h2o_data<-as.h2o(df) # datanı h2o formatına çevirmək

h2o_data<-h2o.splitFrame(h2o_data,ratios = c(0.8,0.19),seed=1) # proporsiyanı göstərmək

train<-h2o_data[[1]]

test<-h2o_data[[2]]

#validation<-h2o_data[[3]]


# Anomal nümunələri üzə çıxaran modelin qurulması
anomaly_model <- h2o.deeplearning(x = names(train), # sütun adları
                                  training_frame = train, # data
                                  activation = "Tanh", # aktivasiya funskiyası
                                  autoencoder = TRUE, # autoencoder-i aktiv etmək
                                  hidden = c(100,50,100), # qatların sayı
                                  sparse = T, # eyni neyron 0 və 1-ləri, yəni hər iki qrupu emal etməyəcək,
                                  # çünki hər bir neyron 1 qrupu daha yaxşı analiz edə bilir, nəyinki eyni
                                  # anda hər 2 qrupu.
                                  l1 = 1e-4, # öyrənmə sürəti
                                  epochs = 100) # əməliyyatı təkrarlama sayı


recon_error <- h2o.anomaly(anomaly_model, test) # proqnoz vermək

recon_error <- as.data.frame(recon_error) # data frame-ə çevirmək

recon_error %>% tibble::as.tibble() %>% head(.,20) # ilk 2o-ə nəzər yetirmək

plot.ts(recon_error) # qrafik yaratmaq

anomaly <- h2o.anomaly(anomaly_model, test) %>% # proqnoz etmək
  as.data.frame() %>% # data frame- çevirmək
  tibble::rownames_to_column() %>% # sətr nömrələri ilə yeni sütun yaratmaq
  mutate(Class = as.vector(test[31])) # orijinal datada olan Class sütununu yeni dataya əlavə etmək


anomaly <- anomaly %>%
  mutate(outlier = ifelse(Reconstruction.MSE > 0.006905535 , "outlier", "no_outlier"))

anomaly %>%
  group_by(Class, outlier) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  mutate(freq2 =  n / nrow(anomaly))

ggplot(anomaly, aes(x = as.numeric(rowname), y = Reconstruction.MSE,
                    color = as.factor(Class))) +
  geom_point(alpha = 0.5,size=2) +
  geom_hline(aes(yintercept = 0.02),color = 'darkgreen') +
  scale_color_manual(values = c('red','blue'))+
  labs(x = "observation number",
       color = "Class") + theme_tq() +
facet_wrap( ~ Class)
