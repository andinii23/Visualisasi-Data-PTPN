library(ggplot2)
data = read.csv(file.choose(), header = TRUE)
summary(data)
View(data)
dataNorm <- na.omit(data)
View(dataNorm)
summary(dataNorm)

#Bivariant 
#a. Kategorik vs Kategorik
#Diagram Batang Bertumpuk
ggplot(dataNorm, aes(x = RoleID, fill = LevelID))+
  geom_bar(position = "fill") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "Proportion")
#membuat diagram batang tergesmentasi
#menambahkan label untuk setiap segmen

library(dplyr) #untuk manipulasi data
library(scales) #menambahkan label untuk setiap segmen
#membuat ringkasan dataset (manipulasi data)

plotdata <- dataNorm %>%
  group_by(RoleID, LevelID) %>%
  dplyr::summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))

#membuat diagram batang tersegmentasi
#menambahkan label untuk setiap segmen

ggplot(plotdata,
       aes(x = factor(RoleID),
           y = pct,
           fill = factor(LevelID)))+
  geom_bar(stat = "identity",
           position = "fill")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = percent)+
  geom_text(aes(label = lbl),
            size = 3,
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+ #menggunakan tama minimal
  labs(y = "Percent",
       fill = "Level",
       x = "Profesi",
       title = "Profesi di PTPN berdasarkan Level")+
  theme(axis.text.x = element_text(angle = 90))

