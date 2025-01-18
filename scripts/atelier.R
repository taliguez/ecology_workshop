### Atelier R - worskshop ecology ####
library(tidyverse) #regroupement de package
library(ratdat) #library de jeu de données dans le désert d'arizona sur des souris 

### exploration de donnees
?complete_old
summary(complete_old)
head(complete_old) #tibble --> dataframe contenant plus d'info
str(complete_old)

### ggplot
library(ggplot2) #ggplot2 est dans tidyverse gg=grammar of graphics

##format de base
#ggplot(data=data, mapping=aes(variables))+
  #geom_function()+

#By step
#ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length))

ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length))+
  geom_point(alpha=0.2) ##alpha=niveau de transparence

complete_old<-filter(complete_old, !is.na(weight) & !is.na(hindfoot_length)) #filter = fonction de tydiverse


ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length))+
  geom_point(alpha=0.1, color="blue") ##alpha=niveau de transparence


ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length, color=plot_type))+ #plot_type=variable du dataset
  geom_point(alpha=0.2) ##alpha=niveau de transparence


## forme point varie selon le sexe (sex)
ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length, shape=sex))+ #plot_type=variable du dataset
  geom_point(alpha=0.2) ##alpha=niveau de transparence

table(complete_old$sex)

## couleur varie en fonction des années
ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length, color=year))+ #plot_type=variable du dataset
  geom_point(alpha=0.2) ##alpha=niveau de transparence

###avoir meilleur couleur
ggplot(data=complete_old, mapping=aes(x=weight, y=hindfoot_length, color=plot_type))+ #plot_type=variable du dataset
  geom_point(alpha=0.2) + ##alpha=niveau de transparence
  scale_color_viridis_d() + #change l'échelle de la variable color
  scale_x_log10() #change directement la variable en log10

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot() 

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot() +
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot() +
  geom_point()+
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot() +
  geom_jitter()+
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length, color=plot_type))+
  geom_boxplot() +
  geom_jitter(alpha=0.1)+
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot() +
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète

ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète


ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape=NA, fill=NA) +
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète


###faire un violin diagram 
ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_violin(fill=NA) +
  scale_x_discrete(labels=label_wrap_gen(width=10)) #variable discrète



ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape=NA, fill=NA) +
  scale_x_discrete(labels=label_wrap_gen(width=10))+ #variable discrète
  theme_bw() + #black and white 
  theme(legend.position="none")+ #theme permet de changer specifiquement le theme
  labs(x="Plot type", y="Hindfoot length (mm)") #changer le nom des axes

#faire en deux tableaux
plot_final<-ggplot(complete_old, mapping=aes(x=plot_type, y=hindfoot_length))+
  geom_jitter(alpha=0.1, aes(color=plot_type))+
  geom_boxplot(outlier.shape=NA, fill=NA) +
  facet_wrap(vars(sex), ncol=1)+ #ou nrow=1 si préfères cote à cote
  scale_x_discrete(labels=label_wrap_gen(width=10))+ #variable discrète
  theme_bw() + #black and white 
  theme(legend.position="none")+ #theme permet de changer specifiquement le theme
  labs(x="Plot type", y="Hindfoot length (mm)") #changer le nom des axes


#on peut faire plot_final + pour ajouter des lignes de commande à plot_final

ggsave(filename="figures/plot_final.png", plot=plot_final, height=6, width=8)


#### PARTIE 2 - TIDYVERSE ####

surveys<-read_csv(file="data/raw/surveys_complete_77_89.csv") #diff avec read_csv ?
#read_csv() will always read variables containing text as character variables. In contrast, the base R function read. csv() will, by default, convert any character variable to a factor. 
View(surveys)
str(surveys)

#select() -> colonnes
#filter() -> lignes
#mutate() -> créer colonnes
#group_by()
#summarize

####select
select(surveys, plot_id, species_id)
select(surveys, c(3,4)) #selectionne les mêmes colonnes par position
select(surveys, -plot_id) #select every col without plot_id
select(surveys, where(is.numeric)) #on prend toutes les variables qui sont numeric 
select(surveys, where(anyNA)) #donne toutes les colonnes avec au moins un NA

####filter
filter(surveys, year==1988) #selectionne toutes les lignes avec l'année 1988
filter(surveys, species_id %in% c("RM", "DO")) #regarde toutes les species id qui font partie de l'un ou l'autre
filter(surveys, year==1988 & species_id %in% c("RM", "DO")) #selectionne les deux conditions

#données entre 1980 et 1985 et variables year, month, species_id, plot_id
filter(surveys, between(year, 1980, 1985))
filter(surveys, year>=1980 & year<= 1985) #idem
select(surveys, year, month, species_id, plot_id)

#première façon: créer un objet
surveys_80_85<-filter(surveys, year>=1980 & year<= 1985)
select(surveys_80_85, year, month, species_id, plot_id)
##peut avoir bcp d'objet et c'est peut etre pas super

#Deuxième façon: 
select(filter(surveys, year>=1980 & year<= 1985),year, month, species_id, plot_id)
#on peut ne rien voir si bcp d'arguments

##Troisieme facon: pipe Control shift M --> raccourci 
surveys %>% #pipe combine des lignes de codes
  filter(year== 1980:1985) %>% 
  select(year, month, species_id, plot_id)

#le pipe permet de mettre surveys en argument de chaque début de code 


surveys %>% 
  filter(year==1988) %>% 
  select(record_id, month, species_id)


####mutate
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000,
         weight_lbs=weight_kg*2.2) %>%  #crée une nouvelle colonne à partir du ancienne
  relocate(weight_kg, .after=record_id) %>% 
  relocate(weight_lbs, .after=weight_kg) #change l'ordre des colonnes

#faire une seule colonne de date
surveys %>% 
  mutate(date=paste(year, month, day, sep="-")) %>% 
  relocate(date, .after=year)

library(lubridate) #library qui permet de traiter les dates : ymd --> transforme character en date

surveys %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  relocate(date, .after=year)

surveys %>% 
  group_by(sex) %>% 
  summarize(mean.weight=mean(weight, na.rm=TRUE), 
            count=n()) #count=nb d'occurences

#challenge
challenge<- surveys %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count=n())

View(challenge)  

ggplot(challenge, mapping=aes(x=date, y=count, color=sex))+
  geom_line()+
  theme_bw() + #black and white 
  labs(x="Date", y="Nb observations") #changer le nom des axes

#On peut tout mettre d'un coup : 
surveys %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count=n()) %>% 
  ggplot(mapping=aes(x=date, y=count, color=sex))+
  geom_line()+
  theme_bw() + #black and white 
  labs(x="Date", y="Nb observations") #changer le nom des axes

#pour enlever les na
surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  group_by(sex, date) %>% 
  summarize(count=n()) %>% 
  ggplot(mapping=aes(x=date, y=count, color=sex))+
  geom_line()+
  theme_bw() + #black and white 
  labs(x="Date", y="Nb observations") #changer le nom des axes

