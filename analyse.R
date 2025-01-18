#Charger packages
library(tidyverse)
library(ratdat)

#Graphique
ggplot(data=complete_old, aes(x=weight, y=hindfoot_length))+
  geom_point(color="red2", alpha=0.2)

#commande pour git
#git add analyse.R
#git status
#git commit -m "Creer le script analyse"
#git log