#Charger packages
library(tidyverse)
library(ratdat)

#Graphique
ggplot(data=complete_old, aes(x=weight, y=hindfoot_length))+
  geom_point(color="blue")

#commande pour git
#git add analyse.R
#git status
#git commit -m "Creer le script analyse"
#git log
#git revert HEAD --> revenir au precedent script