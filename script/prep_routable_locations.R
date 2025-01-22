##The input files were created in QGIS using the "join by nearest feature" 
## tool and selecting the nearest "to_id" which is a node in the dodgr
##graph

##for dist_dodgr function, dodgr accepts a list of node ids as the to 
##and from argument

library(tidyverse)

bg <- read_csv("../data/blockgroup_routable_nodeid.csv")

##not an easy way to identify which node is at the "end" of the segment, meaning
##that there will be some variability in whether each route is leaving/arriving to 
##the beginning or the end of the identified segment
simp_bg <- bg %>% group_by(fid) %>% summarise(node_id = first(to_id)) %>% select(fid, node_id)

write_csv(simp_bg, "../data/bg_simp_routable.csv")
