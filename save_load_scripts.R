#delete the abs_sim network to reduce the workspace
save(abs_sim_00,abs_sim_01,abs_sim_02,abs_sim_03,abs_sim_92,abs_sim_93,abs_sim_94,abs_sim_95,abs_sim_96,abs_sim_97,abs_sim_98,abs_sim_99,file ="all_abs_sim_matrix.RData")
rm(abs_sim_00,abs_sim_01,abs_sim_02,abs_sim_03,abs_sim_92,abs_sim_93,abs_sim_94,abs_sim_95,abs_sim_96,abs_sim_97,abs_sim_98,abs_sim_99)

#save and delete all the abstracts file here
save(abstracs_00,abstracs_01,abstracs_02,abstracs_03,abstracs_92,abstracs_93,abstracs_94,abstracs_95,abstracs_96,abstracs_97,abstracs_98,abstracs_99,file = "all_abstracts_data.RData")
rm(abstracs_00,abstracs_01,abstracs_02,abstracs_03,abstracs_92,abstracs_93,abstracs_94,abstracs_95,abstracs_96,abstracs_97,abstracs_98,abstracs_99)

#save and delete the keyvectors of each abstracts
save(keyvec_00,keyvec_01,keyvec_02,keyvec_03,keyvec_92,keyvec_93,keyvec_94,keyvec_95,keyvec_96,keyvec_97,keyvec_98,keyvec_99,file = "all_keyvectors.RData)" )
rm(keyvec_00,keyvec_01,keyvec_02,keyvec_03,keyvec_92,keyvec_93,keyvec_94,keyvec_95,keyvec_96,keyvec_97,keyvec_98,keyvec_99)

#save and delete keywords_xx files
save(keywords_00,keywords_01,keywords_02,keywords_03,keywords_92,keywords_93,keywords_94,keywords_95,keywords_96,keywords_97,keywords_98,keywords_99,file = "all_keywords.RData")
rm(keywords_00,keywords_01,keywords_02,keywords_03,keywords_92,keywords_93,keywords_94,keywords_95,keywords_96,keywords_97,keywords_98,keywords_99)

#save citetation matrix
library(readr)
cite_net <- read_table2("Cit-HepPh.txt", 
                        +     col_names = FALSE, col_types = cols(X1 = col_character(), 
                                                                  +         X2 = col_character()), skip = 4)
View(cite_net)
colnames(cite_net)=c("from","to")
for (i in 1:2)
{
  tmp=which(nchar(cite_net[[i]])==4)
  cite_net[[i]][tmp]=paste0("000",cite_net[[i]][tmp])
  
  tmp=which(nchar(cite_net[[i]])==5)
  cite_net[[i]][tmp]=paste0("00",cite_net[[i]][tmp])
  
  tmp=which(nchar(cite_net[[i]])==6)
  cite_net[[i]][tmp]=paste0("0",cite_net[[i]][tmp])
  cat(paste0(i,"\t"))
}
save(cite_net,file = "cite_net.RData")

#save_cited_network for each year
cited_paper_network=find_cited_paper_yearwise(cite_net)
save(cited_paper_network,file = "cited_papers_network.RData")