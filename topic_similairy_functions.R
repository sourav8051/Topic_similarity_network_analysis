
#dir_name="C:/google drive/Coding workspace/1.Updated Thesis related works 2020/1.CrossVertex Network_ICCCA2020 AND journal version/15.ICCCA conf June20/Citation network codes/citation dataset/1992"

library(foreach)
library(doParallel)

get_abstracts=function(dir_name)
{
  #https://stackoverflow.com/questions/47016169/how-to-extract-a-specific-portion-of-text-from-text-file-in-r
  #get all the .abs file and preprocess by fetching the abstracts only and save in a xls file
  setwd(dir_name)
  library(stringi)
  library(stringr)
  listoffile=list.files(pattern = "*.abs")
  newtext=matrix(data=NA,nrow = length(listoffile),ncol = 2)
  year=tail(unlist(strsplit(dir_name,"/")),1)
  print(year)
  
  for(i in 1:length(listoffile))
  {
    filetext=readLines(listoffile[i])
    filetext=paste0(filetext,collapse = " ")
    filetext=str_replace(filetext,"(\\$).*(\\$)","")
    newtext[i,1]=listoffile[i]
    newtext[i,2]=unlist(strsplit(filetext,'\\\\'))[length(unlist(strsplit(filetext,'\\\\')))-1]
    # write.table(newtext,file = listoffile[i],sep = "")
    cat(paste0(i,"\t"))
  }
  write.csv(newtext,paste0("abstracts_",as.character(year),".csv"))
  return(newtext)
}

find_keywords=function(dir_name)
{
  #https://cran.r-project.org/web/packages/textrank/vignettes/textrank.html#Identify_relevant_keywords
  library(textrank)
  library(udpipe)
  library(readr)
  year=tail(unlist(strsplit(dir_name,"/")),1)
  txt <- read_csv(paste0("abstracts_",as.character(year),".csv"))
  #txt=txt[1:20,]
  tagger <- udpipe_download_model("english")
  tagger <- udpipe_load_model(tagger$file_model)
  keywords=list()
  for (i in 1:dim(txt)[1])
  {
  txt1 <- udpipe_annotate(tagger, as.character(txt$V2[i]))
  txt1 <- as.data.frame(txt1)
  txt1$textrank_id <- unique_identifier(txt1, c("doc_id", "paragraph_id", "sentence_id"))
  sentences <- unique(txt1[, c("textrank_id", "sentence")])
  terminology <- subset(txt1, upos %in% c("NOUN", "ADJ"))
  terminology <- terminology[, c("textrank_id", "lemma")]
  head(terminology)
  #keywords[[i]][1]=txt$V1[i]
  keywords[[i]]=terminology$lemma
  cat(paste0(i,"\t"))
  }
  write.csv(unlist(keywords),paste0("all_keywords_",as.character(year),".csv"))
  return(keywords)
}



#preprocess_keywords=function(keyw)
#{
  #delete absurd or missing keywords
  #str_starts(t1,"['-./:^_{}[0-9][a-z]]")

gen_keyw_vector=function(abs,keyw)
{
  #it will gen a feature vector (binary) for all the papers of given year: abstract_XX and keywords_XX
  if (dim(abs)[1]!=length(keyw))
  {
    stop("given abstract and keywords files are not of same size")
    return(NULL)
  }
  
  all_keys=toupper(unlist(keyw))
  uniq_keys=sort(unique(all_keys))
  uniq_keys=uniq_keys[-which(str_starts(uniq_keys,"['-./:^_{}[0-9]]")==TRUE)]
  uniq_keys=uniq_keys[-which(nchar(uniq_keys)<4)]
  
  abs_vec=matrix(data=0,nrow = dim(abs)[1],ncol = length(uniq_keys))
  rownames(abs_vec)=str_replace(abs[,1],".abs","")
  colnames(abs_vec)=uniq_keys
  
  for (i in 1:length(keyw))
  {
    ky=toupper(keyw[[i]])
    abs_vec[i,which(uniq_keys %in% ky)]=1
    cat(paste0(i,"\t"))
  }
  return(abs_vec)
}

find_sim_matrix=function(mat)
{
  #given a binary matrix with n rows it will output a n*n similarity using jaccard distance
  library(philentropy)
  #jaccard(keyvec_92[1,],keyvec_92[1,],testNA = FALSE)
  sim_mat=distance(mat,method = "jaccard")
  rownames(sim_mat)=rownames(mat)
  colnames(sim_mat)=rownames(mat)
  sim_mat=1-sim_mat
  diag(sim_mat)=0
  warning("used: 1-jaccard similarity: here 0 means similar, 1 means dissimilar")
  return(sim_mat)
}

build_network=function(abs_sim)
{
  library(igraph)
  cutoff=median(unique(as.vector(abs_sim)))
  abs_sim[abs_sim>cutoff]=1
  abs_sim[abs_sim<=cutoff]=0
  g1=graph_from_adjacency_matrix(abs_sim,mode = "undirected")
  print(paste0("vertex count: ",vcount(g1),"  ","edge count:",ecount(g1)))
  return(g1)
}

find_netGDD=function(net1,net2)
{
  library(NetworkSim)
  gdd=netGDD(net1,net2)
  return(gdd)
}


#1 abstracs=get_abstracts(dir_name)
#2 keywords=find_keywords()
#3 keyvec=gen_keyw_vector(abstracs,keywords)
#4 abs_sim=find_sim_matrix(keyvec)
#5 abs_network=build_network(abs_sim)
