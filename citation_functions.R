
#load the citation network file 
load("cite_net.RData")
find_cited_paper_yearwise=function(cite_net)
{
  print("the networks of papers which cited those papers in a particular year")
  years=c("92","93","94","95","96","97","98","99","00","01","02","03")
  cited_net_yrs=list()
  for (i in 1:length(years))
  {
    #tmp_net=paste0("cite_net",years[i])
    t1=which(substr(cite_net[[2]], start = 1, stop = 2)==years[i]) #which cited papers r published in 92/93/....
    cited_net_yrs[[i]]=cite_net[t1,]
    rm(t1)
  }
  return(cited_net_yrs)
}