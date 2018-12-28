library(tm)
library(wordcloud)
library(stringr)
library(rtweet)
library(wordcloud2)
library(data.table)
library(proxy)
library(Rgraphviz)
library(graph)
library(ggplot2)

create_token(app = app,consumer_key = consumer_key,consumer_secret = consumer_secret, access_token = access_token,access_secret = access_secret)
twt=search_tweets(q="#CPNS2018 OR #cpns OR #cpns2018 OR #CPNS OR cpns",n=50000,include_rts=FALSE,type="recent",retryonratelimit = TRUE,lang="id")

## data yang telah di download
load("D:/kampus/kerja/twt/data.RData")
ts_plot(twt,by="days")
teks=twt$text 
teks=gsub("http[^[[:space:]]*","",teks)
teks=gsub("@\\w+", "", teks)
teks=gsub("[^[:alpha:][:space:]]*","",teks)
teks=gsub("[[:punct:]]", "", teks)
teks=str_replace_all(teks,"#[a-z,A-Z]*","") #menghapus hastags
teks=tolower(teks) # tolower untuk membuat huruf kapital jadi huruf kecil
kk=read.csv2(choose.files(),header=FALSE)
for (i in 1:nrow(kk)){
  teks=gsub(as.character(kk[i,1]),as.character(kk[i,2]),teks)
}
url="https://raw.githubusercontent.com/nurandi/snippet/master/data/stopwords-id.txt"

#stopword.base=as.matrix(stopwordslangs[stopwordslangs$lang=="in",2])[,1] #evaluasi lagi
stopword=readLines(url)
stopword=c(stopword,"admin","sbg","haha","klo","dasar","kompetensi","dpt","cpns","biar","dri","nih","lho","dah","terus","iii","hai","via","utk","ternyata","kah","tau","sih","kab","tahu","sdh","krn","aja","untuk","kalo","atas","min","udah","mlm","tgl","berikut","tag","kaka","dlu","mimin","blm","ayo","yah","mari","gue","yaa","ane","yuk","nya","dgn","sdh","skrg","guys","kau","dimana","yaaa","#bkngoid","jadiasn","jadi","ujiant","tahun","orang","kota","hari","amp","jam","terbaru","besok","naar","pagi","teman","mas","pak","tanggal","baru","pertama","terima","lewat","mulai","kak","pas","terimakasih","satu","masuk","mba","kemarin","sampe","soalnya","menjadi","mbak","iya","banget","udh","malam","gitu","kabupaten","seleksi","tdk","gak","calon","seleksit","org","thn","kakak","pake","jga","cma","dlm","msh","bro","taun","askmiminbkngoid")
tweets.teks.corpus <- Corpus(VectorSource(teks))
a=tm_map(tweets.teks.corpus, function(x)removeWords(x,stopword))
a=tm_map(a, stripWhitespace)
a=tm_map(a, removeNumbers)
tdm=TermDocumentMatrix(a)
tdm1=removeSparseTerms(tdm,0.999)

m=as.matrix(tdm1)
#f=as.matrix(tdm.idf)
v=sort(rowSums(m),decreasing=TRUE)
d=data.frame(word = names(v),freq=v)
ggplot(d[d[,"freq"]>=170,],aes(x=word,y=freq))+geom_bar(stat="identity",color="blue")+xlab("KATA")+ylab("JUMLAH")+coord_flip()
g=d
g[1,2]=4000
wordcloud2(g,size=1.5)
freq.terms=findFreqTerms(tdm1,lowfreq = 350)
plot(tdm1,term=freq.terms,corThreshold=0.05,weighting=T)
freq.terms.min=findFreqTerms(dtm1,lowfreq = 50,highfreq = 350)
plot(tdm1,term=freq.terms.min,corThreshold=0.05,weighting=T)

#dtm=DocumentTermMatrix(a)
#dtm1=removeSparseTerms(dtm,0.999)
#dtm.idf=weightTfIdf(dtm1)
#dtm.idf.mat=as.matrix(dtm1)
#doc.0=apply(dtm.idf.mat,2,sum)
#dtm.idf.mat=dtm.idf.mat[,!doc.0==0]
#tdm.idf.mat.norm=tdm.idf.mat/rowSum(tdm.idf.mat)
#dis.mat=dist(dtm.idf.mat,method = "cosine")
#hirarki=hclust(dis.mat)
#a=cutree(hirarki,4)

#data.klus=as.matrix(dtm1)
#data.klus=as.data.table(data.klus)
#kluster=kmeans(dtm.idf.mat,centers=4,iter.max = 50)

#kluster.plot=function(klus){
#  kluster=data.klus[kluster$cluster==klus]
#  frek=sort(colSums(kluster),decreasing=TRUE)
#  dd=data.frame(word = names(frek),freq=frek)  
#  return(dd)
#}
  
#kluster1
#wordcloud2(kluster.plot(1),size=1.5)
#kluster2
#wordcloud2(kluster.plot(2),size=1.5)
#kluster3
#wordcloud2(kluster.plot(3),size=1.5)
#kluster4
#wordcloud2(kluster.plot(4),size=1.5)

#mengambil mention bkn
indeks=function(user){
id=c()
for (i in 1:dim(twt)[1]){
  if (sum(sapply(unlist(twt$mentions_screen_name[i]),function (x) isTRUE(x==user)))==1) {id[i]=i}
else {id[i]=0}  
}
id=id[id!=0]
return(id)
}

id=indeks("BKNgoid")
#data tweet mention ke bkn
teks.to.bkn=twt$text[id]
teks.to.bkn=twt$text[id]

#plot
tes=as.data.table(twt)
tes1=tes[,.(created_at,text)]
tes1$created_at=as.Date(tes1$created_at)
tes1=tes1[,.N,by=.(created_at)]
tes2=tes[id,.(created_at,text)]
tes2$created_at=as.Date(tes2$created_at)
tes2=tes2[,.N,by=.(created_at)]
ggplot(data=tes1,aes(x=created_at,y=N))+geom_line(aes(color="Total Tweet"))+geom_line(data=tes2,aes(color="Mention ke BKN"))+xlab("tanggal")+ylab("Jumlah Tweet")+labs(color="Legend text")

tweet.user=tes[,.(created_at,screen_name,text)]
tweet.user$created_at=as.Date(tweet.user$created_at)
tweet.user1=tweet.user[,.N,by=.(screen_name)]
tweet.user1=tweet.user1[order(N,decreasing = TRUE)]
ggplot(tweet.user1[N>30],aes(fill=screen_name,x=screen_name,y=N))+geom_bar(stat="identity")+theme(axis.text.x = element_blank()) + ggtitle("Jumlah Tweet Terbanyak Berdasarkan Username")


teks.to.bkn=gsub("http[^[[:space:]]*","",teks.to.bkn)
teks.to.bkn=gsub("@\\w+", "", teks.to.bkn)
teks.to.bkn=gsub("[^[:alpha:][:space:]]*","",teks.to.bkn)
teks.to.bkn=gsub("[[:punct:]]", "", teks.to.bkn)
teks.to.bkn=str_replace_all(teks.to.bkn,"#[a-z,A-Z]*","") #menghapus hastags
teks.to.bkn=tolower(teks.to.bkn) # tolower untuk membuat huruf kapital jadi huruf kecil
for (i in 1:nrow(kk)){
  teks.to.bkn=gsub(as.character(kk[i,1]),as.character(kk[i,2]),teks.to.bkn)
}
tweets.to.bkn.teks.corpus <- Corpus(VectorSource(teks.to.bkn))
b=tm_map(tweets.to.bkn.teks.corpus, function(x)removeWords(x,stopword))
b=tm_map(b, stripWhitespace)
b=tm_map(b, removeNumbers)
tdm.bkn=TermDocumentMatrix(b)
tdm.bkn=removeSparseTerms(tdm.bkn,0.999)
mat=as.matrix(tdm.bkn)
#f=as.matrix(tdm.idf)
frek.mat=sort(rowSums(mat),decreasing=TRUE)
datamat=data.frame(word = names(frek.mat),freq=frek.mat)
wordcloud2(datamat,size=2)
freq.terms.bkn=findFreqTerms(tdm.bkn,lowfreq = 50)
plot(tdm.bkn,term=freq.terms.bkn,corThreshold=0.05,weighting=T)
findAssocs(tdm.bkn, terms = "permenpan", corlimit = 0.1)

# klustering word
#tdm.idf.bkn=weightTfIdf(tdm.bkn)
tdm.idf.bkn.mat=as.matrix(tdm.idf.bkn)
doc.0=apply(tdm.idf.bkn.mat,2,sum)
tdm.idf.bkn.mat=tdm.idf.bkn.mat[,!doc.0==0]
tdm.idf.bkn.mat=tdm.idf.bkn.mat/colSums(tdm.idf.bkn.mat)
dis.mat=dist(tdm.idf.bkn.mat,method = "cosine")
hirarki=hclust(dis.mat)
a=cutree(hirarki,3)

kluster.plot=function(klus){
kluster=mat[a==klus,]
frek=sort(rowSums(kluster),decreasing=TRUE)
dd=data.frame(word = names(frek),freq=frek)  
return(dd)
}




#kluster1
wordcloud2(kluster.plot(1),size=1.5)
kluster1=mat[a==1,]
kluster1=as.TermDocumentMatrix(kluster1,weighting = T)
freq.terms.bkn1=findFreqTerms(kluster1,lowfreq = 50)
plot(kluster1,term=freq.terms.bkn1,corThreshold=0.05,weighting=T)
#kluster2
wordcloud2(kluster.plot(2),size=1.5)
kluster2=mat[a==2,]
kluster2=as.TermDocumentMatrix(kluster2,weighting = T)
freq.terms.bkn2=findFreqTerms(kluster2,lowfreq = 50)
plot(kluster2,term=freq.terms.bkn2,corThreshold=0.05,weighting=T)
#kluster3
wordcloud2(kluster.plot(3),size=1.5)
kluster3=mat[a==3,]
kluster3=as.TermDocumentMatrix(kluster3,weighting = T)
freq.terms.bkn3=findFreqTerms(kluster3,lowfreq = 25)
plot(kluster3,term=freq.terms.bkn3,corThreshold=0.05,weighting=T)

