score.sentiments=function(results,pos.words,neg.words,.progress="none")
{
  require(plyr)
  require(stringr)
  #results is the resulting dataframe
  scores=laply(results,function(results,pos.words,neg.words){
  results=gsub('[[:punct:]]','',results)
  results=gsub('[[:cntrl]]','',results)
  results=gsub('\\d+','',results)
  results=tolower(results)
  word.list=str_split(results,'\\s+')
  words<-unlist(word.list)
  pos.matches<-match(words,pos.words)
  neg.matches<-match(words,neg.words)
  score<-sum(pos.matches-neg.matches)
  return(score)
  },pos.words,neg.words,.progress-.progress)
  scores.df<-data.frame(score=scores,text=results)
  return(scores.df)
   }



