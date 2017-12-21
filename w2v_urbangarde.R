library(RMeCab)
library(tidyverse)
library(wordVectors)
library(tsne)
dic_path <- "/Users/svpcadmin/mecab-ipadic-neologd/build/mecab-ipadic-2.7.0-20070801-neologd-20170907/mecab-user-dict-seed.20170907.csv.dic"

w2v_urbangarde <- function(w2v_txt){
  tf <- tempfile()
  w2v_txt %>% RMeCabText(dic = dic_path) %>% map(function(x) 
    ifelse((x[[2]] %in% c("名詞", "形容詞", "動詞")) && 
             (!x[[3]] %in% c("数", "接尾")) && 
             (x[[8]] != "*"), 
           x[[8]], "")
    ) %>% paste(" ", collapse = "") %>%
  write(file = tf, append = TRUE)
  model <- tf %>% train_word2vec("tf.bin", min_count = 2)
  unlink("tf.bin")
  unlink(tf)
  return(model)
}

#get_lyrics(){
  # なんらかの方法で歌詞を取ってきて一つのテキストファイルに追加していく。
  # 敬意を込めてtxtファイルに手打ち...しようと思いましたが、さすがに断念しました。
#}

w2v_txt <- "urbangarde.txt"
model <- w2v_urbangarde(w2v_txt)

#model %>% nearest_to(vector = .[["前衛"]], n = 20) %>% round(2) %>% print()

model %>% plot()