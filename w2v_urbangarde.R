library(RMeCab)
library(tidyverse)
library(wordVectors)
library(tsne)
library(rvest)

dic_path <- "/Users/svpcadmin/mecab-ipadic-neologd/build/mecab-ipadic-2.7.0-20070801-neologd-20170907/mecab-user-dict-seed.20170907.csv.dic"
urban_urls_path = "http://j-lyric.net/artist/a055612/"

w2v_urbangarde <- function(w2v_txt){
  w2v_txt %>% 
    RMeCabText() %>% 
    map(function(x) 
    ifelse((x[[2]] %in% c("名詞", "形容詞", "動詞")) && 
             (!x[[3]] %in% c("数", "非自立", "代名詞","接尾")) && 
             (x[[8]] != "*"), 
           x[[8]], "")
    ) %>% 
    paste(" ", collapse = "") %>% 
    as.character() %>%
    write(file = "tf.bin", append = TRUE)
  
  model <- "tf.bin" %>% train_word2vec(
    vectors = 200, 
    window = 12,
    threads = 3
    )
  return(model)
}

get_lyrics <- function(urban_urls_path){
  urban_lyric_urls <- urban_urls_path %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    paste0("http://j-lyric.net", .) %>% 
    .[grep("artist",.)] %>% 
    .[2:length(.)]
  
  for(i in 1:length(urban_lyric_urls)){
  #for(i in 1:10){
    lyrics <- read_html(urban_lyric_urls[i]) %>% 
      html_nodes(xpath = '//*[@id="Lyric"]') %>% 
      html_text()
    urban_lyrics <- paste0(urban_lyrics, "" ,lyrics)
    print(lyrics)
    Sys.sleep(1)
  }
  return(urban_lyrics)
}

#w2v_txt <- get_lyrics(urban_urls_path)
#write.table(w2v_txt,"urbangarde.txt",col.names = F,row.names = F)
model <- w2v_urbangarde("urbangarde.txt")
#model %>% nearest_to(vector = .[["前衛"]], n = 20) %>% round(2) %>% print()
#model %>% plot()
