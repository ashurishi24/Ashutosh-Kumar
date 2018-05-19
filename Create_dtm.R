
create_DTM = function(text)
{
  
  
  require(tibble)
  textdf = data_frame(text = text) # yields 120x1 tibble. i.e., each doc = 1 row here.
  
  # Tokenizing ops. Words first.
  textdf %>% unnest_tokens(word, text)   # try ?unnest_tokens
  
  
  # First, build a datafame
  ak_nokia <- textdf %>% 
    unnest_tokens(word, text) %>%     # word tokenization 
    anti_join(stop_words)    # run ?join::dplyr 
  
  # First convert Nokia corpus to tidy format, i.e. a tibble with token and count per row per document.
  
  tidy_nokia = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  
  tidy_nokia
  
  nokia_dtm <- tidy_nokia %>% cast_sparse(doc, word, n)
  # class(nokia_dtm)  # Matrix
  return(nokia_dtm)
  
}
