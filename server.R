#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#


library(rvest)
library(stringi)
library(purrr)
library(tidytext)
library(tidyr)
library(textstem)
library(magrittr)
library(dplyr)
library(stringr)
library(nnet)
library(ggplot2)

#function for plotting tfidf
plotting <- function(tidy_df_2,tag) {
  
  df_tf_idf <- tidy_df_2 %>%
    mutate(tags = strsplit(as.character(tags), ",")) %>% 
    unnest(tags) %>%
    filter(!is.na(tags)) %>% 
    unnest_tokens(word, description) %>% 
    filter(!word %in% stop_words$word, #removing stop words
           str_detect(word, "^[a-z']+$")) %>% #removing numbers or any other special character
    count(tags, word, sort = TRUE) %>% ungroup() %>%
    bind_tf_idf(word,tags,n)
  
  Interior <- ggplot(df_tf_idf %>% arrange(desc(tf_idf)) %>%
                       filter(tags == tag) %>% 
                       top_n(10),
                     aes(x = word, y = tf_idf)) +
    geom_bar(aes(alpha = tf_idf), 
             stat="identity", 
             fill = "#4169E1") +
    coord_flip() +
    #scale_y_continuous(limits = c(0, 0.002)) +
    labs(x = NULL, y = "tf-idf", title = tag) +
    scale_alpha_continuous(range = c(0.6, 1), guide = FALSE)
  Interior
}

shinyServer(function(input, output) {
#function for web scraping for train 
 scrape <- reactive({
   main_url <- input$main_url
    df=data.frame()
    for (year in 2012:2016)
    {
      url <- paste(substr(main_url,1,nchar(main_url)-1),"-",year,"/","consumer-reviews" , "/", sep="")
      webpage <- read_html(url)
      No_of_reviews <- as.numeric(html_text(html_nodes(webpage,'[itemprop="reviewCount"]')))
      base_url <- paste(url,"?pg=%d&nr=10","/",sep="")
      map_df(1:ceiling(No_of_reviews/10), function(i) {
        pg <- read_html(sprintf(base_url,i))
        data.frame(years = year ,
                   description  = html_text(html_nodes(pg,'[itemprop="description"]')),
                   rating = as.numeric(html_text(html_nodes(pg,'[itemprop="reviewRating"] [itemprop="ratingValue"]'))),stringsAsFactors=FALSE)
      }) -> Toyota
      df <- rbind(df,Toyota)
    }
    return(df)
  })
 #function for web scraping for test
 scrape_test <- reactive({main_url <- input$main_url
 df=data.frame()
 for (year in 2017)
 {
   url <- paste(substr(main_url,1,nchar(main_url)-1),"-",year,"/","consumer-reviews" , "/", sep="")
   webpage <- read_html(url)
   No_of_reviews <- as.numeric(html_text(html_nodes(webpage,'[itemprop="reviewCount"]')))
   base_url <- paste(url,"?pg=%d&nr=10","/",sep="")
   map_df(1:ceiling(No_of_reviews/10), function(i) {
     pg <- read_html(sprintf(base_url,i))
     data.frame(years = year ,
                description  = html_text(html_nodes(pg,'[itemprop="description"]')),
                rating = as.numeric(html_text(html_nodes(pg,'[itemprop="reviewRating"] [itemprop="ratingValue"]'))),stringsAsFactors=FALSE)
   }) -> Toyota
   df <- rbind(df,Toyota)
 }
 return(df)})
 #train snippet
  output$table <- renderDataTable({ 
    data.table(scrape()) 
  })
  
  # test snippet
  output$table1 <- renderDataTable({ 
    data.table(scrape_test())
  })
  
  # Normalized and tagged for train reactive function
  Normalized <- reactive({
    df <- scrape()
    df['review_id']=1:nrow(df)
    tidy_df <- df %>% select(review_id,years,description,rating) %>% unnest_tokens(word, description)  #unnesting the text
    trial <- tidy_df %>%
      group_by(review_id) %>%
      summarise(Normalized_text = paste(word, collapse = " ")) #unnesting and then aggregating by review id to get normalized text
    
    tidy_df_1 <- merge(df,trial,by='review_id',all.x=TRUE) #joining with the initial dataframe 
    #tagging
    tidy_df$tags[tidy_df['word']=='service'] <- 'service'
    tidy_df$tags[tidy_df['word']=='price'] <- 'price'
    tidy_df$tags[tidy_df['word']=='interior'] <- 'interior'
    tidy_df$tags[tidy_df['word']=='handling'] <- 'handling'
    
    trial <- tidy_df %>% 
      filter(!is.na(tags)) %>% 
      group_by(review_id) %>%
      summarise(tags = paste(unique(tags), collapse = ",")) #getting tags comma separated review id wise
    
    tidy_df_2 <- merge(tidy_df_1,trial,by='review_id',all.x=TRUE) # joining with the inital dataframe to get tags column
    return(tidy_df_2)
  })
  
  # Normalized and tagged for test reactive function
  Normalized_test <- reactive({
    df <- scrape_test()
    df['review_id']=1:nrow(df)
    tidy_df <- df %>% select(review_id,years,description,rating) %>% unnest_tokens(word, description)  #normalizing the text
    trial <- tidy_df %>%
      group_by(review_id) %>%
      summarise(Normalized_text = paste(word, collapse = " "))
    
    tidy_df_1 <- merge(df,trial,by='review_id',all.x=TRUE) 
    tidy_df$tags[tidy_df['word']=='service'] <- 'service'
    tidy_df$tags[tidy_df['word']=='price'] <- 'price'
    tidy_df$tags[tidy_df['word']=='interior'] <- 'interior'
    tidy_df$tags[tidy_df['word']=='handling'] <- 'handling'
    
    trial <- tidy_df %>% 
      filter(!is.na(tags)) %>% 
      group_by(review_id) %>%
      summarise(tags = paste(unique(tags), collapse = ","))
    
    tidy_df_2 <- merge(tidy_df_1,trial,by='review_id',all.x=TRUE) #answer
    return(tidy_df_2)
  })
  
  output$table2 <- renderDataTable({ 
    data.table(Normalized())
  })
  #Score
  sentiment_score <- reactive({
    df <- scrape()
    df['review_id']=1:nrow(df)
    AFINN <-get_sentiments("afinn")
    
    negative_qualifiers <- c("no", "not", "never", " without", "don't")
    
   #finding the words with negative words and reversing their score in the unigram model
     
    negative_scores <- df %>% 
      select(review_id,description) %>% 
      unnest_tokens(bigram, description, token='ngrams',n=2) %>%
      separate(bigram, c("word1","word2"), sep=" ") %>%
      mutate(word2=lemmatize_words(word2)) %>%
      inner_join(AFINN, by = c(word2="word")) %>%
      filter(word1 %in% negative_qualifiers) %>%
      count(review_id,word1,word2,score) %>% 
      group_by(review_id) %>%
      summarise(score=sum(n*score))
    
    scores_uni <- df %>% 
      select(review_id,description) %>% 
      unnest_tokens(word, description) %>%
      mutate(word=lemmatize_words(word)) %>%
      inner_join(AFINN,by="word") %>%
      group_by(review_id) %>%
      summarize(sentiment = sum(score))
    
    
    
    final_scores <- merge(scores_uni,negative_scores,by='review_id',all.x=TRUE)
    final_scores[is.na(final_scores)] <- 0
    final_scores$sentiment <- final_scores$sentiment - 2*final_scores$score + 1 #reversing the scores
    final_scores$score <- NULL #deleting the extra column 
    tidy_df_2 <- Normalized()
    tidy_df_2_score <- merge(tidy_df_2,final_scores,by='review_id',all.x=TRUE) #adding the score to the initial dataframe
    return(tidy_df_2_score)
  })
  #findind the sentiment score for test
  sentiment_test_score <- reactive({
    df <- scrape_test()
    df['review_id']=1:nrow(df)
    AFINN <-get_sentiments("afinn")
    
    negative_qualifiers <- c("no", "not", "never", " without", "don't")
    
    #finding the words with negative words and reversing their score in the unigram model
    
    negative_scores <- df %>% 
      select(review_id,description) %>% 
      unnest_tokens(bigram, description, token='ngrams',n=2) %>%
      separate(bigram, c("word1","word2"), sep=" ") %>%
      mutate(word2=lemmatize_words(word2)) %>%
      inner_join(AFINN, by = c(word2="word")) %>%
      filter(word1 %in% negative_qualifiers) %>%
      count(review_id,word1,word2,score) %>% 
      group_by(review_id) %>%
      summarise(score=sum(n*score))
    
    scores_uni <- df %>% 
      select(review_id,description) %>% 
      unnest_tokens(word, description) %>%
      mutate(word=lemmatize_words(word)) %>%
      inner_join(AFINN,by="word") %>%
      group_by(review_id) %>%
      summarize(sentiment = sum(score))
    
    
    
    final_scores <- merge(scores_uni,negative_scores,by='review_id',all.x=TRUE)
    final_scores[is.na(final_scores)] <- 0
    final_scores$sentiment <- final_scores$sentiment - 2*final_scores$score + 1 #reversing
    final_scores$score <- NULL
    tidy_df_2 <- Normalized_test()
    tidy_df_2_score <- merge(tidy_df_2,final_scores,by='review_id',all.x=TRUE)
    return(tidy_df_2_score)
  })
  # table with sentiment score
  output$table3 <- renderDataTable({ 
    data.table(sentiment_score())
  })
  # average rating compared to average sentiment score
  output$average2 <- renderTable({
    df <- sentiment_score()
    output <- data.frame(cbind(c("Average Rating","Average Sentiment Score"),c(mean(df$rating),mean(df$sentiment,na.rm = TRUE))))
    colnames(output) <- NULL
    output
  })
  
  #average tags wise
  output$average3 <- renderTable({
    df <- sentiment_score()
    tags_score <- df %>% 
      mutate(tags = strsplit(as.character(tags), ",")) %>% #to split the comma separated tags column 
      unnest(tags) %>%
      filter(!is.na(tags)) %>%
      group_by(tags) %>%
      summarize(Average_sentimentscore=mean(sentiment,na.rm=TRUE),average_rating=mean(rating))
    tags_score
  }) 
  
  #running the model and predicting the results
  output$table4 <- renderDataTable({
    df_train <- sentiment_score()
    df_test <- sentiment_test_score()
    mod <- multinom(rating ~ sentiment,data=df_train) #multinomial logistic regression
    df_test['predicted'] <- predict(mod,newdata=df_test,type='class') #predicting
    df_test
  })
  # Accuracy of the model
  output$Accuracy <- renderTable({
    df_train <- sentiment_score()
    df_test <- sentiment_test_score()
    mod <- multinom(rating ~ sentiment,data=df_train)
    df_test['predicted'] <- predict(mod,newdata=df_test,type='class')
    accuracy <- nrow(df_test[df_test['rating']==df_test['predicted'],])/nrow(df_test[!is.na(df_test$predicted),])
    output <- data.frame(c("The Accuracy is"),c(accuracy))
    colnames(output) <- NULL
    output
  })
  #TFIDF tagwise
  output$table5 <- renderDataTable({
    tidy_df_2 <- Normalized()
    df_tf_idf <- tidy_df_2 %>%
      mutate(tags = strsplit(as.character(tags), ",")) %>%  #splitting the tags into separate rows
      unnest(tags) %>%
      filter(!is.na(tags)) %>% 
      unnest_tokens(word, description) %>% 
      filter(!word %in% stop_words$word, #removing stop words
             str_detect(word, "^[a-z']+$")) %>% #removing numbers or any other special character
      count(tags, word, sort = TRUE) %>% ungroup() %>% # counting by tag and word
      bind_tf_idf(word,tags,n) #tfidf function
     df_tf_idf[order(df_tf_idf$tags,-df_tf_idf$tf_idf),]
  })
  # Visualization
  output$plot1 <- renderPlot({
    df <- Normalized()
    plotting(df,"interior")
  })
  output$plot2 <- renderPlot({
    df <- Normalized()
    plotting(df,"service")
  })
  output$plot3 <- renderPlot({
    df <- Normalized()
    plotting(df,"handling")
  })
  output$plot4 <- renderPlot({
    df <- Normalized()
    plotting(df,"price")
  })
  ##############################################################################################
  
  output$First_Page <- renderUI(HTML("<p><ul><li> I have put an extra column called review id in my dataframe for using it as a primary key for joining and grouping in my code as the review description isn't unique.   </li></p>
                                  <p><li> The sentiment score has been calculated by taking into account words that are followed by a negative quantifier such as no,not etc and reversing their score from the unigram model</li></p>
                                     <p><li> This reversal was done by subtracting twice of the score of the original word and adding 1 to this from the exisiting unigram score. Adding of 1 is just to negate the score we would consider for words like no,not etc. in the unigram model</li></p>  
                                     <p><li> I used Afinn dictionary for finding the score of the words and I used a multinomial logistic regression to train my data</li></p>
                                     <p><li> Thanks, please move on to the next tab. The data might take some time to load, please be patient. </li></p>
                                     </ul>"))
})
