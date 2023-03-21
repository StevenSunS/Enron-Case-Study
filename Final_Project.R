library(data.table)
library(stringr)
library(tidyverse)
library(lubridate)
library(igraph)
library(tidytext)
library(tm)
library(wordcloud)

#Generate edge list
data <- fread('/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/enron_sender_receiver_email_edge_list_directed.csv',
              header = T)

email <- fread('/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/emails.csv',
               header = T)

email$date <- vapply(strsplit(email$message, "\n"), `[`, 2, FUN.VALUE=character(1))
email$day <- vapply(str_sub(email$date, 12, -22), `[`, FUN.VALUE=character(1))
email$subject <- vapply(strsplit(email$message, "\n"),
                        `[`, 5, FUN.VALUE=character(1))
email <- cbind(email, data)
email <- email %>% select(-date)
email$date <- as.Date(email$day, "%d %B %Y")
email$date <- vapply(str_sub(email$date, 1, -4), `[`, FUN.VALUE=character(1))
email$subject <- gsub("Subject: ", "", email$subject)
email$subject <- str_to_lower(email$subject)
email$subject <- gsub("re: ", "", email$subject) 
email$subject <- gsub("re:", "", email$subject) 
email$subject <- gsub("fw: ", "", email$subject) 
email$subject <- gsub("/", "", email$subject)
email$subject <- gsub("@\\S*", "", email$subject) 
email$subject <- gsub("mimeversion", "", email$subject) 
email$subject <- gsub("[[:punct:]]", "", email$subject)
email$sender_affiliation <- vapply(strsplit(email$sender, ", "), 
                                   `[`, 1, FUN.VALUE=character(1))
email$sender_affiliation <- vapply(strsplit(email$sender_affiliation, "@"), 
                                   `[`, 2, FUN.VALUE=character(1))
email$receiver_affiliation <- vapply(strsplit(email$receiver, ", "), 
                                     `[`, 1, FUN.VALUE=character(1))
email$receiver_affiliation <- vapply(strsplit(email$receiver_affiliation, "@"), 
                                     `[`, 2, FUN.VALUE=character(1))

write_csv(email, '/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/emails.csv')




#Data Processing
data$receiver_1 <- vapply(strsplit(data$receiver, ", "), 
                          `[`, 1, FUN.VALUE=character(1))

data$receiver_2 <- vapply(strsplit(data$receiver, ", "), 
                          `[`, 2, FUN.VALUE=character(1))

data$receiver_3 <- vapply(strsplit(data$receiver, ", "), 
                          `[`, 3, FUN.VALUE=character(1))

data <- cbind(data, email$day)

edgelist <- data %>%
  gather(receiver_type, receiver, receiver_1:receiver_3) %>%
  mutate(date = V2) %>%
  select(-receiver_type, -V2) %>%
  filter(is.na(receiver) == F)

edgelist$date <- as.Date(edgelist$date, "%d %B %Y")

edgelist$date <- vapply(str_sub(edgelist$date, 1, -4), `[`, FUN.VALUE=character(1))

edgelist$sender_affiliation <- vapply(strsplit(edgelist$sender, "@"), 
                                  `[`, 2, FUN.VALUE=character(1))

edgelist$receiver_affiliation <- vapply(strsplit(edgelist$receiver, "@"), 
                                    `[`, 2, FUN.VALUE=character(1))

internal <- edgelist %>%
  filter(sender_affiliation == 'enron.com' &
           receiver_affiliation == 'enron.com') %>%
  select(-sender_affiliation, -receiver_affiliation) %>%
  filter(date >= '1999-01') %>%
  filter(is.na(sender) == F & is.na(receiver) == F &
           sender != receiver)
  

internal$sender <- vapply(strsplit(internal$sender, "@"), 
                          `[`, 1, FUN.VALUE=character(1))
internal$receiver <- vapply(strsplit(internal$receiver, "@"), 
                            `[`, 1, FUN.VALUE=character(1))

month_list <- internal %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  distinct(date)


for (i in c(31:)) {
  
  if (i < 4) {
    agg_list <- internal %>%
      filter(date <= as.vector(as.matrix(month_list))[i]) %>%
      select(-date)
  } else {
    agg_list <- internal %>%
      filter(date <= as.vector(as.matrix(month_list))[i]) %>%
      filter(date >= as.vector(as.matrix(month_list))[i-3]) %>%
      select(-date)
  }
  
  tie_plot <- graph.data.frame(agg_list, directed = T)
  
  plot(tie_plot,
       vertex.size = 1,
       edge.arrow.size = 0.001,
       vertex.label.cex = 0.7,
       vertex.label.color = "black",
       vertex.label.dist = 0,
       vertex.frame.color = adjustcolor("white", alpha.f = 0),
       vertex.color = adjustcolor("white", alpha.f = 0),
       edge.color= adjustcolor('lightblue', alpha.f = 0.2),
       display.isolates = FALSE,
       vertex.label = ifelse(page_rank(tie_plot)$vector >= as.numeric(tail(sort(page_rank(tie_plot)$vector),4)[1]),
                             names(page_rank(tie_plot)$vector), NA),
       main = paste('Enron Internal Network Structure in', (as.vector(as.matrix(month_list))[i])))
}



agg_list[agg_list$sender %in% names(tail(sort(betweenness(tie_plot)),400)),]

betweenness(tie_plot)

(betweenness(tie_plot)-min(betweenness(tie_plot)))/(max(betweenness(tie_plot))-min(betweenness(tie_plot)))


#External Emails

edgelist %>%
  filter(sender_affiliation == 'enron.com' &
           receiver_affiliation != 'enron.com') %>%
  group_by(sender) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Power Manipulation
blackout <- email %>%
  filter(sender_affiliation == 'enron.com' &
           receiver_affiliation == 'enron.com') %>%
  filter(((grepl('Death', message) | grepl('death', message)) &
            (grepl('Star', message) | grepl('star', message))) |
            (grepl('Get', message) | grepl('get', message)) &
            (grepl('Shorty', message) | grepl('shorty', message)) |
            (grepl('bigfoot', message) | grepl('Bigfoot', message)) |
            grepl('Ricochet', message) | grepl('ricochet', message)|
           ((grepl('Black', message) | grepl('black', message)) &
              (grepl('Widow', message) | grepl('widow', message))))

#write_csv(blackout, '/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/blackout.csv')


blackout <- fread('/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/blackout.csv')


blackout_monthly <- blackout %>%
  group_by(date) %>%
  summarise(count = n())


blackout_edgelist <- blackout %>%
  select(sender, receiver)

blackout_edgelist$receiver_1 <- vapply(strsplit(blackout_edgelist$receiver, ", "), 
                          `[`, 1, FUN.VALUE=character(1))

blackout_edgelist$receiver_2 <- vapply(strsplit(blackout_edgelist$receiver, ", "), 
                          `[`, 2, FUN.VALUE=character(1))

blackout_edgelist$receiver_3 <- vapply(strsplit(blackout_edgelist$receiver, ", "), 
                          `[`, 3, FUN.VALUE=character(1))

blackout_edgelist_trimmed <- blackout_edgelist %>%
  gather(receiver_type, receiver, receiver_1:receiver_3) %>%
  select(-receiver_type) %>%
  filter(is.na(sender) == F & is.na(receiver) == F &
           sender != receiver)

onetimer <- blackout_edgelist_trimmed %>%
  group_by(sender) %>%
  summarise(count = n()) %>%
  filter(count <= 6)

blackout_edgelist_trimmed_2 <- blackout_edgelist_trimmed %>%
  left_join(onetimer) %>%
  filter(is.na(count) == T) %>%
  select(-count)

blackout_edgelist_trimmed_2$sender <- vapply(strsplit(blackout_edgelist_trimmed_2$sender, "@"), 
                                              `[`, 1, FUN.VALUE=character(1))

blackout_edgelist_trimmed_2$receiver <- vapply(strsplit(blackout_edgelist_trimmed_2$receiver, "@"), 
                                                `[`, 1, FUN.VALUE=character(1))


for (i in c(20:30)) {
  
  if (i < 4) {
    agg_list <- blackout_edgelist_trimmed_2 %>%
      filter(date == as.vector(as.matrix(month_list))[i]) %>%
      select(-date)
  } else {
    agg_list <- blackout_edgelist_trimmed_2 %>%
      filter(date == as.vector(as.matrix(month_list))[i]) %>%
      select(-date)
  }
  
  tie_plot <- graph.data.frame(agg_list, directed = T)
  
  plot(tie_plot,
       vertex.size = 1,
       edge.arrow.size = 0.001,
       vertex.label.cex = 0.7,
       vertex.label.color = "black",
       vertex.label.dist = 0,
       vertex.frame.color = adjustcolor("white", alpha.f = 0),
       vertex.color = adjustcolor("white", alpha.f = 0),
       edge.color= adjustcolor('lightblue', alpha.f = 0.2),
       display.isolates = FALSE,
       vertex.label = ifelse(page_rank(tie_plot)$vector >= as.numeric(tail(sort(page_rank(tie_plot)$vector),4)[1]),
                             names(page_rank(tie_plot)$vector), NA),
       main = paste('Energy Laundary Network in', (as.vector(as.matrix(month_list))[i])))
}

tie_plot <- graph.data.frame(blackout_edgelist_trimmed_2, directed = F)

plot(tie_plot,
     vertex.size = 1,
     edge.arrow.size = 0.001,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = adjustcolor("white", alpha.f = 0),
     vertex.color = adjustcolor("white", alpha.f = 0),
     edge.color= adjustcolor('lightblue', alpha.f = 2),
     display.isolates = FALSE,
     vertex.label = ifelse(page_rank(tie_plot)$vector >= as.numeric(tail(sort(page_rank(tie_plot)$vector),7)[1]),
                           names(page_rank(tie_plot)$vector), NA),
     main = 'Players Involved in Illegal Megawatt Laundering')


CEO <- email %>%
  filter(sender == 'jeff.skilling@enron.com' |
           sender == 'kenneth.lay@enron.com' |
           sender == 'andrew.fastow@enron.com')

email %>%
  filter(sender == 'andrew.fastow@enron.com')


trader <- email %>%
  filter((sender == 'tim.belden@enron.com' | receiver == 'tim.belden@enron.com') |
           (sender == 'debra.davidson@enron.com' | receiver == 'debra.davidson@enron.com') |
           (sender == 'anna.mehrer@enron.com' | receiver == 'anna.mehrer@enron.com') |
           (sender == 'holden.salisbury@enron.com' | receiver == 'holden.salisbury@enron.com') |
           (sender == 'cara.semperger@enron.com' | receiver == 'cara.semperger@enron.com') |
           (sender == 'diana.scholtes@enron.com' | receiver == 'diana.scholtes@enron.com')) 




#Quit
quit <- email %>%
  group_by(sender) %>%
  summarise(date = max(date)) %>%
  group_by(date) %>%
  summarise(count = n())


  email %>%
  filter(sender_affiliation == 'enron.com' &
             receiver_affiliation == 'enron.com') %>%
  group_by(sender) %>%
  filter(date == max(date))


#word cloud

subject_words <- email %>%
  filter(sender_affiliation == 'enron.com' &
           receiver_affiliation == 'enron.com') %>%
  select(message, date) %>%
  filter(date >= '2001-03' & date <= '2001-10')
subject_words$message <- gsub("Subject", "", subject_words$message) 
subject_words$message <- gsub("Mime-Version", "", subject_words$message) 
subject_words$message <- gsub("Content-Type", "", subject_words$message) 
subject_words$message <- gsub("xfolder", "", subject_words$message) 
subject_words$message <- gsub("Date", "", subject_words$message) 
subject_words$message <- gsub("textplain", "", subject_words$message) 
subject_words$message <- gsub("JavaMail", "", subject_words$message) 
subject_words$message <- gsub("X-cc", "", subject_words$message) 
subject_words$message <- gsub("X-Folder", "", subject_words$message) 
subject_words$message <- gsub("Message-ID", "", subject_words$message) 
subject_words$message <- gsub("X-bcc", "", subject_words$message) 
subject_words$message <- gsub("Message-ID", "", subject_words$message) 
subject_words$message <- gsub("X-FileName", "", subject_words$message) 
subject_words$message <- gsub("X-Origin", "", subject_words$message) 
subject_words$message <- gsub("X-To", "", subject_words$message) 
subject_words$message <- gsub("X-From", "", subject_words$message) 


docs <- Corpus(VectorSource(subject_words$message))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
df <- df %>% filter(freq < 400)

wordcloud(words = df$word, freq = df$freq, min.freq = 20,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


#email keywords

Incentive <- email %>%
  filter(sender_affiliation == 'enron.com' &
           receiver_affiliation == 'enron.com') %>%
  filter(grepl('bonus', message) | grepl('Bonus', message) |
           ((grepl('cook', message) | grepl('Cook', message)) &
              (grepl('book', message) | grepl('Book', message))) |
           grepl('Merit', message) | grepl('merit', message) |
           ((grepl('Stock', message) | grepl('stock', message)) &
              (grepl('Option', message) | grepl('option', message))))

#write_csv(Incentive, '/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/Incentive.csv')

Incentive <- fread('/Users/HanwenSun/Desktop/Emory MSBA/Social Network/Final Project/Incentive.csv')

Incentive_edgelist <- Incentive %>%
  filter(date >= '2001-02') %>%
  select(sender, receiver)

Incentive_edgelist$receiver_1 <- vapply(strsplit(Incentive_edgelist$receiver, ", "), 
                                       `[`, 1, FUN.VALUE=character(1))

Incentive_edgelist$receiver_2 <- vapply(strsplit(Incentive_edgelist$receiver, ", "), 
                                       `[`, 2, FUN.VALUE=character(1))

Incentive_edgelist$receiver_3 <- vapply(strsplit(Incentive_edgelist$receiver, ", "), 
                                       `[`, 3, FUN.VALUE=character(1))

Incentive_edgelist_trimmed <- Incentive_edgelist %>%
  gather(receiver_type, receiver, receiver_1:receiver_3) %>%
  select(-receiver_type) %>%
  dplyr::filter(is.na(sender) == F & is.na(receiver) == F &
                  sender != receiver)

onetimer <- Incentive_edgelist_trimmed %>%
  group_by(sender) %>%
  summarise(count = n()) %>%
  dplyr::filter(count == 1)

Incentive_edgelist_trimmed_2 <- Incentive_edgelist_trimmed %>%
  left_join(onetimer) %>%
  dplyr::filter(is.na(count) == T) %>%
  select(-count)

Incentive_edgelist_trimmed_2$sender <- vapply(strsplit(Incentive_edgelist_trimmed_2$sender, "@"), 
                                      `[`, 1, FUN.VALUE=character(1))

Incentive_edgelist_trimmed_2$receiver <- vapply(strsplit(Incentive_edgelist_trimmed_2$receiver, "@"), 
                                        `[`, 1, FUN.VALUE=character(1))

tie_plot <- graph.data.frame(Incentive_edgelist_trimmed_2, directed = T)
tie_plot_2 <- graph.data.frame(Incentive_edgelist_trimmed_2[Incentive_edgelist_trimmed_2$sender %in% 
                                          names(tail(sort(page_rank(tie_plot)$vector),15000)),], 
                               directed = T)

plot(tie_plot_2,
     vertex.size = 1,
     edge.arrow.size = 0.001,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     vertex.frame.color = adjustcolor("white", alpha.f = 0),
     vertex.color = adjustcolor("white", alpha.f = 0),
     edge.color= adjustcolor('lightblue', alpha.f = 1),
     display.isolates = FALSE,
     vertex.label = ifelse(page_rank(tie_plot)$vector >= as.numeric(tail(sort(page_rank(tie_plot)$vector),1)[1]),
                           names(page_rank(tie_plot)$vector), NA),
     main = 'Network-level Bonus Distribution After 2001-02')


Incentive_monthly <- Incentive %>%
  group_by(date) %>%
  summarise(count = n())

k_core <- data.table()
empty_cols <- c('month', 'k_core')
k_core[, empty_cols] <- NA

month_list <- internal %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  distinct(date)
  
suppressWarnings(
  for (i in c(1:43)) {
      
      if (i < 4) {
        agg_list <- internal %>%
          filter(date <= as.vector(as.matrix(month_list))[i]) %>%
          select(-date)
      } else {
        agg_list <- internal %>%
          filter(date <= as.vector(as.matrix(month_list))[i]) %>%
          filter(date >= as.vector(as.matrix(month_list))[i-3]) %>%
          select(-date)
      }
      
      tie_plot <- graph.data.frame(agg_list, directed = T)
        
        k_core <- k_core %>%
          add_row(month = as.vector(as.matrix(month_list))[i],
                  k_core = mean(coreness(tie_plot)))
  }
)


external_contacts <- email %>%
  select(sender_affiliation, receiver_affiliation, date) %>%
  filter(is.na(sender_affiliation) == F &
           is.na(receiver_affiliation) == F,
         sender_affiliation != receiver_affiliation)

external_contacts_monthly <- external_contacts %>%
  group_by(date) %>%
  summarise(count = n())


regression <- k_core %>%
  mutate(date = month) %>%
  select(-month) %>%
  left_join(external_contacts_monthly %>%
              mutate(monthly_external_contacts = count) %>%
              select(-count)) %>%
  left_join(blackout_monthly%>%
              mutate(monthly_blackout = count) %>%
              select(-count)) %>%
  left_join(quit %>%
              mutate(monthly_resign = count) %>%
              select(-count)) %>%
  left_join(Incentive_monthly %>%
              mutate(monthly_incentive = count) %>%
              select(-count))

regression[is.na(regression)] <- 0

regression_2 <- regression %>%
  filter(date >= '2000-01' & date <= '2001-01')

regression_3 <- regression %>%
  filter(date >= '2001-01' & date <= '2001-11')

summary(lm(k_core ~ monthly_resign + monthly_incentive + monthly_blackout + monthly_external_contacts,
    data = regression_3))



Incentive$date <- as.Date(Incentive$day, "%d %B %Y")

Incentive$date <- vapply(str_sub(Incentive$date, 1, -4), `[`, FUN.VALUE=character(1))

Incentive_manager <- Incentive %>%
  group_by(receiver) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

Incentive_month <- Incentive %>%
  group_by(date) %>%
  summarise(count = n())

Incentive_roles <- Incentive %>%
  group_by(sender) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

receiver_affiliation <- email %>%
  group_by(receiver_affiliation) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

sender_affiliation <- data %>%
  group_by(sender_affiliation) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



#External contacts
external_contacts <- email %>%
  select(sender_affiliation, receiver_affiliation, date) %>%
  filter(is.na(sender_affiliation) == F &
           is.na(receiver_affiliation) == F,
         sender_affiliation != receiver_affiliation) %>%
  filter((sender_affiliation == 'kslaw.com' | receiver_affiliation == 'kslaw.com') |
           (sender_affiliation == 'jpmorgan.com' | receiver_affiliation == 'jpmorgan.com') |
           (sender_affiliation == 'accenture.com' | receiver_affiliation == 'accenture.com') |
           (sender_affiliation == 'citicorp.com' | receiver_affiliation == 'citicorp.com')) %>%
  filter(date >= as.vector(as.matrix(month_list))[17] &
           date <= as.vector(as.matrix(month_list))[34]) %>%
  select(-date) %>% 
  filter(receiver_affiliation != 'enron.com>' &
                             sender_affiliation != 'enron.com>' &
           receiver_affiliation != 'citi.com' &
           sender_affiliation != 'citi.com' &
           receiver_affiliation != 'ibjbank.co.jp' &
           sender_affiliation != 'ibjbank.co.jp' &
           receiver_affiliation != 'ev1.net' &
           sender_affiliation != 'ev1.net' &
           receiver_affiliation != 'velaw.com' &
           sender_affiliation != 'velaw.com' &
           receiver_affiliation != 'aol.com' &
           sender_affiliation != 'aol.com' &
           receiver_affiliation != 'thlee.com' &
           sender_affiliation != 'thlee.com' &           
           receiver_affiliation != 'bracepatt.com' &
           sender_affiliation != 'bracepatt.com')

external_contacts <- email %>%
  select(sender_affiliation, receiver_affiliation, date) %>%
  filter(is.na(sender_affiliation) == F &
           is.na(receiver_affiliation) == F,
         sender_affiliation != receiver_affiliation) %>% 
  filter(date >= as.vector(as.matrix(month_list))[17] &
           date <= as.vector(as.matrix(month_list))[34]) %>%
  filter(receiver_affiliation != 'enron.com>' &
           sender_affiliation != 'enron.com>')

tie_plot <- graph.data.frame(external_contacts, directed = F)
tie_plot <- graph.data.frame(external_contacts[external_contacts$sender_affiliation %in% 
                                                names(tail(sort(betweenness(tie_plot)),200)),], 
                               directed = F)

plot(tie_plot,
     vertex.size = 1,
     edge.arrow.size = 0.001,
     vertex.label.cex = 1,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = adjustcolor("white", alpha.f = 0),
     vertex.color = adjustcolor("white", alpha.f = 0),
     edge.color= adjustcolor('lightblue', alpha.f = 1),
     display.isolates = FALSE,
     vertex.label = V(tie_plot)$name,
     main = 'Enron Syndicate Network')




#Phillip K. Allen, who worked on the gas desk on the West Coast, received a $4.4 million bonus; Timothy Belden, an executive on the power desk in Portland, Ore., got a $5.2 million bonus. And Mr. Arnold, who made about $700 million in trading profits for Enron in 2001, largely by trading gas on the West Coast, was paid the biggest bonus, $8 million.
#In Enron's energy-trading unit, for example, John J. Lavorato, a top executive, and John D. Arnold, a gas trader, each received cash bonuses of $8 million to keep them from leaving Enron last fall.


#Aug 14, 2001
#sherron Watkins to Ken Lay

Aug_14 <- email %>%
  filter(receiver == 'john.arnold@enron.com')
  

Stock_Option <- email %>%
  filter(grepl('bonus', subject) | grepl('Bonus', subject))

strsplit(Stock_Option$message[31], "\n")[[1]]

data$sender_affiliation <- strsplit(data$sender, "@")[[1]][2]
strsplit(data$receiver[7],  ',') %>%
  as.data.frame(col.names = 'Investors')


strsplit(email$message[2003], "\n")[[1]]

strsplit(Aug_14$message[39], "\n")[[1]]


