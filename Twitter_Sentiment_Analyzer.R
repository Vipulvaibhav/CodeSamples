tdata <- as.data.frame(c()) #to store the model learning matrix
mymodel <- c() # to store the model globally
hdt <- c() # store Hoffending tree
load("my_oauth.Rdata") # loading the assumed pre-built authorization key values file to connect with twitter stream
#-------------------------------------------------------------------------------#
#It is assumed that the my_oauth.Rdata file has already been created using your tokens with following commands:
# requestURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#consumerKey <- "<your consumer key>"
#consumerSecret <- "<your consumer secret key>"
#my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret,requestURL = requestURL, accessURL = accessURL, authURL = authURL)
#save(my_oauth, file = "my_oauth.Rdata")
#---------------------------------------------------------------------------------#

#the following function assumes that the environment is cleaned and hence installs all the required packages and loads them in the environment
Initial_setup <- function()
{
  install.packages("streamR")
  install.packages("ROAuth")
  install.packages("tm")
  install.packages("stringr")
  install.packages("devtools")
  install.packages("Stack")
  library("streamR")
  library("ROAuth")
  library("tm")
  library("stringr")
  library("devtools")
  library("Stack")
  require(RMOA)
  #Below we establish the connection one time using a popup window where the user needs to authorize the application with a one time token
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  #call function to start streaming the tweets with a counter value 1 indicating its first ever run of this model
  create_data(1)
}
#The below function creates dataframe containing the received tweets
create_data <- function(counter)
{
  counter <- counter
  #Getting tweets with keywords love and hate
  filterStream(file.name = "tweets_love.json", track = c("love"),follow = NULL, locations = NULL, language = NULL, timeout = 10, tweets = 500, oauth = my_oauth, verbose = TRUE)
  filterStream(file.name = "tweets_hate.json", track = c("hate"),follow = NULL, locations = NULL, language = NULL, timeout = 10, tweets = 500, oauth = my_oauth, verbose = TRUE)
  #parsing the tweets received to remove geographiccal attributes
  tweet_love <- parseTweets("tweets_love.json", simplify = TRUE, verbose = TRUE)
  tweet_hate <- parseTweets("tweets_hate.json", simplify = TRUE, verbose = TRUE)
  #merging the two dataframes 
  tweets_write <- Stack(tweet_love,tweet_hate)
  t_data <- as.data.frame(tweets_write)
  #passing the data to the function to get cleaned and used in model for training if it is first run or prediction in case it is a test data
  test_data(t_data,counter)
}
#Function to clean and test/train classification model
test_data <- function(datframe,counter)
{
  #removing the files created while streaming tweets in order to save disk space
  file.remove("tweets_love.json")
  file.remove("tweets_hate.json")
  counter <- counter
  tweet_love <- datframe
  tweets_new_love <- c()
  #creating a matrix to store the tweet texts as they are cleaned of everything except alphabets
  out <- matrix(NA, nrow=nrow(tweet_love), ncol=1)
  for(i in 1:nrow(tweet_love))
  {
    str <- tweet_love$text[i]
    str2 <- c(str_replace_all(str,"[^[:alpha:]]"," "))  #removing non-alphabetic things from the text column of tweets
    out[i,] <- str2
  }
  #Creating a corpus object from the cleaned matrix object  
  tweetLove_corpus <- VectorSource(out)
  tweets_love <- VCorpus(tweetLove_corpus, readerControl = list(reader = readPlain,language = "en"))
  tweets_love <- tm_map(tweets_love, content_transformer(tolower)) #converting the text to lowercase  
  tweets_love <- tm_map(tweets_love, content_transformer(removePunctuation))  #removing any left over punctuations
  tweets_love <- tm_map(tweets_love, content_transformer(stripWhitespace)) #stripping off white spaces introduced in the steps above
  # Removing the stop words from each tweet and then storing the result as separate documents in the VCorpus object
  for(i in 1:nrow(tweet_love))
  {
    tweets_love[[i]] <- removeWords(tweets_love[[i]], stopwords("english"))
    tweets_love[[i]] <- stemDocument(tweets_love[[i]]) #stemming the text finally
  }
  tdm_love <- TermDocumentMatrix(tweets_love) #Creating the term document matrix for all the tweets stored as documents
  #Converting the termdocumentmatrix into a matrix and then into a data frame for further analytics
  td <- as.matrix(tdm_love)
  df2 <- as.data.frame(td)
  #transposing the matrix so that columns are words and rows are the tweets
  df1 <- as.data.frame(t(df2))
  #Checking if this is the first run and hence the model needs to be built
  if(counter==1)
  {
    df1["Total" ,] <- colSums(df1) #Calculating the sum of occurence of each words in all tweets 
    sum_vec <- df1["Total",] 
    res <- sort(sum_vec,decreasing = TRUE, method = "quick", index.return=TRUE) #Sorting the calculated totals
    result <- res[,21:ncol(res)] #We only keep top 20 freuquent words
    df1 <- df1[ , -which(names(df1) %in% colnames(result))] # eliminating the columns other than top 20 words
    #Assigning class to each tweet based on whether it has word hate or love?
    for(i in 1:nrow(df1))
    {
      if(df1$love[i]!=0)
      {
        df1$class[i]="Love"
      }
      else
      {
        df1$class[i]="Hate"
      }
    }
    #Removing the column corresponding to the word love and hate
    df1 <- df1[ , -which(names(df1) %in% c("love","hate"))]
    df1 <- df1[-c(nrow(df1)),] #deleting the total row created to find the top 20 words
    tdata <<- df1 #storing this dataframe to the global variable
    df1$class <- as.factor(df1$class)
    #Creating the model using Hoeffding decision tree
    hdt <<- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
    #Factorizing the input data frame
    df1 <- factorise(df1)
    #creating the data stream
    df1datastream <- datastream_dataframe(data=df1)
    #Creating and storing the model as a global model so that the same model is used on test data
    mymodel <<- trainMOA(model = hdt, formula = class~., data = df1datastream, reset = FALSE)
    #running the model to predict on training data
    scores <- predict(mymodel, newdata=df1, type="response")  
    #Storing the confusion matrix
    ct <- table(df1$class, scores)
    print(ct)
    #calling function to calculate the performance matrices
    calMetric(ct)
  }
  else #if the model is already trained
  {
    #assigning the actual classes
    for(i in 1:nrow(df1))
    {
      if(df1$hate[i]!=0)
      {
        df1$class[i]="Hate"
      }
      else
      {
        df1$class[i]="Love"
      }
    }
    #passing the input matrix to get the intersection of input features with model features
    dftest_rec <- get_similarity(df1)
    dftest <- dftest_rec[ , -which((names(dftest_rec) %in% c("class")))] #removing the class column from test data
    dftest <- factorise(dftest)
    #predicting the class using the model
    scores <- predict(mymodel, newdata=dftest, type="response")  
    str(scores)
    #storing the confusion matrix
    ct <- table(dftest_rec$class, scores)
    print(ct)
    #calculating the performance matrices
    calMetric(ct)
    #dftest$class <- df1$class
    dftest_rec <- factorise(dftest_rec)
    #creating the data stream using the input data with true class
    dftestdatastream <- datastream_dataframe(data=dftest_rec)
    #using the test data as training data to feedback the matrix with new data
    mymodel <<- trainMOA(model = hdt, formula = class~., data = dftestdatastream, reset = FALSE)
  }
  #Caling for next batch of tweets
  create_data(2)
}
#this function uses the confusion matrix and calculates and prints the various performance matrices
calMetric <- function(ct)
{
  if(dim(ct)[2]==2)
  {
  TP <- ct[1,1]
  FP <- ct[2,1]
  TN <- ct[2,2]
  FN <- ct[1,2]
  }
  else
  {
    TP <- 0
    FP <- ct[1,1]
    TN <- ct[2,1]
    FN <- (0)
  }
  Accuracy <- (TP+TN)/(TP+TN+FP+FN)
  Precision <- TP/(TP+FP)
  Recall <- TP/(TP+FN)
  F1Measure <- 2*Precision*Recall/(Precision + Recall)
  Sensitivity <- TP/(TP+FN)
  Specificity <- TN/(TN+FP)
  cat("Accuracy:",Accuracy,"\nPrecision:",Precision,"\nRecall:",Recall,"\nF1-Measure:",F1Measure,"\nSensitivity:",Sensitivity,"\nSpecificity:",Specificity)
}
#this function does the intersection of the model features with the input data features
get_similarity <- function(datafrm)
{
  test_data <- datafrm
  # removing words which are not used as features by the model
  test_data <- test_data[ , -which(!(names(test_data) %in% colnames(tdata)))] 
  col_name <- colnames(test_data)
  col_name_feature <- names(tdata)
  #adding the words used by model if they are missing and assigning that word count to 0
  for(i in 1:length(col_name_feature))
  {
    if(!(col_name_feature[i] %in% col_name))
    {
      test_data$col_name_feature[i] <- c(0)
    }
  }
  return(test_data)  
}
#The start function called to be executed on running the script
Initial_setup()