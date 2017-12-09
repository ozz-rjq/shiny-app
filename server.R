library(shiny)

textValue <- ""
keyValue <- ""

keyNumbArr <- integer()
txtNumbArr <- integer()
encrypted <- character()
decrypted <- character()

areEqual <- function(a, b){
  if (nchar(a) == nchar(b) && nchar(a) > 0)
    return(TRUE)
  else
    return(FALSE)
}

getLength <- function(message){
  length <- nchar(message)
  
  return(paste(length, "символ(и/ів)", sep=" "))
}

eliminateSpaces <- function(message){
  length <- nchar(message)
  result <- ""
  
  for(i in 1:length){
    char <- substr(message, start = i, stop = i)
    if(char == " ")
      next
    result <- paste(result, char, sep="")
  }
  
  result
}

loopString <- function(longerLen, shorter){
  shorterLen <- nchar(shorter)
  result <- ""
  val <- 1
  
  for (i in 1:longerLen){
    result <- paste(result, substr(shorter, start = val, stop = val), sep="")
    
    if (val%%shorterLen == 0)
      val = 1
    else
      val <- val+1
  }
  
  result
}

encodeChar <- function(char){
  numb <- 0
  for(j in 1:length(LETTERS))
    if (char == LETTERS[j])
      numb <- j
    
  numb
}

toChars <- function(arg){
  result <- character()
  
  for(i in 1:length(arg))
    for(j in 1:length(LETTERS)){
      if(as.integer(arg[i]) == j)
        result <- c(result, LETTERS[j])
    }
      
  result 
}

toStr <- function(message){
  length <- length(message)
  result <- ""
  
  for(i in 1:length){
    result <- paste(result, message[i], sep="")
  }
  
  result
}

genKey <- function(length){
  result <- integer()
  
  for(i in 1:length){
    samp <- sample(1:26, 1)
    result <- c(result, samp)
  }
  
  result <- toStr(toChars(result))
}

encrypt <- function(text, key){
  length <- nchar(text)
  
  for(i in 1:length){
    txtChar <- substr(text, start = i, stop = i)
    keyChar <- substr(key, start = i, stop = i)
    
    keyNumbArr <- c(keyNumbArr, encodeChar(keyChar))
    txtNumbArr <- c(txtNumbArr, encodeChar(txtChar))
  }
  
  encryptedNumbs <- (keyNumbArr + txtNumbArr)%%26
  
  for(i in 1:length(encryptedNumbs)){
    if(encryptedNumbs[i] == 0)
      encryptedNumbs[i] <- encryptedNumbs[i]+26
  }
  
  encrypted <- toStr(toChars(encryptedNumbs))
}

decrypt <- function(text, key){
  length <- nchar(text)
  
  for(i in 1:length){
    txtChar <- substr(text, start = i, stop = i)
    keyChar <- substr(key, start = i, stop = i)
    
    keyNumbArr <- c(keyNumbArr, encodeChar(keyChar))
    txtNumbArr <- c(txtNumbArr, encodeChar(txtChar))
  }
  
  decryptedNumbs <- (((txtNumbArr - keyNumbArr)+26)%%26)
  
  for(i in 1:length(decryptedNumbs)){
    if(decryptedNumbs[i] == 0)
      decryptedNumbs[i] <- decryptedNumbs[i]+26
  }
  
  decrypted <- toStr(toChars(decryptedNumbs))
}

shinyServer(
  function(input, output) {
    cipherType <-  reactive({input$cipher})
    text <- reactive(toupper(eliminateSpaces({input$text})))
    key <- reactive(toupper(eliminateSpaces({input$key})))
    
    run <- function(){
      if(nchar(text()) > nchar(key()) && nchar(key()) != 0){
        keyValue <- loopString(nchar(text()), key())
        textValue <- text()
      }
      else{
        textValue <- text()
        keyValue <- key()
      }
      
      if(!areEqual(textValue, keyValue))
        return("...")
      
      if(cipherType() == "encrypt")
        encrypt(textValue, keyValue)
      else
        decrypt(textValue, keyValue)
    }
  
    output$result = renderText({
       run()
    })
    
    output$length = renderText({
      if (getLength(text()) == "0 символ(и/ів)")
        ""
      else
        getLength(text())
    })
    
    output$keygen <- renderText({
      length <- nchar(text())
      randomKey <- genKey(length)
      
      if(length == 0)
        "Введіть повідомлення та натисніть кнопку \"Виконати\",
         щоб згенерувати випадковий ключ"
      else
        randomKey
    })
  }
)