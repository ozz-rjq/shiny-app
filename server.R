library(shiny)

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

toStr <- function(message){
  length <- length(message)
  result <- ""
  
  for(i in 1:length){
    result <- paste(result, message[i], sep="")
  }
  
  result
}

encodeChar <- function(char){
  for(j in 1:length(LETTERS))
    if (char == LETTERS[j]){
      numb <- j
    }
  numb
}

toChars <- function(arg){
  result <- character()
  
  for(i in 1:length(arg))
    for(j in 1:length(LETTERS))
      if(as.integer(arg[i]) == j)
        result <- c(result, LETTERS[j])
      
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

shinyServer(
  function(input, output) {
    cipherType <-  reactive({input$cipher})
    text <- reactive(toupper(eliminateSpaces({input$text})))
    key <- reactive(toupper(eliminateSpaces({input$key})))
    
    keyNumbArr <- integer()
    txtNumbArr <- integer()
    encrypted <- character()
    decrypted <- character()
    
    type <- function(){
      if(!areEqual(text(), key()))
        return("...")
      
      if(cipherType() == "encrypt")
        encrypt()
      else
        decrypt()
    }
    
    encrypt <- function(){
      for(i in 1:nchar(text())){
        txtChar <- substr(text(), start = i, stop = i)
        keyChar <- substr(key(), start = i, stop = i)
          
        keyNumbArr <- c(keyNumbArr, encodeChar(keyChar))
        txtNumbArr <- c(txtNumbArr, encodeChar(txtChar))
      }
      
      encryptedNumbs <- (keyNumbArr + txtNumbArr)%%26
      
      for(i in 1:length(encryptedNumbs)){
        if(encryptedNumbs[i] == 0)
          encryptedNumbs[i] <- encryptedNumbs[i]+26
      }
      
      encrypted <- toChars(encryptedNumbs)
      
      encrypted <- toStr(encrypted)
    }
    
    decrypt <- function(){
      for(i in 1:nchar(text())){
        txtChar <- substr(text(), start = i, stop = i)
        keyChar <- substr(key(), start = i, stop = i)
        
        keyNumbArr <- c(keyNumbArr, encodeChar(keyChar))
        txtNumbArr <- c(txtNumbArr, encodeChar(txtChar))
      }
      
      decryptedNumbs <- (((txtNumbArr - keyNumbArr)+26)%%26)
      
      for(i in 1:length(decryptedNumbs)){
        if(decryptedNumbs[i] == 0)
          decryptedNumbs[i] <- decryptedNumbs[i]+1
      }
      
      decrypted <- toChars(decryptedNumbs)
      
      decrypted <- toStr(decrypted)
    }
    
    run <- function(){
      type()
    }
  
    output$result = renderText({
       run()
    })
    
    output$keygen <- renderText({
      length <- nchar(text())
      randomKey <- genKey(length)
      
      if(length == 0)
        "Натисніть кнопку \"виконати\", щоб згенерувати ключ"
      else
        randomKey
    })
    
  
    output$numberOfSymbols = renderText({
      if (getLength(text()) == "0 символ(и/ів)")
        ""
      else
        getLength(text())
    })
  }
)