library(shiny)

areEqual <- function(a, b){
  if (nchar(a) == nchar(b) && nchar(a) > 0)
    return(TRUE)
  else
    return(FALSE)
}

shinyServer(
  function(input, output) {
   
    cipherType <-  reactive({input$cipher})
    text <- reactive(toupper({input$text}))
    key <- reactive(toupper({input$key}))
    
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
          
        for(j in 1:length(LETTERS))
          if (keyChar == LETTERS[j]){
            keyNumb <- j
            keyNumbArr <- c(keyNumbArr, keyNumb)
            keyNumbArr <- as.integer(keyNumbArr)
          }
        
        for(j in 1:length(LETTERS))
          if (txtChar == LETTERS[j]){
            txtNumb <- j
            txtNumbArr <- c(txtNumbArr, txtNumb)
            txtNumbArr <- as.integer(txtNumbArr)
          }
      }
      
      encryptedNumbs <- (keyNumbArr + txtNumbArr)%%26
      
      for(i in 1:length(encryptedNumbs)){
        if(encryptedNumbs[i] == 0)
          encryptedNumbs[i] <- encryptedNumbs[i]+26
      }
      
      for(i in 1:length(encryptedNumbs))
        for(j in 1:length(LETTERS))
          if(as.integer(encryptedNumbs[i]) == as.integer(j))
            encrypted <- c(encrypted, LETTERS[j])
      
      encrypted 
    }
    
    decrypt <- function(){
      for(i in 1:nchar(text())){
        txtChar <- substr(text(), start = i, stop = i)
        keyChar <- substr(key(), start = i, stop = i)
        
        for(j in 1:length(LETTERS))
          if (keyChar == LETTERS[j]){
            keyNumb <- j
            keyNumbArr <- c(keyNumbArr, keyNumb)
            keyNumbArr <- as.integer(keyNumbArr)
          }
        
        for(j in 1:length(LETTERS))
          if (txtChar == LETTERS[j]){
            txtNumb <- j
            txtNumbArr <- c(txtNumbArr, txtNumb)
            txtNumbArr <- as.integer(txtNumbArr)
          }
      }
      
      decryptedNumbs <- (((txtNumbArr - keyNumbArr)+26)%%26)
      
      for(i in 1:length(decryptedNumbs)){
        if(decryptedNumbs[i] == 0)
          decryptedNumbs[i] <- decryptedNumbs[i]+1
      }
      
      for(i in 1:length(decryptedNumbs))
        for(j in 1:length(LETTERS))
          if(as.integer(decryptedNumbs[i]) == as.integer(j))
            decrypted <- c(decrypted, LETTERS[j])
      
      decrypted
    }
  
    output$result = renderText({
       type()
    })
  }
)