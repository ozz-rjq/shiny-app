library(shiny)

shinyUI(fluidPage(
  htmlTemplate("www/index.html"),
  
  tags$script('
    document.querySelector("#run").onclick = function() {
      var text = document.querySelector("#text").value;
      var key = document.querySelector("#key").value;
      result = document.querySelector("#result").value;

      var radio = document.querySelectorAll("input[name=cipherType]");

      var cipher = (radio[0].checked ? "encrypt" : "decrypt");
              
      Shiny.onInputChange("text", text);
      Shiny.onInputChange("key", key);
      Shiny.onInputChange("result", result);
      Shiny.onInputChange("cipher", cipher);
    };
  ')
))
