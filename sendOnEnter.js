// the jQuery function
jQuery(document).ready(function(){
  jQuery(document).keypress(function(evt){
    if (evt.keyCode == 13){
      // Enter, simulate clicking send
      jQuery('#send').click();
    }
  });
})


// the JS function
// JavaScript code snippet for loggin in with "Enter" keypress
/*
document.onkeypress = function(e) {
  var number = e.which;
  Shiny.onInputChange("pressedKey", number);
};
*/
