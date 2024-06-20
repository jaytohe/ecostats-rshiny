$( document ).ready(function() {
  Shiny.addCustomMessageHandler('erroralert', function(error) {
  // The received value (error)
  alert(error.title + "\n" + error.msg);
  });
});
