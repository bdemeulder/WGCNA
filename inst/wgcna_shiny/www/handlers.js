$(document).ready(function() {
  // scrol to the top of the page when navigating through tab using the buttons 
  // at the end of the page
  $('#GoTab1, #GoTab2, #GoTab3, #BackTab0, #BackTab1, #BackTab2').click( function() {
    $('html').scrollTop(0);
    $('body').scrollTop(0);
  });
  
  // reordering datTrait alert box
  Shiny.addCustomMessageHandler("subject_match", function(message) {
    alert(message.value);
  });
  
  // Showing/hiding loading div
  Shiny.addCustomMessageHandler("showWaitMessage", function(message) {
    if (message.value) {
      $("#loading").show();
    } else {
      setTimeout(function(){$("#loading").hide();},1000);
    }
  });
  
  // Disabling buttons
  Shiny.addCustomMessageHandler("disable", function(message) {
    var div = $(message.div);
    if(message.value){
      div.attr('disabled', 'true');
    }else{
      div.removeAttr('disabled');
    }
  });
  
  // Show js for the selected gene table
  Shiny.addCustomMessageHandler("show", function(message) {
    var div = $(message.div);
    if(message.value){
      div.css({"display": ""});
    }else{
      div.css({"display": "none"});
    }
  });
  
  // Alert box for invalid input parameters
  Shiny.addCustomMessageHandler("invalid", function(message) {
    alert("Invalid value for the parameter." + message );
  });
  
  // Alert box when fewer than 3 modules are defined
  Shiny.addCustomMessageHandler("only_one_module", function(message) {
    alert("Fewer than 3 modules are defined, you should probably change the values of the parameters to detect more.");
  });
  
  // Alert box when using initial module instead of merged
  Shiny.addCustomMessageHandler("nomerge", function(message) {
    alert("Using initial modules instead of merged modules.");
  });
  
  // Alert box when trying to render empty network
  Shiny.addCustomMessageHandler("nonode", function(message) {
    alert("There is no node in the Network you are trying to represent!");
  });
	
	// Alert box when uploading big dataset
  Shiny.addCustomMessageHandler("too_big", function(message) {
    alert("The dataset you are trying to upload has many features!\nBeware that the analysis may take a while.");
  });
   
  // Alert box for removing genes or samples with a lot of missing data
  Shiny.addCustomMessageHandler("missing_data", function(message) {
    alert( message.genes + "feature(s) and " + message.samples + " sample(s) were removed\ndue to many missing data (> 50%).");
  });
  
  // Alert box when finishing calculating with time
  Shiny.addCustomMessageHandler("time", function(message) {
    setTimeout(function(){alert( "Done! \nThis took "+ message.value + ".");},1000);
  });
  
  // Confirm box when trying to render huge network
  Shiny.addCustomMessageHandler("networksize",function(message) {
    if(message.value > 100){
      Shiny.onInputChange("network_answer", confirm("The network contains " + message.value + " nodes.\nAre you sure you want to display it?"));
    }else{
      Shiny.onInputChange("network_answer", true);
    }
  });
  
  // changing size of main when collapsing help 
  $("#help1").on("show.bs.collapse hide.bs.collapse", function(){
      $(".arrow-left, .arrow-right").toggleClass("arrow-left arrow-right");
      $(".main, .main_full").toggleClass("main main_full");
  });
  
  // Showing loading when plotly recalculates
  $(document).on('shiny:recalculating', function(event){
    if(['plot_GSvsMM1_plotly','plot_GSvsMM2_plotly','plot_GSvsMM3_plotly','plot_GSvsMM4_plotly'].indexOf(event.target.id) > -1){
      $("#loading").show();
    }
  });
    
  // Hiding loading when plotly finished recalculating
  $(document).on('shiny:value', function(event){
    if(['plot_GSvsMM_ui','plot_GSvsMM1_plotly','plot_GSvsMM2_plotly','plot_GSvsMM3_plotly','plot_GSvsMM4_plotly','plot_GSvsMM1', 'plot_GSvsMM2', 'plot_GSvsMM3', 'plot_GSvsMM4'].indexOf(event.target.id) > -1){
      $("#loading").hide();
    }
  }); 

  // Confirm box when quiting or resetting
  Shiny.addCustomMessageHandler("sure",function(message) {
    Shiny.onInputChange("answer_sure_"+ message.value,confirm("All unsaved work will be lost. \nAre you sure you want to " + message.value + "?"));
  });

  // Alert box if datExpr contains not only numeric values
  Shiny.addCustomMessageHandler("datExpr_num",function(message) {
    alert("Invalid expression matrice uploaded. \nIt should only contain numeric values.");
  });
});



