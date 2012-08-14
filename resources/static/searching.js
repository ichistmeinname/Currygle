
function createRequestObject() {
  var ro;
  var browser=navigator.appName;
 
  if(browser=="Microsoft Internet Explorer") {
    ro=new ActiveXObject("Microsoft.XMLHTTP");
  } else {
    ro=new XMLHttpRequest();
  }
  return ro;
}
  
var http=createRequestObject();

function searchQuery() { 
  location.href="/currygle?query="
   +encodeURIComponent(document.getElementById("query").value);
  return false;
}

function getCompletions(e) {
  var query=encodeURIComponent(document.getElementById("query").value);
  var autocomplete=$('input').typeahead();
  switch(e.keyCode) {
    case 9: // tab
    case 13: // enter

    return;
     
    case 27: // escape
        //autocomplete.data('typeahead').hide();
    return;
    case 37: // arrows
    case 38:
    case 39:
    case 40:    
    e.preventDefault()
        //autocomplete.data('typeahead').keyup(e)
    return;
   }
  var lastBlankPos = query.lastIndexOf(' ');
  if (query.length>lastBlankPos+1) {
      query=query.substring(lastBlankPos);
    }
  if (query.length>0) {
      http.open('get', 'completions?query='+query);
      http.onreadystatechange=getHTTPResponse;
      http.send(null);
  }
}

function getHTTPResponse() {
  if (http.readyState==4) {
      var suggestions = eval(http.responseText);
      var autocomplete=$('input').typeahead();
      autocomplete.data('typeahead').source=suggestions;
      autocomplete.data('typeahead').lookup();
  }
}