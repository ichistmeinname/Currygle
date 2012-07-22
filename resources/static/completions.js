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

function getCompletions() {
  var query=document.getElementById("query").value;
  console.log("query: "+query);
  var lastBlankPos=query.lastIndexOf(' ');
  if (query.length>lastBlankPos+1)
    {
      query=query.substring(lastBlankPos);
    }
  if (query.length>0) {
      http.open('get', 'completions?query='+query);
      http.onreadystatechange=getHTTPResponse();
      http.send(null);
  } else {
      hide();
  }
}

function getHTTPResponse() {
  if (http.readyState==4) {
    var suggestions = eval(http.responseText);	
      console.log("suggestions?:"+suggestions);
    return suggestions;
  }
}