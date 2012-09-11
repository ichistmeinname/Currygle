// Create Request Object
function createRequestObject(request)
{
  var browser=navigator.appName;
 
  if(browser=="Microsoft Internet Explorer")
  {
    request=new ActiveXObject("Microsoft.XMLHTTP");
  }
  else
  {
    request=new XMLHttpRequest();
  }
}

// var request;
// createRequestObject(request);

// React on Ajax Response. Insert List Items (the Word-Completions) into Suggestion List.
function handleResponseAutoCompleter(data)
{
  var output='';
//  if(http.readyState==4)
  //{
    var sugg = [];
    sugg = data;	
    var i = 0;

    var query=document.getElementById("query").value;
    var lastBlankPos=query.lastIndexOf(' ');
    if(query.length>lastBlankPos+1)
    {
      query=query.substr(0, lastBlankPos+1);
    }
    else
    {
      query="";
    }

    output += '<ul class="typeahead dropdown-menu">';
    
    console.log(sugg.map(prepareItem));
    /*for(var suggestion in suggestions)
    {
      var sugg = suggestions[suggestion]; 
      if(sugg != "")
      {
        output  += '<li><a href="#">'
                + query + sugg
                + '</a></li>';
      }
      i++;*/
      
   // }
    output += '</ul>';
    selectedSuggestion = '';
    numSuggestions = data.length;
  //}
  document.getElementById("suggestion").innerHTML=output;
  if (numSuggestions > 0) {
    document.getElementById("suggestion").style.visibility="visible";
  } else {
    document.getElementById("suggestion").style.visibility="hidden";
  }
}

function prepareItem(item) {
    if(item != "") {
        item = '<li><a href="#">'
                + item
                + '</a></li>';
        }
    return item;
}

  

var selectedSuggestion = '';
var numSuggestions = 0;

// Move over function
function suggestOverMouse(div) {
 if (selectedSuggestion != '') {
   suggestOut(document.getElementById(selectedSuggestion));
 }
 selectedSuggestion = div.id;
 div.className = 'suggest_link_over';
}

// Move over function
function suggestOver(div) {
 selectedSuggestion = div.id;
 div.className = 'suggest_link_over';
 document.getElementById("query").value = div.innerHTML;
}

// Move out function
function suggestOut(div) {
 div.className = 'suggest_link';
}

// Handle key-Up events
function keyUpHandler(e)
{
  keyIn = e.keyCode;
  if (keyIn == 38) {
    if (selectedSuggestion != '' && selectedSuggestion > 0) {
      // Up key has been pressed in the form-input.
      suggestOut(document.getElementById(selectedSuggestion));
      suggestOver(document.getElementById(selectedSuggestion - 1));
    }
  } else if (keyIn == 40) {
    // Down key has been pressed in the form-input.
    if (selectedSuggestion == '') {
      suggestOver(document.getElementById('0'));
    } else if (selectedSuggestion < numSuggestions-1) {
      suggestOut(document.getElementById(selectedSuggestion));
      suggestOver(document.getElementById(selectedSuggestion -(-1)));
    }
  } else if (keyIn == 37 || keyIn == 39) {
    // Nothing
  } else {
    // A key has been pressed in the form-input.
    // Send Ajax-Request to retrieve list of word-completions.
    var query=document.getElementById("query").value;
    var lastBlankPos=query.lastIndexOf(' ');
    if(query.length>lastBlankPos+1)
    {
      query=query.substring(lastBlankPos);
    }
    if(query.length>0)
    {
        $.get('completions?query='+query, function(data){
        handleResponseAutoCompleter(data);
        });
      // http.open('get', 'completions?query='+query, true);
      // http.onreadystatechange=handleResponseAutoCompleter;
      // http.send(null);
    } else {
      hide();
    }
  }
}

// Suggestion List Item has been clicked.
// Insert selected word-completion into form-input.
function setSuggestion(suggestion)
{
  document.getElementById("query").value=suggestion;
  hide();
  location.href="querypage?query=" + suggestion;
}
 
// Hide Suggestion List.
function hide()
{
  var sugg = document.getElementById("suggestion");
  if ( sugg.hasChildNodes() )
  {
    while ( sugg.childNodes.length >= 1 )
    {
      sugg.removeChild( sugg.firstChild );       
    } 
  }
  document.getElementById("suggestion").style.visibility="hidden";
}
