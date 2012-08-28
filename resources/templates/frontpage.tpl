<html>
  <head>
    <title>Curr(y)gle</title>
    <meta content="text/html; charset=UTF-8"
  http-equiv="content-type">
    <link rel="stylesheet" type="text/css" href="stylesheet.css" />
    <link rel="stylesheet" type="text/css" href="responsive.css" />
    <script src="jquery-1.7.2.js"></script>
    <script src="searching.js"></script>
    <script src="bootstrap.js"></script>
  </head>
  <body>
    <div class="row-fluid">
      <div class="span12">
	<div class="hero-unit">
	  <h1>Curr(y)gle</h1>
	  <p>Keep calm and curry on</p>
	</div>
      </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop"><div class="sidebar">S</div></div>
      <div class="span9">
	<form onsubmit="return searchQuery();" class="well">
	  <input id="query" type="search" value=$(oldquery)
             provide-data="typeahead" class="search-query"
             onkeyup="getCompletions(event)"> 
	  <button type="submit" class="btn">Search</button>
	</form>
      </div>
    </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop"><div class="sidebar">S</div></div>
      <div class="span9"><div class="well"><result/></div></div>
    </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop"><div class="sidebar">S</div></div>
      <div class="span9">
	<pager/>
      </div>
    </div>
    <div class="row-fluid">
      <div class="span12"><div class="footer">F</div></div>
    </div>
    </div>
  </body>
</html>