<html>
  <head>
    <title>Curr(y)gle</title>
    <link rel="stylesheet" type="text/css" href="stylesheet.css" />
    <script src="jquery-1.7.2.js"></script>
    <script src="searching.js"></script>
    <script src="bootstrap.js"></script>
  </head>
  <body>
    <header>
    <div class="row-fluid">
      <div class="span12">
	<div class="hero-unit">
	  <h1>Curr(y)gle</h1>
	  <p>Spice up your code</p>
	</div>
      </div>
    </div>
    </header>
    <div class="row-fluid">
      <div class="span2"> Sidebar</div>
      <div class="span10 offset2">
	<form onsubmit="return searchQuery();" class="well">
	  <input id="query" type="search" value=$(oldquery)
  provide-data="typeahead" class="search-query" onkeyup="getCompletions(event)">
	  <button type="submit" class="btn">Search</button>
	</form>
      </div>
      <div class="span10 offset2"><div class="well"><result/></div></div>
      <div class="span10 offset2">
	<pager/>
      </div>
      <div class="span12">Footer</div>
    </div>
  </body>
</html>
