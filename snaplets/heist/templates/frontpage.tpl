<!DOCTYPE html>
<html>
<head>
  <title>Curr(y)gle</title>
  <meta content="text/html; charset=UTF-8" http-equiv="content-type">
  <link rel="stylesheet" type="text/css" href="./stylesheet.css" />
  <script src="./jquery-1.7.2.js"></script>
  <script src="./searching.js"></script>
  <script src="./bootstrap.js"></script>
</head>
<body>
  <div class="row-fluid">
    <div class="span12">
      <div class="hero-unit">
        <h1>
          <a href=".">Curr(y)gle</a>
        </h1>
        <p>Keep calm and curry on</p>
      </div>
    </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop">
        <div class="sidebar">S</div>
      </div>
      <div class="span9">
        <form onsubmit="return searchQuery();" class="well">
          <input id="query" type="search" value=${oldquery}
                class="search-query"
                onkeyup="getCompletions(event)" />
          <button type="submit" class="btn">Search</button>
        </form>
      </div>
    </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop">
        <div class="sidebar">S</div>
      </div>
      <div class="span9">
        <div class="well"><result/></div>
      </div>
    </div>
    <div class="row-fluid">
      <div class="span3 visible-desktop">
        <div class="sidebar">S</div>
      </div>
      <div class="span9"><pager/></div>
    </div>
    <div class="clearfix"/>
      <div class="row-fluid">
        <div class="footer-grass"></div>
        <div class="footer"></div>
      </div>
    </div>
</body>
</html>
