<!DOCTYPE html>
<html>
<head>
  <title>Curr(y)gle</title>
  <meta content="text/html; charset=UTF-8" http-equiv="content-type">
  <link rel="stylesheet" type="text/css" href="./static/bootstrap.min.css" />
  <link rel="stylesheet" type="text/css" href="./static/stylesheet.css" />
  <script src="./static/jquery-1.7.2.js"></script>
  <script src="./static/bootstrap.js"></script>
  <script src="./static/bloodhound.js"></script>
  <script src="./static/typeahead.bundle.js"></script>
  <script src="./static/typeahead.jquery.js"></script>
  <script src="./static/searching.js"></script>
</head>
<body>
  <div class="header color1">
    <div id="title">
     <h1>
       <a href=".">Curr(y)gle</a>
     </h1>
     <p>Keep calm and curry on</p>
    </div>
    <form onsubmit="return searchQuery();">
     <div id="query">
       <input id="query-input" type="text" value=${oldquery}
         class="search-query typeahead" />
       <input type="submit" class="btn search-btn" value="Search"/>
     </div>
    </form>
    <div style="clear:both"/>
  </div>
  <div class="content">
      <div class="results centered"><result/></div>
  </div>
  <div class="pagination centered"><pager/></div>
  <div class="footer color1">
    <p class="footer-text">© <a
    href="https://github.com/ichistmeinname">Sandra Dylus</a>
    2012-2015<br/>
    powered by <a href="www.haskell.org"><img class="haskell-logo" src="./static/haskell.png" /></a></p>
  </div>
</body>
</html>
