<html>
  <head>
    <title>Curr(y)gle</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
    <script type="text/javascript" src="ajax.js"></script>
  </head>
  <body>
    <div class="header"/>
    <table cellspacing="0" cellpadding="0">
      <tr>
        <td class="contentTd">
          <div class="content">
            <div class="form">
              <form action="/querypage">
                <input id="query" name="query" type="text" autocomplete="off"
                       value=$(oldquery) onblur="hide()" onkeyup="keyUpHandler(event)"/>
                <input class="button" name="button" type="submit" value="Suchen"/>
                <div id="suggestion"/>
              </form>
            </div>
            <div class="result">
              <result/>
            </div>
            <div class="pager">
              <pager/>
            </div>
          </div>
        </td>
        <td class="siderTd">
          <div class="sider"/>
        </td>
      </tr>
    </table>
    <div class="footer">
      Powered by
      <a class="footerHref" href="http://www-ps.informatik.uni-kiel.de/currywiki/g">Curry</a>,
      <a class="footerHref" href="https://github.com/fortytools/holumbus">Holumbus</a> &amp;
    </div>
  </body>
</html>
