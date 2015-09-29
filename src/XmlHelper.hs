module XmlHelper where

import qualified Data.Text      as T (pack)
import           Data.List           (intercalate)
import qualified Text.XmlHtml   as X (Node (..))

-- Return the HTML info text for the number of search results found
docCount :: Int -> X.Node
docCount count = htmlLiClass "info"
  [htmlTextNode $ unwords ["Found", show count, "docs"]]

-- Returns the HTML info text, if no resuls were found
noResults :: X.Node
noResults = htmlLiClass "info"
  [htmlTextNode "Sorry, there are no matching results."]

-- Constant for the pagination label
_previousPage :: String
_previousPage = "Prev"

-- Constant for the pagination label
_nextPage :: String
_nextPage = "Next"

htmlTextNode :: String -> X.Node
htmlTextNode = X.TextNode . T.pack

htmlDiv :: String -> [X.Node] -> X.Node
htmlDiv = makeHtml "div" ""

htmlUl :: [X.Node] -> X.Node
htmlUl = makeHtml "ul" "" ""

htmlLiClass :: String -> [X.Node] -> X.Node
htmlLiClass = makeHtml "li" ""

htmlLi :: [X.Node] -> X.Node
htmlLi = htmlLiClass ""

htmlHref :: String -> String -> [X.Node] -> X.Node
htmlHref = makeHtml "a"

htmlh3 :: [X.Node] -> X.Node
htmlh3 = makeHtml "h3" "" ""

htmlP :: [X.Node] -> X.Node
htmlP = makeHtml "p" "" ""

icon :: [X.Node] -> X.Node
icon = makeHtml "i" "" "icon-leaf"

italic :: String -> X.Node
italic str = makeHtml "i" "" "" [htmlTextNode str]

bold :: String -> X.Node
bold str = makeHtml "b" "" "" [htmlTextNode str]

br :: [X.Node]
br = [makeHtml "br" "" "" []]

-- Flexible html construction (like <attribute class="css"> </> or <a href=link class=link> </a>
makeHtml :: String -> String -> String -> [X.Node] -> X.Node
makeHtml attribute link css =
  X.Element (T.pack attribute) (href ++ cssClass)
 where cssClass = eitherOr css [] [(T.pack "class", T.pack css)]
       href     = eitherOr link [] [(T.pack "href", T.pack link)]
       eitherOr str nothing result = if str == "" then nothing else result

-- If a given list is empty, return the empty list and return content otherwise
ifNotEmpty :: Eq a => [a] -> [b] -> [b]
ifNotEmpty list content = if list == [] then [] else content

-- Distinguishes between the text for a module and a author
modOrAuthorText :: String -> String
modOrAuthorText othersAttribute
  | othersAttribute == "module" = "defined in "
  | othersAttribute == "author" = "coded by "
  | otherwise                   = ""

-- Returns the HTML node for the result's caption (<h3>)
captionHtml :: String -> [X.Node]
captionHtml text = [htmlh3 ([icon []] ++ [htmlTextNode text])]

-- Creates the HTML list item for a signature
sigHtml :: [X.Node] -> [X.Node]
sigHtml content = [htmlLiClass "signature" content]

-- Creates the HTML node for one result item
resultHtml :: String -> [X.Node] -> X.Node
resultHtml uri result = htmlDiv "" [htmlHref uri "result-item" result]

-- Creates the HTML text node for the contructors of a data
typeConsHtml :: [String] -> X.Node
typeConsHtml constr = htmlTextNode ("Constructors: " ++ intercalate ", " constr)

-- Creates the HTML list item for a given curryInfo description text
descriptionHtml :: String -> [X.Node]
descriptionHtml text = [htmlLiClass "description" [htmlTextNode text]]

-- Creates one "document" of the search results as HTML node
-- Short example:
-- <div><a href=uri class="result-item"><h3>TITLE</h3><ul><li class="description>DESCR</li></ul></a></div>
resultNode :: String -> String -> (String, String) -> String -> [(String, String)] -> X.Node
resultNode title uri (oAtt, oText) descr sig =
  resultHtml uri (captionHtml title ++
                 [htmlUl (sigHtml signature ++ othersHtml ++ descriptionHtml descr)])
 where signature = ifNotEmpty sig [typeConsHtml $ map (\(name, consSig) -> name ++ " :: " ++ consSig) sig]
       othersHtml = ifNotEmpty oText [htmlLiClass oAtt [htmlTextNode $ modOrAuthorText oAtt ++ oText]]

example :: X.Node
example =
  htmlDiv "examples" (welcome ++ br ++ howToUse ++ br ++ br ++ list ++
   [htmlUl [htmlLi mods, htmlLi inMods, htmlLi funcs, htmlLi types, htmlLi author, htmlLi nondet, htmlLi det
            , htmlLi flex, htmlLi rigid, htmlLi sig, htmlLi and1, htmlLi and2, htmlLi or1, htmlLi not1, htmlLi special]])
   -- ++ mods ++ br ++ inMods ++ br ++ funcs ++ br ++ types ++ br ++ author ++ br ++ nondet ++ br ++ det
   -- ++ br ++ flex ++ br ++ rigid ++ br ++ and ++ br ++ and2 ++ br ++ or ++ br ++ not ++ br ++ special)
 where welcome  = [htmlTextNode "Since you're already here, why don't you stay a bit and use this little Curry API search engine."]
       howToUse = [htmlTextNode "For the start, try to search for the popular function "]
                  ++ [italic "map"]
                  ++ [htmlTextNode " by typing "]
                  ++ [bold ":function map"]
                  ++ [htmlTextNode ". This restricts the search results to functions, so you won't find any module or type by this name."]
       list     = [htmlTextNode "The following listing shows you all available options to construct a search term:"]
       mods     = [bold ":module io"]
                  ++ [htmlTextNode " - searches for modules starting with "]
                  ++ [italic "io"]
                  ++ [htmlTextNode " (also "]
                  ++ [bold ":m io"]
                  ++ [htmlTextNode "), without a following name, it searches for all modules"]
       inMods   = [bold ":inModule io"]
                  ++ [htmlTextNode " - restricts the search to the module "]
                  ++ [italic "io"]
                  ++ [htmlTextNode ", but searches for all contexts (also "]
                  ++ [bold ":in io"] ++ [htmlTextNode ")"]
       funcs    = [bold ":function map"]
                  ++ [htmlTextNode " - searches for functions starting with "]
                  ++ [italic "map"]
                  ++ [htmlTextNode " (also "]
                  ++ [bold ":f map"]
                  ++ [htmlTextNode "), without a following name, it searches for all functions"]
       types    = [bold ":type either"]
                  ++ [htmlTextNode " - searches for types starting with "]
                  ++ [italic "either"]
                  ++ [htmlTextNode " (also "]
                  ++ [bold ":t either"]
                  ++ [htmlTextNode "), without a following name, it searches for all types"]
       author   = [bold ":author donald"]
                  ++ [htmlTextNode " - searches for modules with "]
                  ++ [italic "donald"]
                  ++ [htmlTextNode " as author (also: "]
                  ++ [bold ":a donald"] ++ [htmlTextNode ")"]
       nondet   = [bold ":nondet [name]"]
                  ++ [htmlTextNode " - searches for non-deterministic functions (also "]
                  ++ [bold ":nd"]
                  ++ [htmlTextNode "), a following name is optional"]
       det      = [bold ":det [name]"]
                  ++ [htmlTextNode " - searches for deterministic functions (also "]
                  ++ [bold ":d"]
                  ++ [htmlTextNode "), a following name is optional"]
       flex     = [bold ":flexible [name]"]
                  ++ [htmlTextNode " - searches for flexible functions (also "]
                  ++ [bold ":fl"]
                  ++ [htmlTextNode "), a following name is optional"]
       rigid    = [bold ":rigid [name]"]
                  ++ [htmlTextNode " - searches for rigid functions (also "]
                  ++ [bold ":ri"]
                  ++ [htmlTextNode "), a following name is optional"]
       sig      = [bold ":signature a"]
                  ++ [htmlTextNode " - searches for signature starting with "]
                  ++ [italic "a"]
                  ++ [htmlTextNode " (also "]
                  ++ [bold ":s a"] ++ [htmlTextNode ")"]
       and1      = [bold ":function map AND maybe"]
                  ++ [htmlTextNode " - searches for a function starting with map and searches for maybe in all contexts (like description or signature)"]
       and2     = [bold ":function map maybe"]
                  ++ [htmlTextNode " - does the same thing, because all parts of an query are automatically applied by "]
                  ++ [bold "AND"]
       or1       = [bold ":type either OR :function either"]
                  ++ [htmlTextNode " - "]
                  ++ [bold "OR"]
                  ++ [htmlTextNode " can be used as binary operator"]
       not1      = [bold "io NOT :module io"]
                  ++ [htmlTextNode " - searches for "]
                  ++ [italic "io"]
                  ++ [htmlTextNode " in all contexts except for modules"]
       special  = [htmlTextNode "upper character, "]
                 ++ [bold "->"]
                 ++ [htmlTextNode ", tuples, lists are all identifiers to search for signatures"]


-- Creates a htmt list item (possibly with a class attribute) with a html link
-- <li class="active"><a href=$link>$pageNumber</a></li>
pageLinks :: String -> Int -> Int -> X.Node
pageLinks query actPage pageNumber =
  htmlLiClass cssStyle [htmlHref (if cssStyle == "active" then "" else paginationLink query pageNumber) "" [htmlTextNode $ show pageNumber]]
 where cssStyle = if actPage == pageNumber then "active" else "display"

-- Concats the link for the pagination for a given query and page
paginationLink :: String -> Int -> String
paginationLink query nr = "./results?query=" ++ query ++ "&page=" ++ show nr

-- Creates the pagination
pagination :: String -> Int -> Int -> [X.Node]
pagination query actPage numberOfPages =
  previous ++ firstPage ++ map (pageLinks query actPage) pageNumbers ++ lastPage ++ next
 where previous = if actPage > 1 then page _previousPage "display"
                                 else page _previousPage "disabled"
       next     = if actPage < numberOfPages then page _nextPage "display"
                                             else page _nextPage "disabled"
       pageNumber name
         | name == _previousPage = actPage - 1
         | name == _nextPage      = actPage + 1
         | otherwise              = strToInt 0 name
       firstPage = -- add first page if it's out of range
         if head pageNumbers < 2 then []
            else page "1" "display" ++ page "..." "disabled"
       lastPage  = -- add last page if it's out of range
         if last pageNumbers > (numberOfPages-1)  then []
            else page "..." "disabled" ++ page (show numberOfPages) "display"
       link name = paginationLink query (pageNumber name)
       pageNumbers -- just show a range of 9 pages
         | numberOfPages < 10 = [1..numberOfPages]
         | actPage + 4 >= numberOfPages = [start..numberOfPages]
         | actPage < 5 = [1..9]
         | otherwise   = [(max (actPage-4) 1)..(min (actPage+4) numberOfPages)]
       start = 2 * numberOfPages - actPage - 10
       page name cssClass =
         [htmlLiClass cssClass [htmlHref (if cssClass == "display" then link name else "") "" [htmlTextNode name]]]

-- Returns the pagination as HTML node that is used in the pager splice.
mkPagerLink :: String -> Int -> Int -> [X.Node]
mkPagerLink query actPage number =
  [htmlDiv "pagination pagination-centered"
    [htmlUl (pagination query actPage number)]]

-- Converts a String to an Int, returns defaultValue if conversion fails.
strToInt :: Int -> String -> Int
strToInt def str = case reads str of
  [(i, "")] -> i
  _         -> def
