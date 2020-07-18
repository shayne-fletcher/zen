module Html(HTML, text, unMk) where

import Data.Coerce

newtype HTML = Mk String

unMk :: HTML -> String
unMk (Mk s) = s

text :: String -> HTML
text s = Mk (escapeSpecialCharacters s)

linesH :: HTML -> [HTML]
linesH = coerce lines

escapeSpecialCharacters = undefined
