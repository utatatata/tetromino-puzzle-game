module Engine.CLI where

import Prelude
import Ansi.Codes (EscapeCode(..), escapeCodeToString)
import Data.Array as A
import Data.Foldable (intercalate)
import Data.List (range)
import Data.Map as M
import Data.Maybe (maybe)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Engine.Types (Size, Space, Vector2, Window(..), projectOnScreen)

foreign import cols :: Effect Int

foreign import rows :: Effect Int

getWindowSize :: Effect Size
getWindowSize = { width: _, height: _ } <$> cols <*> rows

foreign import eol :: Effect Char

foreign import write :: String -> Effect Unit

foreign import cursorTo :: Vector2 -> Effect Unit

foreign import moveCursor :: Vector2 -> Effect Unit

resetCursor :: Effect Unit
resetCursor = cursorTo { x: 0, y: 0 }

foreign import clearScreenDown :: Effect Unit -> Effect Unit

hideCursor :: Effect Unit
hideCursor = write (escapeCodeToString HideCursor)

showCursor :: Effect Unit
showCursor = write (escapeCodeToString ShowCursor)

drawingArea :: Window -> Effect Size
drawingArea w =
  case w of
    FullScreen -> identity
    Window s -> min s
    <$> getWindowSize

safeWrite :: Window -> Space Char -> Effect Unit
safeWrite w space = do
  eolStr <- SCU.singleton <$> eol
  da <- drawingArea w
  write
    <<< intercalate eolStr
    <<< map
        ( \y ->
            SCU.fromCharArray
              <<< A.fromFoldable
              <<< map (\x -> maybe ' ' identity <<< M.lookup { x, y } $ projectOnScreen space)
              $ range 0 (da.width - 1)
        )
    $ range 0 (da.height - 1)
