module Engine.Drawing where

import Prelude
import Control.Monad.RWS (RWSResult(..), RWST, ask, censor, local, runRWST, tell)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as M
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Engine.Classes (class Affine, class Drawable, class Location, rotateL, rotateR, translate)
import Engine.Types (Axis(..), Space, Vector3)

newtype DrawReader
  = DrawReader Vector3

derive instance newtypeDrawReader :: Newtype DrawReader _

newtype DrawWriter
  = DrawWriter (Space Char)

derive instance newtypeDrawWriter :: Newtype DrawWriter _

derive newtype instance semigroupDrawWriter :: Semigroup DrawWriter

derive newtype instance monoidDrawWriter :: Monoid DrawWriter

newtype Drawing a
  = Drawing (RWST DrawReader DrawWriter Unit Effect a)

runDrawing :: forall a. Drawing a -> Effect (Tuple a (Space Char))
runDrawing m = do
  RWSResult _ a (DrawWriter space) <- runRWST (unwrap m) (DrawReader { x: 0, y: 0, z: 0 }) unit
  pure $ Tuple a space

derive instance newtypeDrawing :: Newtype (Drawing a) _

derive newtype instance functorDrawing :: Functor Drawing

derive newtype instance applyDrawing :: Apply Drawing

derive newtype instance applicativeDrawing :: Applicative Drawing

derive newtype instance bindDrawing :: Bind Drawing

derive newtype instance monadDrawing :: Monad Drawing

derive newtype instance monadRecDrawing :: MonadRec Drawing

derive newtype instance monadAskDrawing :: MonadAsk DrawReader Drawing

derive newtype instance monadReaderDrawing :: MonadReader DrawReader Drawing

derive newtype instance monadTellDrawing :: MonadTell DrawWriter Drawing

derive newtype instance monadWriterDrawing :: MonadWriter DrawWriter Drawing

derive newtype instance monadStateDrawing :: MonadState Unit Drawing

instance locationDrawing :: Location Drawing where
  getLocation = unwrap <$> ask

instance affineDrawing :: Affine Drawing where
  translate v = local $ over DrawReader (add v)
  rotateL (Midpoint center) =
    translate center
      <<< censor (over DrawWriter (foldMapWithIndex \{ x, y, z } v -> M.singleton { x: negate y, y: x, z: z } v))
      <<< translate (negate center)
  rotateL (Lattice center) = translate { x: -1, y: 0, z: 0 } <<< rotateL (Midpoint center)
  rotateR (Midpoint center) =
    translate center
      <<< censor (over DrawWriter (foldMapWithIndex \{ x, y, z } v -> M.singleton { x: y, y: negate x, z: z } v))
      <<< translate (negate center)
  rotateR (Lattice center) = translate { x: 0, y: -1, z: 0 } <<< rotateR (Midpoint center)

instance drawableDrawing :: Drawable Drawing where
  dot v c = tell (wrap (M.singleton v c))
