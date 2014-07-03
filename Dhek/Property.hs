--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Property
--
--------------------------------------------------------------------------------
module Dhek.Property where

--------------------------------------------------------------------------------
import Prelude hiding (any)
import Control.Applicative ((<*>), (<$>))
import Data.Foldable (for_)

--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((.~), (&))
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Instr
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Types

--------------------------------------------------------------------------------
onProp :: GUI -> Instr ()
onProp gui
    = do sOpt <- getSelected
         nOpt <- liftIO $ gtkLookupEntryText $ guiNameEntry gui
         vOpt <- liftIO $ gtkLookupEntryText $ guiValueEntry gui
         iOpt <- liftIO $ gtkGetIndexPropValue gui
         tOpt <- liftIO $ Gtk.comboBoxGetActiveText $ guiTypeCombo gui
         let r = (,,) <$> sOpt <*> nOpt <*> tOpt
         for_  r $ \(r,n,t) ->
             do let r1  = r & rectName  .~ n
                            & rectType  .~ t
                            & rectValue .~ vOpt
                            & rectIndex .~ iOpt
                setSelected (Just r1)
                addEvent UpdateRectProps
