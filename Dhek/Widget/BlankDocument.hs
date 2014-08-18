{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Widget.BlankDocument
--
--
--------------------------------------------------------------------------------
module Dhek.Widget.BlankDocument
    ( BlankDocumentEvent(..)
    , OpenInput(..)
    , PageDimension(..)
    , PageCount(..)
    , newBlankDocumentWidget
    ) where

--------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Text (unpack)
import qualified Graphics.UI.Gtk as Gtk
import           System.FilePath
import           Text.Shakespeare.I18N

--------------------------------------------------------------------------------
import Dhek.I18N
import Dhek.PDF.Type
import Dhek.Widget.Type

--------------------------------------------------------------------------------
data BlankDocument = BlankDocument

--------------------------------------------------------------------------------
data BlankDocumentEvent a where
    BlankDocumentOpen :: BlankDocumentEvent OpenInput

--------------------------------------------------------------------------------
data OpenInput = OpenInput PageDimension PageCount

--------------------------------------------------------------------------------
mkMessage "BlankDocument"
          (joinPath ["messages", "widget", "blank-document"])
          "en"

--------------------------------------------------------------------------------
mkBlankDocI18n :: IO (BlankDocumentMessage -> String)
mkBlankDocI18n
    = fmap (\l -> unpack . renderMessage BlankDocument [l]) determineLang

--------------------------------------------------------------------------------
newBlankDocumentWidget :: (DhekMessage -> String)
                       -> Gtk.Window
                       -> IO (Widget BlankDocumentEvent)
newBlankDocumentWidget msgStr parent
    = do bdMsgStr <- mkBlankDocI18n

         win     <- Gtk.dialogNew
         table   <- Gtk.tableNew 3 3 False
         spin    <- Gtk.spinButtonNewWithRange 1 200 1
         store   <- Gtk.listStoreNew [A2_P .. A5_L]
         combo   <- Gtk.comboBoxNewWithModel store
         render  <- Gtk.cellRendererTextNew
         label   <- Gtk.labelNew $ Just $ bdMsgStr MsgDescription
         plbl    <- Gtk.labelNew $ Just $ bdMsgStr MsgPageDimension
         nlbl    <- Gtk.labelNew $ Just $ bdMsgStr MsgNumberPage
         l1align <- Gtk.alignmentNew 0 0 0 0
         l2align <- Gtk.alignmentNew 0 0 0 0
         l3align <- Gtk.alignmentNew 0 0 0 0
         salign  <- Gtk.alignmentNew 0 0 0 0
         calign  <- Gtk.alignmentNew 0 0 0 0
         talign  <- Gtk.alignmentNew 0 0 1 0

         Gtk.set win [ Gtk.windowTitle     Gtk.:= msgStr MsgOpenBlank
                     , Gtk.windowResizable Gtk.:= False
                     ]
         Gtk.windowSetModal win True
         Gtk.windowSetTransientFor win parent

         Gtk.cellLayoutPackStart combo render False
         Gtk.cellLayoutSetAttributes combo render store
             (layoutMapping $ dimStr bdMsgStr)
         Gtk.comboBoxSetActive combo 0

         ob <- Gtk.dialogAddButton win (msgStr MsgOpen) Gtk.ResponseOk
         cl <- Gtk.dialogAddButton win (msgStr MsgCancel) Gtk.ResponseClose

         Gtk.labelSetLineWrap label True
         Gtk.labelSetJustify label Gtk.JustifyFill
         Gtk.alignmentSetPadding talign 20 20 20 20

         Gtk.containerAdd salign spin
         Gtk.containerAdd calign combo
         Gtk.containerAdd l1align label
         Gtk.containerAdd l2align plbl
         Gtk.containerAdd l3align nlbl
         Gtk.containerAdd talign table

         Gtk.tableAttachDefaults table l1align 0 3 0 1
         Gtk.tableAttachDefaults table l2align 0 1 1 2
         Gtk.tableAttachDefaults table calign 1 2 1 2
         Gtk.tableAttachDefaults table l3align 0 1 2 3
         Gtk.tableAttachDefaults table salign 1 2 2 3

         Gtk.tableSetRowSpacings table 10
         Gtk.widgetShowAll talign
         box <- Gtk.dialogGetUpper win
         Gtk.containerAdd box talign

         -- Close handler
         _ <- Gtk.on cl Gtk.buttonActivated $ Gtk.widgetHide win

         _ <- Gtk.on win Gtk.deleteEvent $ liftIO $
                  do Gtk.widgetHide win
                     return True

         let widget = Widget
                      { widgetRegister = blankDocumentRegister ob win combo
                                         spin store
                      , widgetShow     = blankDocumentShow win
                      , widgetHide     = Gtk.widgetHide win
                      }

         return widget

--------------------------------------------------------------------------------
blankDocumentRegister :: Gtk.Button
                      -> Gtk.Dialog
                      -> Gtk.ComboBox
                      -> Gtk.SpinButton
                      -> Gtk.ListStore PageDimension
                      -> BlankDocumentEvent a
                      -> (a -> IO ())
                      -> IO Release
blankDocumentRegister btn dialog cb spin store BlankDocumentOpen handler
    = do cid <- Gtk.on btn Gtk.buttonActivated $
                    do Gtk.widgetHide dialog
                       idx <- Gtk.comboBoxGetActive cb
                       val <- Gtk.listStoreGetValue store idx
                       cnt <- Gtk.spinButtonGetValueAsInt spin
                       handler $ OpenInput val (PageCount cnt)
         return $ Gtk.signalDisconnect cid

--------------------------------------------------------------------------------
blankDocumentShow :: Gtk.Dialog -> IO ()
blankDocumentShow win
    = do Gtk.widgetShowAll win
         _ <- Gtk.dialogRun win
         return ()

--------------------------------------------------------------------------------
dimStr :: (BlankDocumentMessage -> String) -> PageDimension -> String
dimStr msgStr pd
    = msgStr $
          case pd of
              A2_P -> MsgA2_P
              A2_L -> MsgA2_L
              A3_P -> MsgA3_P
              A3_L -> MsgA3_L
              A4_P -> MsgA4_P
              A4_L -> MsgA4_L
              A5_P -> MsgA5_P
              A5_L -> MsgA5_L

--------------------------------------------------------------------------------
layoutMapping :: (PageDimension -> String)
              -> PageDimension
              -> [Gtk.AttrOp Gtk.CellRendererText]
layoutMapping msgStr pd = [Gtk.cellText Gtk.:= msgStr pd]
