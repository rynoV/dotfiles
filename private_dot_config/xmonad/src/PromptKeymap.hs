{-# LANGUAGE TupleSections #-}

-- | Xmonad prompt keymap
module PromptKeymap where

import Data.Bifunctor (bimap, first)
import qualified Data.Map as M
import XMonad
import XMonad.Prompt
import qualified XMonad.StackSet as W

-- | Modified version of the vimlike keymap provided by
-- XMonad.Prompt. Fixes normal mode shift key shortcuts and adds
-- ctrl+w shortcut.
myVimLikeXPKeymap' ::
  -- | Modifies the prompt color when entering normal mode.
  -- The default is @setBorderColor "grey22"@ - same color as
  -- the default background color.
  (XPColor -> XPColor) ->
  -- | Prompter to use in normal mode. The default of 'id'
  -- balances 'defaultPrompter' but @("[n] " ++)@ is a good
  -- alternate with 'defaultPrompter' as @("[i] " ++)@.
  (String -> String) ->
  -- | Filter applied to the X Selection before pasting. The
  -- default is 'id' but @filter isPrint@ is a good
  -- alternate.
  (String -> String) ->
  -- | Predicate identifying non-word characters. The default
  -- is 'isSpace'. See the documentation of other keymaps for
  -- alternates.
  (Char -> Bool) ->
  M.Map (KeyMask, KeySym) (XP ())
myVimLikeXPKeymap' fromColor promptF pasteFilter notWord =
  M.fromList $
    map
      (first (0,))
      [ (xK_Return, setSuccess True >> setDone True),
        (xK_KP_Enter, setSuccess True >> setDone True),
        (xK_BackSpace, deleteString Prev),
        (xK_Delete, deleteString Next),
        (xK_Left, moveCursor Prev),
        (xK_Right, moveCursor Next),
        (xK_Home, startOfLine),
        (xK_End, endOfLine),
        (xK_Down, moveHistory W.focusUp'),
        (xK_Up, moveHistory W.focusDown'),
        ( xK_Escape,
          moveCursor Prev
            >> modifyColor fromColor
            >> setPrompter promptF
            >> promptSubmap (return ()) normalVimXPKeymap
            >> resetColor
            >> resetPrompter
        )
      ]
      ++ map (first (controlMask,)) [(xK_w, killWord' notWord Prev), (xK_g, quit)]
  where
    normalVimXPKeymap =
      M.fromList $
        map
          (first (0,))
          [ (xK_i, setModeDone True),
            (xK_a, moveCursor Next >> setModeDone True),
            (xK_s, deleteString Next >> setModeDone True),
            (xK_x, deleteString Next >> clipCursor),
            (xK_Delete, deleteString Next >> clipCursor),
            ( xK_p,
              moveCursor Next
                >> pasteString' pasteFilter
                >> moveCursor Prev
            ),
            (xK_0, startOfLine),
            (xK_Escape, quit),
            (xK_Down, moveHistory W.focusUp'),
            (xK_j, moveHistory W.focusUp'),
            (xK_Up, moveHistory W.focusDown'),
            (xK_k, moveHistory W.focusDown'),
            (xK_Right, moveCursorClip Next),
            (xK_l, moveCursorClip Next),
            (xK_h, moveCursorClip Prev),
            (xK_Left, moveCursorClip Prev),
            (xK_BackSpace, moveCursorClip Prev),
            (xK_e, moveCursorClip Next >> moveWord' notWord Next),
            (xK_b, moveCursorClip Prev >> moveWord' notWord Prev),
            (xK_w, moveWord' (not . notWord) Next >> moveCursorClip Next),
            (xK_f, promptBuffer bufferOne >>= toHeadChar Next),
            (xK_d, promptSubmap (setModeDone True) deleteVimXPKeymap),
            ( xK_c,
              promptSubmap (setModeDone True) changeVimXPKeymap
                >> setModeDone True
            )
          ] ++ [((controlMask,xK_g), quit)]
          ++ map
            (first (shiftMask,))
            [ -- For some reason the dollar key doesn't work
              -- (xK_dollar, endOfLine >> moveCursor Prev),
              (xK_d, killAfter >> moveCursor Prev),
              (xK_c, killAfter >> setModeDone True),
              (xK_p, pasteString' pasteFilter >> moveCursor Prev),
              (xK_a, endOfLine >> setModeDone True),
              (xK_i, startOfLine >> setModeDone True),
              (xK_f, promptBuffer bufferOne >>= toHeadChar Prev)
            ]
    deleteVimXPKeymap =
      M.fromList $
        map
          (bimap (0,) (>> setModeDone True))
          [ (xK_e, deleteString Next >> killWord' notWord Next >> clipCursor),
            (xK_w, killWord' (not . notWord) Next >> clipCursor),
            (xK_0, killBefore),
            (xK_b, killWord' notWord Prev),
            (xK_d, setInput "")
          ]
          ++ map
            (bimap (shiftMask,) (>> setModeDone True))
            [ -- (xK_dollar, killAfter >> moveCursor Prev)
            ]
    changeVimXPKeymap =
      M.fromList $
        map
          (bimap (0,) (>> setModeDone True))
          [ (xK_e, deleteString Next >> killWord' notWord Next),
            (xK_0, killBefore),
            (xK_b, killWord' notWord Prev),
            (xK_c, setInput ""),
            (xK_w, changeWord notWord)
          ]
          ++ map
            (bimap (shiftMask,) (>> setModeDone True))
            [ -- (xK_dollar, killAfter)
            ]
