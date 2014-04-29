module SimpleVM.Parser.Parser where

import Control.Monad.State
import Data.Maybe

--data InterpSTate = IS { stack :: [Value], pc :: Int }

data Instr = Self | Literal Value | Send | SelfSend | SuperSend | Delegate | NonLocalReturn | IdxExtension
data Value = SelfV | Lit LitVal | Index Int
data LitVal = IntLit Int | StringLit String

type InterpState = [Value]
type Interpreter a = State InterpState a

top :: Interpreter (Maybe Value)
top = do
  stack <- get
  case stack of
    [] -> return Nothing
    (top:_) -> return $ Just top

pop :: Interpreter (Maybe InterpState)
pop = do
    stack <- get
    case stack of
        [] -> return Nothing
        (_:xs) -> return $ Just xs

push :: Value -> Interpreter ()
push v = do
    stack <- get
    put (v:stack)

handleInstruction :: Instr -> Interpreter ()
handleInstruction instr =
    case instr of
        Self -> handleSelf
        Literal v -> handleLiteral v
        _ -> error "not implemented"

handleSelf :: Interpreter ()
handleSelf = push SelfV

handleLiteral :: Value -> Interpreter ()
handleLiteral v = push v

-- Type signature subject to change
-- handleSend :: Interpreter ()
-- handleSend
