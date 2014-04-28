module SimpleVM.Parser.Types where

import Control.Monad.State

--data InterpSTate = IS { stack :: [Value], pc :: Int }

data Instr = Self | Literal Value | Send | SelfSend | SuperSend | Delegate | NonLocalReturn | IdxExtension
data Value = SelfV | Lit LitVal | Index Int
data LitVal = IntLit Int | StringLit String

type Stack a = [a]
type InterpState = [Value]
type Interpreter a = State InterpState a

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
