module SimpleVM.Parser.Types where

import Control.Monad.State

--data InterpSTate = IS { stack :: [Value], pc :: Int }

data Instr = Self | Literal | Send | SelfSend | SuperSend | Delegate | NonLocalReturn | IdxExtension
data Value = SelfV | Index Int

type Stack a = [a]
type Interpreter a = State InterpState a
type InterpState = [Value]

handleInstruction :: Instr -> Interpreter ()
handleInstruction instr =
    case instr of
        Self -> handleSelf
        _ -> error "not implemented"
        -- Literal -> handlerLiteral

handleSelf :: Interpreter ()
handleSelf = push SelfV

push :: Value -> Interpreter ()
push v = do
    stack <- get
    put (v:stack)
