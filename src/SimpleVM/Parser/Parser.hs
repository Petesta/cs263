module SimpleVM.Parser.Parser where

import Control.Monad.State
import Data.Maybe

data Instr    = Self | Literal Value | Send Selector | SelfSend | SuperSend | Delegate | NonLocalReturn | IdxExtension
data LitVal   = IntLit Int | StringLit String
data Selector = Selector
data Value    = SelfV | Method ObjData | Lit LitVal | Index Int

data ObjData = ObjData {
    mName   :: String,
    numArgs :: Int
}

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
        Send s -> handleSend s
        _ -> error "not implemented"

-- Push self onto the execution stack
handleSelf :: Interpreter ()
handleSelf = push SelfV

-- Push literal value onto the execution stack
handleLiteral :: Value -> Interpreter ()
handleLiteral v = push v

-- TODO: Determine what result to push and where it's located
-- Find selector, top the stack and determine how many arguments
-- need to be popped. Then push a result.
handleSend :: Selector -> Interpreter ()
handleSend s = do
    -- selector <- selectorTable s
    topElement <- top
    case topElement of
        Just value -> case value of
            SelfV -> do
                pop
                push SelfV
            Method (ObjData _ num) -> do
                replicateM_ num pop
                push SelfV
        Nothing -> error "empty value from top"
