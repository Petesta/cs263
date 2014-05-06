module SimpleVM.Parser.Parser where

import Control.Monad.State
import Data.Maybe

import qualified Data.Map as M

data Instr    = Self | Literal Value | Send Selector | SelfSend Selector | SuperSend | Delegate | NonLocalReturn | IdxExtension
data LitVal   = IntLit Int | StringLit String
data Selector = Selector String
data Value    = SelfV | Method ObjData | Lit LitVal | SendValue | Index Int

data ObjData = ObjData {
    mName   :: String,
    numArgs :: Int,
    vmt     :: VMT 
}

-- Virtual Method Table (Execution Stack)
type VMT = M.Map Selector (Maybe Value)

-- Operand Stack
type InterpState   = [Value]
type Interpreter a = State InterpState a

instance Eq Selector where
    (Selector s1) == (Selector s2) = s1 == s2

instance Ord Selector where
    compare (Selector s1) (Selector s2) = compare s1 s2

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

selectorTable :: M.Map Selector (Maybe Value) -> Selector -> Maybe Value
selectorTable m s = fromMaybe (error "Method doesn\'t exist") $ M.lookup s m

-- Push self onto the execution stack
handleSelf :: Interpreter ()
handleSelf = push SelfV

-- Push literal value onto the execution stack
handleLiteral :: Value -> Interpreter ()
handleLiteral v = push v

-- TODO: send consumes the top of the stack and invokes a method (aka message) on it.
handleSend :: Selector -> Interpreter ()
handleSend s = do
    topElement <- top
    case topElement of
        Just value -> case value of
            SelfV -> do
                pop
                push SelfV
            Method (ObjData _ num vmt) -> do
                pop
                case selectorTable vmt s of
                    Just method -> do
                        replicateM_ num pop
                        push method
                    Nothing -> error "Method doesn\'t exist"
        Nothing -> error "empty stack"

-- TODO: Determine what result to push and where it's located
-- Find selector and send selector to self on top of stack
{-
handleSelfSend :: Selector -> Interpreter ()
hanldeSelfSend s = do
    -- selector <- selectorTable s
    topElement <- top
    case topElement of
        Just value -> do
            pop
            push SelfV -- TODO: not SelfV but needs to be result
        Nothing -> error "empty stack"
-}

handleInstruction :: Instr -> Interpreter ()
handleInstruction instr =
    case instr of
        Self -> handleSelf
        Literal v -> handleLiteral v
        Send s -> handleSend s
        _ -> error "not implemented"
