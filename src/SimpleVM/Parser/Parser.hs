module SimpleVM.Parser.Parser where

import Control.Monad.State
import Data.Maybe

import qualified Data.Map as M

data Instr    = Self | Literal Value | Send Selector | SelfSend Selector | SuperSend | Delegate | NonLocalReturn | IdxExtension
data LitVal   = IntLit Int | StringLit String
data Selector = Selector String
data Value    = SelfV | Object ObjectImpl | Lit LitVal | SendValue | Index Int

data ObjectImpl = ObjectImpl {
    mName      :: String,
    numArgs    :: Int,
    vmt        :: VMT 
}

-- Virtual Method Table (Execution Stack)
type VMT = M.Map Selector (Maybe Value)

-- Operand Stack
data InterpState   = InterpState [Value] ObjectImpl
--type InterpState   = [Value]
type Interpreter a = State InterpState a
-- go to inter state

instance Eq Selector where
    (Selector s1) == (Selector s2) = s1 == s2

instance Ord Selector where
    compare (Selector s1) (Selector s2) = compare s1 s2

top :: Interpreter (Maybe Value)
top = do
  InterpState stack objImpl <- get
  let result = case stack of
        [] -> Nothing
        (top:_) -> Just top
  return result

pop :: Interpreter (Maybe Value)
pop = do
    InterpState stack objImpl <- get
    let (result, tail) = case stack of
            [] -> (Nothing, [])
            (x:xs) -> (Just x, xs)
    put $ InterpState tail objImpl
    return result

push :: Value -> Interpreter ()
push v = do
    InterpState stack objImpl <- get
    put $ InterpState (v:stack) objImpl
    -- stack %= (v:)

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
    let Object (ObjectImpl name num vmt) = fromMaybe (error "Top element isn\'t type Method") $ topElement
    pop

    let method = fromMaybe (error "Method doesn\'t exist") $ selectorTable vmt s
    replicateM_ num pop
    push method

-- TODO: Determine what result to push and where it's located
--handleSelfSend :: Selector -> Interpreter ()
--hanldeSelfSend s = do
--    topElement <- top
--    let SelfV = fromMaybe (error "Type")
--    pop

--    case topElement of
--        Just value -> do
--            pop
--            --push SelfV -- TODO: not SelfV but needs to be result
--            case selectorTable vmt s of
--                Just _ -> do
--                    replicateM_ num pop
--                    push _
--        Nothing -> error "empty stack"

handleInstruction :: Instr -> Interpreter ()
handleInstruction instr =
    case instr of
        Self -> handleSelf
        Literal v -> handleLiteral v
        Send s -> handleSend s
        --SelfSend s -> handleSelfSend s
        _ -> error "not implemented"
