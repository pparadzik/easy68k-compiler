module WhileInterpreter where 

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.HashMap.Strict
import Data.List (sort)

import While

evalAExp :: HashMap String Integer -> AExpr -> Integer
evalAExp hmap expr = case expr of 
        Var id -> lookupDefault 0 id hmap
        IntConst c -> c
        Neg e -> -(evalAExp hmap e)
        ABinary op e1 e2 -> (getOp op) (evalAExp hmap e1) (evalAExp hmap e2)
        where getOp Add = (+)
              getOp Subtract = (-)
              getOp Multiply = (*)
              getOp Divide = quot

evalBExp hmap expr = case expr of 
    BoolConst c -> c
    Not e -> not $ evalBExp hmap e
    BBinary bop e1 e2 -> 
        (case bop of {And -> (&&); Or -> (||)}) (evalBExp hmap e1) (evalBExp hmap e2)
    RBinary rop e1 e2 ->
        (case rop of {Greater -> (>); Less -> (<)}) (evalAExp hmap e1) (evalAExp hmap e2)

interpretStatement :: HashMap String Integer -> Stmt -> HashMap String Integer
interpretStatement hmap stmt =
    case stmt of 
       Assign idnt expr -> insert idnt (evalAExp hmap expr) hmap
       If bexpr s1 s2 -> if evalBExp hmap bexpr 
                         then interpretStatement hmap s1 
                         else interpretStatement hmap s2
       While bexpr s -> let newHmap = interpretStatement hmap s 
                        in 
                        if evalBExp hmap bexpr 
                        then interpretStatement newHmap (While bexpr s)
                        else hmap
       Skip -> hmap
       Seq (s:ss) -> let newHmap = interpretStatement hmap s
                     in interpretStatement newHmap (Seq ss)
       Seq [] -> hmap
                     

interpreter str = interpretStatement empty $ parseString str
interpretString str = mapM_ ((\s -> do {putStr s; putStrLn ""}) . 
            (\p -> fst p ++ " " ++ show (snd p))) (sort (toList (interpreter str)))
    
main = getContents >>= interpretString

interpretFromFile :: String -> IO ()
interpretFromFile file = readFile file >>= interpretString
    