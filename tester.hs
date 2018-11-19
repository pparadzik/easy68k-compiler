import While
import WhileCompiler

main = do
  let tags = parseString "a:=3; b:=4; if (a < b) then {c:=0} else {c:=1}"
  --putStrLn $ show tags
  putStrLn $ translate tags
