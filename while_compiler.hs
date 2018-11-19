module WhileCompiler where

import System.IO
import Numeric (showHex)
import Data.Either
import Data.List
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import System.Random

import While

-- ideja -- napisati kompajler u x68 assembly
-- konstrukti assembly-a koje trebam koristiti
-- zasad -- sve je unsigned + sve je bajt radi jednostavnosti
-- sve varijable treba inicijalizirati prije koristenja
    -- TODO lookup u hashmapu mora baciti error ako varijabla jos ne postoji
    -- zasad je blazeno nesvjestan toga, na korisniku je da pazi! :D

data Addr = Const Integer
          | DReg Integer
          | AReg Integer
          | ADeref Integer
          | AInc Integer
          | ADec Integer
          | Mem Integer
          | Label String
          -- | Offset Integer -- vjerojatno ne treba jer to radi assembler za nas, moram pogledati tocno
          deriving (Eq)

instance Show Addr where
  show adr = case adr of
    Const i -> "#$" ++ showHex i ""
    DReg i -> check i $ "d" ++ show i
    AReg i -> check i $ "a" ++ show i
    ADeref i -> check i $ "(a" ++ show i ++ ")"
    AInc i -> check i $ "(a" ++ show i ++ ")+"
    ADec i -> check i $ "-(a" ++ show i ++ ")"
    -- TODO: mem treba masu checkova :D
    Mem i -> "$" ++ showHex i ""
    Label s -> s
    where
      check :: Integer -> String -> String
      check i s = if i > 7 || i < 0
                    then error "register index out of range!"
                    else s

data Size = SL | SW | SB
    deriving (Eq)
instance Show Size where
  show SL = ".l"
  show SW = ".w"
  show SB = ".b"

data Branch = BCC | BCS | BPL | BMI | BNE | BEQ | BRA
  deriving (Eq)
instance Show Branch where
  show BCC = "\tbcc"
  show BCS = "\tbcs"
  show BPL = "\tbpl"
  show BMI = "\tbmi"
  show BNE = "\tbne"
  show BEQ = "\tbeq"
  show BRA = "\tbra"

  -- kod dijelim na instrukcije i direktive
data Code = -- instrukcije
            IMove Size Addr Addr
          | IAdd Size Addr Addr
          | ISub Size Addr Addr
          | IDiv Size Addr Addr
          | IMul Size Addr Addr
          | IJmp Addr
          | ICmp Size Addr Addr
        -- u ovom slucaju adresa je label ili offset (assembly brine za to)
          | IBcc Branch Addr
          | IJsr Addr
          | INeg Size Addr
          | IAnd Size Addr Addr
          | IOr Size Addr Addr
          | ITrap Integer -- trap kod! -- za ispis, izlaz, unos itd.
          | ITst Size Addr
        -- | IMovem [Addr] [Addr] -- za context switch!

          -- direktive
          | DDs Size String Integer -- naziv labela i velicina
          | DOrg Integer
          | DLabel String
          | DEnd String
          deriving (Eq)

instance Show Code where
  -- TODO: ovo se moze ljepse s case-om
  -- TODO: ovo nije dovoljno za immediate i quick varijante
  show (IMove sz ad1 ad2) = "\tmove" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IAdd sz ad1 ad2) = "\tadd" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (ISub sz ad1 ad2) = "\tsub" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IMul sz ad1 ad2) = "\tmulu" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IDiv sz ad1 ad2) = "\tdivu" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (ICmp sz ad1 ad2) = "\tcmp" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (ITst sz ad) = "\ttst" ++ show sz ++ " " ++ show ad ++ "\n"

  show (INeg sz ad) = "\tneg" ++ show sz ++ " " ++ show ad ++ "\n"
  show (IAnd sz ad1 ad2) = "\tand" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IOr sz ad1 ad2) = "\tor" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"

  show (IBcc br ad) = show br ++ " " ++ show ad ++ "\n"
  show (IJsr ad) = "\tjsr " ++ show ad ++ "\n"

  -- TODO: kod ovih stringova od varijabli i labela provjeravati da nemaju razmake!
  show (DDs sz str i) = str ++ " " ++ "ds" ++ show sz ++ " " ++ show i ++ "\n"
  -- show (DDc str i) = str ++ " " ++ "ds" ++ show sz ++ show i
  show (DOrg addr) = "\torg $" ++ showHex addr "" ++ "\n"
  show (DLabel str) = str ++ ":\n"
  show (DEnd str) = "\tend " ++ str ++ "\n"

--instance Show Instruction
data Sign = Signed | Unsigned
  deriving Eq
data CState = CState -- compiler state - pamti stanja kompajlera kod prolaska kroz komande
  { vars :: HashMap String (Size, Sign) -- ime -> (byte|word|long, signed|unsigned)
  , ifLabels :: [String] -- samo popis, da znam da se ne ponavljaju
  , loopLabels :: [String] -- isto ko gore, samo popis
  --, gen :: StdGen -- treba mi za raditi random stringove
  }
  --deriving Eq

emptyState = CState HM.empty [] [] --newStdGen

stateUnion c1 c2 =
  c2 { vars = HM.union (vars c1) (vars c2)
     , ifLabels = nub (ifLabels c1 ++ ifLabels c2)
     , loopLabels = nub (loopLabels c1 ++ loopLabels c2)
     }

data Program = Program CState [Code] -- header, body

-- TODO naravno ove adrese moraju biti stringovi koji predstavljaju hex broj
  -- nek to rijesi show
var_prefix = "var_" -- prefix koji se daje svim labelima koji oznacavaju varijable, zasad
startAddress = 0x1000
startLabel = "asdfasdf" -- TODO: rjesiti se labela, koristiti samo adrese
initialStack = Const 0x10000
initializationDirectives = [DOrg startAddress, DLabel startLabel]
initializationCode = [IMove SL initialStack (AReg 7)]
endDirectives = [DEnd startLabel]

{-
-- statementi while-a, za podsjetnik
data Stmt = Function String Stmt [Stmt] -- funkcija treba parametre
      | Seq [Stmt]
      | Assign String AExpr
      | If BExpr Stmt Stmt
      | While BExpr Stmt
      | Skip
      | Expr AExpr -- TODO: zakaj je ovo tu? :D
        deriving (Show)
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving (Show)
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)
data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

data Branch = BCC | BCS | BPL | BMI | BNE | BEQ
-}

mapOpToCommand :: ABinOp -> Addr -> Addr -> Code
mapOpToCommand op = case op of
                      Add -> IAdd SB
                      Subtract -> ISub SB
                      Multiply -> IMul SW
                      Divide -> IDiv SW

-- kaze na fju koja iz inta izvuce dal je jednak nuli il ne
__builtin_int_to_bool = Label "__builtin_int_to_bool"

-- kaze na fju koja radi usporedbu vrijednosti na stacku
-- fje su TODO
__builtin_compare_lt = Label "__builtin_compare_lt"
__builtin_compare_gt = Label "__builtin_compare_gt"


-- TODO: zato jer nemam tipove i jer su glupo rjeseni boolovi, ovo je ruzna ali
-- nuzna improvizacija
-- fja stavlja 0 ili 1 na stack, ovisno o tome kak se evaluiral izraz
-- ovisno o tome, pozivajuca fja definira dal da skoci ili ne
translateBexp state bexp =
  case bexp of
    -- prva dva su bezuvjetni skokovi
    BoolConst True -> [IMove SB (Const 1) (ADec 7)]
    BoolConst False -> [IMove SB (Const 0) (ADec 7)]
    -- prvo, evaluiramo bexp2. nakon toga, uzmem to kaj je bilo na stacku i negiram
    Not bexp2 -> translateBexp state bexp2 ++ [INeg SB (ADeref 7)] -- TODO: ovo je pitanje dal radi dobro
    BBinary bop bexp1 bexp2 ->
      -- bop moze biti and ili or
      translateBexp state bexp1 ++ translateBexp state bexp2 ++
      [ IMove SB (AInc 7) (DReg 0)
      , IMove SB (AInc 7) (DReg 1)] ++
      -- sad na vrhu stacka imam 0 ili 1, napravim and ili or
      case bop of
        And -> [IAnd SB (DReg 0) (DReg 1)]
        Or -> [IOr SB (DReg 0) (DReg 1)]
    RBinary rop aexp1 aexp2 ->
      -- rop moze biti greater ili lesser
      -- prvo, prevedem aexpove u vrijednosti
      let (_, code1) = translateCommand state (Expr aexp1)
          (_, code2) = translateCommand state (Expr aexp2)
      -- code1 na stack stavlja rezultat izvrsavanja aexp1
      -- code2 isto
      -- nakon code1 i code2 na vrhu stacka imamo dvije vrijednosti koje treba usporediti
      in code1 ++ code2 ++
      [IJsr (case rop of {Greater -> __builtin_compare_gt; Less -> __builtin_compare_lt})]
    -- nakon ovog, na stacku je 0 ili 1

getNextLabel state = (state {ifLabels = label:ifLabels state}, label)
  where len = length (ifLabels state)
        label = "if_label_" ++ show len

-- sequence == samo prevod svega najedamput (TODO je neki state
-- monad koji zna kaj se dogadjalo prije zbog optimizacije)
--translateCommand state (Seq l) = concat $ map translateCommand l
-- TODO: ovo mi se ne svidja :D (mislim da bude problema sa stackom)
translateCommand :: CState -> Stmt -> (CState, [Code])
translateCommand state (Seq ls) =
  case ls of
    [] -> (state, [])
    (l:lss) -> let (newstate, code) = translateCommand state l
                   (retstate, retcode) = translateCommand newstate (Seq lss)
               in (retstate, code ++ retcode)

translateCommand state (If bexp stm1 stm2) =
  -- imam dva nezavisna statementa
  let codeBexp = translateBexp state bexp
      (state2, label) = getNextLabel state
      (state3, code1) = translateCommand state2 stm1
      -- TODO: rjesiti bolje, zasad, code2 stoji u memoriji nakon code1 -- to
      -- znaci da zna za njegove labele state u biti samo pamti koji labeli su
      -- slobodni i koje varijable su vec definirane
      -- samo labeli su bitni u biti
      (state4, code2) = translateCommand state3 stm2
      in (state3, codeBexp ++
         -- gornji kod na stack baci 0 ili 1
         [ITst SB (AInc 7), IBcc BNE (Label label)] ++
         code1 ++ [IBcc BRA (Label (label++"_end"))] ++
         [DLabel label] ++
         code2 ++
         [DLabel (label++"_end")])

-- aritmeticki izrazi -- kad zavrse, rezultat mora biti stavljen na vrh stoga
  -- TODO: div vjerojatno ne valja tj. treba puno toga provjeriti (mozda napravi
  -- interrupt!)
translateCommand state (Expr exp) =
  case exp of
    -- ako je izraz samo varijabla, stavim njen label na vrh stoga
    -- TODO: ako varijabla ne postoji, skrsiti se ili ju inicijalizirati -- sad radim bez statea
    Var s -> (state, [IMove SB (Label (var_prefix ++ s)) (ADec 7)])
    IntConst i -> (state,[IMove SB (Const i) (ADec 7)])
    --Neg AExpr -> 
    -- za operaciju, e1 i e2 moram zapakirati u statement
    ABinary op e1 e2 ->
      -- TODO: ova sintaksa MOLI za neki sequencing :D
      let (s1, code1) = translateCommand state (Expr e1)
          (s2, code2) = translateCommand state (Expr e2)
      in-- uzmem operande sa stoga
      (s2, code1 ++ code2 ++
      [ IMove SB (AInc 7) (DReg 0)
      , IMove SB (AInc 7) (DReg 1)
      -- izracunam kaj vec treba
      , mapOpToCommand op (DReg 0) (DReg 1)
      -- pomaknem rezultat na vrh stoga
      , IMove SB (DReg 1) (ADec 7)])

-- varijable == eksplicitni DS od tocno jednog bajta
-- plus prateca naredba u inicijalizaciji
translateCommand state (Assign str aexp) =
  let (s1, code1) = translateCommand state (Expr aexp)
      -- dodam novu varijablu u state
      s2 = s1 { vars = HM.insert var_label (SB, Unsigned) (vars s1) }
  -- TODO: ako vec nije definiran label, naravno, al pustim to programu koji sve sklapa skupa
  -- TODO: trenutno nema promjene vrijednosti varijable jedamput kad ju definiram :D
  -- nakon evaluacije aexp-a, rezultat je na vrhu stoga
  in (s2, code1 ++ [IMove SB (AInc 7) (Label var_label)])
  where var_label = var_prefix ++ str

--translate st = []
getVarsFromState state =
  -- TODO: i nomenklatura za state je uzasna :D
  map (\x -> DDs SB x 1) $ HM.keys (vars state)

translate stmt =
  let (state, code) = translateCommand emptyState stmt
      -- na pocetak koda trebam staviti sve varijable
      varLabels = getVarsFromState state
  in concat $ map show $ varLabels ++ initializationDirectives ++ initializationCode ++ code ++ endDirectives

{-translate statement =
  let (cmds, dirs') = partitionEithers (translateCommand statement)
      dirs = nub dirs' -- nema smisla imati dvije iste direktive
      -- sve direktive hocu smjestiti na pocetak
      init = map Right (initializationDirectives ++ dirs ++ [DLabel startLabel]) ++ map Left initializationCode
      end = map Right endDirectives
      prog = init ++ map Left cmds ++ end
  in putStr $ concat $ map (either show show) prog
-}

-- varijable -- najlakse je napraviti memoriju za svaku varijablu (ko label)
    -- nije prosirivo -- kaj ako imamo lokalne varijable?
    -- ako hocu rekurziju, onda nemrem imati fiksne memorijske lokacije za varijable
    -- stack je jedina moguca stvar za varijable
-- dizajn jezika -- c ima heap i stack (dio je OS-ovog runtimea)
-- kaj ja imam? -- samo sistemski i korisnicki stack
                -- lako mijenjam lokaciju na kojoj radim stack
                -- kaj mogu imati -- stack za svaku funkciju posebno
-- kaj mi treba (nemam funkcije jos, nemam pointere, sve je globalno)
    -- header + footer za 68k
    -- varijable se koriste u nekim aritmetickim izrazima
        -- bitno je koristiti registre za aritmeticke izraze
        -- optimizacija? :D
        -- pretvarati u stack izraze pa evaluirati naopacke

-- pri kompajliranju mogu paziti koje sve varijable sam do sad dovukel u registre (ko cache)
    -- ne samo varijable nego i obicne adrese
    -- ne znam dal je to moguce jer nemrem znati koje adrese imam u registrima bez izvrsavanja
    -- eventualno, ako promijenim adresu onda "ponistim" cache
-- TODO: u nekom trenu dodati fore za generiranje position independent koda
    -- kolko vidim, umjesto fiksnih adresa za sve varijable i slicno moze se koristiti pomak od
    -- PC-a
             -- fora je u tome da KOMPAJLER zna kaj je generiral, i zna kolko se maknul od npr. labela
             -- ne, glavna fora je da ako ne generiram binarni kod onda nemrem loadati programe
             -- u memoriju :D
    -- direktive ne valjaju tu -- nemrem koristiti direktive za varijable! (al ko jebe to sad)


-- zasad nek sve naredbe budu nezavisne -- znaci, ne racunam na nikakvo stanje registara
-- svaka instrukcija sprema svoj dio u memoriju i cita svoje operande iz memorije




