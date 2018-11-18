module WhileCompiler where

import System.IO
import Numeric (showHex)
import Data.Either
import Data.List
import Data.HashMap.Lazy (HashMap)

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

data Directive = DDs Size String Integer -- naziv labela i velicina
               -- | DDc Size String [Integer]
               | DOrg Integer
               | DLabel String
               | DEnd String
               deriving (Eq)

instance Show Directive where
  -- TODO: kod ovih stringova provjeravati da nemaju razmake!
  show (DDs sz str i) = str ++ " " ++ "ds" ++ show sz ++ " " ++ show i ++ "\n"
  -- show (DDc str i) = str ++ " " ++ "ds" ++ show sz ++ show i
  show (DOrg addr) = "\torg $" ++ showHex addr "" ++ "\n"
  show (DLabel str) = str ++ ":\n"
  show (DEnd str) = "\tend " ++ str ++ "\n"

data Instruction = IMove Size Addr Addr
                 | IAdd Size Addr Addr
                 | ISub Size Addr Addr
                 | IDiv Size Addr Addr
                 | IMul Size Addr Addr
                 | IJmp Addr
                 | ICmp Size Addr Addr
                -- u ovom slucaju adresa je label ili offset (assembly brine za to)
                 | IBcc Branch Addr
                 | ITrap Integer -- trap kod! -- za ispis, izlaz, unos itd.
                -- | INot Addr -- TODO: tu adresa mora biti registar ili memorija pa ne diram sad
                -- | IMovem [Addr] [Addr] -- za context switch!
                deriving (Eq)

instance Show Instruction where
  -- TODO: ovo se moze ljepse s case-om
  show (IMove sz ad1 ad2) = "\tmove" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IAdd sz ad1 ad2) = "\tadd" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (ISub sz ad1 ad2) = "\tsub" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IMul sz ad1 ad2) = "\tmulu" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IDiv sz ad1 ad2) = "\tdivu" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (ICmp sz ad1 ad2) = "\tcmp" ++ show sz ++ " " ++ show ad1 ++ ", " ++ show ad2 ++ "\n"
  show (IBcc br ad) = show br ++ " " ++ show ad ++ "\n"

--instance Show Instruction
type Code = Either Instruction Directive
data Sign = Signed | Unsigned
  deriving Eq
data CState = CState
  { vars :: HashMap String (Size, Sign) -- ime -> (byte|word|long, signed|unsigned)
  , ifLabels :: [String]
  , loopLabels :: [String]
  }
  deriving Eq

data Program = Program CState [Code] -- header, body

-- TODO naravno ove adrese moraju biti stringovi koji predstavljaju hex broj
  -- nek to rijesi show
var_prefix = "var_" -- prefix koji se daje svim labelima koji oznacavaju varijable, zasad
startAddress = 0x1000
startLabel = "asdfasdf" -- TODO: rjesiti se labela, koristiti samo adrese
initialStack = Const 0x10000
initializationDirectives = [DOrg startAddress]
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

mapOpToCommand :: ABinOp -> Addr -> Addr -> Instruction
mapOpToCommand op = case op of
                      Add -> IAdd SB
                      Subtract -> ISub SB
                      Multiply -> IMul SW
                      Divide -> IDiv SW


-- sequence == samo prevod svega najedamput (TODO je neki state
-- monad koji zna kaj se dogadjalo prije zbog optimizacije)
translateCommand (Seq l) = concat $ map translateCommand l

translateCommand (If bexp s1 s2) =
  case bexp of
    -- BoolConst b ->
    -- Not b ->
    -- RBinary bbop b1 b2 ->
    -- svakak prvo trebam izracunati rezultate a1 i a2
    ----------------------------------------------------------------
    -- NOTE kad je operacija binarni and ili or
    {-BBinary bbop a1 a2 ->
      translateCommand (Expr a1)
      ++
      translateCommand (Expr a2)
      ++
      map Left
      [  IMove SB (AInc 7) (DReg 0)
      ,  IMove SB (AInc 7) (DReg 1)
      ,  ICmp SB (DReg 0) (DReg 1)
      ,

      ++ translateCommand s1
      ++ [Left (IBcc BRA (Label "preskok_smece"))] ++  [Right (DLabel "neko_smece")]
      ++ translateCommand s2
      ++ [Right (DLabel "preskok_smece")]
-}

    -- NOTE kad je operacija usporedba
    ----------------------------------------------------------------
    RBinary rbop a1 a2 ->
      translateCommand (Expr a1)
      ++
      translateCommand (Expr a2)
      -- sad na stacku imam ta dva rezultata, ovisno o naredbi trebamo branch
      ++
      map Left
      [  IMove SB (AInc 7) (DReg 0)
      ,  IMove SB (AInc 7) (DReg 1)
      ,  ICmp SB (DReg 0) (DReg 1)
      ,  IBcc (case rbop of {Greater -> BCS; _ -> BCC}) (Label "neko_smece")] -- uvjet je okrenut
      ++ translateCommand s1
      ++ [Left (IBcc BRA (Label "preskok_smece"))] ++  [Right (DLabel "neko_smece")]
      ++ translateCommand s2
      ++ [Right (DLabel "preskok_smece")]

      -- problem: treba staviti i label nekud
      -- rjesenje: zasad eksplicitni label... znam da je smece al jebiga :D

-- aritmeticki izrazi -- kad zavrse, rezultat mora biti stavljen na vrh stoga
  -- TODO: div vjerojatno ne valja tj. treba puno toga provjeriti (mozda napravi interrupt!)
translateCommand (Expr exp) =
  case exp of
    -- ako je izraz samo varijabla, stavim njen label na vrh stoga
    -- TODO: ako varijabla ne postoji, skrsiti se ili ju inicijalizirati -- sad radim bez statea
    Var s -> map Left [IMove SB (Label (var_prefix ++ s)) (ADec 7)]
    IntConst i -> map Left [IMove SB (Const i) (ADec 7)]
    --Neg AExpr -> 
    -- za operaciju, e1 i e2 moram zapakirati u statement
    ABinary op e1 e2 -> translateCommand (Expr e1) ++ translateCommand (Expr e2) ++
      -- uzmem operande sa stoga
      map Left
      [ IMove SB (AInc 7) (DReg 0)
      , IMove SB (AInc 7) (DReg 1)
      -- izracunam kaj vec treba
      , mapOpToCommand op (DReg 0) (DReg 1)
      -- pomaknem rezultat na vrh stoga
      , IMove SB (DReg 1) (ADec 7)]

-- varijable == eksplicitni DS od tocno jednog bajta
-- plus prateca naredba u inicijalizaciji
translateCommand (Assign str aexp) =
  -- TODO: ako vec nije definiran label, naravno, al pustim to programu koji sve sklapa skupa
  -- TODO: trenutno nema promjene vrijednosti varijable jedamput kad ju definiram :D
  map Right [DDs SB (var_label) 1] -- definiram label
  ++
  translateCommand (Expr aexp)
  ++
  -- nakon evaluacije aexp-a, rezultat je na vrhu stoga
  map Left [IMove SB (AInc 7) (Label var_label)]
  where var_label = var_prefix ++ str

translate statement =
  let (cmds, dirs') = partitionEithers (translateCommand statement)
      dirs = nub dirs' -- nema smisla imati dvije iste direktive
      -- sve direktive hocu smjestiti na pocetak
      init = map Right (initializationDirectives ++ dirs ++ [DLabel startLabel]) ++ map Left initializationCode
      end = map Right endDirectives
      prog = init ++ map Left cmds ++ end
  in putStr $ concat $ map (either show show) prog

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




