type scalval =
    SVar of int
  | SLit of float

type boolval =
    BVar of int
  | BLit of bool

type intinst =
    SCAL of int * scalval
  | BOOL of int * boolval
  | STR of int * string
  | ADD of int * scalval * scalval
  | SUB of int * scalval * scalval
  | MUL of int * scalval * scalval
  | DIV of int * scalval * scalval
  | POW of int * scalval * scalval
  | MADD of int * int * int
  | MSUB of int * int * int
  | MMUL of int * int * int
  | SMUL of int * int * scalval
  | MPOW of int * int * scalval
  | INIT of int
  | SET of int * scalval * scalval * scalval
  | GET of int * scalval * scalval * int
  | TRAN of int * int
  | DIM of int * scalval * scalval
  | PRINT of int
  | SINI of int
  | SCPY of int * int
  | SFRE of int
  | SCAT of int * int * int
  | SCMP of int * int * int
  | SFSC of int * scalval
  | SFMA of int * int
  | SLT of int * scalval * scalval
  | SLE of int * scalval * scalval
  | SGT of int * scalval * scalval
  | SGE of int * scalval * scalval
  | SEQ of int * scalval * scalval
  | SNE of int * scalval * scalval
  | SMEQ of int * int * int
  | SMNE of int * int * int
  | OR of int * boolval * boolval
  | AND of int * boolval * boolval
  | NOT of int * boolval
  | BRAF of boolval * int
  | JMP of int
  | LABL of int
  | ROWS of int * int
  | COLS of int * int
  | RDIM of int * scalval * scalval

type iprog = intinst list
