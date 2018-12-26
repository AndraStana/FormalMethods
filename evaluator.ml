type tPrim = Tint| Tfloat| Tbool| Tvoid;;


type typ = Tprim of tPrim
    | Tclass of string
    | Tbot ;;


type vall = Vnull
    | Int of int
    | Float of float
    | Bool of bool
    | Vvoid


and

exp = Value of vall
        | Var of string 
        | Vfld of string * string (* value field = classInstanceName + fieldName *)
        | AsgnV of string * exp (* assign variable *)
        | AsgnF of string * string * exp (* assign field *) 
        | Blk of blkExp
        | Seq of exp * exp   
        | If of string * blkExp * blkExp (*  exp1 =then blk, exp2 = else blk *)
        | MthCall of string * string * varList (* className + methodName + variableList*)
        | And of exp * exp
        | Or of exp * exp
        | Not of exp
        | AddInt of exp * exp
        | MulInt of exp * exp
        | DivInt of exp * exp
        | DiffInt of exp * exp
        | AddFloat of exp * exp
        | MulFloat of exp * exp
        | DivFloat of exp * exp
        | DiffFloat of exp * exp
        | Eq of exp * exp (* equal *)
        | NEq of exp * exp (* not equal *)
        | Ge of exp * exp (* greater or equal *)
        | Gt of exp * exp (* greater than *)
        | Le of exp * exp (* lower or equal *)
        | Lt of exp * exp (* lower than *)
        | NewObj of string * varList
        | Cast of string * string (* Cast("className", "variableName") *)
        | InstanceOf of string * string (* InstanceOf("variableName", "className") *)
        | While of string * blkExp


and



varList = exp list

and

blkExp = Bvar of typ * string * exp
        | Bnvar of exp;;


type progr = (string * classDecl) list 

and

classDecl = (string * string * fldDeclList * mthDeclList)

and 

fldDeclList = fldDecl list

and 

fldDecl = typ * string 

and 

mthDeclList = mthDecl list

and

mthDecl = (typ * string * fPrmList * blkExp)

and

fPrmList = fPrm list

and

fPrm = (typ * string);;
