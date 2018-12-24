open Evaluator


(* if m then {z=z+1} else {z=2} *)
let exp= If ("m",
                Bnvar ( AsgnV ("z", AddInt ( Var "z",(Value (Int 1)) ))),
                Bnvar ( AsgnV ("z", Value (Int 2) ))
            );;


(*-------------------------------------------------------------------------------------*)
(*
class A extends Object
    {
        int f1;
        #
        int m1(int a, int b) { (int c)
        c=a+b;this.f1=this.f1+c;c};
    }
*)



(* classDecl *)
let a  = ("A", "Object",

        (* fields Declaration List *)
        [ (Tprim Tint),"f1"],

        (* methods Declaration List *)
        [
            (
                (Tprim Tint), "m1",

                (* parameters List *)
                [
                    (Tprim Tint, "a" );
                    (Tprim Tint, "b" )
                ],

                (* body *)
                Blk ( Bvar (  (Tprim Tint), "c",   

                        (* Seq(  exp, Seq ( exp, exp)  ) *)
                        Seq(  AsgnV ("c", AddInt ( Var "a", Var "b"))   ,
                            Seq ( 
                                AsgnF("this","f1", AddInt( Vfld ("this","f1"), Var "c") ),
                                (Var "c")
                            ) 
                        )
                    )
                )  
            )         
        ]

    );;

(*-------------------------------------------------------------------------------------*)

(*
class B extends A
{
    A f2;
    #
    A m2(A x, A y) {
        (A z) 
            { (int n)
                n=x.m1(1,2)-y.m1(2,1);
                {
                    (bool m) m=(x.f1-y.f1)>n;
    
                    if m then {z=new A(m)} else {z=new A(n)}
                }
            };
        this.f2=z;z
    }
}
*)


(* classDecl *)
let b = ("B", "A",

            (* fields Declaration List *)
            [
               (Tclass "A", "f2") 
            ],


            (* methods Declaration List *)
            [
                (
                    (Tclass "A") , "m2",

                    (* parameters List *)
                    [
                        ( Tclass "A", "x");
                        ( Tclass "A", "y")
                    ],

                    (* body *)
                   Blk (
                        Bvar(
                            (Tclass "A"), "z",
                            Seq ( 
                                Blk(
                                    Bvar( 
                                        (Tprim Tint), "n",
                                        Seq( 
                                            AsgnV( "n",
                                                DiffInt (
                                                    MthCall ("x","m1", [ Value (Int 1);  Value (Int 2)  ]),
                                                    MthCall ("y","m1", [ Value (Int 2); Value (Int 1)  ])
                                                )
                                            ),
                                            Blk(
                                                Bvar(
                                                    (Tprim Tbool), "m",
                                                    Seq(
                                                        AsgnV("m", 
                                                            Gt(    
                                                                DiffInt(  Vfld("x", "f1"), Vfld("y", "f1")),
                                                                (Var "n")
                                                            )                                                        
                                                        ) 
                                                        , 
                                                        If ("m",
                                                            Bnvar ( AsgnV ("z", NewObj ("A", [ (Var "m") ] ))  ),
                                                            Bnvar ( AsgnV ("z", NewObj ("A", [ (Var "n") ] ) ))
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                ),
                                Seq(
                                    AsgnF("this","f2", (Var "z") ),
                                    (Var "z")
                                )
                            )
                        )
                   )
                )
            ]
 );;

 (*-------------------------------------------------------------------------------------*)

(*
Class Main extends Object
{ #
    Void main(){ 
        (B o1) o1=new B(0,null);
        { 
            (A o2) o2=new A(2);
            {  
                (A o3) o3=new A(3);
                o2 =o1.m2(o2,o3)
            }
        }
    }
}
*)
(*class decl*)
let main=("Main", "Object",
    (*fields decl list*)
    [],
    (*methods decl list*)
    [
        (Tprim Tvoid), "main", 
        (*param list*)
        [],
        (*body*)
        Blk(
            Bvar( 
                (Tclass "B"), "o1",
                Seq(
                    AsgnV( "o1", NewObj ( "B", [ Value (Int 0); Value (Vnull)])),
                    Blk(
                        Bvar( (Tclass "A"), "o2",
                            Seq (
                                AsgnV("o2", NewObj ( "A", [Value (Int 2) ])), 
                                Blk(
                                    Bvar(
                                        (Tclass "A"), "o3",
                                        Seq(
                                            AsgnV("o3", NewObj ("A", [(Value (Int 3))])),
                                            AsgnV("o2", MthCall ("o1", "m2", [ ( Var "o2"); (Var "o3")]))
                                        )
                                    )
                                )
                            ) 
                        )
                    )
                )
            )
        )
    ]
);;

let myProgram = [ ("A",a ); ("B",b); ("Main", main)  ];;

(*-------------------------------------------------------------------------------------*)


 (* *********************************         EXCEPTIONS          *********************************       *)


exception VariableNotDefinedException of string;;
exception ClassNotDefinedException of string;;
exception TypesDontMatchException of string;;


 (* *********************************         PRINTING FUNCTIONS          *********************************       *)


let print_typ givenType = match givenType with
    | (Tprim Tint) -> Printf.printf "int "
    | (Tprim Tfloat) -> Printf.printf "float "
    | (Tprim Tbool) -> Printf.printf "bool "
    | (Tprim Tvoid) -> Printf.printf "void "
    | (Tclass className) -> Printf.printf "Class: %s " className
    | (Tbot) -> Printf.printf "bottom "
    | _ -> Printf.printf "TYPE NOT FOUND ";;
    


 let rec print_field_list list = match list with
    | [] ->()
    | h::t -> match h with 
        | (fName, fType) ->  Printf.printf "Field name: %s " fName;  (print_typ fType);  Printf.printf " \n"; (print_field_list t);;

       

(*-------------------------------------------------------------------------------------*)

 (* *********************************         SUBTYPING          *********************************       *)


let rec existsClassInProgram program className = match program with
    | [] -> false
    | _ when className="Object" -> true
    | head::tail -> match head with 
        | (cName, cDeclaration) when cName = className ->  true 
        | (_, classDeclaration) -> (existsClassInProgram tail className);;


(* B extends A => subtype B A = true *)
(* needed wholeProgram parameter in order to search again in the whole program whether class B is a subtype of a subtype of class A *)
 let rec isSubclassRec wholeProgram program className1 className2 = match program, className1, className2 with
    | _, c1,c2 when  c1 = c2  -> true
    | [],_,_ -> false
    | head::tail,c1,c2 -> match head with
        | (className, classDecl) when c1 = className -> (match classDecl with 
            | (_, baseClass, _, _) -> if baseClass = c2 then true else (isSubclassRec wholeProgram wholeProgram baseClass c2) )
        | (_ , _)-> ( isSubclassRec wholeProgram tail c1 c2 ) ;;

let isSubclass program className1 className2 = (isSubclassRec program program className1 className2);;



let subtype program type1 type2 = match type1, type2 with
    | Tprim(Tint), Tprim(Tint) -> true
    | Tprim(Tfloat), Tprim(Tfloat) -> true
    | Tprim(Tbool), Tprim(Tbool) -> true
    | Tprim(Tvoid), Tprim(Tvoid) -> true
    | Tclass(className), Tclass("Object") -> (existsClassInProgram program className)
    | Tbot, Tclass(className) -> (existsClassInProgram program className)
    | Tclass(className1), Tclass(className2) -> (existsClassInProgram program className1) && (existsClassInProgram program className2) && (isSubclass program className1 className2 )
    | _ , _ -> false;;


Printf.printf "\n \n----------- SUBTYPING TESTS -------------\n \n";;

(* Testing FUNCTION: existsClassInProgram *)
let response = (existsClassInProgram myProgram "Main");;
Printf.printf "Exists in program: %b\n" response;;

(* Testing FUNCTION: subtype *)
let subtypeResponse1 =  subtype myProgram (Tclass "B") (Tclass "Object") ;;
Printf.printf "%b --> subtype: B is subtype of Object \n" subtypeResponse1;;
let subtypeResponse2 =  subtype myProgram Tbot (Tclass "Main") ;;
Printf.printf "%b --> subtype: bottom is subtype of Main \n" subtypeResponse2;;
let subtypeResponse3=  subtype myProgram (Tclass "B") (Tclass "A") ;;
Printf.printf "%b --> subtype: B is subtype of A \n" subtypeResponse3;;
let subtypeResponse4=  subtype myProgram (Tclass "Main") (Tclass "A") ;;
Printf.printf "%b --> subtype: Main is subtype of A \n" subtypeResponse4;;


 (* *********************************         FIELDLIST          *********************************       *)



(* -------------------------------------------------------------------------------------------------------- *)

(*doesn't work*)
(* 
let rec fieldlist program className = match program with
    | [] -> []
    | h::t -> match h with 
        | (cName, cDecl) when cName = className -> 
           ( match cDecl with (_,b,f,_) ->List.append f ( fieldlist program b ) )
        | (_ , cDecl) -> (fieldlist t className);;
*)

(*transform field list from (type,string) to (string,type) *)
let rec reverse_list lst = match lst with
    | [] -> []
    | h::t -> match h with 
         (fType,fName) -> (fName,fType):: (reverse_list t);;

let rec fieldlist program className = match program with
    | [] -> []
    | h::t -> match h with 
        | (cName, cDecl) when cName = className -> 
           ( match cDecl with (_,_,f,_) -> (reverse_list f) )
        | (_ , cDecl) -> (fieldlist t className);;

let rec getBaseClass program className = match program with 
    | [] -> "" 
    | h::t -> match h with 
        | (cName, cDecl) when cName = className ->
            (match cDecl with (_,base,_,_) -> base)
        | (_, cDecl) -> (getBaseClass t className);;

let rec getFields program className = match className with
  | "Object" -> []
  | aux -> List.append (fieldlist program aux) (getFields program (getBaseClass program aux ));;
 (* 
    | aux -> (fieldList program aux) ;; (*varianta 2*)
*)




Printf.printf "\n \n----------- FIELDLIST TESTS -------------\n \n";;

let ast = [("A",a);("B",b);("Main",main)];;

let result1=(getFields ast "A");; (*list of evaluator.typ and string *)
Printf.printf "\t  Field list of classss A: \n";;
(print_field_list result1);;


let result2=(getFields ast "B");; (*list of evaluator.typ and string *)
Printf.printf "\t  Field list of classss B: \n";;
(print_field_list result2);;



(*---------------------Well-typed expressions------------- *)
(* looks for a field with given name in env= list of (string*type) and returns its type*)


      (*``````````````````````` Temporary commenting this  `````````````````````*)
   (* 

let rec typeFromEnv env vName = match env with
    | [] -> raise (VariableNotDefinedException vName)
    | h::t -> match h with
        | (name,vType) when name=vName -> vType
        | (_,vType) -> (typeFromEnv t vName);; 
    
 let wellTypedExpr program environment expCrt = match expCrt with
    | Value (Vnull) -> Tbot
    | Value (Int v ) -> Tprim Tint 
    | Value (Float v ) -> Tprim Tfloat
    | Value (Vvoid) -> Tprim Tvoid
    | Value (Bool v ) -> Tprim Tbool 
    | Var v -> typeFromEnv environment v
    (*value field= className + fieldName *)
    | Vfld (classN, fieldN) -> if (existsClassInProgram program classN) then (typeFromEnv (getFields program classN) fieldN ) else raise (ClassNotDefinedException classN)
    (* var=expression; exp must be a subtype of var*)
   
  
   | AsgnV (varN,exp) -> (
        let varType= ( wellTypedExpr program environment (Var varN)) and expType= (wellTypedExpr program env exp) in
        if (subtype program expType varType ) then
            (Tprim Tvoid)
        else 
            raise (TypesDontMatchException "@Assign var")
    )
    (* classN.fieldN=exp*)
    | AssgnF (classN,fieldN,exp) -> 
    (
        if (existsClassInProgram program classN) then
        (
            let fieldType= (typeFromEnv (getFields program classN) fieldN) and expType= (wellTypedExpr program exp) in
            if (subtype program expType fieldType) then
                (Tprim Tvoid)
            else
                raise (TypesDontMatchException "@Assign field")
        )
        else raise (ClassNotDefinedException classN)
    )
    (*Bvar: typ var=expr *)

    | Bvar (typ,varN,exp) -> ;; 



(*environment is a list of (string*typ) *)
 let env =  [("f2",(Tclass "A"));("f1",(Tprim Tint))];; 
(*
let result_type= wellTypedExpr ast env (Value (Bool true));;
let result_type= wellTypedExpr ast env (Var "n");;
*)
 let result_type= wellTypedExpr ast env (Vfld ("A","Field"));; 
 
   *)

