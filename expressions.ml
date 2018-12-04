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
                                            AsgnV( "o3", NewObj ("A", [(Value (Int 3))])),
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
(*-------------------------------------------------------------------------------------*)





let rec existsClassInProgram program className = match program with
    | [] -> false
    | _ when className="Object" -> true
    | head::tail -> match head with 
        | (cName, cDeclaration) when cName = className ->  true 
        | (_, classDeclaration) -> (existsClassInProgram tail className);;




(* let rec isSubclass program className1 className2 = match program with
    | [] -> false
    | head::tail ->( match head with
        | (classDecl, className) when className1 = className -> (match classDecl with ->
            | (_, baseClass, _, _) -> if baseClass = className2 then true else isSubclass()


        )

    ) *)

    





(* todo incomplete!! *)
let subtype program type1 type2 = match type1, type2 with
    | Tprim(Tint), Tprim(Tint) -> true
    | Tprim(Tfloat), Tprim(Tfloat) -> true
    | Tprim(Tbool), Tprim(Tbool) -> true
    | Tprim(Tvoid), Tprim(Tvoid) -> true
    | Tclass(className), Tclass("Object") -> (existsClassInProgram program className)
    | Tbot, Tclass(className) -> (existsClassInProgram program className)
    | Tclass(className1), Tclass(className2) -> className1 = className2 && (existsClassInProgram program className1) && (existsClassInProgram program className2) 
    (* | inheritance rule *)
    | _ , _ -> false;;
    

let myProgram = [ ("A",a ); ("B",b); ("Main", main)  ];;

let response = (existsClassInProgram myProgram "B");;

Printf.printf "%b\n" response;;

let subtypeResponse1 =  subtype myProgram (Tclass "B") (Tclass "Object")   ;;


(*doesn't work*)
(* 
let rec fieldlist program className = match program with
    | [] -> []
    | h::t -> match h with 
        | (cName, cDecl) when cName = className -> 
           ( match cDecl with (_,b,f,_) ->List.append f ( fieldlist program b ) )
        | (_ , cDecl) -> (fieldlist t className);;
*)

let rec fieldlist program className = match program with
    | [] -> []
    | h::t -> match h with 
        | (cName, cDecl) when cName = className -> 
           ( match cDecl with (_,_,f,_) -> f)
        | (_ , cDecl) -> (fieldlist t className);;

let rec getBaseClass program className = match program with 
    | [] -> "" 
    | h::t -> match h with 
        | (cName, cDecl) when cName = className ->
            (match cDecl with (_,base,_,_) -> base)
        | (_, cDecl) -> (getBaseClass t className);;

let rec getFields program className = match className with
  | "Object" -> []
  | aux -> (fieldlist program aux)::(getFields program (getBaseClass program aux ));;
 (* 
    | aux -> (fieldList program aux) ;; (*varianta 2*)
*)

let ast = [("A",a);("B",b);("Main",main)];;
let result=(getFields ast "B");; (*list of evaluator.typ and string *))
