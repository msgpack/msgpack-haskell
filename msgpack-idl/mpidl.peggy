idl :: Spec = decl* !.

decl :: Decl
  = "message" identifier typeParam "{" field* "}"
    { MPMessage $1 $2 $3 }
  / "exception" identifier typeParam ("<" identifier)? "{" field* "}"
    { MPException $1 $2 $3 $4 }
  / "type" identifier "=" ftype
    { MPType $1 $2 }
  / "enum" identifier "{" (integer ":" identifier)* "}"
    { MPEnum $1 $2 }
  / "service" identifier (":" integer)? "{" method* "}"
    { MPService $1 $2 $3 }

typeParam :: [T.Text]
  = "<" (identifier, ",") ">"
  / "" { [] }

method :: Method
  = "inherit" identifier { InheritName $1 }
  / "inherit" "*" { InheritAll }
  / "inherit"? rtype identifier "(" (field , ",") ")"
    { Function (isJust $1) $3 $2 $4 }

field :: Field
  = integer ":" ftype identifier ("=" literal)?
    { Field $1 $2 $3 $4 }

ftype :: Type
  = ftypeNN "?" { TNullable $1 }
  / ftypeNN

rtype :: Maybe Type
  = "void" { Nothing }
  / ftype { Just $1 }

ftypeNN :: Type
  = "byte"   { TInt True  8  }
  / "short"  { TInt True  16 }
  / "int"    { TInt True  32 }
  / "long"   { TInt True  64 }
  / "ubyte"  { TInt False 8  }
  / "ushort" { TInt False 16 }
  / "uint"   { TInt False 32 }
  / "ulong"  { TInt False 64 }
  / "float"  { TFloat False }
  / "double" { TFloat True }
  / "bool"   { TBool }
  / "raw"    { TRaw }
  / "string" { TString }
  / "object" { TObject }

  / "list" "<" ftype ">" { TList $1 }
  / "map" "<" ftype "," ftype ">" { TMap $1 $2 }
  / "tuple" "<" (ftype , ",") ">" { TTuple $1 }

  / identifier ("<" (ftype , ",") ">")?
    { TUserDef $1 (fromMaybe [] $2) }

literal ::: Literal
  = integer { LInt $1 }
  / "true"  { LBool True }
  / "false" { LBool False }
  / "null"  { LNull }
  / '\"' charLit* '\"' { LString $ T.pack $1 }

charLit :: Char
  = '\\' escChar
  / ![\'\"] .

escChar :: Char
  = 'n' { '\n' }
  / 'r' { '\r' }
  / 't' { '\t' }
  / '\\' { '\\' }
  / '\"' { '\"' }
  / '\'' { '\'' }

integer ::: Int
  = [0-9]+ { read $1 }

identifier ::: T.Text
  = [a-zA-Z_][a-zA-Z0-9_]* { T.pack ($1 : $2) }

skip :: () = [ \r\n\t] { () } / comment
comment :: () = '#' _:(!'\n' . { () })* '\n' { () }
delimiter :: () = [()[\]{}<>;:,./?] { () }
