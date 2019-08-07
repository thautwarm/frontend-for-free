# RBNF

## Usage

```
RBNF -in <input json filename>
     [-aeson <output json filename>]

     # pretty print generated ir:
     [-dump]
     # look ahead k
     -k int
     # to specify if traceback supported(with a little performance cost)
     [-trace]
```

## Note: Front End
```haskell
infix 5 -->
infix 6 |=

number = "number"
negation = "-"
multiply = "*"

a |= b = CBind a b

a --> b = (a, b, Nothing)
case' a = CTerm (Case a)
parsers = S.fromList [
    "Number"   --> case' number
    , "Factor" --> CAlt [
            CNonTerm "Number",
            CSeq [case' negation, "a" |= CNonTerm "Factor" ]
        ]
    , "Mul"    --> CAlt [
            CSeq [ CPred (MTerm "always_true"), CNonTerm "Factor" ],
            CSeq [ CNonTerm "Mul", case' multiply, CNonTerm "Factor"]
        ]
    ]
```

Its equivalent notation of BNF derivative is

```
Number -> number
Factor -> Number | '-' a=Factor
Mul    -> ?always_true Factor
        | Mul '*' Factor
```

## Note: Left Recursion Hamdling

Left recursion handling, lookahead decision trees,
type inference, etc.

- Left recursion handling:

![Left recur](./lr.png)


# Note: Lookahead decision trees(by ID3 algorithm)

```
======== Node1 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [6]
          LAShift (Case "number")
              [6]
      LAShift (Case "number")
          [6]
  LAShift (Case "number")
      [12]
--- LA optimization:
case elts[0]
      LAShift (Case "-") => [6]
      LAShift (Case "number") => [12]

======== Node19 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [20]
          LAShift (Case "number")
              [20]
      LAShift (Case "number")
          LAShift (Case "*")
              [20]
  LAShift (Case "number")
      LAShift (Case "*")
          LAShift (Case "-")
              [28]
          LAShift (Case "number")
              [28]
--- LA optimization:
case elts[1]
      LAShift (Case "*") => [28]
      LAShift (Case "-") => [20]
      LAShift (Case "number") => [20]

======== Node38 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [39]
          LAShift (Case "number")
              [39]
      LAShift (Case "number")
          LAShift (Case "*")
              [39]
  LAShift (Case "number")
      LAShift (Case "*")
          [48]
--- LA optimization:
case elts[1]
      LAShift (Case "*") => [48]
      LAShift (Case "-") => [39]
      LAShift (Case "number") => [39]
```



## Note: Type Inference and Code Generation

The type inference is based on HM unification and propositioinal logics(logic formulas in disjunctive normal form).

RBNF currentlt provides 2 sorts of IRs named after alphabet table:

- A IR: Untyped, no declarations

    ```haskell
    data AIR
        = AAssign AName AIR
        | ACall AIR [AIR]
        | AAttr AIR String
        | APrj  AIR Int
        | AIf AIR AIR AIR
        | AWhile AIR AIR
        | ASwitch AIR [(AIR, AIR)] AIR
        | ADef AName [AName] AIR
        | ABlock [AIR]
        -- literal
        | AVar AName
        | AInt Integer
        | AStr String
        | ABool Bool
        | ATuple [AIR]
        | AAnd AIR AIR
        | AOr  AIR AIR
    ```
- B IR: typed, with correct declarations and scoping resolutions.

    ```haskell
    data BBase a
    = BAssign AName a
    | BDecl AName a
    | BCall a [a]
    | BAttr a String
    | BPrj  a Int
    | BIf a a a
    | BWhile a a
    | BSwitch a [(a, a)] a
    | BDef [AName] a
    | BMutual [AName] [a]
    -- literal
    | BVar AName
    | BInt Integer
    | BStr String
    | BBool Bool
    | BTuple [a]
    | BAnd a a
    | BOr  a a

    data FixT f t = InT {tag :: t, outT :: f (FixT f t)}
    type BIR a = FixT BBase a
    ```

## Pretty Printing of Generated A IR


```
def lr.step.Mul(.slot.0, %state, %tokens)
    %off =  %tokens.offset
    .slot.1 =  %match_tk(%tokens, %tk_id("*"))
    if %==(.slot.1, %null)
    then %null
    else .tmp.0.flag =  False
         .tmp.0.result =
             if %peekable(%tokens, 1)
             then switch %peek(%tokens, 1).idint
                  case %tk_id("number") :
                    %off =  %tokens.offset
                    .slot.2 =  %match_tk(%tokens, %tk_id("-"))
                    if %==(.slot.2, %null)
                    then %null
                    else .slot.3.check =  parse.Factor(%state, %tokens)
                         if %==(.slot.3.check, %null)
                         then %null
                         else .slot.3 =  .slot.3.check
                              Mul.Factor.a.0 =  .slot.3
                              .slot.-1 =  tuple(.slot.2, .slot.3)
                              .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                              .slot.-3 =  tuple(.slot.0, .slot.1, .slot.-2)
                              .slot.-4 =  %mk_ast("Mul", tuple(.slot.-3))
                              .slot.-4
                  case %tk_id("-") :
                    %off =  %tokens.offset
                    .slot.2 =  %match_tk(%tokens, %tk_id("-"))
                    if %==(.slot.2, %null)
                    then %null
                    else .slot.3.check =  parse.Factor(%state, %tokens)
                         if %==(.slot.3.check, %null)
                         then %null
                         else .slot.3 =  .slot.3.check
                              Mul.Factor.a.0 =  .slot.3
                              .slot.-1 =  tuple(.slot.2, .slot.3)
                              .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                              .slot.-3 =  tuple(.slot.0, .slot.1, .slot.-2)
                              .slot.-4 =  %mk_ast("Mul", tuple(.slot.-3))
                              .slot.-4
                  case %tk_id("*") :
                    .slot.2.check =  parse.Number(%state, %tokens)
                    if %==(.slot.2.check, %null)
                    then %null
                    else .slot.2 =  .slot.2.check
                         .slot.-1 =  %mk_ast("Factor", tuple(.slot.2))
                         .slot.-2 =  tuple(.slot.0, .slot.1, .slot.-1)
                         .slot.-3 =  %mk_ast("Mul", tuple(.slot.-2))
                         .slot.-3
                  default : .tmp.0.flag =  True
                            %null
             else %null
         if %==(%null, .tmp.0.result) or .tmp.0.flag
         then .tmp.0.result
         else %null
def lr.loop.Mul(.slot.0, %state, %tokens)
    lr.Mul.try =  lr.step.Mul(.slot.0, %state, %tokens)
    while %!=(lr.Mul.try, %null)
        .slot.0 =  lr.Mul.try
        lr.Mul.try =  lr.step.Mul(.slot.0, %state, %tokens)
    .slot.0
def parse.Factor(%state, %tokens)
    .tmp.0.flag =  False
    .tmp.0.result =
        if %peekable(%tokens, 0)
        then switch %peek(%tokens, 0).idint
             case %tk_id("number") :
               %off =  %tokens.offset
               .slot.0 =  %match_tk(%tokens, %tk_id("number"))
               if %==(.slot.0, %null)
               then %null
               else .slot.-1 =  %mk_ast("Number", tuple(.slot.0))
                    .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                    .slot.-2
             case %tk_id("-") :
               %off =  %tokens.offset
               .slot.0 =  %match_tk(%tokens, %tk_id("-"))
               if %==(.slot.0, %null)
               then %null
               else .slot.1.check =  parse.Factor(%state, %tokens)
                    if %==(.slot.1.check, %null)
                    then %null
                    else .slot.1 =  .slot.1.check
                         Factor.a.0 =  .slot.1
                         .slot.-1 =  tuple(.slot.0, .slot.1)
                         .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                         .slot.-2
             default : .tmp.0.flag =  True
                       %null
        else %null
    if %==(%null, .tmp.0.result) or .tmp.0.flag
    then .tmp.0.result
    else %null
def parse.Mul(%state, %tokens)
    if always_true(%state)
    then .tmp.0.flag =  False
         .tmp.0.result =
             if %peekable(%tokens, 1)
             then switch %peek(%tokens, 1).idint
                  case %tk_id("number") :
                    %off =  %tokens.offset
                    .slot.0 =  %match_tk(%tokens, %tk_id("-"))
                    if %==(.slot.0, %null)
                    then %null
                    else .slot.1.check =  parse.Factor(%state, %tokens)
                         if %==(.slot.1.check, %null)
                         then %null
                         else .slot.1 =  .slot.1.check
                              Mul.Factor.a.0 =  .slot.1
                              .slot.-1 =  tuple(.slot.0, .slot.1)
                              .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                              .slot.-3 =  %mk_ast("Mul", tuple(.slot.-2))
                              lr.loop.Mul(.slot.-3, %state, %tokens)
                  case %tk_id("-") :
                    %off =  %tokens.offset
                    .slot.0 =  %match_tk(%tokens, %tk_id("-"))
                    if %==(.slot.0, %null)
                    then %null
                    else .slot.1.check =  parse.Factor(%state, %tokens)
                         if %==(.slot.1.check, %null)
                         then %null
                         else .slot.1 =  .slot.1.check
                              Mul.Factor.a.0 =  .slot.1
                              .slot.-1 =  tuple(.slot.0, .slot.1)
                              .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                              .slot.-3 =  %mk_ast("Mul", tuple(.slot.-2))
                              lr.loop.Mul(.slot.-3, %state, %tokens)
                  case %tk_id("*") :
                    %off =  %tokens.offset
                    .slot.0 =  %match_tk(%tokens, %tk_id("number"))
                    if %==(.slot.0, %null)
                    then %null
                    else .slot.-1 =  %mk_ast("Number", tuple(.slot.0))
                         .slot.-2 =  %mk_ast("Factor", tuple(.slot.-1))
                         .slot.-3 =  %mk_ast("Mul", tuple(.slot.-2))
                         lr.loop.Mul(.slot.-3, %state, %tokens)
                  default : .tmp.0.flag =  True
                            %null
             else %null
         if %==(%null, .tmp.0.result) or .tmp.0.flag
         then .tmp.0.result
         else %null
    else %null
def parse.Number(%state, %tokens)
    %off =  %tokens.offset
    .slot.0 =  %match_tk(%tokens, %tk_id("number"))
    if %==(.slot.0, %null)
    then %null
    else .slot.-1 =  %mk_ast("Number", tuple(.slot.0))
         .slot.-1
```

## Pretty Printing of Generated B IR

```
rec lr.step.Mul : (State,Tokens) -> ast () =
    (.slot.0, %state, %tokens) ->
        var %off :int =  [int]%tokens.offset
        var .slot.1 :(bool,any) =  [(bool,any)]
                                   %match_tk(%tokens, [int] %tk_id("*"))
        [ast ()]if [bool]
                   %==(.slot.1, %null)
        then %null
        else var .tmp.0.flag :bool =  False
             var .tmp.0.result :ast () =
                 [ast ()]if [bool]
                            %peekable(%tokens, 1)
                 then switch [int][token ()]
                                  %peek(%tokens, 1).idint
                      case [int]
                           %tk_id("number") :
                        %off :int =  [int]%tokens.offset
                        var .slot.2 :(bool,any) =  [(bool,any)]
                                                   %match_tk(%tokens,
                                                   [int]
                                                   %tk_id("-"))
                        [ast ()]if [bool]
                                   %==(.slot.2, %null)
                        then %null
                        else var .slot.3.check :ast () =  [ast ()]
                                                          parse.Factor(%state,
                                                          %tokens)
                             [ast ()]if [bool]
                                        %==(.slot.3.check, %null)
                             then %null
                             else var .slot.3 :ast () =  .slot.3.check
                                  var Mul.Factor.a.0 :ast () =  .slot.3
                                  var .slot.-1 :((bool,any),ast ()) =  [((bool,any),ast ())]tuple( .slot.2
                                                                       , .slot.3 )
                                  var .slot.-2 :ast () =  [ast ()]
                                                          %mk_ast("Factor",
                                                          [((bool,any),ast ())]tuple( .slot.-1 ))
                                  var .slot.-3 :((ast (),bool,any),ast ()) =  [((ast (),bool,any),ast ())]tuple( .slot.0
                                                                              , .slot.1
                                                                              , .slot.-2 )
                                  var .slot.-4 :ast () =  [ast ()]
                                                          %mk_ast("Mul",
                                                          [((ast (),bool,any),ast ())]tuple( .slot.-3 ))
                                  .slot.-4
                      case [int]
                           %tk_id("-") :
                        %off :int =  [int]%tokens.offset
                        var .slot.2 :(bool,any) =  [(bool,any)]
                                                   %match_tk(%tokens,
                                                   [int]
                                                   %tk_id("-"))
                        [ast ()]if [bool]
                                   %==(.slot.2, %null)
                        then %null
                        else var .slot.3.check :ast () =  [ast ()]
                                                          parse.Factor(%state,
                                                          %tokens)
                             [ast ()]if [bool]
                                        %==(.slot.3.check, %null)
                             then %null
                             else var .slot.3 :ast () =  .slot.3.check
                                  var Mul.Factor.a.0 :ast () =  .slot.3
                                  var .slot.-1 :((bool,any),ast ()) =  [((bool,any),ast ())]tuple( .slot.2
                                                                       , .slot.3 )
                                  var .slot.-2 :ast () =  [ast ()]
                                                          %mk_ast("Factor",
                                                          [((bool,any),ast ())]tuple( .slot.-1 ))
                                  var .slot.-3 :((ast (),bool,any),ast ()) =  [((ast (),bool,any),ast ())]tuple( .slot.0
                                                                              , .slot.1
                                                                              , .slot.-2 )
                                  var .slot.-4 :ast () =  [ast ()]
                                                          %mk_ast("Mul",
                                                          [((ast (),bool,any),ast ())]tuple( .slot.-3 ))
                                  .slot.-4
                      case [int]
                           %tk_id("*") :
                        var .slot.2.check :ast () =  [ast ()]
                                                     parse.Number(%state,
                                                     %tokens)
                        [ast ()]if [bool]
                                   %==(.slot.2.check, %null)
                        then %null
                        else var .slot.2 :ast () =  .slot.2.check
                             var .slot.-1 :ast () =  [ast ()]
                                                     %mk_ast("Factor",
                                                     [ast ()]tuple(.slot.2))
                             var .slot.-2 :((ast (),bool,any),ast ()) =  [((ast (),bool,any),ast ())]tuple( .slot.0
                                                                         , .slot.1
                                                                         , .slot.-1 )
                             var .slot.-3 :ast () =  [ast ()]
                                                     %mk_ast("Mul",
                                                     [((ast (),bool,any),ast ())]tuple( .slot.-2 ))
                             .slot.-3
                      default : .tmp.0.flag :bool =  True
                                %null
                 else %null
             [ast ()]if [bool]
                        %==(%null, .tmp.0.result) or .tmp.0.flag
             then .tmp.0.result
             else %null
rec lr.loop.Mul : (State,Tokens) -> ast () =
    (.slot.0, %state, %tokens) ->
        var lr.Mul.try :ast () =  [ast ()]
                                  lr.step.Mul(.slot.0, %state, %tokens)
        [()]while [bool]
                  %!=(lr.Mul.try, %null)
            .slot.0 :ast () =  lr.Mul.try
            lr.Mul.try :ast () =  [ast ()]
                                  lr.step.Mul(.slot.0, %state, %tokens)
        .slot.0
rec parse.Factor : (State,Tokens) -> ast () =
    (%state, %tokens) ->
        var .tmp.0.flag :bool =  False
        var .tmp.0.result :ast () =
            [ast ()]if [bool]
                       %peekable(%tokens, 0)
            then switch [int][token ()]
                             %peek(%tokens, 0).idint
                 case [int]
                      %tk_id("number") :
                   var %off :int =  [int]%tokens.offset
                   var .slot.0 :(bool,any) =  [(bool,any)]
                                              %match_tk(%tokens,
                                              [int]
                                              %tk_id("number"))
                   [ast ()]if [bool]
                              %==(.slot.0, %null)
                   then %null
                   else var .slot.-1 :ast () =  [ast ()]
                                                %mk_ast("Number",
                                                [(bool,any)]tuple(.slot.0))
                        var .slot.-2 :ast () =  [ast ()]
                                                %mk_ast("Factor",
                                                [ast ()]tuple(.slot.-1))
                        .slot.-2
                 case [int]
                      %tk_id("-") :
                   var %off :int =  [int]%tokens.offset
                   var .slot.0 :(bool,any) =  [(bool,any)]
                                              %match_tk(%tokens,
                                              [int]
                                              %tk_id("-"))
                   [ast ()]if [bool]
                              %==(.slot.0, %null)
                   then %null
                   else var .slot.1.check :ast () =  [ast ()]
                                                     parse.Factor(%state,
                                                     %tokens)
                        [ast ()]if [bool]
                                   %==(.slot.1.check, %null)
                        then %null
                        else var .slot.1 :ast () =  .slot.1.check
                             var Factor.a.0 :ast () =  .slot.1
                             var .slot.-1 :((bool,any),ast ()) =  [((bool,any),ast ())]tuple( .slot.0
                                                                  , .slot.1 )
                             var .slot.-2 :ast () =  [ast ()]
                                                     %mk_ast("Factor",
                                                     [((bool,any),ast ())]tuple( .slot.-1 ))
                             .slot.-2
                 default : .tmp.0.flag :bool =  True
                           %null
            else %null
        [ast ()]if [bool]
                   %==(%null, .tmp.0.result) or .tmp.0.flag
        then .tmp.0.result
        else %null
rec parse.Mul : (State,Tokens) -> ast () =
    (%state, %tokens) ->
        [ast ()]if [bool]
                   always_true(%state)
        then var .tmp.0.flag :bool =  False
             var .tmp.0.result :ast () =
                 [ast ()]if [bool]
                            %peekable(%tokens, 1)
                 then switch [int][token ()]
                                  %peek(%tokens, 1).idint
                      case [int]
                           %tk_id("number") :
                        var %off :int =  [int]%tokens.offset
                        var .slot.0 :(bool,any) =  [(bool,any)]
                                                   %match_tk(%tokens,
                                                   [int]
                                                   %tk_id("-"))
                        [ast ()]if [bool]
                                   %==(.slot.0, %null)
                        then %null
                        else var .slot.1.check :ast () =  [ast ()]
                                                          parse.Factor(%state,
                                                          %tokens)
                             [ast ()]if [bool]
                                        %==(.slot.1.check, %null)
                             then %null
                             else var .slot.1 :ast () =  .slot.1.check
                                  var Mul.Factor.a.0 :ast () =  .slot.1
                                  var .slot.-1 :((bool,any),ast ()) =  [((bool,any),ast ())]tuple( .slot.0
                                                                       , .slot.1 )
                                  var .slot.-2 :ast () =  [ast ()]
                                                          %mk_ast("Factor",
                                                          [((bool,any),ast ())]tuple( .slot.-1 ))
                                  var .slot.-3 :ast () =  [ast ()]
                                                          %mk_ast("Mul",
                                                          [ast ()]tuple( .slot.-2 ))
                                  [ast ()]
                                  lr.loop.Mul(.slot.-3, %state, %tokens)
                      case [int]
                           %tk_id("-") :
                        var %off :int =  [int]%tokens.offset
                        var .slot.0 :(bool,any) =  [(bool,any)]
                                                   %match_tk(%tokens,
                                                   [int]
                                                   %tk_id("-"))
                        [ast ()]if [bool]
                                   %==(.slot.0, %null)
                        then %null
                        else var .slot.1.check :ast () =  [ast ()]
                                                          parse.Factor(%state,
                                                          %tokens)
                             [ast ()]if [bool]
                                        %==(.slot.1.check, %null)
                             then %null
                             else var .slot.1 :ast () =  .slot.1.check
                                  var Mul.Factor.a.0 :ast () =  .slot.1
                                  var .slot.-1 :((bool,any),ast ()) =  [((bool,any),ast ())]tuple( .slot.0
                                                                       , .slot.1 )
                                  var .slot.-2 :ast () =  [ast ()]
                                                          %mk_ast("Factor",
                                                          [((bool,any),ast ())]tuple( .slot.-1 ))
                                  var .slot.-3 :ast () =  [ast ()]
                                                          %mk_ast("Mul",
                                                          [ast ()]tuple( .slot.-2 ))
                                  [ast ()]
                                  lr.loop.Mul(.slot.-3, %state, %tokens)
                      case [int]
                           %tk_id("*") :
                        var %off :int =  [int]%tokens.offset
                        var .slot.0 :(bool,any) =  [(bool,any)]
                                                   %match_tk(%tokens,
                                                   [int]
                                                   %tk_id("number"))
                        [ast ()]if [bool]
                                   %==(.slot.0, %null)
                        then %null
                        else var .slot.-1 :ast () =  [ast ()]
                                                     %mk_ast("Number",
                                                     [(bool,any)]tuple(.slot.0))
                             var .slot.-2 :ast () =  [ast ()]
                                                     %mk_ast("Factor",
                                                     [ast ()]tuple(.slot.-1))
                             var .slot.-3 :ast () =  [ast ()]
                                                     %mk_ast("Mul",
                                                     [ast ()]tuple(.slot.-2))
                             [ast ()]
                             lr.loop.Mul(.slot.-3, %state, %tokens)
                      default : .tmp.0.flag :bool =  True
                                %null
                 else %null
             [ast ()]if [bool]
                        %==(%null, .tmp.0.result) or .tmp.0.flag
             then .tmp.0.result
             else %null
        else %null
rec parse.Number : (State,Tokens) -> ast () =
    (%state, %tokens) ->
        var %off :int =  [int]%tokens.offset
        var .slot.0 :(bool,any) =  [(bool,any)]
                                   %match_tk(%tokens, [int] %tk_id("number"))
        [ast ()]if [bool]
                   %==(.slot.0, %null)
        then %null
        else var .slot.-1 :ast () =  [ast ()]
                                     %mk_ast("Number",
                                     [(bool,any)]tuple(.slot.0))
             .slot.-1

```