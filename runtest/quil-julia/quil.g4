list[p] : p         { [$1] }
        | list[p] p { array_push($1, $2) }
        ;

separated_list[comma, p]
        : p         { [$1] }
        | separated_list[comma, p] comma p { array_push($1, $3) }
        ;


START               : NEWLINE? separated_list[NEWLINE, allInstr] NEWLINE?;

allInstr            : defGate
                    | defCircuit
                    | instr
                    ;

instr               : gate
                    | measure
                    | defLabel
                    | halt
                    | jump
                    | jumpWhen
                    | jumpUnless
                    | resetState
                    | wait
                    | classicalUnary
                    | classicalBinary
                    | classicalComparison
                    | load
                    | store
                    | nop
                    | include
                    | pragma
                    | memoryDescriptor
                    ;

# C. Static and Parametric Gates

gate                : modifier* name ( LPAREN param ( COMMA param )* RPAREN )? qubit+ ;

name                : IDENTIFIER ;
qubit               : INT | IDENTIFIER;

param               : expression ;

modifier            : CONTROLLED
                    | DAGGER ;

# D. Gate Definitions

defGate             : DEFGATE name ( ( LPAREN variable ( COMMA variable )* RPAREN ) | ( AS gateType ) )? COLON NEWLINE matrix ;

variable            : PERCENTAGE IDENTIFIER ;

gateType            : MATRIX | PERMUTATION ;

matrix              : separated_list[NEWLINE, matrixRow];
matrixRow           : expression ( COMMA expression )* ;

# E. Circuits

defCircuit          : DEFCIRCUIT name ( LPAREN variable ( COMMA variable )* RPAREN )? qubitVariable* COLON NEWLINE circuit ;

qubitVariable       : IDENTIFIER ;

circuit             : separated_list[NEWLINE, instr] ;

# F. Measurement

measure             : MEASURE qubit addr? ;
addr                : IDENTIFIER | ( IDENTIFIER? LBRACKET INT RBRACKET );

# G. Program control

defLabel            : LABEL label ;
label               : AT IDENTIFIER ;
halt                : HALT ;
jump                : JUMP label ;
jumpWhen            : JUMPWHEN label addr ;
jumpUnless          : JUMPUNLESS label addr ;

# H. Zeroing the Quantum State

resetState          : RESET qubit? ; # NB: cannot be named "reset" due to conflict with Antlr implementation

# I. Classical/Quantum Synchronization

wait                : WAIT ;

# J. Classical Instructions

memoryDescriptor    : DECLARE IDENTIFIER IDENTIFIER ( LBRACKET INT RBRACKET )? ( SHARING IDENTIFIER ( offsetDescriptor )* )? ;
offsetDescriptor    : OFFSET INT IDENTIFIER ;

classicalUnary      : ( NEG | NOT | TRUE | FALSE ) addr ;
classicalBinary     : logicalBinaryOp | arithmeticBinaryOp | move | exchange | convert ;
logicalBinaryOp     : ( AND | OR | IOR | XOR ) addr ( addr | INT ) ;
arithmeticBinaryOp  : ( ADD | SUB | MUL | DIV ) addr ( addr | number ) ;
move                : MOVE addr ( addr | number );
exchange            : EXCHANGE addr addr ;
convert             : CONVERT addr addr ;
load                : LOAD addr IDENTIFIER addr ;
store               : STORE IDENTIFIER addr ( addr | number );
classicalComparison : ( EQ | GT | GE | LT | LE ) addr addr ( addr | number );

# K. The No-Operation Instruction

nop                 : NOP ;

# L. File Inclusion

include             : INCLUDE STRING ;

# M. Pragma Support

pragma              : PRAGMA IDENTIFIER pragma_name* STRING? ;
pragma_name         : IDENTIFIER | INT ;

# Expressions (in order of precedence)

nonPowerExp         : '(' expression ')'                       #parenthesisExp
                    | '+' expression                           #signedExp
                    | '-' expression                           #signedExp
                    | function LPAREN expression RPAREN         #functionExp
                    | number                                    #numberExp
                    | variable                                  #variableExp
                    | addr                                      #addrExp
                    ;

expression          : nonPowerExp POWER expression
                    | nonPowerExp { $1 }
                    | expression ( TIMES | DIVIDE ) expression  #mulDivExp
                    | expression ( PLUS | MINUS ) expression    #addSubExp
                    ;


function            : SIN | COS | SQRT | EXP | CIS ;
sign                : PLUS | MINUS ;

# Numbers
# We suffix -N onto these names so they don't conflict with already defined Python types

number              : MINUS? ( imaginaryN | I | PI ) ;

imaginaryN          : realN I
                    | realN
                    ;

realN               : FLOAT | INT ;

##########
# LEXER
##########

# Keywords

AS                  : 'AS' ;
DEFGATE             : 'DEFGATE' ;
DEFCIRCUIT          : 'DEFCIRCUIT' ;
MEASURE             : 'MEASURE' ;

LABEL               : 'LABEL' ;
HALT                : 'HALT' ;
JUMP                : 'JUMP' ;
JUMPWHEN            : 'JUMP-WHEN' ;
JUMPUNLESS          : 'JUMP-UNLESS' ;

RESET               : 'RESET' ;
WAIT                : 'WAIT' ;
NOP                 : 'NOP' ;
INCLUDE             : 'INCLUDE' ;
PRAGMA              : 'PRAGMA' ;

DECLARE             : 'DECLARE' ;
SHARING             : 'SHARING' ;
OFFSET              : 'OFFSET' ;

NEG                 : 'NEG' ;
NOT                 : 'NOT' ;
TRUE                : 'TRUE' ; # Deprecated
FALSE               : 'FALSE' ; # Deprecated

AND                 : 'AND' ;
IOR                 : 'IOR' ;
XOR                 : 'XOR' ;
OR                  : 'OR' ;   # Deprecated

ADD                 : 'ADD' ;
SUB                 : 'SUB' ;
MUL                 : 'MUL' ;
DIV                 : 'DIV' ;

MOVE                : 'MOVE' ;
EXCHANGE            : 'EXCHANGE' ;
CONVERT             : 'CONVERT' ;

EQ                  : 'EQ';
GT                  : 'GT';
GE                  : 'GE';
LT                  : 'LT';
LE                  : 'LE';

LOAD                : 'LOAD' ;
STORE               : 'STORE' ;

PI                  : 'pi' ;
I                   : 'i' ;

SIN                 : 'SIN' ;
COS                 : 'COS' ;
SQRT                : 'SQRT' ;
EXP                 : 'EXP' ;
CIS                 : 'CIS' ;

MATRIX              : 'MATRIX' ;
PERMUTATION         : 'PERMUTATION' ;

# Operators

PLUS                : '+' ;
MINUS               : '-' ;
TIMES               : '*' ;
DIVIDE              : '/' ;
POWER               : '^' ;

# Modifiers

CONTROLLED          : 'CONTROLLED' ;
DAGGER              : 'DAGGER' ;

# Identifiers

IDENTIFIER          : <IDENTIFIER>;

# Numbers

INT                 : <INT> ;
FLOAT               : <FLOAT>;

# String

STRING              : <STRING>;

# Punctuation

PERIOD              : '.' ;
COMMA               : ',' ;
LPAREN              : '(' ;
RPAREN              : ')' ;
LBRACKET            : '[' ;
RBRACKET            : ']' ;
COLON               : ':' ;
PERCENTAGE          : '%' ;
AT                  : '@' ;
QUOTE               : '"' ;
UNDERSCORE          : '_' ;

# Whitespace

TAB                 : <TAB> ;
NEWLINE             : <NEWLINE> ;

# Skips

COMMENT             : <COMMENT> ;
SPACE               : <SPACE> ;
