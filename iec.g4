grammar iec;

// Parser Rules
compilation_unit
    : unit_declaration
    | program_organization_unit
    ;

unit_declaration
    : UNIT identifier SEMICOLON
      interface_section
      implementation_section
      END_IMPLEMENTATION
    ;

program_organization_unit
    : function_declaration
    | function_implementation
    | function_block_declaration
    | function_block_implementation
    | program_declaration
    | program_implementation
    ;

interface_section
    : INTERFACE
      (uses_clause)?
      (variable_declaration | function_declaration | type_declaration)*
      END_INTERFACE
    ;

uses_clause
    : USES identifier_list SEMICOLON
    ;

identifier_list
    : identifier (COMMA identifier)*
    ;

implementation_section
    : IMPLEMENTATION
      (function_implementation | pascal_function_implementation)*
    ;

function_declaration
    : FUNCTION identifier (COLON return_type)? SEMICOLON
    ;

function_implementation
    : FUNCTION identifier (COLON return_type)?
      variable_section
      statement_list
      END_FUNCTION
    ;

pascal_function_implementation
    : FUNCTION identifier param_list? (COLON return_type)?
      SEMICOLON
      (BEGIN)?
      statement_list
      END SEMICOLON
    ;

param_list
    : L_PAREN param_declaration (SEMICOLON param_declaration)* R_PAREN
    ;

param_declaration
    : identifier_list (COLON data_type)? (ASSIGN expression)?
    | VAR_INPUT identifier_list COLON data_type (ASSIGN expression)? END_VAR
    | VAR_OUTPUT identifier_list COLON data_type (ASSIGN expression)? END_VAR
    | VAR_IN_OUT identifier_list COLON data_type (ASSIGN expression)? END_VAR
    ;

function_block_declaration
    : FUNCTION_BLOCK identifier SEMICOLON
    ;

function_block_implementation
    : FUNCTION_BLOCK identifier
      variable_section
      statement_list
      END_FUNCTION_BLOCK
    ;

program_declaration
    : PROGRAM identifier SEMICOLON
    ;

program_implementation
    : PROGRAM identifier
      variable_section
      statement_list
      END_PROGRAM
    ;

variable_section
    : (variable_declaration)*
    ;

variable_declaration
    : VAR_INPUT
      (variable_spec)*
      END_VAR
    | VAR_OUTPUT
      (variable_spec)*
      END_VAR
    | VAR_IN_OUT
      (variable_spec)*
      END_VAR
    | VAR_TEMP
      (variable_spec)*
      END_VAR
    | VAR
      (variable_spec)*
      END_VAR
    | VAR_GLOBAL
      (constant_spec)? 
      (variable_spec)*
      END_VAR
    ;

constant_spec
    : CONSTANT
    ;

variable_spec
    : identifier_list COLON data_type (ASSIGN expression)? SEMICOLON
    ;

return_type
    : data_type
    | VOID
    ;

data_type
    : elementary_type (string_length_spec)?
    | array_type
    | struct_type
    | identifier
    ;

string_length_spec
    : L_BRACKET integer_literal R_BRACKET
    ;

elementary_type
    : BOOL
    | INT
    | DINT
    | UINT
    | UDINT
    | USINT
    | SINT
    | BYTE
    | WORD
    | DWORD
    | REAL
    | LREAL
    | STRING
    | TIME
    | DATE
    | TIME_OF_DAY
    | DATE_AND_TIME
    | VOID
    ;

// Handling arrays with ranges
array_type
    : ARRAY L_BRACKET subrange (COMMA subrange)* R_BRACKET OF data_type
    | ARRAY L_BRACKET RANGE_DOTS R_BRACKET OF data_type  // Dynamic array [..]
    ;

subrange
    : INT_LITERAL RANGE_DOTS INT_LITERAL  // Direct handling of numeric ranges like 0..63
    | expression RANGE_DOTS expression
    ;

struct_type
    : STRUCT
      (struct_element)*
      END_STRUCT
    ;

struct_element
    : identifier COLON data_type SEMICOLON
    ;

type_declaration
    : TYPE
      (type_spec)*
      END_TYPE
    ;

type_spec
    : identifier COLON (enum_spec | data_type) SEMICOLON
    ;

enum_spec
    : L_PAREN identifier_list R_PAREN
    ;

statement_list
    : statement*
    ;

statement
    : assignment_statement
    | if_statement
    | for_statement
    | while_statement
    | repeat_statement
    | case_statement
    | return_statement
    | exit_statement
    | func_call_statement
    ;

assignment_statement
    : variable ASSIGN expression SEMICOLON
    ;

func_call_statement
    : func_call SEMICOLON
    ;

variable
    : direct_variable
    | symbolic_variable
    ;

direct_variable
    : '%' location_prefix size_prefix integer_literal
    ;

symbolic_variable
    : variable_name
    ;

variable_name
    : identifier variable_access*
    ;

variable_access
    : array_access
    | record_access
    | multi_element_access
    ;

array_access
    : L_BRACKET expression R_BRACKET
    ;

record_access
    : '.' identifier
    ;

multi_element_access
    : '[' expression ',' expression ']'
    | '[' expression RANGE_DOTS expression ']'
    ;

location_prefix 
    : 'I'
    | 'Q'
    | 'M'
    ;

size_prefix
    : 'X'
    | 'B'
    | 'W'
    | 'D'
    | 'L'
    ;

if_statement
    : IF expression THEN
      statement_list
      (ELSIF expression THEN statement_list)*
      (ELSE statement_list)?
      END_IF SEMICOLON?
    ;

for_statement
    : FOR identifier ASSIGN expression TO expression (BY expression)? DO
      statement_list
      END_FOR SEMICOLON?
    ;

while_statement
    : WHILE expression DO
      statement_list
      END_WHILE SEMICOLON?
    ;

repeat_statement
    : REPEAT
      statement_list
      UNTIL expression SEMICOLON?
      END_REPEAT SEMICOLON?
    ;

case_statement
    : CASE expression OF
      case_element*
      (ELSE statement_list)?
      END_CASE SEMICOLON?
    ;

case_element
    : case_condition COLON statement_list
    ;

case_condition
    : expression
    | expression RANGE_DOTS expression
    ;

exit_statement
    : EXIT SEMICOLON
    ;

return_statement
    : RETURN SEMICOLON?
    | RETURN expression SEMICOLON?
    ;

expression
    : expr_or
    ;

expr_or
    : expr_and (OR expr_and)*
    ;

expr_and
    : expr_xor (AND expr_xor)*
    ;

expr_xor
    : expr_bit (XOR expr_bit)*
    ;

expr_bit
    : expr_comp (BIT_AND expr_comp)*
    ;

expr_comp
    : expr_add (comparison_operator expr_add)?
    ;

expr_add
    : expr_mul (('+' | '-') expr_mul)*
    ;

expr_mul
    : expr_unary (('*' | '/' | MOD) expr_unary)*
    ;

expr_unary
    : (NOT | '-') expr_unary
    | expr_primary
    ;

expr_primary
    : constant
    | variable
    | func_call
    | binary_operation
    | array_constructor
    | struct_constructor
    | L_PAREN expression R_PAREN
    ;

// Fix: SHL/SHR function call syntax
binary_operation
    : SHL L_PAREN expression COMMA expression R_PAREN
    | SHR L_PAREN expression COMMA expression R_PAREN
    | SHL L_PAREN expression R_PAREN
    | SHR L_PAREN expression R_PAREN
    ;

constant
    : integer_literal
    | real_literal
    | string_literal
    | binary_literal
    | hex_literal
    | boolean_literal
    | time_literal
    | null_literal
    | identifier  // Enum constant
    ;

// Fix: Function call syntax
func_call
    : identifier L_PAREN (expression (COMMA expression)*)? R_PAREN
    ;

array_constructor
    : L_BRACKET expression (COMMA expression)* R_BRACKET
    ;

struct_constructor
    : identifier L_PAREN struct_elem (COMMA struct_elem)* R_PAREN
    ;

struct_elem
    : identifier ASSIGN expression
    ;

comparison_operator
    : '=' | '<>' | '<' | '>' | '<=' | '>='
    ;

integer_literal
    : INT_LITERAL
    | BYTE_LITERAL
    | WORD_LITERAL
    | DWORD_LITERAL
    ;

real_literal
    : REAL_LITERAL
    ;

string_literal
    : STRING_LITERAL
    ;

binary_literal
    : BIN_LITERAL
    ;

hex_literal
    : HEX_LITERAL 
    ;

boolean_literal
    : TRUE
    | FALSE
    ;

time_literal
    : TIME_LITERAL
    ;

null_literal
    : NULL
    ;

identifier
    : IDENTIFIER
    ;

// Lexer Rules - Important tokens first to ensure proper priority
RANGE_DOTS : '..';
BIT_AND : '&';
L_BRACKET : '[';
R_BRACKET : ']';
L_PAREN : '(';
R_PAREN : ')';
SEMICOLON : ';';
COLON : ':';
COMMA : ',';
ASSIGN : ':=';
// Keywords following token definitions
UNIT : [uU][nN][iI][tT];
INTERFACE : [iI][nN][tT][eE][rR][fF][aA][cC][eE];
IMPLEMENTATION : [iI][mM][pP][lL][eE][mM][eE][nN][tT][aA][tT][iI][oO][nN];
END_IMPLEMENTATION : [eE][nN][dD][_][iI][mM][pP][lL][eE][mM][eE][nN][tT][aA][tT][iI][oO][nN];
END_INTERFACE : [eE][nN][dD][_][iI][nN][tT][eE][rR][fF][aA][cC][eE];
USES : [uU][sS][eE][sS];
FUNCTION : [fF][uU][nN][cC][tT][iI][oO][nN];
FUNCTION_BLOCK : [fF][uU][nN][cC][tT][iI][oO][nN][_][bB][lL][oO][cC][kK];
END_FUNCTION : [eE][nN][dD][_][fF][uU][nN][cC][tT][iI][oO][nN];
END_FUNCTION_BLOCK : [eE][nN][dD][_][fF][uU][nN][cC][tT][iI][oO][nN][_][bB][lL][oO][cC][kK];
BEGIN : [bB][eE][gG][iI][nN];
END : [eE][nN][dD];
PROGRAM : [pP][rR][oO][gG][rR][aA][mM];
END_PROGRAM : [eE][nN][dD][_][pP][rR][oO][gG][rR][aA][mM];
VAR_INPUT : [vV][aA][rR][_][iI][nN][pP][uU][tT];
VAR_OUTPUT : [vV][aA][rR][_][oO][uU][tT][pP][uU][tT];
VAR_IN_OUT : [vV][aA][rR][_][iI][nN][_][oO][uU][tT];
VAR_TEMP : [vV][aA][rR][_][tT][eE][mM][pP];
VAR : [vV][aA][rR];
VAR_GLOBAL : [vV][aA][rR][_][gG][lL][oO][bB][aA][lL];
END_VAR : [eE][nN][dD][_][vV][aA][rR];
CONSTANT : [cC][oO][nN][sS][tT][aA][nN][tT];
TYPE : [tT][yY][pP][eE];
END_TYPE : [eE][nN][dD][_][tT][yY][pP][eE];
BOOL : [bB][oO][oO][lL];
INT : [iI][nN][tT];
DINT : [dD][iI][nN][tT];
UINT : [uU][iI][nN][tT];
UDINT : [uU][dD][iI][nN][tT];
USINT : [uU][sS][iI][nN][tT];
SINT : [sS][iI][nN][tT];
BYTE : [bB][yY][tT][eE];
WORD : [wW][oO][rR][dD];
DWORD : [dD][wW][oO][rR][dD];
REAL : [rR][eE][aA][lL];
LREAL : [lL][rR][eE][aA][lL];
STRING : [sS][tT][rR][iI][nN][gG];
TIME : [tT][iI][mM][eE];
DATE : [dD][aA][tT][eE];
TIME_OF_DAY : [tT][iI][mM][eE][_][oO][fF][_][dD][aA][yY];
DATE_AND_TIME : [dD][aA][tT][eE][_][aA][nN][dD][_][tT][iI][mM][eE];
ARRAY : [aA][rR][rR][aA][yY];
OF : [oO][fF];
STRUCT : [sS][tT][rR][uU][cC][tT];
END_STRUCT : [eE][nN][dD][_][sS][tT][rR][uU][cC][tT];
IF : [iI][fF];
THEN : [tT][hH][eE][nN];
ELSIF : [eE][lL][sS][iI][fF];
ELSE : [eE][lL][sS][eE];
END_IF : [eE][nN][dD][_][iI][fF];
FOR : [fF][oO][rR];
TO : [tT][oO];
BY : [bB][yY];
DO : [dD][oO];
END_FOR : [eE][nN][dD][_][fF][oO][rR];
WHILE : [wW][hH][iI][lL][eE];
END_WHILE : [eE][nN][dD][_][wW][hH][iI][lL][eE];
REPEAT : [rR][eE][pP][eE][aA][tT];
UNTIL : [uU][nN][tT][iI][lL];
END_REPEAT : [eE][nN][dD][_][rR][eE][pP][eE][aA][tT];
CASE : [cC][aA][sS][eE];
END_CASE : [eE][nN][dD][_][cC][aA][sS][eE];
EXIT : [eE][xX][iI][tT];
VOID : [vV][oO][iI][dD];
RETURN : [rR][eE][tT][uU][rR][nN];
NOT : [nN][oO][tT];
OR : [oO][rR];
XOR : [xX][oO][rR];
MOD : [mM][oO][dD];
AND : [aA][nN][dD];
SHL : [sS][hH][lL];
SHR : [sS][hH][rR];
TRUE : [tT][rR][uU][eE];
FALSE : [fF][aA][lL][sS][eE];
NULL : [nN][uU][lL][lL];
// Literals
INT_LITERAL : [0-9]+;
BYTE_LITERAL : [0-9]+ ('B' | 'b');
WORD_LITERAL : [0-9]+ ('W' | 'w');
DWORD_LITERAL : [0-9]+ ('D' | 'd');
REAL_LITERAL : [0-9]+ '.' [0-9]* | '.' [0-9]+;
STRING_LITERAL : '\'' (~'\'')* '\'';
TIME_LITERAL : 't#' (~'#')* | 'T#' (~'#')*;
BIN_LITERAL : '2#' [01]+ | '2' '#' [01]+;
HEX_LITERAL : '16#' [0-9A-Fa-f]+ | '16' '#' [0-9A-Fa-f]+;

IDENTIFIER : [a-zA-Z_][a-zA-Z0-9_]*;

COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT : '(*' .*? '*)' -> skip;
WS : [ \t\r\n]+ -> skip;