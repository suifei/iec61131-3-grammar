grammar IEC;

// Parser rules for overall program structure
program
    : (namespace_declaration | library_unit | pou_declaration | configuration_declaration)+
    ;

library_unit
    : 'UNIT' identifier ';'
      (EOL | WHITESPACE)*
      interface_section
      (EOL | WHITESPACE)*
      implementation_section
    ;

interface_section
    : 'INTERFACE'
      (EOL | WHITESPACE)*
      uses_directive?
      (EOL | WHITESPACE)*
      (function_declaration_prototype ';')*
      (EOL | WHITESPACE)*
      type_declarations?
      (EOL | WHITESPACE)*
      variable_declarations*
      (EOL | WHITESPACE)*
      'END_INTERFACE'
    ;

implementation_section
    : 'IMPLEMENTATION'
      (function_declaration | function_block_declaration)*
      'END_IMPLEMENTATION'
    ;

uses_directive
    : 'USES' identifier_list ';'
    ;

identifier_list
    : identifier (',' identifier)*
    ;

// Namespace declarations
namespace_declaration
    : 'NAMESPACE' 'INTERNAL'? namespace_h_name using_directive* namespace_elements 'END_NAMESPACE'
    ;

namespace_elements
    : (data_type_decl | function_declaration | function_block_declaration 
      | program_declaration | class_declaration | interface_declaration | namespace_declaration)+
    ;

using_directive
    : 'USING' namespace_h_name (',' namespace_h_name)* ';'
    ;

namespace_h_name
    : namespace_name ('.' namespace_name)*
    ;

namespace_name
    : identifier
    ;

pou_declaration
    : using_directive*
    ( global_var_declarations
    | data_type_decl
    | access_declarations
    | function_declaration 
    | function_block_declaration
    | class_declaration 
    | interface_declaration
    | namespace_declaration
    )+
    ;

// Configuration declarations
configuration_declaration
    : 'CONFIGURATION' identifier global_var_declarations?
      (single_resource_declaration | resource_declaration+) 
      access_declarations? 
      config_initialization?
      'END_CONFIGURATION'
    ;

resource_declaration
    : 'RESOURCE' identifier 'ON' identifier
      global_var_declarations? single_resource_declaration
      'END_RESOURCE'
    ;

single_resource_declaration
    : (task_configuration ';')* (program_configuration ';')+
    ;

task_configuration
    : 'TASK' identifier task_initialization
    ;

task_initialization
    : '(' ('SINGLE' ':=' data_source ',')? 
        ('INTERVAL' ':=' data_source ',')? 
        'PRIORITY' ':=' unsigned_int ')'
    ;

data_source
    : constant 
    | global_var_access
    | direct_variable
    ;

global_var_access
    : (identifier '.')? identifier ('.' identifier)?
    ;

program_configuration
    : 'PROGRAM' ('RETAIN' | 'NON_RETAIN')? identifier ('WITH' identifier)? ':'
      derived_function_block_name ('(' program_conf_elements ')')?
    ;

program_conf_elements
    : program_conf_element (',' program_conf_element)*
    ;

program_conf_element
    : fb_task 
    | prog_cnxn
    ;

fb_task
    : function_block_instance_name 'WITH' identifier
    ;

prog_cnxn
    : variable ':=' prog_data_source 
    | variable '=>' data_sink
    ;

prog_data_source
    : constant 
    | enumerated_value 
    | global_var_access 
    | direct_variable
    ;

data_sink
    : global_var_access 
    | direct_variable
    ;

config_initialization
    : 'VAR_CONFIG' (config_inst_init ';')* 'END_VAR'
    ;

config_inst_init
    : identifier '.' identifier '.' ((function_block_instance_name | class_instance_name) '.')*
      ( variable_name located_at? ':' located_var_spec_init
      | ((function_block_instance_name ':' function_block_type_name)
         | (class_instance_name ':' class_type_name)) ':=' structure_initialization)
    ;

// Access path declarations
access_declarations
    : 'VAR_ACCESS' (access_declaration ';')* 'END_VAR'
    ;

access_declaration
    : identifier ':' access_path ':' data_type_name access_direction?
    ;

access_path
    : (identifier '.')? direct_variable
    | (identifier '.')? (identifier '.')?
      ((function_block_instance_name | class_instance_name) '.')* variable
    ;

access_direction
    : 'READ_WRITE' 
    | 'READ_ONLY'
    ;

// Data type declarations
data_type_decl
    : 'TYPE' (type_declaration ';')+ 'END_TYPE'
    ;

type_declarations
    : 'TYPE' (type_declaration ';')+ 'END_TYPE'
    ;

type_declaration
    : simple_type_declaration
    | subrange_type_declaration
    | enumerated_type_declaration
    | array_type_declaration
    | structure_type_declaration 
    | string_type_declaration
    | ref_type_declaration
    ;

simple_type_declaration
    : identifier ':' simple_spec_init
    ;

simple_spec_init
    : simple_specification (':=' constant_expression)?
    ;

simple_specification
    : elementary_type_name
    | simple_type_name
    ;

simple_type_name
    : (namespace_name '.')* identifier
    ;

subrange_type_declaration
    : identifier ':' subrange_spec_init
    ;

subrange_spec_init
    : subrange_spec (':=' signed_integer)?
    ;

subrange_spec
    : integer_type_name '(' subrange ')'
    | subrange_type_name
    ;

subrange_type_name
    : (namespace_name '.')* identifier
    ;

subrange
    : constant_expression '..' constant_expression
    ;

enumerated_type_declaration
    : identifier ':' 
      ( (elementary_type_name? named_value_spec_init)
      | enum_spec_init
      )
    ;

named_value_spec_init
    : '(' enum_value_spec (',' enum_value_spec)* ')' (':=' enumerated_value)?
    ;

enum_spec_init
    : (('(' identifier (',' identifier)* ')') | enum_type_name) (':=' enumerated_value)?
    ;

enum_type_name
    : (namespace_name '.')* identifier
    ;

enum_value_spec
    : identifier (':=' (integer_literal | constant_expression))?
    ;

enumerated_value
    : (enum_type_name '#')? identifier
    ;

array_type_declaration
    : identifier ':' array_spec_init
    ;

array_spec_init
    : array_spec (':=' array_initialization)?
    ;

array_spec
    : array_type_name
    | 'ARRAY' '[' array_range (',' array_range)* ']' 'OF' data_type_name
    ;

array_range
    : subrange 
    | constant_expression '..' constant_expression
    ;

array_type_name
    : (namespace_name '.')* identifier
    ;

array_initialization
    : '[' array_element_init (',' array_element_init)* ']'
    ;

array_element_init
    : array_element_init_value 
    | unsigned_int '(' array_element_init_value? ')'
    ;

array_element_init_value
    : constant_expression
    | enumerated_value
    | structure_initialization
    | array_initialization
    ;

structure_type_declaration
    : identifier ':' structure_specification
    ;

structure_specification
    : structure_declaration
    | structure_spec_init
    ;

structure_spec_init
    : struct_type_name (':=' structure_initialization)?
    ;

struct_type_name
    : (namespace_name '.')* identifier
    ;

structure_declaration
    : 'STRUCT' ('OVERLAP')? (structure_element_declaration ';')+ 'END_STRUCT'
    ;

structure_element_declaration
    : identifier (located_at multibit_part_access?)? ':' 
      ( simple_spec_init
      | subrange_spec_init
      | enum_spec_init
      | array_spec_init
      | structure_spec_init
      )
    ;

structure_initialization
    : '(' structure_element_init (',' structure_element_init)* ')'
    ;

structure_element_init
    : identifier ':=' 
      ( constant_expression
      | enumerated_value
      | array_initialization
      | structure_initialization
      | ref_value
      )
    ;

string_type_declaration
    : identifier ':' string_type_name (':=' character_string)?
    ;

ref_type_declaration
    : identifier ':' ref_spec_init
    ;

ref_spec_init
    : ref_spec (':=' ref_value)?
    ;

ref_spec
    : 'REF_TO' data_type_name
    ;

ref_value
    : ref_addr
    | 'NULL'
    ;

ref_addr
    : 'REF' '(' (variable | function_block_instance_name | class_instance_name) ')'
    ;

ref_assign
    : ref_name ':=' (ref_name | ref_deref | ref_value)
    ;

ref_deref
    : ref_name '^'+
    ;

ref_name
    : identifier
    ;

// Variable declarations
variable_declarations
    : input_declarations
    | output_declarations
    | input_output_declarations
    | var_declarations
    | retain_var_declarations
    | temp_var_declarations
    | external_var_declarations
    | global_var_declarations
    ;

input_declarations
    : 'VAR_INPUT' ('RETAIN' | 'NON_RETAIN')? (input_declaration ';')* 'END_VAR'
    ;

input_declaration
    : variable_declaration_init
    | edge_declaration
    | array_conform_declaration
    ;

edge_declaration
    : variable_list ':' 'BOOL' ('R_EDGE' | 'F_EDGE')
    ;

variable_declaration_init
    : variable_list ':' 
      ( simple_spec_init
      | string_var_declaration
      | ref_spec_init
      | array_var_declaration_init
      | structure_var_declaration_init
      | fb_declaration_init
      | interface_spec_init
      )
    ;

interface_spec_init
    : interface_type_name (':=' interface_value)?
    ;

interface_value
    : variable
    | function_block_instance_name
    | class_instance_name
    | 'NULL'
    ;

interface_type_name
    : (namespace_name '.')* identifier
    ;

variable_list
    : identifier (',' identifier)*
    ;

array_var_declaration_init
    : variable_list ':' array_spec_init
    ;

array_conform_declaration
    : variable_list ':' 'ARRAY' '[' '*' (',' '*')* ']' 'OF' data_type_name
    ;

structure_var_declaration_init
    : variable_list ':' structure_spec_init
    ;

output_declarations
    : 'VAR_OUTPUT' ('RETAIN' | 'NON_RETAIN')? (output_declaration ';')* 'END_VAR'
    ;

output_declaration
    : variable_declaration_init
    | array_conform_declaration
    ;

input_output_declarations
    : 'VAR_IN_OUT' (input_output_declaration ';')* 'END_VAR'
    ;

input_output_declaration
    : variable_declaration
    | array_conform_declaration
    | fb_declaration_no_init
    ;

variable_declaration
    : variable_list ':' 
      ( simple_specification
      | string_var_declaration
      | array_var_declaration
      | structure_var_declaration
      | ref_spec
      | interface_type_name
      | elementary_type_name
      )
    ;

array_var_declaration
    : variable_list ':' array_spec
    ;

structure_var_declaration
    : variable_list ':' struct_type_name
    ;

var_declarations
    : 'VAR_TEMP' ((variable_declaration | ref_var_declaration | interface_var_declaration) ';')* 'END_VAR'
    | 'VAR' ('CONSTANT')? access_specifier? (variable_declaration_init ';')* 'END_VAR'
    | 'VAR' 'RETAIN' access_specifier? (variable_declaration_init ';')* 'END_VAR'
    | 'VAR' 'NON_RETAIN' access_specifier? (variable_declaration_init ';')* 'END_VAR'
    | input_declarations
    | output_declarations
    | input_output_declarations
    | external_var_declarations
    | global_var_declarations
    ;

retain_var_declarations
    : 'VAR' 'RETAIN' access_specifier? (variable_declaration_init ';')* 'END_VAR'
    ;

non_retain_var_declarations
    : 'VAR' 'NON_RETAIN' access_specifier? (variable_declaration_init ';')* 'END_VAR'
    ;

temp_var_declarations
    : 'VAR_TEMP' ((variable_declaration | ref_var_declaration | interface_var_declaration) ';')* 'END_VAR'
    ;

ref_var_declaration
    : variable_list ':' ref_spec
    ;

interface_var_declaration
    : variable_list ':' interface_type_name
    ;

external_var_declarations
    : 'VAR_EXTERNAL' ('CONSTANT')? (external_declaration ';')* 'END_VAR'
    ;

external_declaration
    : identifier ':' 
      ( simple_specification
      | array_spec
      | struct_type_name
      | function_block_type_name
      | ref_type_name
      )
    ;

ref_type_name
    : (namespace_name '.')* identifier
    ;

string_var_declaration
    : s_byte_string_var_declaration
    | d_byte_string_var_declaration
    ;

s_byte_string_var_declaration
    : variable_list ':' s_byte_string_spec
    ;

s_byte_string_spec
    : 'STRING' ('[' unsigned_int ']')? (':=' s_byte_character_string)?
    ;

d_byte_string_var_declaration
    : variable_list ':' d_byte_string_spec
    ;

d_byte_string_spec
    : 'WSTRING' ('[' unsigned_int ']')? (':=' d_byte_character_string)?
    ;

global_var_declarations
    : 'VAR_GLOBAL' ('CONSTANT' | 'RETAIN')? (global_var_declaration ';')* 'END_VAR'
    ;

global_var_declaration
    : global_var_spec ':' 
      ( located_var_spec_init
      | function_block_type_name
      )
    ;

global_var_spec
    : (identifier (',' identifier)*)
    | (identifier located_at)
    ;

located_var_spec_init
    : simple_spec_init 
    | array_spec_init 
    | structure_spec_init 
    | s_byte_string_spec 
    | d_byte_string_spec
    ;

located_at
    : 'AT' direct_variable
    ;

direct_variable
    : '%' ('I' | 'Q' | 'M') ('X' | 'B' | 'W' | 'D' | 'L' | '*')? unsigned_int ('.' unsigned_int)*
    ;

multibit_part_access
    : '.' (unsigned_int | '%' ('X' | 'B' | 'W' | 'D' | 'L')? unsigned_int)
    ;

// Partly specified location
loc_partly_var_decl
    : 'VAR' ('RETAIN' | 'NON_RETAIN')? (loc_partly_var)* 'END_VAR'
    ;

loc_partly_var
    : identifier 'AT' '%' ('I' | 'Q' | 'M') '*' ':' var_spec ';'
    ;

var_spec
    : simple_specification 
    | array_spec 
    | struct_type_name
    | ('STRING' | 'WSTRING') ('[' unsigned_int ']')?
    ;

// Function declarations
function_declaration_prototype
    : 'FUNCTION' identifier (':' data_type_name)? 
    ;

function_declaration
    : 'FUNCTION' identifier (':' data_type_name)? using_directive*
      (io_var_declarations | func_var_declarations | temp_var_declarations)*
      function_body
      'END_FUNCTION'
    ;

io_var_declarations
    : input_declarations
    | output_declarations
    | input_output_declarations
    ;

func_var_declarations
    : external_var_declarations
    | var_declarations
    ;

function_body
    : var_declarations* statement_list
    | var_declarations* instruction_list
    ;

// FB declarations
function_block_declaration
    : 'FUNCTION_BLOCK' ('FINAL' | 'ABSTRACT')? identifier using_directive*
      ('EXTENDS' (function_block_type_name | class_type_name))?
      ('IMPLEMENTS' interface_name_list)?
      (fb_io_var_declarations | func_var_declarations | temp_var_declarations | other_var_declarations)*
      (method_declaration)* 
      (function_block_body | /* empty */)
      'END_FUNCTION_BLOCK'
    ;

fb_io_var_declarations
    : fb_input_declarations
    | fb_output_declarations
    | input_output_declarations
    ;

fb_input_declarations
    : 'VAR_INPUT' ('RETAIN' | 'NON_RETAIN')? (fb_input_declaration ';')* 'END_VAR'
    ;

fb_input_declaration
    : variable_declaration_init
    | edge_declaration
    | array_conform_declaration
    ;

fb_output_declarations
    : 'VAR_OUTPUT' ('RETAIN' | 'NON_RETAIN')? (fb_output_declaration ';')* 'END_VAR'
    ;

fb_output_declaration
    : variable_declaration_init
    | array_conform_declaration
    ;

other_var_declarations
    : retain_var_declarations
    | non_retain_var_declarations
    | loc_partly_var_decl
    ;

function_block_type_name
    : standard_function_block_name
    | derived_function_block_name
    ;

standard_function_block_name
    : 'SR' | 'RS' | 'R_TRIG' | 'F_TRIG' | 'CTU' | 'CTD' | 'CTUD' | 'TP' | 'TON' | 'TOF'
    ;

derived_function_block_name
    : (namespace_name '.')* identifier
    ;

function_block_instance_name
    : (namespace_name '.')* identifier ('^')*
    ;

fb_declaration_no_init
    : identifier (',' identifier)* ':' function_block_type_name
    ;

fb_declaration_init
    : fb_declaration_no_init (':=' structure_initialization)?
    ;

function_block_body
    : statement_list
    | instruction_list
    | sfc_network
    ;

// Sequential Function Chart (SFC)
sfc_network
    : initial_step (step | transition | action)*
    ;

initial_step
    : 'INITIAL_STEP' identifier ':' (action_association ';')* 'END_STEP'
    ;

step
    : 'STEP' identifier ':' (action_association ';')* 'END_STEP'
    ;

action_association
    : identifier '(' action_qualifier? (',' identifier)* ')'
    ;

action_qualifier
    : 'N' | 'R' | 'S' | 'P' | 'P0' | 'P1'
    | (('L' | 'D' | 'SD' | 'DS' | 'SL') ',' time_literal)
    ;

transition
    : 'TRANSITION' identifier? ('(' 'PRIORITY' ':=' unsigned_int ')')?
      'FROM' steps 'TO' steps 
      ( (':=' expression ';') 
      | (':' statement_list) 
      | (':=' non_empty_instruction) )
      'END_TRANSITION'
    ;

steps
    : identifier 
    | '(' identifier (',' identifier)+ ')'
    ;

action
    : 'ACTION' identifier ':' function_block_body 'END_ACTION'
    ;

// Method declarations
method_declaration
    : 'METHOD' access_specifier? ('FINAL' | 'ABSTRACT')? ('OVERRIDE')? 
      identifier (':' data_type_name)?
      using_directive*
      (io_var_declarations | func_var_declarations | temp_var_declarations)*
      function_body
      'END_METHOD'
    ;

// Class declarations
class_declaration
    : 'CLASS' ('FINAL' | 'ABSTRACT')? identifier using_directive*
      ('EXTENDS' class_type_name)?
      ('IMPLEMENTS' interface_name_list)?
      (func_var_declarations | other_var_declarations)*
      (method_declaration)*
      'END_CLASS'
    ;

class_type_name
    : (namespace_name '.')* identifier
    ;

class_instance_name
    : (namespace_name '.')* identifier ('^')*
    ;

// Interface declarations
interface_declaration
    : 'INTERFACE' identifier using_directive*
      ('EXTENDS' interface_name_list)?
      method_prototype*
      'END_INTERFACE'
    ;

method_prototype
    : 'METHOD' identifier (':' data_type_name)? io_var_declarations* 'END_METHOD'
    ;

interface_name_list
    : interface_type_name (',' interface_type_name)*
    ;

access_specifier
    : 'PUBLIC' | 'PROTECTED' | 'PRIVATE' | 'INTERNAL'
    ;

// Program declarations
program_declaration
    : 'PROGRAM' identifier
      (io_var_declarations | func_var_declarations | temp_var_declarations | other_var_declarations)*
      function_block_body
      'END_PROGRAM'
    ;

// Instruction List (IL) - Deprecated but included for compatibility
instruction_list
    : non_empty_instruction*
    ;

non_empty_instruction
    : (il_label ':')? (il_simple_operation | il_expression | il_jump_operation | il_fb_call | il_formal_fct_call | il_return)
    ;

il_label
    : identifier
    ;

il_simple_operation
    : il_simple_operator (il_operand)?
    | il_function_name il_operand_list?
    ;

il_expression
    : il_expr_operator '(' il_operand? il_simple_instruction_list? ')'
    ;

il_jump_operation
    : il_jump_operator il_label
    ;

il_fb_call
    : il_call_operator 
      ( (function_block_instance_name | il_function_name | identifier | 'THIS'
         | (('THIS' '.') (function_block_instance_name | class_instance_name) '.' identifier))
        ('(' (il_param_list? | il_operand_list?) ')')?  
      | 'SUPER' '(' ')')
    ;

il_formal_fct_call
    : il_function_name '(' il_param_list? ')'
    ;

il_simple_instruction_list
    : il_simple_instruction+
    ;

il_simple_instruction
    : (il_simple_operation | il_expression | il_formal_fct_call)
    ;

il_operand
    : constant 
    | enumerated_value 
    | variable
    ;

il_operand_list
    : il_operand (',' il_operand)*
    ;

il_param_list
    : il_param_instruction* il_param_last_instruction
    ;

il_param_instruction
    : (il_param_assign | il_param_out_assign) ','
    ;

il_param_last_instruction
    : (il_param_assign | il_param_out_assign)
    ;

il_param_assign
    : il_assignment (il_operand | ('(' il_simple_instruction_list ')'))
    ;

il_param_out_assign
    : il_assign_out_operator variable
    ;

il_function_name
    : (namespace_name '.')* identifier
    ;

il_simple_operator
    : 'LD' | 'LDN' | 'ST' | 'STN' | 'S' | 'R' | 'S1' | 'R1' | 'CLK' | 'CU' | 'CD' | 'PV'
    | 'IN' | 'PT' | il_expr_operator
    ;

il_expr_operator
    : 'AND' | '&' | 'OR' | 'XOR' | 'ANDN' | '&N' | 'ORN'
    | 'XORN' | 'ADD' | 'SUB' | 'MUL' | 'DIV'
    | 'MOD' | 'GT' | 'GE' | 'EQ' | 'LT' | 'LE' | 'NE'
    ;

il_call_operator
    : 'CAL' | 'CALC' | 'CALCN'
    ;

il_jump_operator
    : 'JMP' | 'JMPC' | 'JMPCN'
    ;

il_return
    : 'RET' | 'RETC' | 'RETCN'
    ;

il_assignment
    : identifier ':='
    ;

il_assign_out_operator
    : 'NOT'? identifier '=>'
    ;

// Structured Text (ST) statements and expressions
statement_list
    : (statement? ';')*
    ;

statement
    : assignment_statement
    | subprogram_control_statement
    | selection_statement
    | iteration_statement
    | variable_declaration_init
    ;

assignment_statement
    : (variable ':=' expression)
    | ref_assign
    | assignment_attempt
    ;

assignment_attempt
    : (ref_name | ref_deref) '?=' (ref_name | ref_deref | ref_value)
    ;

subprogram_control_statement
    : function_call
    | fb_invocation
    | 'SUPER' '(' ')'
    | 'RETURN'
    ;

fb_invocation
    : (function_block_instance_name | identifier | 'THIS'
      | (('THIS' '.')? ((function_block_instance_name | class_instance_name) '.')+ identifier))
      '(' (param_assignment (',' param_assignment)*)? ')'
    ;

param_assignment
    : ((identifier ':=')? expression)
    | ref_assign
    | ('NOT'? identifier '=>' variable)
    ;

selection_statement
    : if_statement
    | case_statement
    ;

if_statement
    : 'IF' expression 'THEN' statement_list 
      ('ELSIF' expression 'THEN' statement_list)* 
      ('ELSE' statement_list)? 'END_IF'
    ;

case_statement
    : 'CASE' expression 'OF' case_selection+ ('ELSE' statement_list)? 'END_CASE'
    ;

case_selection
    : case_list ':' statement_list
    ;

case_list
    : case_list_element (',' case_list_element)*
    ;

case_list_element
    : subrange
    | constant_expression
    ;

iteration_statement
    : for_statement
    | while_statement
    | repeat_statement
    | 'EXIT'
    | 'CONTINUE'
    ;

for_statement
    : 'FOR' identifier ':=' for_list 'DO' statement_list 'END_FOR'
    ;

for_list
    : expression 'TO' expression ('BY' expression)?
    ;

while_statement
    : 'WHILE' expression 'DO' statement_list 'END_WHILE'
    ;

repeat_statement
    : 'REPEAT' statement_list 'UNTIL' expression 'END_REPEAT'
    ;

// Expressions
expression
    : xor_expression ('OR' xor_expression)*
    ;

constant_expression
    : expression
    ;

xor_expression
    : and_expression ('XOR' and_expression)*
    ;

and_expression
    : compare_expression (('&' | 'AND') compare_expression)*
    ;

compare_expression
    : equ_expression (('=' | '<>') equ_expression)*
    ;

equ_expression
    : add_expression (('<' | '>' | '<=' | '>=') add_expression)*
    ;

add_expression
    : term (('+' | '-') term)*
    ;

term
    : power_expression (('*' | '/' | 'MOD') power_expression)*
    ;

power_expression
    : unary_expression ('**' unary_expression)*
    ;

unary_expression
    : ('-' | '+' | 'NOT')? primary_expression
    ;

primary_expression
    : constant
    | enumerated_value
    | variable_access
    | function_call
    | ref_value
    | '(' expression ')'
    ;

variable_access
    : variable multibit_part_access?
    ;

variable
    : direct_variable
    | symbolic_variable
    ;

symbolic_variable
    : (('THIS' '.') | (namespace_name '.')+)? (var_access | multi_elem_var)
    ;

var_access
    : variable_name 
    | ref_deref
    ;

variable_name
    : identifier
    ;

multi_elem_var
    : var_access (subscript_list | struct_variable)+
    ;

subscript_list
    : '[' expression (',' expression)* ']'
    ;

struct_variable
    : '.' var_access
    ;

// Function calls
function_call
    : function_name '(' (param_assignment (',' param_assignment)*)? ')'
    ;

function_name
    : standard_function_name
    | derived_function_name
    ;

standard_function_name
    : 'TRUNC' | 'ABS' | 'SQRT' | 'LN' | 'LOG' | 'EXP'
    | 'SIN' | 'COS' | 'TAN' | 'ASIN' | 'ACOS' | 'ATAN' | 'ATAN2'
    | 'ADD' | 'SUB' | 'MUL' | 'DIV' | 'MOD' | 'EXPT' | 'MOVE'
    | 'SHL' | 'SHR' | 'ROL' | 'ROR'
    | 'AND' | 'OR' | 'XOR' | 'NOT'
    | 'SEL' | 'MAX' | 'MIN' | 'LIMIT' | 'MUX'
    | 'GT' | 'GE' | 'EQ' | 'LE' | 'LT' | 'NE'
    | 'LEN' | 'LEFT' | 'RIGHT' | 'MID' | 'CONCAT' | 'INSERT' | 'DELETE' | 'REPLACE' | 'FIND'
    // type conversion functions
    | 'TO_BOOL' | 'TO_BYTE' | 'TO_WORD' | 'TO_DWORD' | 'TO_LWORD'
    | 'TO_SINT' | 'TO_INT' | 'TO_DINT' | 'TO_LINT'
    | 'TO_USINT' | 'TO_UINT' | 'TO_UDINT' | 'TO_ULINT'
    | 'TO_REAL' | 'TO_LREAL'
    | 'TRUNC_INT' | 'TRUNC_DINT' | 'TRUNC_REAL'
    | 'BYTE_TO_USINT' | 'USINT_TO_BYTE' | 'DINT_TO_USINT'
    | typed_function_name
    ;

typed_function_name
    : ADD_typed | SUB_typed | MUL_typed | DIV_typed
    | conversion_typed
    ;

ADD_typed
    : 'ADD_' ('SINT' | 'INT' | 'DINT' | 'LINT' | 'USINT' | 'UINT' | 'UDINT' | 'ULINT' | 'REAL' | 'LREAL')
    ;

SUB_typed
    : 'SUB_' ('SINT' | 'INT' | 'DINT' | 'LINT' | 'USINT' | 'UINT' | 'UDINT' | 'ULINT' | 'REAL' | 'LREAL')
    ;

MUL_typed
    : 'MUL_' ('SINT' | 'INT' | 'DINT' | 'LINT' | 'USINT' | 'UINT' | 'UDINT' | 'ULINT' | 'REAL' | 'LREAL')
    ;

DIV_typed
    : 'DIV_' ('SINT' | 'INT' | 'DINT' | 'LINT' | 'USINT' | 'UINT' | 'UDINT' | 'ULINT' | 'REAL' | 'LREAL')
    ;

conversion_typed
    : elementary_type_name '_TO_' elementary_type_name
    | elementary_type_name '_TRUNC_' integer_type_name
    | elementary_type_name '_BCD_TO_' integer_type_name
    | integer_type_name '_TO_BCD_' elementary_type_name
    ;

derived_function_name
    : (namespace_name '.')* identifier
    ;

// Data types and constants
data_type_name
    : elementary_type_name
    | derived_type_name
    ;

derived_type_name
    : simple_type_name
    | subrange_type_name
    | enum_type_name
    | array_type_name
    | struct_type_name
    | string_type_name
    | ref_type_name
    | interface_type_name
    | function_block_type_name
    | class_type_name
    ;

string_type_name
    : 'STRING' ('[' unsigned_int ']')?
    | 'WSTRING' ('[' unsigned_int ']')?
    | 'CHAR'
    | 'WCHAR'
    ;

elementary_type_name
    : numeric_type_name
    | bit_string_type_name
    | string_type_name
    | date_type_name
    | time_type_name
    ;

numeric_type_name
    : integer_type_name
    | real_type_name
    ;

integer_type_name
    : signed_integer_type_name
    | unsigned_integer_type_name
    ;

signed_integer_type_name
    : 'SINT' | 'INT' | 'DINT' | 'LINT'
    ;

unsigned_integer_type_name
    : 'USINT' | 'UINT' | 'UDINT' | 'ULINT'
    ;

real_type_name
    : 'REAL' | 'LREAL'
    ;

time_type_name
    : 'TIME' | 'LTIME'
    ;

date_type_name
    : 'DATE' | 'LDATE'
    ;

tod_type_name
    : 'TIME_OF_DAY' | 'TOD' | 'LTOD'
    ;

dt_type_name
    : 'DATE_AND_TIME' | 'DT' | 'LDT'
    ;

bit_string_type_name
    : boolean_type_name
    | multi_bit_type_name
    ;

boolean_type_name
    : 'BOOL'
    ;

multi_bit_type_name
    : 'BYTE' | 'WORD' | 'DWORD' | 'LWORD'
    ;

// Constants, literals and values
constant
    : numeric_literal
    | character_string
    | time_literal
    | bit_string_literal
    | boolean_literal
    ;

numeric_literal
    : integer_literal
    | real_literal
    ;

integer_literal
    : (integer_type_name '#')? (signed_integer | binary_integer | octal_integer | hex_integer)
    ;

signed_integer
    : ('+' | '-')? unsigned_int
    ;

unsigned_int
    : DECIMAL_INT
    ;

binary_integer
    : '2#' BINARY_INT
    ;

octal_integer
    : '8#' OCTAL_INT
    ;

hex_integer
    : '16#' HEX_INT
    ;

real_literal
    : (real_type_name '#')? signed_integer '.' unsigned_int ('E' signed_integer)?
    ;

bit_string_literal
    : (multi_bit_type_name '#')? (unsigned_int | binary_integer | octal_integer | hex_integer)
    ;

boolean_literal
    : (boolean_type_name '#')? ('0' | '1' | 'FALSE' | 'TRUE')
    ;

character_string
    : ('STRING#')? string_literal
    | ('WSTRING#')? string_literal
    | ('CHAR#')? string_literal
    | ('WCHAR#')? string_literal
    ;

string_literal
    : s_byte_character_string
    | d_byte_character_string
    ;

s_byte_character_string
    : SINGLE_BYTE_STRING
    ;

d_byte_character_string
    : DOUBLE_BYTE_STRING
    ;

time_literal
    : duration
    | time_of_day
    | date
    | date_and_time
    ;

duration
    : (time_type_name | 'T' | 'LT') '#' ('+' | '-')? interval
    ;

interval
    : days
    | hours
    | minutes
    | seconds
    | milliseconds
    | microseconds
    | nanoseconds
    ;

days
    : fixed_point 'd'
    | unsigned_int 'd' '_'? hours
    | unsigned_int 'd' '_'?
    ;

hours
    : fixed_point 'h'
    | unsigned_int 'h' '_'? minutes
    | unsigned_int 'h' '_'?
    ;

minutes
    : fixed_point 'm'
    | unsigned_int 'm' '_'? seconds
    | unsigned_int 'm' '_'?
    ;

seconds
    : fixed_point 's'
    | unsigned_int 's' '_'? milliseconds
    | unsigned_int 's' '_'?
    ;

milliseconds
    : fixed_point 'ms'
    | unsigned_int 'ms' '_'? microseconds
    | unsigned_int 'ms' '_'?
    ;

microseconds
    : (fixed_point 'us') | (unsigned_int 'us' '_'?)? nanoseconds?
    ;

nanoseconds
    : fixed_point 'ns'
    ;

fixed_point
    : unsigned_int ('.' unsigned_int)?
    ;

time_of_day
    : (tod_type_name | 'LTIME_OF_DAY') '#' daytime
    ;

daytime
    : unsigned_int ':' unsigned_int ':' fixed_point
    ;

date
    : (date_type_name | 'D' | 'LD') '#' date_literal
    ;

date_literal
    : unsigned_int '-' unsigned_int '-' unsigned_int
    ;

date_and_time
    : (dt_type_name | 'LDATE_AND_TIME') '#' date_literal '-' daytime
    ;

// Identifiers
identifier
    : IDENTIFIER
    ;

// Lexer rules
EOL
    : '\r'? '\n'
    ;

INTERNAL
    : 'INTERNAL'
    ;

// Basic tokens
IDENTIFIER
    : [A-Za-z_][A-Za-z0-9_]*
    ;

DECIMAL_INT
    : [0-9]('_'?[0-9])*
    ;

BINARY_INT
    : [01]('_'?[01])*
    ;

OCTAL_INT
    : [0-7]('_'?[0-7])*
    ;

HEX_INT
    : [0-9A-Fa-f]('_'?[0-9A-Fa-f])*
    ;

// String tokens with escape sequence handling
SINGLE_BYTE_STRING
    : '\'' (
        ~['\r\n] |
        '$$' |
        '$\'' |
        '$L' | '$l' |
        '$N' | '$n' |
        '$P' | '$p' |
        '$R' | '$r' |
        '$T' | '$t' |
        '$' [0-9A-Fa-f][0-9A-Fa-f]
      )* '\'' 
    ;
    
DOUBLE_BYTE_STRING
    : '"' (
        ~["\r\n] |
        '$$' |
        '$"' |
        '$L' | '$l' |
        '$N' | '$n' |
        '$P' | '$p' |
        '$R' | '$r' |
        '$T' | '$t' |
        '$' [0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]
      )* '"'
    ;

// Comments and whitespace
COMMENT
    : ('//' ~[\r\n]* (EOL | EOF)
    | '(*' .*? '*)'
    | '/*' .*? '*/') -> channel(HIDDEN)
    ;

PRAGMA
    : '{' .*? '}' -> channel(HIDDEN)
    ;

WHITESPACE
    : [ \t\r\n]+ -> skip
    ;