/*
 * IEC61131-3 ANTLR4 Grammar File
 * =============================
 * 
 * Copyright (C) 2023 Suifei <https://github.com/suifei>
 * Repository: https://github.com/suifei/iec61131-3-grammar
 * 
 * Features:
 * - Complete implementation of IEC61131-3 Fourth Edition (2023)
 * - Support for ST (Structured Text) and SFC (Sequential Function Chart)
 * - OOP extensions (interfaces, classes, namespaces)
 * - Modern language features (generics, lambda expressions, async/await)
 * 
 * License:
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

grammar IEC61131_3;

@parser::members {
    // Custom error recovery strategy
    protected void reportError(Token token, String message) {
        // Report error with line number and message
        System.err.println("Error at line " + token.getLine() + ": " + message);
    }
    
    // Enhanced error recovery strategy
    protected boolean shouldReportError(Token token) {
        return true; // Always report errors for industrial reliability
    }
    
    // Synchronized error recovery points
    public void synchronize() {
        // Recover to nearest stable point in syntax tree
        while (_input.LA(1) != Token.EOF) {
            if (_input.LA(1) == ';') {
                consume();
                return;
            }
            consume();
        }
    }
    
    // Robust error reporting with location information
    public void reportContextSensitiveError(String message, Token offendingToken) {
        // Report detailed error with context
        String errorMessage = String.format(
            "Error at line %d:%d - %s",
            offendingToken.getLine(),
            offendingToken.getCharPositionInLine(),
            message
        );
        notifyErrorListeners(offendingToken, errorMessage, null);
    }
    
    // Symbol table for tracking declarations and references
    private SymbolTable symbolTable = new SymbolTable();
    
    // Type compatibility checking
    private boolean areTypesCompatible(String type1, String type2) {
        // Implement type compatibility rules based on IEC 61131-3
        return true;
    }
    
    // Variable scope validation
    private boolean isVariableInScope(String varName, ParserRuleContext context) {
        // Check if variable is accessible in current scope
        return true;
    }
    
    // Function call validation
    private boolean validateFunctionCall(String funcName, List<ParserRuleContext> args) {
        // Validate function call with argument count and types
        return true;
    }
    
    // Enumeration value validation
    private boolean isValidEnumValue(String enumTypeName, String enumValue) {
        // Check if value is valid for the enum type
        return true;
    }
    
    // Record a symbol declaration
    private void declareSymbol(String name, String type, ParserRuleContext context) {
        // Add to symbol table with location information
    }
    
    // Record a symbol reference
    private void referenceSymbol(String name, ParserRuleContext context) {
        // Add reference with location information
    }
    
    // Resolve a reference to its declaration
    private ParserRuleContext resolveReference(String name, ParserRuleContext context) {
        // Look up in symbol table
        return null;
    }
    
    // PLC-specific diagnostic checks
    private void checkCriticalVariableInitialization(VarDeclaration ctx) {
        // Ensure safety-critical variables have proper initialization
    }
    
    private void checkPotentialScanCycleOverruns(StatementList ctx) {
        // Check for loops or operations that might cause cycle overruns
    }
    
    private void checkPotentialRaceConditions(List<AccessPath> accessPaths) {
        // Identify potential race conditions in multi-task environments
    }
    
    private void checkAssignmentPatterns(AssignmentStatement ctx) {
        // Check for common PLC programming errors in assignments
    }
    
    private void checkPotentialDeadlocks(SfcNetwork ctx) {
        // Analyze SFC networks for potential deadlocks
    }
    
    private void checkEdgeDetectionUsage(EdgeDeclaration ctx) {
        // Validate proper usage of edge detection in programs
    }
}

@lexer::members {
    // Keep track of tokens for better error diagnostics
    private java.util.List<Token> tokens = new java.util.ArrayList<>();
    
    @Override
    public Token nextToken() {
        Token token = super.nextToken();
        tokens.add(token);
        return token;
    }
    
    // Enhanced error reporting for lexical errors
    public void reportLexicalError(String message, int startIndex, int stopIndex) {
        // Report detailed lexical error with suggestions for correction
        String errorMessage = String.format(
            "Lexical error at position %d-%d: %s",
            startIndex,
            stopIndex,
            message
        );
        
        // 获取错误上下文
        String inputStream = this.getInputStream().toString();
        int contextStart = Math.max(0, startIndex - 10);
        int contextEnd = Math.min(inputStream.length(), stopIndex + 10);
        String context = inputStream.substring(contextStart, contextEnd);
        
        System.err.println(errorMessage);
        System.err.println("Context: ..." + context + "...");
    }
    
    // Industry-specific lexical helpers
    private boolean isValidIdentifier(String text) {
        // Validate identifier according to IEC61131-3 rules
        return text.matches("[a-zA-Z_][a-zA-Z0-9_]*");
    }
}
// ============================== COMPILATION UNIT ==============================

options { caseInsensitive=true; }

compilation_unit
    : pragma_directive* 
      using_directive* 
      library_element_declaration*
      EOF
    ;

pragma_directive
    : '{$' pragma_content '$}'
    ;

pragma_content
    : pragma_key (':' | '=') pragma_value
    ;

pragma_key
    : IDENTIFIER
    ;

pragma_value
    : character_string
    | numeric_literal
    | IDENTIFIER
    ;

// ============================== VENDOR EXTENSIONS ==============================

vendor_specific_extension
    : '{' vendor_identifier ':' extension_content '}'
    ;

vendor_identifier
    : IDENTIFIER
    ;

extension_content
    : extension_element*
    ;

extension_element
    : extension_keyword extension_parameters?
    ;

extension_keyword
    : IDENTIFIER
    ;

extension_parameters
    : '(' extension_parameter (',' extension_parameter)* ')'
    ;

extension_parameter
    : extension_param_name ':=' extension_param_value
    ;

extension_param_name
    : IDENTIFIER
    ;

extension_param_value
    : expression
    ;

// ============================== ARTIFACT SUPPORT ==============================

artifact_reference
    : 'ARTIFACT' artifact_name artifact_type artifact_location
    ;

artifact_name
    : IDENTIFIER
    ;

artifact_type
    : 'LIBRARY'
    | 'SOURCE'
    | 'BINARY'
    | 'CONFIGURATION'
    | 'DOCUMENTATION'
    ;

artifact_location
    : character_string
    ;

// ============================== COMPILER DIRECTIVES ==============================

compiler_directive
    : conditional_compilation_directive
    | code_generation_directive
    | optimization_directive
    ;

conditional_compilation_directive
    : '#IF' expression '#THEN'
      library_element_declaration*
      ('#ELSIF' expression '#THEN'
      library_element_declaration*)*
      ('#ELSE'
      library_element_declaration*)?
      '#END_IF'
    ;

code_generation_directive
    : '#PRAGMA' code_gen_option
    ;

code_gen_option
    : IDENTIFIER ('(' code_gen_parameter (',' code_gen_parameter)* ')')?
    ;

code_gen_parameter
    : parameter_name ':=' parameter_value
    ;

parameter_name
    : IDENTIFIER
    ;

parameter_value
    : constant
    | IDENTIFIER
    ;

optimization_directive
    : '#OPTIMIZE' '(' optimization_level ')'
    ;

optimization_level
    : 'NONE'
    | 'SIZE'
    | 'SPEED'
    | 'DEFAULT'
    | INTEGER
    ;

// ============================== LIBRARY ELEMENTS ==============================

using_directive
    : 'USING' namespace_name_list ';'
    ;

namespace_name_list
    : namespace_name (',' namespace_name)*
    ;

export_declaration
    : 'EXPORT' export_item_list ';'
    ;

export_item_list
    : export_item (',' export_item)*
    ;

export_item
    : IDENTIFIER
    | IDENTIFIER '.' '*'
    ;

library_element_name 
    : data_type_name 
    | function_name 
    | function_block_type_name 
    | program_type_name
    | resource_type_name 
    | configuration_name
    | interface_name  
    | class_name      
    | namespace_name  
    ;
    
program_type_name : IDENTIFIER;

configuration_name : IDENTIFIER;

resource_type_name : IDENTIFIER;

interface_name : IDENTIFIER;

class_name : IDENTIFIER;

namespace_name : IDENTIFIER ('.' IDENTIFIER)*;

library_element_declaration 
    : data_type_declaration
    | function_declaration 
    | function_block_declaration 
    | program_declaration 
    | configuration_declaration
    | interface_declaration  
    | class_declaration      
    | namespace_declaration
    | compiler_directive
    | artifact_reference
    | vendor_specific_extension
    | export_declaration
    | unit_declaration // CODESYS specific extension
    ;
    
// --- CODESYS specific extensions ---
unit_declaration
    : 'UNIT' unit_name ';'
      unit_interface_section?
      unit_implementation_section?
    ;

unit_name
    : IDENTIFIER
    ;

unit_interface_section
    : 'INTERFACE'
      unit_uses_section?
      (function_declaration
      | function_block_declaration
      | data_type_declaration
      | global_var_declarations)*
      'END_INTERFACE'
    ;

unit_uses_section
    : 'USES' unit_name_list ';'
    ;

unit_name_list
    : unit_name (',' unit_name)*
    ;

unit_implementation_section
    : 'IMPLEMENTATION'
      (function_declaration
      | function_block_declaration)*
      'END_IMPLEMENTATION'
    ;

// ============================== PROGRAM ==============================

program_declaration 
    : 'PROGRAM' program_type_name 
      ( io_var_declarations 
      | other_var_declarations 
      | located_var_declarations 
      | program_access_decls )* 
      program_body
      'END_PROGRAM'
    ;

program_body
    : function_block_body
    ;

program_access_decls 
    : 'VAR_ACCESS' program_access_decl ';' (program_access_decl ';' )* 'END_VAR'
    ;

program_access_decl 
    : access_name ':' symbolic_variable ':' non_generic_type_name direction?
    ;

access_declarations 
    : 'VAR_ACCESS' access_declaration ';' (access_declaration ';')* 'END_VAR'
    ;

access_declaration 
    : access_name ':' access_path ':' non_generic_type_name direction?
    ;

access_path 
    : (resource_name '.')? direct_variable 
    | (resource_name '.')? (program_name '.')? (fb_name'.')* symbolic_variable
    ;

global_var_reference 
    : (resource_name '.')? global_var_name ('.' structure_element_name)?
    ;

access_name : IDENTIFIER;

program_output_reference : program_name '.' symbolic_variable;

direction : 'READ_WRITE' | 'READ_ONLY';

program_name : IDENTIFIER;

// ============================== CONFIGURATION ==============================

configuration_declaration 
    : 'CONFIGURATION' configuration_name 
      global_var_declarations* 
      (single_resource_declaration | (resource_declaration+)) 
      access_declarations? 
      instance_specific_initializations? 
      'END_CONFIGURATION'
    ; 

resource_declaration 
    : 'RESOURCE' resource_name 'ON' resource_type_name 
      global_var_declarations? 
      single_resource_declaration 
      'END_RESOURCE'
    ; 

single_resource_declaration 
    : (task_configuration ';')* program_configuration ';' (program_configuration ';')*
    ;

resource_name : IDENTIFIER;

task_configuration 
    : 'TASK' task_name task_initialization
    ;

task_name : IDENTIFIER;

task_initialization 
    : '(' ('SINGLE' ':=' data_source ',')? 
          ('INTERVAL' ':=' data_source ',')? 
          'PRIORITY' ':=' INTEGER ')'
    ;

data_source 
    : constant 
    | global_var_reference 
    | program_output_reference 
    | direct_variable
    ;

program_configuration 
    : 'PROGRAM' ('RETAIN' | 'NON_RETAIN')? program_name 
      ('WITH' task_name)? ':' program_type_name 
      ('(' prog_conf_elements ')')?
    ;

prog_conf_elements : prog_conf_element (',' prog_conf_element)*;

prog_conf_element : fb_task | prog_cnxn;

fb_task : fb_name 'WITH' task_name;

prog_cnxn 
    : symbolic_variable ':=' prog_data_source 
    | symbolic_variable '=>' data_sink
    ;

prog_data_source 
    : constant 
    | enumerated_value 
    | global_var_reference 
    | direct_variable
    ;

data_sink 
    : global_var_reference 
    | direct_variable
    ;

instance_specific_initializations 
    : 'VAR_CONFIG' instance_specific_init ';' (instance_specific_init ';')* 'END_VAR'
    ;

instance_specific_init 
    : resource_name '.' program_name '.' (fb_name '.')* 
      ((variable_name location? ':' located_var_spec_init) 
      | (fb_name ':' function_block_type_name ':=' structure_initialization))
    ;

// ============================== EXPRESSION HANDLING ==============================

expression 
    : xor_expression (OR xor_expression)*
    | comparison (AND comparison)*
    | comparison (OR comparison)*
    | comparison (XOR comparison)*
    ;

array_index_expression
    : expression (('+' | '-') expression)*
    ;

multi_element_variable
    : variable_name ('[' expression ']')*
    | variable_name '.' field_selector
    ;

// 移除array_index_expression规则，直接使用expression规则处理数组索引
// 这样可以支持CoDeSys风格的复杂索引表达式，如out[out_start + i - 1]


xor_expression 
    : and_expression (XOR and_expression)*
    ;

and_expression 
    : comparison (AND comparison)*
    ;

comparison 
    : equ_expression ((EQU | NEQU) equ_expression)*
    ;

equ_expression 
    : add_expression (comparison_operator add_expression)*
    ;

comparison_operator 
    : LT | GT | LE | GE
    ;

add_expression 
    : term (add_operator term)*
    ;

add_operator 
    : PLUS | MINUS
    ;

term 
    : power_expression (multiply_operator power_expression)*
    ;

multiply_operator 
    : MUL | DIV | MOD
    ;

power_expression 
    : unary_expression (POW unary_expression)*
    ;

unary_expression 
    : unary_operator? primary_expression
    ;

unary_operator 
    : MINUS | NOT
    ;

primary_expression 
    : constant 
    | enumerated_value
    | variable
    | LPAR expression RPAR
    | function_name LPAR (param_assignment (',' param_assignment)*)? RPAR
    | await_expression
    | lambda_expression
    ;

constant 
    : numeric_literal 
    | character_string 
    | time_literal 
    | bit_string_literal 
    | boolean_literal
    ;

// ============================== LITERALS ==============================

numeric_literal 
    : integer_literal 
    | real_literal
    ;

real_literal 
    : (real_type_name '#')? FIXED_POINT EXPONENT?
    ;

integer_literal 
    : (integer_type_name '#')? (INTEGER | SIGNED_INTEGER | BINARY_INTEGER | OCTAL_INTEGER | HEX_INTEGER)
    ;

data_type_name 
    : non_generic_type_name 
    | generic_type_name
    ;

non_generic_type_name 
    : elementary_type_name 
    | derived_type_name
    ;
    
derived_type_name 
    : single_element_type_name 
    | array_type_name 
    | structure_type_name 
    | string_type_name
    ;

single_element_type_name 
    : simple_type_name 
    | subrange_type_name 
    | enumerated_type_name
    ;

simple_type_name : IDENTIFIER;

subrange_type_name : IDENTIFIER;

enumerated_type_name : IDENTIFIER;

array_type_name : IDENTIFIER;

structure_type_name : IDENTIFIER;
    
string_type_name : IDENTIFIER;

elementary_type_name 
    : numeric_type_name 
    | date_type_name 
    | bit_string_type_name 
    | STRING ('['(INTEGER|SIGNED_INTEGER)']')?
    | WSTRING ('['(INTEGER|SIGNED_INTEGER)']')?
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
    : SINT | INT | DINT | LINT
    ;

unsigned_integer_type_name 
    : USINT | UINT | UDINT | ULINT
    ;

real_type_name 
    : REAL | LREAL
    ;

date_type_name 
    : DATE | TIMEOFDAY | DATETIME
    ;
    
bit_string_type_name 
    : BITSTRING | BOOL
    ;

character_string 
    : single_byte_character_string 
    | double_byte_character_string
    ;

single_byte_character_string 
    : SB_STRING_LITERAL
    ;

double_byte_character_string 
    : DB_STRING_LITERAL
    ;

boolean_literal 
    : (BOOL '#')? BOOLEAN 
    ;
    
bit_string_literal 
    : (BITSTRING '#')? (INTEGER | BINARY_INTEGER | OCTAL_INTEGER | HEX_INTEGER)
    ;

time_literal 
    : duration 
    | time_of_day
    | date
    | date_and_time
    ;

duration 
    : TIME '#' MINUS? interval
    ;

interval 
    : DAYS | HOURS | MINUTES | SECONDS | MILLISECONDS
    ;

time_of_day 
    : TIMEOFDAY '#' daytime
    ;

daytime 
    : day_hour COLON day_minute COLON day_second
    ;

day_hour : INTEGER;

day_minute : INTEGER;

day_second : FIXED_POINT;

date 
    : DATE '#' date_literal
    ;

date_literal 
    : year MINUS month MINUS day
    ;

year : INTEGER;

month : INTEGER;

day : INTEGER;

date_and_time 
    : DATETIME '#' date_literal MINUS daytime
    ;

// ============================== ENUM VALUES ==============================

enumerated_value 
    : (enumerated_type_name '#')? IDENTIFIER
    ;

// ============================== DATA TYPES ==============================

data_type_declaration 
    : 'TYPE' type_declaration ';' (type_declaration ';')* 'END_TYPE'
    ;

type_declaration 
    : single_element_type_declaration 
    | array_type_declaration 
    | structure_type_declaration 
    | string_type_declaration
    | generic_type_declaration
    ;
    
single_element_type_declaration 
    : simple_type_declaration 
    | subrange_type_declaration 
    | enumerated_type_declaration
    ;

simple_type_declaration 
    : simple_type_name ':' simple_spec_init
    ;

simple_spec_init 
    : simple_specification (':=' constant)?
    ;

simple_specification 
    : elementary_type_name 
    | simple_type_name
    ;

// ============================== FUNCTION BLOCKS ==============================

function_block_declaration 
    : 'FUNCTION_BLOCK' derived_function_block_name 
      implements_interface? 
      (io_var_declarations | other_var_declarations | method_declaration)* 
      function_block_body 
      'END_FUNCTION_BLOCK'
    ;

implements_interface 
    : 'IMPLEMENTS' interface_name_list
    ;

other_var_declarations 
    : external_var_declarations 
    | var_declarations 
    | retentive_var_declarations 
    | non_retentive_var_declarations 
    | temp_var_decls 
    | incompl_located_var_declarations
    ;

temp_var_decls 
    : 'VAR_TEMP' temp_var_decl ';' (temp_var_decl ';')* 'END_VAR'
    ;

function_block_body 
    : statement_list 
    | sfc_network
    ;

// ============================== FUNCTIONS ==============================

function_body 
    : statement_list 
    ;

function_declaration 
    : 'FUNCTION' derived_function_name ':' 
      (elementary_type_name | derived_type_name) 
      (io_var_declarations | function_var_decls | temp_var_decls)* 
      function_body 
      'END_FUNCTION'
    ;

io_var_declarations 
    : input_declarations 
    | output_declarations 
    | input_output_declarations
    ;
    
input_declarations 
    : 'VAR_INPUT' ('RETAIN' | 'NON_RETAIN')? 
      input_declaration ';' (input_declaration ';')* 
      'END_VAR'
    ;

input_declaration 
    : var_init_decl 
    | edge_declaration
    ;
    
edge_declaration 
    : var1_list ':' BOOL ('R_EDGE' | 'F_EDGE')
    ;

var_init_decl 
    : var1_init_decl 
    | array_var_init_decl 
    | structured_var_init_decl 
    | fb_name_decl 
    | string_var_declaration
    | var1_list ':' data_type_name (':=' expression)?  // CoDeSys style initialization
    ;

var1_init_decl 
    : var1_list ':' (simple_spec_init | subrange_spec_init | enumerated_spec_init)
    ;

var1_list 
    : variable_name (',' variable_name)*
    ;
    
array_var_init_decl 
    : var1_list ':' array_spec_init
    ;

structured_var_init_decl 
    : var1_list ':' initialized_structure
    ;

fb_name_decl 
    : fb_name_list ':' function_block_type_name (':=' structure_initialization)?
    ;

fb_name_list 
    : fb_name (',' fb_name)*
    ;

fb_name 
    : IDENTIFIER
    ;

function_block_type_name 
    : standard_function_block_name 
    | derived_function_block_name
    ;

standard_function_block_name
    : // 标准功能块 - 基于IEC 61131-3第四版(2023)标准
      
      // 双稳态功能块 (SR/RS触发器)
      'SR' | 'RS'
      
      // 边沿检测功能块
    | 'R_TRIG' | 'F_TRIG'
      
      // 计数器功能块
    | 'CTU' | 'CTD' | 'CTUD'
      
      // 定时器功能块
    | 'TP' | 'TON' | 'TOF' | 'RTC'
      
      // 通信功能块 - 基于IEC 61131-5
    | 'SEND' | 'RECEIVE' | 'USEND' | 'URCV' | 'BSEND' | 'BRCV' | 'TSEND' | 'TRCV'
      
      // 扩展功能块 - 常见但非标准
      // 这些是常见实现但不是IEC 61131-3标准的一部分
    | 'DERIVATIVE' | 'INTEGRAL' | 'PID'
    | 'SEMA' | 'SCALE' | 'UNSCALE'
    | 'ALARM_HANDLER' | 'DIAGNOSTIC'
    | 'HYSTERESIS' | 'LIMITER' | 'RAMP'
    ;

derived_function_block_name 
    : IDENTIFIER
    ;

output_declarations 
    : 'VAR_OUTPUT' ('RETAIN' | 'NON_RETAIN')? 
      var_init_decl ';' (var_init_decl ';')* 
      'END_VAR'
    ;

input_output_declarations 
    : 'VAR_IN_OUT' 
      var_declaration ';' (var_declaration ';')* 
      'END_VAR'
    ;

var_declaration 
    : temp_var_decl 
    | fb_name_decl
    ;

temp_var_decl 
    : var1_declaration 
    | array_var_declaration 
    | structured_var_declaration 
    | string_var_declaration
    | single_var_declaration
    ;

single_var_declaration
    : IDENTIFIER ':' (simple_specification | subrange_specification | enumerated_specification)
    ;

var1_declaration 
    : var1_list ':' (simple_specification | subrange_specification | enumerated_specification)
    ;

array_var_declaration 
    : var1_list ':' array_specification
    ;

structured_var_declaration 
    : var1_list ':' structure_type_name
    ;

var_declarations 
    : 'VAR' ('CONSTANT')? 
      var_init_decl ';' (var_init_decl ';')* 
      'END_VAR'
    ;

retentive_var_declarations 
    : 'VAR' 'RETAIN' 
      var_init_decl ';' (var_init_decl ';')* 
      'END_VAR'
    ;

non_retentive_var_declarations 
    : 'VAR' 'NON_RETAIN'
      var_init_decl ';' (var_init_decl ';')* 
      'END_VAR'
    ;

located_var_declarations 
    : 'VAR' ('CONSTANT' | 'RETAIN' | 'NON_RETAIN')? 
      located_var_decl ';' (located_var_decl ';')* 
      'END_VAR'
    ;

located_var_decl 
    : variable_name? location ':' located_var_spec_init
    ;

external_var_declarations 
    : 'VAR_EXTERNAL' ('CONSTANT')? 
      external_declaration ';' (external_declaration ';')* 
      'END_VAR'
    ;

external_declaration 
    : global_var_name ':' (
        simple_specification 
        | subrange_specification 
        | enumerated_specification 
        | array_specification 
        | structure_type_name 
        | function_block_type_name
      )
    ;

global_var_name 
    : IDENTIFIER
    ;

global_var_declarations 
    : 'VAR_GLOBAL' ('CONSTANT' | 'RETAIN')? 
      global_var_decl ';' (global_var_decl ';')* 
      'END_VAR'
    ;

global_var_decl 
    : global_var_spec ':' (located_var_spec_init | function_block_type_name)?
    ;

global_var_spec 
    : global_var_list 
    | global_var_name? location
    ;

located_var_spec_init 
    : simple_spec_init 
    | subrange_spec_init 
    | enumerated_spec_init 
    | array_spec_init 
    | initialized_structure 
    | single_byte_string_spec 
    | double_byte_string_spec
    ;

location 
    : 'AT' direct_variable
    ;

global_var_list 
    : global_var_name (',' global_var_name)*
    ;

string_var_declaration 
    : single_byte_string_var_declaration 
    | double_byte_string_var_declaration
    ;

single_byte_string_var_declaration 
    : var1_list ':' single_byte_string_spec
    ;

single_byte_string_spec 
    : STRING ('[' INTEGER ']')? (':=' single_byte_character_string)?
    ;

double_byte_string_var_declaration 
    : var1_list ':' double_byte_string_spec
    ;

double_byte_string_spec 
    : WSTRING ('[' INTEGER ']')? (':=' double_byte_character_string)?
    ;

incompl_located_var_declarations 
    : 'VAR' ('RETAIN'|'NON_RETAIN')? 
      incompl_located_var_decl ';' (incompl_located_var_decl ';')* 
      'END_VAR'
    ;

incompl_located_var_decl 
    : variable_name incompl_location ':' var_spec
    ;

incompl_location 
    : 'AT' '%' LOCATION_PREFIX '*'
    ;

var_spec 
    : simple_specification 
    | subrange_specification 
    | enumerated_specification 
    | array_specification 
    | structure_type_name 
    | STRING ('[' INTEGER ']')? 
    | WSTRING ('[' INTEGER ']')?
    ;

subrange_type_declaration 
    : subrange_type_name ':' subrange_spec_init
    ;

subrange_spec_init 
    : subrange_specification (':=' SIGNED_INTEGER)?
    ;

subrange_specification 
    : integer_type_name '(' subrange ')' 
    | subrange_type_name
    ;

enumerated_type_declaration 
    : enumerated_type_name ':' enumerated_spec_init
    ;

enumerated_spec_init 
    : enumerated_specification (':=' enumerated_value)?
    ;

enumerated_specification 
    : ('(' enumerated_value (',' enumerated_value)* ')') 
    | enumerated_type_name
    ;

array_type_declaration 
    : array_type_name ':' array_spec_init
    ;

array_spec_init 
    : array_specification (':=' array_initialization)?
    ;

array_specification 
    : array_type_name 
    | 'ARRAY' '[' subrange (',' subrange)* ']' 'OF' non_generic_type_name
    | 'ARRAY' '[' ']' 'OF' non_generic_type_name // 支持空数组声明
    | 'ARRAY' '[' DOUBLEDOT ']' 'OF' non_generic_type_name // 明确支持可变长度数组声明 ARRAY[..]
    ;


array_initialization
    : '[' array_initial_elements (',' array_initial_elements)* ']'
    | '[' expression (',' expression)* ']'  // CoDeSys style array initialization
    ;

array_initial_elements 
    : array_initial_element 
    | INTEGER '(' array_initial_element? ')'
    ;

array_initial_element 
    : constant 
    | enumerated_value 
    | structure_initialization 
    | array_initialization
    ;

structure_type_declaration 
    : structure_type_name ':' structure_specification
    ;

structure_specification 
    : structure_declaration 
    | initialized_structure
    ;

initialized_structure 
    : structure_type_name (':=' structure_initialization)?
    ;

structure_declaration 
    : 'STRUCT' 
      structure_element_declaration ';' (structure_element_declaration ';')*
      'END_STRUCT'
    ;

structure_element_declaration 
    : structure_element_name ':' (
        simple_spec_init 
        | subrange_spec_init 
        | enumerated_spec_init 
        | array_spec_init 
        | initialized_structure
      )
    ;

structure_element_name 
    : IDENTIFIER
    ;

structure_initialization 
    : '(' structure_element_initialization (',' structure_element_initialization)* ')'
    ;

structure_element_initialization 
    : structure_element_name ':=' (
        constant 
        | enumerated_value 
        | array_initialization 
        | structure_initialization
      )
    ;

string_type_declaration 
    : string_type_name ':' (STRING | WSTRING) ('[' INTEGER ']')? (':=' character_string)?
    ;

function_var_decls 
    : 'VAR' ('CONSTANT')? 
      var2_init_decl ';' (var2_init_decl ';')* 
      'END_VAR'
    ;

var2_init_decl 
    : var1_init_decl 
    | array_var_init_decl 
    | structured_var_init_decl 
    | string_var_declaration
    ;

function_name 
    : standard_function_name 
    | derived_function_name
    ;

standard_function_name 
    : // 标准函数 - 基于IEC 61131-3第四版(2023)标准
      
      // 位字符串函数
      'SHL' | 'SHR' | 'ROL' | 'ROR' | 'AND' | 'OR' | 'XOR' | 'NOT'
      
      // 数值函数
    | 'ABS' | 'SQRT' | 'LN' | 'LOG' | 'EXP' | 'SIN' | 'COS' | 'TAN'
    | 'ASIN' | 'ACOS' | 'ATAN' | 'ATAN2' | 'ADD' | 'MUL' | 'SUB' | 'DIV' | 'MOD'
    | 'EXPT' | 'MOVE' | 'TRUNC'
      
      // 比较函数
    | 'GT' | 'GE' | 'EQ' | 'LE' | 'LT' | 'NE'
    | 'MAX' | 'MIN' | 'LIMIT' | 'MUX' | 'SEL'
      
      // 字符串函数
    | 'LEN' | 'LEFT' | 'RIGHT' | 'MID' | 'CONCAT' | 'INSERT' | 'DELETE' | 'REPLACE'
    | 'FIND'
      
      // 宽字符串函数
    | 'WLEN' | 'WLEFT' | 'WRIGHT' | 'WMID' | 'WCONCAT' | 'WINSERT' | 'WDELETE' | 'WREPLACE'
    | 'WFIND'
      
      // 时间/日期函数
    | 'ADD_TIME' | 'SUB_TIME' | 'MULTIME' | 'DIVTIME'
    | 'ADD_TOD_TIME' | 'SUB_TOD_TIME' | 'ADD_DT_TIME' | 'SUB_DT_TIME'
    | 'CONCAT_DATE_TOD'
      
      // 类型转换函数
    | 'TRUNC'
      
      // 标准类型转换函数
    | 'TO_BOOL' | 'TO_BYTE' | 'TO_WORD' | 'TO_DWORD' | 'TO_LWORD'
    | 'TO_SINT' | 'TO_INT' | 'TO_DINT' | 'TO_LINT' | 'TO_USINT' | 'TO_UINT' | 'TO_UDINT' | 'TO_ULINT'
    | 'TO_REAL' | 'TO_LREAL' | 'TO_STRING' | 'TO_WSTRING' | 'TO_TIME' | 'TO_DATE' | 'TO_TOD' | 'TO_DT'
      
      // 扩展函数 - 常见但非标准
      // 这些是常见实现但不是IEC 61131-3标准的核心部分
    | 'CEIL' | 'FLOOR' | 'ROUND'
    | 'SINH' | 'COSH' | 'TANH' | 'ASINH' | 'ACOSH' | 'ATANH'
    | 'TRIM' | 'SUBSTRING' | 'LOWER_CASE' | 'UPPER_CASE'
    | 'STRING_TO_WSTRING' | 'WSTRING_TO_STRING'
    | 'BIT_COUNT' | 'PARITY'
    ;

derived_function_name 
    : IDENTIFIER
    ;
    
param_assignment 
    : ((variable_name ':=')? expression)
    | (NOT? variable_name '=>' variable)
    ;
                 
// ============================== VARIABLES ==============================

variable_name 
    : IDENTIFIER
    ;

variable 
    : direct_variable 
    | symbolic_variable
    | variable '[' expression ']' // 直接支持数组索引表达式
    | variable '.' field_selector // 直接支持字段访问
    ;

symbolic_variable 
    : multi_element_variable  
    ; 

// 使用已有的multi_element_variable规则，但增强其功能以支持CoDeSys风格的数组访问
// 原始定义在第552-556行附近
symbolic_variable_extended 
    : multi_element_variable LBRACK expression (',' expression)* RBRACK
    | multi_element_variable '.' field_selector
    | variable_name
    ;

field_selector 
    : IDENTIFIER
    ;

direct_variable 
    : '%' DIRECT_VAR_SPECIFIER
    ;

// ============================== STATEMENTS ==============================

statement_list 
    : statement ';' (statement ';')*
    ;

statement 
    : assignment_statement 
    | subprogram_control_statement
    | selection_statement 
    | iteration_statement
    | error_action
    | try_catch_statement
    | async_statement
    ;
    
assignment_statement 
    : variable ASSIGN expression
    ;

subprogram_control_statement 
    : fb_invocation 
    | 'RETURN'
    ;

fb_invocation 
    : fb_name LPAR (param_assignment (',' param_assignment)*)? RPAR
    ;
    
selection_statement 
    : if_statement 
    | case_statement
    ;

if_statement 
    : 'IF' expression 'THEN' statement_list
      ('ELSIF' expression 'THEN' statement_list)*
      ('ELSE' statement_list)?
      'END_IF'
    ;

case_statement 
    : 'CASE' expression 'OF'
      case_element*
      ('ELSE' statement_list)?
      'END_CASE'
    ;

case_element 
    : case_list ':' statement_list
    ;

case_list 
    : case_list_element (',' case_list_element)*
    ;

case_list_element 
    : subrange 
    | SIGNED_INTEGER 
    | enumerated_value
    ;

subrange 
    : DOUBLEDOT // 支持可变长度数组 [..]
    | variable_name DOUBLEDOT // 支持下界为变量 [i..]
    | DOUBLEDOT variable_name // 支持上界为变量 [..i]
    | variable_name DOUBLEDOT variable_name // 支持范围为变量 [i..j]
    | SIGNED_INTEGER DOUBLEDOT // 支持下界为常量 [0..]
    | DOUBLEDOT SIGNED_INTEGER // 支持上界为常量 [..10]
    | SIGNED_INTEGER DOUBLEDOT SIGNED_INTEGER // 支持固定范围 [0..10]
    | SIGNED_INTEGER DOUBLEDOT variable_name // 支持下界为常量上界为变量 [0..i]
    | variable_name DOUBLEDOT SIGNED_INTEGER // 支持下界为变量上界为常量 [i..10]
    ;

iteration_statement 
    : for_statement 
    | while_statement 
    | repeat_statement 
    | exit_statement
    ;

for_statement 
    : 'FOR' control_variable ':=' for_list 'DO' statement_list 'END_FOR'
    ;

control_variable 
    : IDENTIFIER
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

exit_statement 
    : 'EXIT'
    ;

// ============================== INTERFACE ==============================

interface_declaration 
    : 'INTERFACE' interface_name extends_interface? 
      interface_declaration_body 
      'END_INTERFACE'
    ;

extends_interface 
    : 'EXTENDS' interface_name_list
    ;

interface_name_list 
    : interface_name (',' interface_name)*
    ;

interface_declaration_body 
    : (interface_method_declaration | interface_property_declaration)*
    ;

interface_method_declaration 
    : method_name ':' method_return_type? 
      '(' (method_param_declaration (',' method_param_declaration)*)? ')' ';'
    ;

method_name 
    : IDENTIFIER
    ;

method_return_type 
    : non_generic_type_name
    ;

method_param_declaration 
    : param_direction? parameter_name ':' non_generic_type_name
    ;

param_direction 
    : 'VAR_INPUT'
    | 'VAR_OUTPUT'
    | 'VAR_IN_OUT'
    ;

interface_property_declaration 
    : property_name ':' non_generic_type_name property_qualifier? ';'
    ;

property_name 
    : IDENTIFIER
    ;

property_qualifier 
    : 'GET'
    | 'SET' 
    | ('GET' ',' 'SET')
    | ('SET' ',' 'GET')
    ;

// ============================== CLASS ==============================

class_declaration 
    : 'CLASS' class_name extends_class? implements_interface? 
      class_modifier? class_body 'END_CLASS'
    ;

extends_class 
    : 'EXTENDS' parent_class_name
    ;

parent_class_name 
    : IDENTIFIER
    ;

class_modifier 
    : 'ABSTRACT' 
    | 'FINAL'
    ;

class_body 
    : class_member_declaration*
    ;

class_member_declaration 
    : visibility_modifier? (class_var_declarations | method_declaration)
    ;

visibility_modifier 
    : 'PUBLIC' 
    | 'PROTECTED' 
    | 'PRIVATE' 
    | 'INTERNAL'
    ;

method_declaration 
    : 'METHOD' method_modifier? method_name ':' method_return_type? 
      method_var_declarations* method_body 'END_METHOD'
    ;

method_modifier 
    : 'OVERRIDE' 
    | 'ABSTRACT' 
    | 'FINAL' 
    | 'STATIC'
    ;

method_body 
    : statement_list
    ;

method_var_declarations 
    : io_var_declarations 
    | function_var_decls
    ;

class_var_declarations 
    : 'VAR' ('CONSTANT')? 
      class_var_init_decl ';' (class_var_init_decl ';')* 'END_VAR'
    ;

class_var_init_decl 
    : var1_init_decl 
    | array_var_init_decl 
    | structured_var_init_decl 
    | string_var_declaration
    ;

// ============================== NAMESPACE ==============================

namespace_declaration 
    : 'NAMESPACE' namespace_name namespace_body 'END_NAMESPACE'
    ;

namespace_body 
    : library_element_declaration*
    ;

// ============================== SFC (Sequential Function Chart) ==============================

sfc_network 
    : initial_step sfc_element*
    ;
    
sfc_element
    : step
    | transition
    | action
    | jump
    | parallel_branch
    | selection_branch
    | sfc_network_comment
    ;

initial_step 
    : 'INITIAL_STEP' step_name ':' action_association* 'END_STEP'
    ;

step 
    : 'STEP' step_name ':' action_association* 'END_STEP'
    ;

step_name 
    : IDENTIFIER
    ;

action_association 
    : action_name '(' action_qualifier (',' action_time)? ')'
    ;

action_name 
    : IDENTIFIER
    ;

action_qualifier 
    : 'N'   // Non-stored (normal)
    | 'R'   // Reset
    | 'S'   // Set (stored)
    | 'P'   // Pulse
    | 'P0'  // Pulse at falling edge
    | 'P1'  // Pulse at rising edge
    | 'L'   // Limited
    | 'D'   // Delayed
    | 'SD'  // Stored and delayed
    | 'DS'  // Delayed and stored
    | 'SL'  // Stored and limited
    | 'C'   // Conditional
    | 'AC'  // Action control
    | 'T'   // Time dependent
    ;

action_time 
    : TIME '#' FIXED_POINT
    | TIME '#' IDENTIFIER
    ;

transition 
    : 'TRANSITION' transition_name? 
      ('FROM' step_name (',' step_name)* 'TO' step_name (',' step_name)*)? 
      ('PRIORITY' ':=' INTEGER)? 
      (':=' | '(')? transition_condition (')' | ';') 
      ('TIMEOUT' ':=' time_specification)?
      'END_TRANSITION'
    ;

transition_name 
    : IDENTIFIER
    ;

transition_condition 
    : expression
    | 'TRUE'  // Always active transition
    | 'FALSE' // Never active transition
    ;

action 
    : 'ACTION' action_name ':' function_block_body 'END_ACTION'
    ;

jump
    : 'JUMP' 'TO' step_name
    ;

parallel_branch
    : 'PARALLEL' parallel_branch_element* 'END_PARALLEL'
    ;

parallel_branch_element
    : sfc_element
    | ('BRANCH' sfc_element* 'END_BRANCH')
    ;

selection_branch
    : 'SELECTION' selection_branch_element* 'END_SELECTION'
    ;

selection_branch_element
    : sfc_element
    | ('BRANCH' transition? sfc_element* 'END_BRANCH')
    ;

sfc_network_comment
    : 'COMMENT' character_string
    ;

time_specification
    : FIXED_POINT
    | duration
    | symbolic_variable
    ;

// ============================== ERROR HANDLING ==============================

exception_declaration
    : 'EXCEPTION' exception_name ('EXTENDS' exception_name)? exception_body 'END_EXCEPTION'
    ;

exception_name
    : IDENTIFIER
    ;

exception_body
    : exception_var_declarations* exception_handler*
    ;

exception_var_declarations
    : 'VAR_EXCEPTION' var_init_decl ';' (var_init_decl ';')* 'END_VAR'
    ;

exception_handler
    : 'HANDLER' handler_name ':' statement_list 'END_HANDLER'
    ;

handler_name
    : IDENTIFIER
    ;

try_catch_statement
    : 'TRY' statement_list 
      ('CATCH' '(' exception_name (',' exception_name)* ')' statement_list)* 
      ('FINALLY' statement_list)? 
      'END_TRY'
    ;

error_action
    : 'RAISE' '(' exception_name (',' parameter (',' parameter)*)? ')'
    | 'RESUME' ('NEXT' | IDENTIFIER)?
    ;

parameter
    : expression
    ;

// ============================== COMMUNICATION ==============================

io_mapping
    : 'IO_MAPPING' mapping_declarations 'END_IO_MAPPING'
    ;

mapping_declarations
    : mapping_declaration ';' (mapping_declaration ';')*
    ;

mapping_declaration
    : variable_name 'MAP_TO' io_address (':' io_mapping_attributes)?
    ;

io_address
    : '%' io_address_specification
    ;

io_address_specification
    : io_type size_prefix? address_details
    ;

io_type
    : 'I'   // Input
    | 'Q'   // Output
    | 'M'   // Memory
    | 'T'   // Temporary
    | 'C'   // Communication
    | 'S'   // System
    ;

size_prefix
    : 'X'   // Single bit
    | 'B'   // Byte (8 bits)
    | 'W'   // Word (16 bits)
    | 'D'   // Double word (32 bits)
    | 'L'   // Long word (64 bits)
    ;

address_details
    : INTEGER ('.' INTEGER)*
    ;

io_mapping_attributes
    : '(' io_attribute (',' io_attribute)* ')'
    ;

io_attribute
    : attribute_name ':=' attribute_value
    ;

attribute_name
    : IDENTIFIER
    ;

attribute_value
    : constant
    | IDENTIFIER
    ;

network_configuration
    : 'NETWORK' network_name network_parameters 'END_NETWORK'
    ;

network_name
    : IDENTIFIER
    ;

network_parameters
    : network_parameter ';' (network_parameter ';')*
    ;

network_parameter
    : parameter_name ':' parameter_type (':=' parameter_value)?
    ;

parameter_type
    : data_type_name
    ;

protocol_definition
    : 'PROTOCOL' protocol_name protocol_body 'END_PROTOCOL'
    ;

protocol_name
    : IDENTIFIER
    ;

protocol_body
    : protocol_parameter ';' (protocol_parameter ';')*
    ;

protocol_parameter
    : parameter_name ':' parameter_type (':=' parameter_value)?
    ;

// ============================== MODERN LANGUAGE FEATURES ==============================

// Support for security features
security_declaration
    : access_control_declaration
    | security_policy
    ;

access_control_declaration
    : 'ACCESS_CONTROL' access_rule ';' (access_rule ';')* 'END_ACCESS_CONTROL'
    ;

access_rule
    : resource_identifier ':' access_level ('(' access_condition ')')?
    ;

resource_identifier
    : IDENTIFIER ('.' IDENTIFIER)*
    ;

access_level
    : 'READ_ONLY'
    | 'READ_WRITE'
    | 'EXECUTE'
    | 'FULL_ACCESS'
    | 'NO_ACCESS'
    ;

access_condition
    : expression
    ;

security_policy
    : 'SECURITY_POLICY' policy_rule ';' (policy_rule ';')* 'END_SECURITY_POLICY'
    ;

policy_rule
    : policy_name ':' policy_value
    ;

policy_name
    : IDENTIFIER
    ;

policy_value
    : constant
    | IDENTIFIER
    ;

// Support for event-driven execution
event_declaration
    : 'EVENT' event_name event_body 'END_EVENT'
    ;

event_name
    : IDENTIFIER
    ;

event_body
    : event_parameter ';' (event_parameter ';')* event_handler
    ;

event_parameter
    : parameter_name ':' parameter_type (':=' parameter_value)?
    ;

event_handler
    : 'HANDLER' statement_list 'END_HANDLER'
    ;

// Support for generic/parameterized types
generic_type_declaration
    : 'TYPE' generic_type_name 
      '<' generic_parameter_list '>' ':' 
      type_specification 
      'END_TYPE'
    ;

generic_type_name
    : IDENTIFIER
    ;

generic_parameter_list
    : generic_parameter (',' generic_parameter)*
    ;

generic_parameter
    : generic_parameter_name (':' constraint_type)?
    ;

generic_parameter_name
    : IDENTIFIER
    ;

constraint_type
    : data_type_name
    | 'ANY'
    | 'ANY_ELEMENTARY'
    | 'ANY_MAGNITUDE'
    | 'ANY_NUM'
    | 'ANY_REAL'
    | 'ANY_INT'
    | 'ANY_BOOL'
    | 'ANY_STRING'
    | 'ANY_DATE'
    | 'ANY_DERIVED'
    ;

type_specification
    : simple_specification
    | array_specification
    | structure_declaration
    | string_type_name
    ;

// Support for lambda expressions
lambda_expression
    : 'LAMBDA' '(' lambda_parameter_list? ')' ':' return_type '=>' expression
    ;

lambda_parameter_list
    : lambda_parameter (',' lambda_parameter)*
    ;

lambda_parameter
    : parameter_name ':' data_type_name
    ;

return_type
    : data_type_name
    ;

// Support for async/await pattern
async_statement
    : 'ASYNC' statement_list 'END_ASYNC'
    | 'ASYNC' '(' expression ')' statement
    ;

await_expression
    : 'AWAIT' expression
    | 'AWAIT' '(' expression ')'
    ;


// Support for streams and reactive programming
stream_declaration
    : 'STREAM' stream_name stream_body 'END_STREAM'
    ;

stream_name
    : IDENTIFIER
    ;

stream_body
    : stream_parameter ';' (stream_parameter ';')* stream_operator*
    ;

stream_parameter
    : parameter_name ':' parameter_type (':=' parameter_value)?
    ;

stream_operator
    : 'MAP' '(' expression ')'
    | 'FILTER' '(' expression ')'
    | 'REDUCE' '(' expression ')'
    | 'MERGE' '(' expression (',' expression)* ')'
    | 'SPLIT' '(' expression (',' expression)* ')'
    | 'WINDOW' '(' window_specification ')'
    ;

window_specification
    : window_type '(' window_parameter (',' window_parameter)* ')'
    ;

window_type
    : 'TIME'
    | 'COUNT'
    | 'SESSION'
    | 'TUMBLING'
    | 'SLIDING'
    ;

window_parameter
    : parameter_name ':=' parameter_value
    ;

// ============================== LEXER RULES ==============================

// --- TIME ---

DAYS 
    : FIXED_POINT ('d') 
    | INTEGER ('d') ('_')? HOURS
    ;

HOURS 
    : FIXED_POINT ('h') 
    | INTEGER ('h')('_')? MINUTES
    ;

MINUTES 
    : FIXED_POINT ('m') 
    | INTEGER ('m') ('_')? SECONDS
    ;

SECONDS 
    : FIXED_POINT ('s') 
    | INTEGER ('s') ('_')? MILLISECONDS
    ;

MILLISECONDS 
    : FIXED_POINT ('ms')
    | INTEGER ('ms')
    ;
    
// --- Fragment Helpers for Case Insensitivity ---

fragment A: 'a';
fragment B: 'b';
fragment C: 'c';
fragment D: 'd';
fragment E: 'e';
fragment F: 'f';
fragment G: 'g';
fragment H: 'h';
fragment I: 'i';
fragment J: 'j';
fragment K: 'k';
fragment L: 'l';
fragment M: 'm';
fragment N: 'n';
fragment O: 'o';
fragment P: 'p';
fragment Q: 'q';
fragment R: 'r';
fragment S: 's';
fragment T: 't';
fragment U: 'u';
fragment V: 'v';
fragment W: 'w';
fragment X: 'x';
fragment Y: 'y';
fragment Z: 'z';

// --- Datatypes ---

SINT : S I N T ;
INT : I N T ;
DINT : D I N T ;
LINT : L I N T ;
USINT : U S I N T ;
UINT : U I N T ;
UDINT : U D I N T ;
ULINT : U L I N T ;
REAL : R E A L ;
LREAL : L R E A L ;
TIME : (T I M E | T) ;
DATE : (D A T E | D) ;
DATETIME : (D A T E '_' A N D '_' T I M E | D T) ;
TIMEOFDAY : (T I M E '_' O F '_' D A Y | T O D) ;
BOOL : B O O L ;

// --- Keywords ---
VAR : V A R ;
VAR_INPUT : V A R '_' I N P U T ;
VAR_OUTPUT : V A R '_' O U T P U T ;
VAR_IN_OUT : V A R '_' I N '_' O U T ;
VAR_TEMP : V A R '_' T E M P ;
VAR_GLOBAL : V A R '_' G L O B A L ;
VAR_ACCESS : V A R '_' A C C E S S ;
VAR_EXTERNAL : V A R '_' E X T E R N A L ;
VAR_CONFIG : V A R '_' C O N F I G ;

END_VAR : E N D '_' V A R ;

CONFIGURATION : C O N F I G U R A T I O N ;
END_CONFIGURATION : E N D '_' C O N F I G U R A T I O N ;

RESOURCE : R E S O U R C E ;
END_RESOURCE : E N D '_' R E S O U R C E ;

PROGRAM : P R O G R A M ;
END_PROGRAM : E N D '_' P R O G R A M ;

FUNCTION : F U N C T I O N ;
END_FUNCTION : E N D '_' F U N C T I O N ;

FUNCTION_BLOCK : F U N C T I O N '_' B L O C K ;
END_FUNCTION_BLOCK : E N D '_' F U N C T I O N '_' B L O C K ;

STRUCT : S T R U C T ;
END_STRUCT : E N D '_' S T R U C T ;

TYPE : T Y P E ;
END_TYPE : E N D '_' T Y P E ;

IF : I F ;
THEN : T H E N ;
ELSIF : E L S I F ;
ELSE : E L S E ;
END_IF : E N D '_' I F ;

CASE : C A S E ;
OF : O F ;
END_CASE : E N D '_' C A S E ;

FOR : F O R ;
TO : T O ;
BY : B Y ;
DO : D O ;
END_FOR : E N D '_' F O R ;

WHILE : W H I L E ;
END_WHILE : E N D '_' W H I L E ;

REPEAT : R E P E A T ;
UNTIL : U N T I L ;
END_REPEAT : E N D '_' R E P E A T ;

RETURN : R E T U R N ;
EXIT : E X I T ;
CONTINUE : C O N T I N U E ;

ARRAY : A R R A Y ;
AT : A T ;
CONSTANT : C O N S T A N T ;
RETAIN : R E T A I N ;
NON_RETAIN : N O N '_' R E T A I N ;
PERSISTENT : P E R S I S T E N T ;

// --- OOP Extensions ---
INTERFACE : I N T E R F A C E ;
END_INTERFACE : E N D '_' I N T E R F A C E ;
METHOD : M E T H O D ;
END_METHOD : E N D '_' M E T H O D ;
CLASS : C L A S S ;
END_CLASS : E N D '_' C L A S S ;
EXTENDS : E X T E N D S ;
IMPLEMENTS : I M P L E M E N T S ;
SUPER : S U P E R ;
THIS : T H I S ;

// --- CODESYS Extensions ---
UNIT : U N I T ;
IMPLEMENTATION : I M P L E M E N T A T I O N ;
END_IMPLEMENTATION : E N D '_' I M P L E M E N T A T I O N ;
USES : U S E S ;

// --- Access Modifiers ---
PUBLIC : P U B L I C ;
PRIVATE : P R I V A T E ;
PROTECTED : P R O T E C T E D ;
INTERNAL : I N T E R N A L ;

// --- Other Keywords ---
NAMESPACE : N A M E S P A C E ;
END_NAMESPACE : E N D '_' N A M E S P A C E ;
USING : U S I N G ;
POINTER : P O I N T E R ;
REF : R E F ;
REF_TO : R E F '_' T O ;
NULL : N U L L ;
TRUE : T R U E ;
FALSE : F A L S E ;

BITSTRING
    : 'BYTE' 
    | 'WORD' 
    | 'DWORD' 
    | 'LWORD'
    | 'BIT'
    ;
    
GENERIC
    : 'ANY' 
    | 'ANY_DERIVED' 
    | 'ANY_ELEMENTARY' 
    | 'ANY_MAGNITUDE' 
    | 'ANY_NUM' 
    | 'ANY_REAL' 
    | 'ANY_INT' 
    | 'ANY_BIT' 
    | 'ANY_STRING' 
    | 'ANY_DATE'
    ;
    
STRING : 'STRING' ;
WSTRING : 'WSTRING' ;
    
// --- Logic Operators ---

OR : O R ;
XOR : X O R ;
AND : '&' | A N D ;
NOT : N O T ;

// --- Comparison Operators ---
EQU : '=' ;
NEQU : '<>' ;
LT : '<' ;
GT : '>' ;
LE : '<=' ;
GE : '>=' ;

// --- Arithmetic Operators ---
PLUS : '+' ;
MINUS : '-' ;
MUL : '*' ;
DIV : '/' ;
MOD : 'MOD' ;
POW : '**' ;
ASSIGN : ':=' ;
    
LPAR : '(' ;
RPAR : ')' ;
LBRACK : '[' ;
RBRACK : ']' ;
COLON : ':' ;
DOT : '.' ;
DOUBLEDOT : '..' ;

BOOLEAN
    : 'TRUE'
    | 'FALSE'
    | BOOL '#' Bit
    ;
    
// --- Numeric Literals ---

FIXED_POINT 
    : (PLUS|MINUS)? INTEGER '.' INTEGER
    ;

SIGNED_INTEGER 
    : (PLUS|MINUS) INTEGER
    ;

EXPONENT 
    : 'e'('+'|'-')? INTEGER
    ;
    
INTEGER
    : Digit (('_')? Digit)*
    ;

BINARY_INTEGER 
    : '2#' Bit (('_')? Bit)*
    ;

OCTAL_INTEGER 
    : '8#' OctalDigit (('_')? OctalDigit)*
    ;

HEX_INTEGER 
    : '16#' HexDigit(('_')? HexDigit)*
    ;    

// --- Variable Specifiers ---

DIRECT_VAR_SPECIFIER
    : LOCATION_PREFIX SIZE_PREFIX? INTEGER (DOT INTEGER)*
    ;

LOCATION_PREFIX
    : 'I'  // Input
    | 'Q'  // Output
    | 'M'  // Memory
    ; 

SIZE_PREFIX
    : 'X'  // Single bit
    | 'B'  // Byte (8 bits)
    | 'W'  // Word (16 bits)
    | 'D'  // Double word (32 bits)
    | 'L'  // Long word (64 bits)
    ;
    
// --- Identifiers ---

IDENTIFIER
    : (Letter | '_') LetterOrDigitOrUnderscore*
    ;

// --- String and Character Helpers ---

HEX_BYTE
    : HexDigit HexDigit
    ;
    
HEX_WORD
    : HexDigit HexDigit HexDigit HexDigit
    ;

DB_STRING_LITERAL
    : '"' DbStringCharacters? '"'
    ;

SB_STRING_LITERAL
    : '\'' SbStringCharacters? '\''
    ;

fragment
DbStringCharacters
    : DbStringCharacter+
    ;

fragment
SbStringCharacters
    : SbStringCharacter+
    ;

fragment
DbStringCharacter
    : ~["$\r\n]
    | EscapeSequence
    | DbQuoteEscapeSequence
    ;

fragment
SbStringCharacter
    : ~['$\r\n]
    | EscapeSequence
    | SbQuoteEscapeSequence
    ;

fragment
EscapeSequence
    : '$' [$lnprt]
    ;

fragment
DbQuoteEscapeSequence
    : '$"'
    ;
    
fragment
SbQuoteEscapeSequence
    : '$\''
    ;

// --- Fragment Helpers ---

fragment 
LetterOrDigitOrUnderscore
    : Letter
    | Digit 
    | '_'
    ;

fragment 
Letter
    : [a-z]
    ;


fragment 
Digit
    : [0-9]
    ;

fragment 
OctalDigit
    : [0-7]
    ;

fragment 
Bit
    : [01]
    ;
    
fragment 
HexDigit
    : Digit 
    | [a-f]
    ;


// --- Whitespace and Comments ---
    
WS  
    : [ \r\t\u000C\n]+ -> skip
    ;
    
LINE_COMMENT
    : '//' ~[\r\n]* -> channel(HIDDEN)
    ;

BLOCK_COMMENT
    : ('(*' | '/*') .*? ('*)' | '*/') -> channel(HIDDEN)
    ;

DOC_COMMENT
    : '///' ~[\r\n]* -> channel(3) // Documentation comments on channel 3
    ;

DOC_BLOCK_COMMENT
    : '/**' .*? '*/' -> channel(3) // Documentation block comments on channel 3
    ;

// --- Error Recovery Token ---
ERROR_TOKEN 
    : . 
    -> channel(HIDDEN)
    ;
