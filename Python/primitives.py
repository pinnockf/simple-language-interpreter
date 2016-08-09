#primitives.py

#String constants for primitives to cutdown on typos
PUSH = "push"
POP = "pop" 
ADD = "add"
SUB = "sub"
MUL = "mul"
DIV = "div"
REM = "rem"
NEG = "neg" 
SWAP = "swap"
QUIT = "quit" 
AND = "and"
NOT = "not"
OR = "or"
EQUAL = "equal"
LESS_THAN = "lessThan"
BIND = "bind"
IFF = "if"
LET = "let"
END = "end"
FUN = "fun"
CALL = "call"
INOUTFUN = "inOutFun"

TRUE_LITERAL = ":true:"
FALSE_LITERAL = ":false:"
ERROR_LITERAL = ":error:"
UNIT_LITERAL = ":unit:"

#Types of Literals
NUM = "num"
BOOLEAN = "boolean"
ERROR = "error"
STRING = "string"
NAME = "name"
UNIT = "unit"
CLOSURE = "closure"


booleans = [TRUE_LITERAL, FALSE_LITERAL]
binary_int_primitives = [ADD, SUB, MUL, DIV, REM, EQUAL, LESS_THAN]
binary_bool_primitives = [AND, OR]
binary_primitives = binary_int_primitives + binary_bool_primitives
valid_neg_types = [NUM, NAME]
valid_not_types = [BOOLEAN, NAME]
valid_bind_types = [NUM, STRING, BOOLEAN, UNIT, NAME]
valid_if_types = [BOOLEAN, NAME]
valid_binary_types = [NUM, BOOLEAN]
valid_parameter_types = [NAME, CLOSURE, NUM, STRING, BOOLEAN, UNIT]
