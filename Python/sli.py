import re
import copy
from primitives import *

class Env:
    
    def __init__(self):
        self.scope_markers = []
        self.master_env_stack = [[{}]] #Stack of stack of maps... inception
        
    def bind(self, name, variable):
        top_env_stack = self.master_env_stack[0]
        current_env = top_env_stack[0]
        current_env[name] = variable
        
    def add_new_env(self):
        top_env_stack = self.master_env_stack[0]
        top_env_stack.insert(0, {})

    def remove_current_env(self):
        top_env_stack = self.master_env_stack[0]     
        top_env_stack.pop(0)
        
    def is_bound(self, name):
        for env_stack in self.master_env_stack:
            for env in env_stack:
                if name in env:
                    return True
        return False
     
    def is_closure(self, fun_name):           
        for env_stack in self.master_env_stack:
            for env in env_stack:
                if fun_name in env:
                    closure = env.get(fun_name)
                    if isinstance(closure, Closure):
                        return True
        return False
    
    
    def get_value(self, name):
        for env_stack in self.master_env_stack:
            for env in env_stack:
                if name in env:
                    return env.get(name)
        return None
    
    
    def add_func_env_stack(self, closure):
        new_top_env_stack = copy.deepcopy(closure.func_env_stack)
        self.master_env_stack.insert(0, new_top_env_stack)
    
    def remove_func_env_stack(self):   
        self.master_env_stack.pop(0)
        
class Closure:
    def __init__(self, env, body, formal_parameter):
        self.func_env_stack = copy.deepcopy(env.master_env_stack[0])
        self.body = body
        self.formal_parameter = formal_parameter
        self.is_inOutFun = False
    
def hw4(input, output):
    with open(input, 'r') as inputfile:
        stack = []
        env = Env()
        scanner = (line for line in inputfile)
        for line in scanner:
            line = line.strip('\n')
            stack = parse_primitive(stack, line, scanner, env, output)
            
def parse_primitive(stack, line, scanner, env, output):
    tokens = line.split()
    primitive = tokens[0]
    
    #1. Push
    if primitive == PUSH:      
        literal = line.split(' ', 1)[1]    
        stack = push(stack, env, literal)  
    
    #2. Pop   
    elif primitive == POP:
        stack = pop(stack)
    
    #3. boolean
    elif primitive in booleans:
        stack.insert(0, primitive)
        
    #4. error
    elif primitive == ERROR_LITERAL:
        stack.insert(0, ERROR_LITERAL)
        
    #5. Add
    #6. Sub
    #7. Mul
    #8. Div
    #9. Rem
    #-------
    #13. And
    #14. Or
    #-------
    #16. equal
    #17. lessThan
    elif primitive in binary_primitives:
        stack = binary_operations_handler(stack, primitive, env)
    
    #10. Neg
    elif primitive == NEG:
        stack = neg(stack, env)
    
    #11. Swap
    elif primitive == SWAP:
        stack = swap(stack)
        
    #12. quit
    elif primitive == QUIT:
        quit(stack, output)
    
    #15. Not
    elif primitive == NOT:
        stack = do_not(stack, env)
        
    #18. bind
    elif primitive == BIND:
        stack = bind(stack, env)
        
    #19. IF
    elif primitive == IFF:
        stack = iff(stack, env)
        
    #20 let
    elif primitive == LET:
        let(stack, env)
        
    #20 end
    elif primitive == END:
        stack = end(stack, env)
    
    #21. fun / 23 inOutFun
    elif primitive == FUN or primitive == INOUTFUN:
        fun_type = primitive
        fun_name = tokens[1]
        formal_parameter = tokens[2]
        stack = fun(stack, env, scanner, fun_type, fun_name, formal_parameter)
    
    elif primitive == CALL:
        stack = call(stack, env, output)
    
    return stack

    #Push function
def push(stack, env, literal):
    type = get_type(literal, env)
    if  type == ERROR:
        stack.insert(0, ERROR_LITERAL)
    else: 
        stack.insert(0, literal)
    
    return stack
    
    
    #Pop function
def pop(stack):    
    if len(stack) < 1:
        stack.insert(0, ERROR_LITERAL)
    else:
        stack.pop(0)
    
    return stack
        
    #Add function
def add(stack, x, y):
    literal = (str(x + y))
    stack. insert(0, literal)
    return stack
    
    #Sub function
def sub(stack, x, y):
    literal = (str(x - y))
    stack. insert(0, literal)
    return stack
    
    #Mul function
def mul(stack, x, y):
    literal = (str(x * y))
    stack. insert(0, literal)
    return stack
    
    #Div function
def div(stack, x, y):
    if y == 0:
        s_y = str(y)
        s_x = str(x)
        stack.insert(0, s_x)
        stack.insert(0, s_y)
        stack.insert(0, ERROR_LITERAL)
    else:
        literal = (str(x // y))
        stack.insert(0, literal)
    
    return stack
        
    #Rem function
def rem(stack, x, y):
    if y == 0:
        s_y = str(y)
        s_x = str(x)
        stack.insert(0, s_x)
        stack.insert(0, s_y)
        stack.insert(0, ERROR_LITERAL)
    else:
        literal = (str(x % y))
        stack.insert(0, literal)
    
    return stack
        
    #Neg function
def neg(stack, env): 
    if len(stack) < 1:
        stack.insert(0, ERROR_LITERAL)
    else:
        literal = stack[0]
        type = get_type(literal, env)
        
        if type not in valid_neg_types:
            stack.insert(0, ERROR_LITERAL)
    
        elif type == NAME and  env.is_bound(literal) == False:
            stack.insert(0, ERROR_LITERAL)
        
        else:
            if type == NAME:
                literal = env.get_value(literal)
                type = get_type(literal, env)
                
            if type != NUM:
                stack.insert(0, ERROR_LITERAL)
            
            else:
                stack.pop(0)
                value = int(literal)
                literal = str(value * -1)
                stack.insert(0, literal)
                
    return stack
    
    #Swap function
def swap(stack):
    if len(stack) < 2:
        stack.insert(0, ERROR_LITERAL)
    else:
        stack[0], stack[1] = stack[1], stack[0]
    
    return stack

    #Quit function
def quit(stack, output):
    with open(output, 'w') as out:     
        for item in stack:
            item = item.strip('"')
            print(item)
            out.write(item + '\n')
    
    stack.clear()
    #And function
def do_and(stack, boolean1, boolean2):
    if boolean1 == TRUE_LITERAL and boolean2 == TRUE_LITERAL:
        stack.insert(0, TRUE_LITERAL)
    else:
       stack.insert(0, FALSE_LITERAL)
       
    return stack
    
    #Or function
def do_or(stack, boolean1, boolean2):
    if boolean1 == TRUE_LITERAL or boolean2 == TRUE_LITERAL:
        stack.insert(0, TRUE_LITERAL)
    else:
       stack.insert(0, FALSE_LITERAL)
       
    return stack
    
    #Not function
def do_not(stack, env):
    if len(stack) < 1:
        stack.insert(0, ERROR_LITERAL)
        
    else:
        literal = stack[0]
        type = get_type(literal, env)
        
        if type not in valid_not_types:
            stack.insert(0, ERROR_LITERAL)
            
        elif type == NAME and env.is_bound == False:
            stack.insert(0, ERROR_LITERAL)
        
        else:
            if type == NAME:
                literal = env.get_value(literal)
                type = get_type(literal, env)
            
            if type != BOOLEAN:
                stack.insert(0, ERROR_LITERAL)
            
            else:
                stack.pop(0)
                if literal == TRUE_LITERAL:
                    stack.insert(0, FALSE_LITERAL)
                elif literal == FALSE_LITERAL:
                    stack.insert(0, TRUE_LITERAL)
    return stack
        
    #Equal function
def equal(stack, x, y):
    if x == y:
        stack.insert(0, TRUE_LITERAL)
    else:
        stack.insert(0, FALSE_LITERAL)
    
    return stack

    #LessThan function
def less_than(stack, x, y):
    if x < y:
        stack.insert(0, TRUE_LITERAL)
    else:
        stack.insert(0, FALSE_LITERAL)
        
    return stack
        
def bind(stack, env):    
    if len(stack) < 2:
        stack.insert(0, ERROR_LITERAL)
    
    else:
        s0_variable = stack[0]
        s0_type = get_type(s0_variable, env)
        s1_name = stack[1]
        s1_type = get_type(s1_name, env)
        
        if s1_type != NAME:
            stack.insert(0, ERROR_LITERAL)
                    
        elif s0_type not in valid_bind_types:
            stack.insert(0, ERROR_LITERAL)  
                  
        elif s0_type == NAME and env.is_bound(s0_variable) == False:
            stack.insert(0, ERROR_LITERAL)
        else:
            stack.pop(0)
            stack.pop(0)
            if s0_type == NAME:
                s0_variable = env.get_value(s0_variable)
                s0_type = get_type(s0_variable, env)
            env.bind(s1_name, s0_variable)
            stack.insert(0, UNIT_LITERAL)
    
    return stack
    
def iff(stack, env):
    if len(stack) < 3:
        stack.insert(0, ERROR_LITERAL)
        
    else:
        z = stack[2]
        type = get_type(z, env)
        
        if type not in valid_if_types:
            stack.insert(0, ERROR_LITERAL)
            
        elif type == NAME and env.is_bound(z) == False:
            stack.insert(0, ERROR_LITERAL)
            
        else:
            if type == NAME:
                z = env.get_value(z)
                type = get_type(z, env)
            
            if type != BOOLEAN:
                stack.insert(0, ERROR_LITERAL)
            
            elif z == TRUE_LITERAL:
                stack.pop(2)
                stack.pop(1)
            elif z == FALSE_LITERAL:
                stack.pop(2)
                stack.pop(0)
    
    return stack
            
    
def let(stack, env):
    env.add_new_env()
    env.scope_markers.insert(0, len(stack))
    
    
def end(stack, env):
    env.remove_current_env()
    last_let_marker = env.scope_markers[0]
    env.scope_markers.pop(0)
    new_stack_size = last_let_marker + 1
    while (len(stack) != new_stack_size):
        stack.pop(1)
        
    return stack
    
    #fun
def fun(stack, env, scanner, fun_type, fun_name, formal_paramter):
    body = []
    for line in scanner:
        line = line.strip('\n')
        if line == 'funEnd':
            break
        body.append(line.strip('\n'))
    
    closure = Closure(env, body, formal_paramter)
    if fun_type == INOUTFUN:
        closure.is_inOutFun = True     
    env.bind(fun_name, closure)
    stack.insert(0, UNIT_LITERAL)
    
    return stack
    
    #call
def call(stack, env, output):
    if len(stack) < 2:
        stack.insert(0, ERROR_LITERAL)
    else:
        called_fun_name = stack[0]
        called_fun_type = get_type(called_fun_name, env)
        called_fun_parameter = stack[1]
        called_fun_parameter_type = get_type(called_fun_parameter, env)
        
        if called_fun_type != CLOSURE:
            stack.insert(0, ERROR_LITERAL)
        elif called_fun_parameter_type == NAME and env.is_bound(called_fun_parameter) == False:
            stack.insert(0, ERROR_LITERAL)
        elif called_fun_parameter_type == ERROR:
            stack.insert(0, ERROR_LITERAL)
        elif called_fun_parameter_type not in valid_parameter_types:
            stack.insert(0, ERROR_LITERAL)
        else:
            stack.pop(0)
            stack.pop(0)
            
            closure = env.get_value(called_fun_name)
            closure = copy.deepcopy(closure)
            env.add_func_env_stack(closure)
            env.bind(called_fun_name, closure)
            
            formal_parameter = closure.formal_parameter
            if called_fun_parameter_type == NAME or called_fun_parameter_type == CLOSURE:
                called_fun_parameter_value = env.get_value(called_fun_parameter)
                env.bind(formal_parameter, called_fun_parameter_value)
            else:
                env.bind(formal_parameter, called_fun_parameter)
                
            fun_stack = []
            function_returned = False
            scanner = (line for line in closure.body)
            for line in scanner:
                if line == 'return':
                    function_returned = True
                    break
                
                fun_stack = parse_primitive(fun_stack, line, scanner, env, output)
            
            if closure.is_inOutFun == True:
                formal_parameter_value = env.get_value(formal_parameter)
                parent_env_stack = env.master_env_stack[1]
                parent_env = parent_env_stack[0]
                parent_env[called_fun_parameter] = formal_parameter_value
                
            call_result = fun_stack[0]
            call_result_type = get_type(call_result, env)
            if call_result_type == NAME:
                call_result = env.get_value(call_result)
            
            env.remove_func_env_stack()
            if function_returned == True:
                stack.insert(0, call_result)
                
    return stack
    #binary_int_handler
def binary_int_handler(stack, primitive, x, y):
    
    stack.pop(0)
    stack.pop(0)
    
    if primitive == ADD:
        stack = add(stack, x,y)
    if primitive == SUB:
        stack = sub(stack, x,y)
    if primitive == MUL:
        stack = mul(stack, x,y)
    if primitive == DIV:
        stack = div(stack, x,y)
    if primitive == REM:
        stack = rem(stack, x,y)
    if primitive == EQUAL:
        stack = equal(stack, x,y)
    if primitive == LESS_THAN:
        stack = less_than(stack, x,y)
        
    return stack
        
    #binary_bool_handler
def binary_bool_handler(stack, primitive, boolean1, boolean2):
    
    stack.pop(0)
    stack.pop(0)
    
    if primitive == AND:
        stack = do_and(stack, boolean1, boolean2)
    elif primitive == OR:
        stack = do_or(stack, boolean1, boolean2)
        
    return stack
    
    #binary_operations_handler
def binary_operations_handler(stack, primitive, env):
    if len(stack) < 2:
        stack.insert(0, ERROR_LITERAL)
        
    else:
        s0 = stack[0]
        s0_type = get_type(s0, env)
        s1 = stack[1]
        s1_type = get_type(s1, env)
        
        if s0_type == NAME and env.is_bound(s0) == False:
            stack.insert(0, ERROR_LITERAL)
            
        if s1_type == NAME and env.is_bound(s1) == False:
            stack.insert(0, ERROR_LITERAL)
        
        else:
            if s0_type == NAME:
                s0 = env.get_value(s0)
                s0_type = get_type(s0, env)           
            if s1_type == NAME:
                s1 = env.get_value(s1)
                s1_type = get_type(s1, env)
                
            if s0_type not in valid_binary_types:
                stack.insert(0, ERROR_LITERAL)                
            elif s1_type not in valid_binary_types:
                stack.insert(0, ERROR_LITERAL)
            elif s0_type != s1_type:  
                stack.insert(0, ERROR_LITERAL)
            
            elif s0_type == NUM and s1_type == NUM:
                y = int(s0)
                x = int(s1)
                stack = binary_int_handler(stack, primitive, x, y)
            elif s0_type == BOOLEAN and s1_type == BOOLEAN:
                stack = binary_bool_handler(stack, primitive, s0, s1)
                
    return stack
            
def get_type(variable, env):
    type = ERROR
    if re.match('^(-?)\\d+$', variable):
        type = NUM
    elif variable[0] == '"':
        type = STRING
    elif variable == TRUE_LITERAL or variable == FALSE_LITERAL:
        type = BOOLEAN
    elif env.is_closure(variable):
        type = CLOSURE 
    elif variable[0].isalpha():
        type = NAME 
    elif variable == UNIT_LITERAL:
        type = UNIT
    
    return type
