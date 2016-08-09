import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;


public class sli{
	
	public static final String PUSH = "push";
	public static final String POP = "pop";
	public static final String ADD = "add";
	public static final String SUB = "sub";
	public static final String MUL= "mul";
	public static final String DIV= "div";
	public static final String REM = "rem";
	public static final String NEG = "neg";
	public static final String SWAP = "swap";
	public static final String QUIT = "quit"; 
	public static final String AND = "and"; 
	public static final String NOT = "not"; 
	public static final String OR = "or"; 
	public static final String EQUAL = "equal"; 
	public static final String LESS_THAN = "lessThan"; 
	public static final String BIND = "bind"; 
	public static final String IFF = "if"; 
	public static final String LET = "let"; 
	public static final String END = "end"; 
	public static final String FUN = "fun"; 
	public static final String CALL = "call";
	public static final String INOUTFUN = "inOutFun";

	//Literal aliases
	public static final String TRUE_LITERAL  = ":true:";
	public static final String FALSE_LITERAL = ":false:";
	public static final String ERROR_LITERAL = ":error:";
	public static final String UNIT_LITERAL = ":unit:";
	
	//Types
	public static final String NUM = "num";
	public static final String BOOLEAN = "bolean";
	public static final String ERROR = "error";
	public static final String STRING = "string";
	public static final String NAME = "name";
	public static final String UNIT = "unit";	
	public static final String CLOSURE = "closure";
	
	public static final Set<String> booleans = new HashSet<String>();
	public static final Set<String> binaryIntPrimitives = new HashSet<String>();
	public static final Set<String> binaryBoolPrimitives = new HashSet<String>();
	public static final Set<String> binaryPrimitives = new HashSet<String>();
	public static final Set<String> validNegTypes = new HashSet<String>();
	public static final Set<String> validNotTypes = new HashSet<String>();
	public static final Set<String> validBindTypes = new HashSet<String>();
	public static final Set<String> validIfTypes = new HashSet<String>();
	public static final Set<String> validBinaryTypes = new HashSet<String>();
	public static final Set<String> validArgTypes = new HashSet<String>();
		
	public static void sli(String inFile, String outFile) throws IOException{
		
		initSetValues();
		ArrayList stack = new ArrayList(); 
		Env env = new Env(); 
				
		Scanner lineScanner = new Scanner(new File(inFile));		
		while(lineScanner.hasNextLine()){
			stack = parsePrimitive(stack, lineScanner, env, "noArg", outFile);
		}	
		lineScanner.close();
    
	//End sli function
	}

	static class Env{	
		public ArrayList<Integer> markers = new ArrayList<Integer>();		
		public ArrayList <ArrayList> masterEnvStack = new ArrayList<ArrayList>();
	
						
		public Env(){
			HashMap initialEnv = new HashMap();
			ArrayList initialEnvStack = new ArrayList();
			initialEnvStack.add(0, initialEnv);
			masterEnvStack.add(0, initialEnvStack);
		}
		public void bind(String name, Object variable){
			ArrayList topEnvStack = (ArrayList) masterEnvStack.get(0);
			HashMap topEnv = (HashMap) topEnvStack.get(0);
			topEnv.put(name, variable);
		}
		public void addNewEnv(){	
			ArrayList topEnvStack = (ArrayList) masterEnvStack.get(0);
			topEnvStack.add(0, new HashMap());
		}		
		public void removeCurrentEnv(){
			ArrayList topEnvStack = (ArrayList) masterEnvStack.get(0);
			topEnvStack.remove(0);			
		}
		public boolean isBound(String name){
			for(ArrayList envStack: masterEnvStack){
				ArrayList topEnvStack = envStack;		
				for(Object i: topEnvStack){
					HashMap env = (HashMap) i;
					if(env.containsKey(name)){
						return true;
					}
				}
			}
			return false;
		}
		public boolean isClosure(String name){
			for(ArrayList envStack: masterEnvStack){
				ArrayList topEnvStack = envStack;	
				for(Object i: topEnvStack){
					HashMap env = (HashMap) i;
					if(env.containsKey(name)){
						Object o = env.get(name);
						if(o instanceof Closure){
							return true;
						}
					}
				}					
			}		
			return false;
		}
		
		public Object getBoundValue(String name){
			for(ArrayList envStack: masterEnvStack){
				ArrayList topEnvStack = envStack;
				for(Object i: topEnvStack){
					HashMap env = (HashMap) i;
					if(env.containsKey(name)){
						return env.get(name);
					}
				}
			}
			Object o = null;
			return o;
		}
		public void addFuncEnvStack(Closure closure){
			ArrayList funcEnvStack = (ArrayList) closure.funcEnvStack.clone();
			ArrayList newTopEnvStack = new ArrayList(funcEnvStack);
			
			masterEnvStack.add(0, newTopEnvStack);			
		}
		public void removeFuncEnvStack(){
			masterEnvStack.remove(0);
		}
	}
	
	static class Closure {
		public ArrayList funcEnvStack = new ArrayList();
		String body = "";
		String formalParameter = "";
		boolean isInOutFun = false;
		
		public Closure(Closure closure){
			ArrayList envStack = closure.funcEnvStack;
			for(Object i: envStack){
				HashMap envMap = (HashMap)i;
				this.funcEnvStack.add(envMap.clone());
			}
			this.body = closure.body;
			this.formalParameter = closure.formalParameter; 
			this.isInOutFun = closure.isInOutFun;
		}
		
		public Closure(Env env, String body, String formalParameter){
			ArrayList topEnvStack = env.masterEnvStack.get(0);
			for(Object i: topEnvStack){
				HashMap envMap = (HashMap)i;
				this.funcEnvStack.add(envMap.clone());
			}
			this.body = body;
			this.formalParameter = formalParameter;
		}
	}
	
	public static ArrayList parsePrimitive(ArrayList stack, Scanner lineScanner, Env env, String argName, String outFile) throws IOException{		
		String line = lineScanner.nextLine();
		String delims = "[ ]+";
		String[] tokens = line.split(delims);
		String primitive = tokens[0];
		
		//1. Push
      if (primitive.equals(PUSH)){ 	

			String literal = line.substring(5);
			stack = push(stack, env, literal, argName);			
      }
		//2. Pop
		else if (primitive.equals(POP)){
			stack = pop(stack);
		}	
		//3. Boolean 
		else if(booleans.contains(primitive)){
			stack.add(0, primitive);
		}	
		//4. Error		
		else if(primitive.equals(ERROR_LITERAL)){
			stack.add(0, ERROR_LITERAL) ;				
		}
		//5. Add 
		//6. Sub
		//7. Mul 
		//8. Div
		//9. Rem
		//------------
		//13. And
		//14. Or
		//------------
		//16. Equal
		//17  LessThan
		else if(binaryPrimitives.contains(primitive)){
			stack = binaryOperationsHandler(stack, primitive, env);
		}
		//10. Neg
		else if (primitive.equals(NEG)){
			stack = neg(stack, env);
		}
		//11. Swap
		else if(primitive.equals(SWAP)){
			stack = swap(stack);
		}
		//12. Quit
		else if(primitive.equals(QUIT)){
			quit(stack, outFile);
		}
		//15. Not
		else if(primitive.equals(NOT)){
			stack = not(stack, env);			
		}
		//18. bind
		else if(primitive.equals(BIND)){
			stack = bind(stack, env);
		}
		//19.if
		else if(primitive.equals(IFF)){
			stack = iff(stack, env);
		}
		//20. let
		else if(primitive.equals(LET)){
			let(stack, env);		
		}
		//20. end
		else if(primitive.equals(END)){
			stack = end(stack, env);
		}
		//21. fun / 23. inOutFun
		else if(primitive.equals(FUN) || primitive.equals(INOUTFUN) ){
			String funType = primitive;
			String funName = tokens[1];
			String formalParameter = tokens[2];
			fun(stack, env, lineScanner, funType, funName, formalParameter);
		}
      	//21. call
		else if(primitive.equals(CALL)){
			stack = call(stack, env, outFile);
		}
        
		return stack;
	}//End parsePrimitive function
	
	////////////
	//Functions/	
	////////////
	
	//push function
	public static ArrayList push(ArrayList stack, Env env, String literal, String argName){
		String type = getType(literal, env);
		if(type.equals(ERROR)){
			stack.add(0, ERROR_LITERAL);
		}
		else{
			stack.add(0, literal);
		}
		return stack;
	}
	
	//pop function
	public static ArrayList pop(ArrayList stack){
		if (stack.size() < 1)
			stack.add(0, ERROR_LITERAL);
		else
			stack.remove(0);

		return stack;
	}
	
	//add function
	public static ArrayList add(ArrayList stack, int x, int y){	
		String literal = Integer.toString(x + y);
		stack.add(0, literal);
		return stack;
	}	
	
	//sub function
	public static ArrayList sub(ArrayList stack, int x, int y){		
		String literal = Integer.toString(x - y);
		stack.add(0, literal);
		return stack;
	}
	
	//mul function
	public static ArrayList mul (ArrayList stack, int x, int y){	
		String literal = Integer.toString(x * y);
		stack.add(0, literal);
		return stack;
	}
	
	//div function
	public static ArrayList div(ArrayList stack, int x, int y){		
		if(y == 0){
			String s_y = Integer.toString(y);
			String s_x = Integer.toString(x);			
			stack.add(0, s_x);
			stack.add(0, s_y);
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String literal = Integer.toString(x / y);
			stack.add(0, literal);
		}		
		return stack;
	}
	
	// rem function
	public static ArrayList rem(ArrayList stack, int x, int y){		
		if(y == 0){
			String s_y = Integer.toString(y);
			String s_x = Integer.toString(x);			
			stack.add(0, s_x);
			stack.add(0, s_y);
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String literal = Integer.toString(x % y);
			stack.add(0, literal);	
		}
		return stack;
	}
	
	//neg function
	public static ArrayList neg(ArrayList stack, Env env){
		if (stack.size() < 1){
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String literal = (String) stack.get(0);
			String type = getType(literal, env);
			
			if(!validNegTypes.contains(type)){
				stack.add(0, ERROR_LITERAL);
			}			
			else if(type.equals(NAME) && !env.isBound(literal)){
				stack.add(0, ERROR_LITERAL);
			}
			else{
				if(type.equals(NAME)){
					literal = (String) env.getBoundValue(literal);
					type = getType(literal, env);
				}				
				if(!type.equals(NUM)){
					stack.add(0, ERROR_LITERAL);
				}
				else{
					stack.remove(0);
					int value = Integer.parseInt(literal);
					literal = Integer.toString(value * -1);
					stack.add(0, literal);
				}			
			}
		}
		return stack;
	}
	
	//swap function
	public static ArrayList swap(ArrayList stack){
		if(stack.size() < 2){
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String literal = (String) stack.get(1);
			stack.remove(1);
			stack.add(0, literal);
		}
		return stack;
	}
		
	//quit function
	public static void quit(ArrayList stack, String outFile) throws IOException{
		PrintWriter pWriter = new PrintWriter(new File(outFile));
		for(Object i: stack){
			String s = (String) i;
			s = s.replace("\"","");
			pWriter.println(s);
			System.out.println(s);
		}
		pWriter.close(); 
		stack.clear();
	}
	
	//and function
	public static ArrayList and(ArrayList stack, String boolean1, String boolean2){
		if(boolean1.equals(TRUE_LITERAL) && boolean2.equals(TRUE_LITERAL))
			stack.add(0, TRUE_LITERAL);		
		else
			stack.add(0, FALSE_LITERAL);
		
		return stack;
	}	
	
	//or function
	public static ArrayList or(ArrayList stack, String boolean1, String boolean2){
		if(boolean1.equals(TRUE_LITERAL) || boolean2.equals(TRUE_LITERAL))
			stack.add(0, TRUE_LITERAL);	
		else
			stack.add(0, FALSE_LITERAL);
		
		return stack;
	}
	
	//not function
	public static ArrayList not(ArrayList stack, Env env){	
		if (stack.size() < 1){
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String literal = (String) stack.get(0);
			String type = getType(literal, env);
			
			if(!validNotTypes.contains(type)){
				stack.add(0, ERROR_LITERAL);
			}			
			else if(type.equals(NAME) && !env.isBound(literal)){
				stack.add(0, ERROR_LITERAL);
			}
			else{
				if(type.equals(NAME)){
					literal = (String) env.getBoundValue(literal);
					type = getType(literal, env);
				}				
				if(!type.equals(BOOLEAN)){
					stack.add(0, ERROR_LITERAL);
				}
				else{
					stack.remove(0);		
					if(literal.equals(TRUE_LITERAL)){
						stack.add(0, FALSE_LITERAL);
					}			
					else if(literal.equals(FALSE_LITERAL)){
						stack.add(0, TRUE_LITERAL);					
					}
				}
			}
		}
		return stack;
	}
	
	//equal function
	public static ArrayList equal(ArrayList stack, int x, int y){
		if(x == y)
			stack.add(0, TRUE_LITERAL);
		else	
			stack.add(0, FALSE_LITERAL);
		
		return stack;
	}
	
	//lessThan function
	public static ArrayList lessThan(ArrayList stack, int x, int y){
		if(x < y)
			stack.add(0, TRUE_LITERAL);
		else	
			stack.add(0, FALSE_LITERAL);
		
		return stack;
	}
	
	//bind function
	public static ArrayList bind(ArrayList stack, Env env){
		if(stack.size() < 2){
			stack.add(0, ERROR_LITERAL);
		}
		else{		
			String s0_variable = (String) stack.get(0);
			String s1_name = (String) stack.get(1);
			String s0_type = getType(s0_variable, env);		
			String s1_type = getType(s1_name, env);
			
			if(!s1_type.equals(NAME)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(!validBindTypes.contains(s0_type)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(s0_type.equals(NAME) && !env.isBound(s0_variable)){	
				stack.add(0, ERROR_LITERAL);
			}
			else{			
				if(s0_type.equals(NAME)){
					s0_variable = (String) env.getBoundValue(s0_variable);
					s0_type = getType(s0_variable, env);					
				}
				env.bind(s1_name, s0_variable);
				stack.remove(0);
				stack.remove(0);
				stack.add(0, UNIT_LITERAL);
			}
		} 
		return stack;
	}
	
	//iF function
	public static ArrayList iff(ArrayList stack, Env env){
		if(stack.size() < 3){
			stack.add(0, ERROR_LITERAL);
		}
		else{		
			String z = (String) stack.get(2);
			String type = getType(z, env);
		
			if(!validIfTypes.contains(type)){
				stack.add(0, ERROR_LITERAL);
			}			
			else if(type.equals(NAME) && !env.isBound(z)){
				stack.add(0, ERROR_LITERAL);
			}
			else{
				if(type.equals(NAME)){
					z = (String) env.getBoundValue(z);
					type = getType(z, env);					
				}
				
				if(!type.equals(BOOLEAN)){
					stack.add(0, ERROR_LITERAL);
				}		
				else if(z.equals(TRUE_LITERAL)){
					stack.remove(2);
					stack.remove(1);
				}
				else if(z.equals(FALSE_LITERAL)){
					stack.remove(2);
					stack.remove(0);
				}
			}
		}
		return stack;
	}
	                               
	//let
	public static void let(ArrayList stack, Env env){
		env.addNewEnv();
		env.markers.add(0, stack.size());
	}
	
	//end
	public static ArrayList end(ArrayList stack, Env env){
		env.removeCurrentEnv();
		int lastLetMarker = env.markers.get(0);
		env.markers.remove(0);
		int newStackSize = lastLetMarker + 1;
		while(stack.size() != newStackSize){
			stack.remove(1);
		}	
		return stack;
	}

	//fun
	public static ArrayList fun(ArrayList stack, Env env, Scanner lineScanner, String funType, String funName, String formalParameter){
		String line = lineScanner.nextLine();
		String body = "";
		while(!line.equals("funEnd")){
			body = body.concat(line + "\n");
			line = lineScanner.nextLine();
		}
		
		Closure closure = new Closure(env, body, formalParameter);
		if(funType.equals(INOUTFUN)){
			closure.isInOutFun = true;
		}		
		env.bind(funName, closure);
		stack.add(0, UNIT_LITERAL);
		
		return stack;		
	}
	
	//call
	public static ArrayList call(ArrayList stack, Env env, String outFile) throws IOException{
		if(stack.size() < 2){
			stack.add(0, ERROR_LITERAL);
		}
		else{
			String calledFunName = (String) stack.get(0);
			String calledFunType = getType(calledFunName, env);
			String calledFunParameter = (String) stack.get(1);
			String calledFunParameterType = getType(calledFunParameter, env);
			
			
			if(!calledFunType.equals(CLOSURE)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(calledFunParameterType.equals(NAME) && !env.isBound(calledFunParameter)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(calledFunParameterType.equals(ERROR)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(!validArgTypes.contains(calledFunParameterType)){
				stack.add(0, ERROR_LITERAL);
			}
			else{
				stack.remove(0);
				stack.remove(0);
				
				Closure closure = (Closure) env.getBoundValue(calledFunName);
				closure = new Closure (closure);
				env.addFuncEnvStack(closure);
				env.bind(calledFunName, closure);
				
				//Bind formal paramater to new env
				String formalParameter = closure.formalParameter;
				if(calledFunParameterType.equals(NAME) || calledFunParameterType.equals(CLOSURE)){
					Object calledFunParameterValue = env.getBoundValue(calledFunParameter);
					env.bind(formalParameter, calledFunParameterValue);
				}
				else{
					env.bind(formalParameter, calledFunParameter);
				}		
				Scanner funcBodyScanner = new Scanner(closure.body);
				Scanner returnScanner = new Scanner(closure.body);
				ArrayList funcStack = new ArrayList();
				
				boolean functionReturned = false;
				while(funcBodyScanner.hasNextLine()){
					String returnLine = returnScanner.nextLine();
					if (returnLine.equals("return")) {
						functionReturned = true;
						break;
					}
					funcStack = parsePrimitive(funcStack, funcBodyScanner, env, calledFunParameter, outFile);					
				}
				
				String callResult = (String) funcStack.get(0);
				if(closure.isInOutFun){
					Object formalParameterValue = env.getBoundValue(formalParameter);
					ArrayList callingEnvStack = env.masterEnvStack.get(1);
					HashMap callingEnv = (HashMap) callingEnvStack.get(0);
					callingEnv.put(calledFunParameter, formalParameterValue);
					//env.bind(calledFunParameter, formalParameterValue);					
				}
				String callResultType = getType(callResult, env);
				if(callResultType.equals(NAME)){
					callResult = (String) env.getBoundValue(callResult);
				}
				
				env.removeFuncEnvStack();
				if(functionReturned){
					stack.add(0, callResult);			
				}
			}			
		}
		return stack;
	}
	
	//binaryIntHandler function
	public static ArrayList binaryIntHandler(ArrayList stack, String primitive, int x, int y){
		
		stack.remove(0);
		stack.remove(0);
		
		if (primitive.equals(ADD)){
			stack = add(stack, x, y);
		}
		else if (primitive.equals(SUB)){
			stack = sub(stack, x, y);
		}
		else if (primitive.equals(MUL)){
			stack = mul(stack, x, y);
		}
		else if (primitive.equals(DIV)){
			stack = div(stack, x, y);
		}
		else if (primitive.equals(REM)){
			stack = rem(stack, x, y);	
		}
		else if (primitive.equals(EQUAL)){
			stack = equal(stack, x, y);
		}
		else if (primitive.equals(LESS_THAN)){
			stack = lessThan(stack, x, y);
		}			
		return stack;
	}
	
	//binaryBoolHandler function
	public static ArrayList binaryBoolHandler(ArrayList stack, String primitive, String boolean1, String boolean2){		
		
		stack.remove(0);
		stack.remove(0);
		
		if(primitive.equals(AND)){
			stack = and(stack, boolean1, boolean2);
		}
		else if(primitive.equals(OR)){
			stack = or(stack, boolean1, boolean2);
		}			
		return stack;
	}
	
	public static ArrayList binaryOperationsHandler(ArrayList stack, String primitive, Env env){
		if(stack.size() < 2){
			stack.add(0, ERROR_LITERAL);
		}	
		else{		
			String s0 = (String) stack.get(0);
			String s1 = (String) stack.get(1);
			String s0_type = getType(s0, env);
			String s1_type = getType(s1,env);
					
			if(s0_type.equals(NAME) && !env.isBound(s0)){
				stack.add(0, ERROR_LITERAL);
			}
			else if(s1_type.equals(NAME) && !env.isBound(s1)){
				stack.add(0, ERROR_LITERAL);
			}
			else{		
				if(s0_type.equals(NAME)){
					s0 = (String) env.getBoundValue(s0);
					s0_type = getType(s0, env);					
				}
				if(s1_type.equals(NAME)){
					s1 = (String) env.getBoundValue(s1);
					s1_type = getType(s1, env);					
				}
				
				if(!validBinaryTypes.contains(s0_type)){
					stack.add(0, ERROR_LITERAL);
				}
				else if(!validBinaryTypes.contains(s1_type)){
					stack.add(0, ERROR_LITERAL);
				}
				else if(!s0_type.equals(s1_type)){
					stack.add(0, ERROR_LITERAL);
				}
				
				else if(s0_type.equals(NUM) && s1_type.equals(NUM)){					
					int y = Integer.parseInt(s0);
					int x = Integer.parseInt(s1);		
					stack = binaryIntHandler(stack, primitive, x, y);
				}
				else if(s0_type.equals(BOOLEAN) && s1_type.equals(BOOLEAN)){
					stack = binaryBoolHandler(stack, primitive, s0, s1);
				}	
			}
		}
		return stack;
	}
	
	public static void initSetValues(){
		booleans.addAll(Arrays.asList(TRUE_LITERAL, FALSE_LITERAL));								
		binaryPrimitives.addAll(Arrays.asList(ADD, SUB, MUL, DIV, REM, EQUAL, LESS_THAN, AND, OR));
		validNegTypes.addAll(Arrays.asList(NUM, NAME));
		validNotTypes.addAll(Arrays.asList(BOOLEAN, NAME));
		validBindTypes.addAll(Arrays.asList(NUM, STRING, BOOLEAN, UNIT, NAME));
		validIfTypes.addAll(Arrays.asList(BOOLEAN, NAME));
		validBinaryTypes.addAll(Arrays.asList(NUM, BOOLEAN));
		validArgTypes.addAll(Arrays.asList(NAME, CLOSURE, NUM, STRING, BOOLEAN, UNIT));
	}
	
	public static String getType(String literal, Env env){
		String type = ERROR;
		if(literal.matches("^(-?)\\d+$")){
			type = NUM;			
		}	
		else if(literal.charAt(0) == '"'){		
			type = STRING;
		}	
		else if(literal.startsWith(":t") || literal.startsWith(":f")){
			type = BOOLEAN;
		}
		else if(env.isClosure(literal)){
			type = CLOSURE;
		}
		else if(Character.isLetter(literal.charAt(0))){	
			type = NAME;
		}
		else if(literal.equals(UNIT_LITERAL)){
			type = UNIT;
		}
		
		
		return type;
	}

}//End sli class
