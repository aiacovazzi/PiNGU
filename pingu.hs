import Data.Char
import Data.Bool
import System.IO

--https://stackoverflow.com/questions/6306233/compiling-haskell-hs-in-windows-to-a-exe
--PiNGU, an user friendly programming language for italian people interpreted in Haskell.
--Made by Antonio Raffaele Iacovazzi mat. 756263.
--Thanks to: 
--Graham Hutton for the basis for the monadic parser showed in his book.
--Nicola Quartaro, Gennaro Gala and Pierluigi Dibari for very good examples of how to apply Hutton's parser idea for aritmethical expression  
--in a more complex scenario like imperative programming languages, theirs works was very inspiring.
--Also thanks to Alberto gigantelli and Ambra Urso for their good ideas and support.

--We start with the parser rhyme:
--A parser for things
--Is a function from strings
--To list of pairs
--Of things and strings

--But in this case we will not work with pairs but with triples instead.
--This because we want to parse a programming language and we need to emulate the computer memory 
--so we create an object that is a list of variable called "Environment".
------------------------------------------------------------------------
-----------------Data structure definition------------------------------
------------------------------------------------------------------------
--Defining the object representing variables:
--[[1]]
--[[1,2,3]]
--[[1,2],[1,2]]
data Variable = Variable {
	name :: String,
	vtype :: String,
	loctype :: String,
	value :: [[Float]] }
	deriving Show
--We have a name, a type, and a value which is a list of list, this in order to manage things like arrays and matrices.
--The inner type in "value" is a float, this because is the most expressive type which allow us to represent numerical values and boolean.

--Defining the environement:
type Env = [Variable]
--Is a simple list of variables.

--The instance of a parser will take in input a function which, starting from the environment and a string will produce a triple made of the environment, 
--the parsed object 'a' and the remaining string to parse.
newtype Parser a = P(Env -> String -> [(Env, a, String)])

--The function "parse" apply the parser given in input to the couple string and environment giving back the results.
parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp
--In practice it apply the function inside the dummy constructor P().

------------------------------------------------------------------------
-----------------Functor-Applicative-Monad definition-------------------
------------------------------------------------------------------------

--Now we need to define instances of parsers for specific tasks, like for example read the first element of a given string.
item :: Parser Char
item = P(\env inp  -> case inp of 
                  []     -> []
                  (x:xs) -> [(env,x,xs)])
--Inside P() we define the anonymous function whic implement the wanted behaviour.

--But there is a problem...
--I can apply the parse on the string just one time, once done this the type of input change (it's more complex) and we cant apply it again.
--We need a kind of strategy in order to apply parsers more time in order to read complex sintactical structures.

--In order to do this we need a monad which allow that kind of behaviour by means of the so called "do notation", 
--a sort of imperative notatation but still remainingin the functional realm.

--First of all we define th parser as a Functor, this allow us to apply a function on the element read by the parser by means of the "fmap" command.
instance Functor Parser where
	-- fmap :: (a -> b) -> Parser a -> Parser b
	fmap g p = P (\env inp -> case parse p env inp of
		[] -> []
		[(env, v,out)] -> [(env, g v, out)])							

--Then we define the parser as an Applicative, this structure allow us to apply the function inside a parser in another parser and to do this in a chain.							  
instance Applicative Parser where
	-- pure :: a -> Parser a
	pure v = P (\env inp -> [(env, v,inp)])
	
	-- <*> :: Parser (a -> b) -> Parser a -> Parser b
	pg <*> px = P (\env inp -> case parse pg env inp of
		[] -> []
		[(env, g,out)] -> parse (fmap g px) env out)
		
--Finally we define the Monad which allow us to apply a function (from simple type to wrapped type, in this case the Parser type) on the result of a parser giving in output another parser.
instance Monad Parser where
	-- (>>=) ::: Parser a -> (a -> Parser b) -> Parser b
	p >>= f = P (\env inp -> case parse p env inp of
		[] -> []
		[(env, v,out)] -> parse (f v) env out)
		
	return a = P (\env inp -> [(env, a,inp)])
	
--Now we define another structure, the Aternative, which allow us to apply, in a sequence of parser, the first which is executed successfully.
--The symbol <|> remind the one used to refer the OR operator in logic.
class Applicative f => Alternative f where
	empty :: f a
	(<|>) :: f a -> f a -> f a
	many :: f a -> f [a]
	some :: f a -> f [a]
	
	many x = some x <|> pure []
	some x = pure (:) <*> x <*> many x
	
instance Alternative Parser where
	-- empty :: Parser a
	empty = P (\env inp -> [])
	-- (<|>) :: Parser a -> Parser a -> Parser a
	p <|> q = P (\env inp -> case parse p env inp of
		[] -> parse q env inp
		[(envout, v,out)] -> [(envout, v,out)])

------------------------------------------------------------------------
-----------------Environment management---------------------------------
------------------------------------------------------------------------
--Search for a specific locator (variable/array/matrix) 
--and give us the whole structure
searchLocation :: Env -> String -> [[Float]]
searchLocation [] queryname = [[]]
searchLocation (x:xs) queryname = if (name x) == queryname 
							      then value x 
								  else searchLocation xs queryname

--Search for the type of a variable
searchVariableType :: Env -> String -> String
searchVariableType [] queryname = "inesistente"
searchVariableType (x:xs) queryname = if (name x) == queryname 
								      then (vtype x) 
									  else searchVariableType xs queryname

--Search for the loc type of a variable (Variabile/Vettore/Matrice)
searchVariableLocType :: Env -> String -> String
searchVariableLocType [] queryname = "inesistente"
searchVariableLocType (x:xs) queryname = if (name x) == queryname 
									     then (loctype x) 
										 else searchVariableLocType xs queryname
										 
--Search for a specific variable and give us the value
searchVariable :: Env -> String -> [[Float]]
searchVariable [] queryname = [[]]
searchVariable (x:xs) queryname = case searchLocation (x:xs) queryname of
	[[]] -> [[]]
	(xs:ys) -> [[(xs:ys)!!0!!0]]

--Search for an array value by j index
searchArrayVariable :: Env -> String -> Int -> [[Float]]
searchArrayVariable [] queryname j = [[]]
searchArrayVariable (x:xs) queryname j = case searchLocation (x:xs) queryname of
	[[]] -> [[]]
	(xs:ys) -> [[(xs:ys)!!0!!j]]

--Search for a matrix value by j k index
searchMatrixVariable :: Env -> String -> Int -> Int -> [[Float]]
searchMatrixVariable [] queryname j k = [[]]
searchMatrixVariable (x:xs) queryname j k = case searchLocation (x:xs) queryname of
	[[]] -> [[]]
	(xs:ys) -> [[(xs:ys)!!j!!k]]
	
--Create a parser  with inside the whole location fetched from the variable
readLocation :: String -> Parser [[Float]]
readLocation name = P (\env input -> case searchLocation env name of
	[[]] -> []
	(xs:ys) -> [(env, xs:ys , input)])		
	
--Create a parser with inside the type fetched from the variable
readVariableType :: String -> Parser String
readVariableType name = P (\env input -> case searchVariableType env name of
	"inesistente" -> []
	xs -> [(env,xs, input)])
	
--Create a parser with inside the location type fetched from the variable
readVariableLocType :: String -> Parser String
readVariableLocType name = P (\env input -> case searchVariableLocType env name of
	"inesistente" -> []
	xs -> [(env,xs, input)])
	
--Create a parser with inside the value fetched from the variable
readVariable :: String -> Parser Float
readVariable name = P (\env input -> case searchVariable env name of
	[[]] -> []
	[[value]] -> [(env,value, input)])

--Create a parser with inside the value fetched from the array in position j
readArrayVariable :: String -> Int -> Parser Float
readArrayVariable name j = P (\env input -> case searchArrayVariable env name j of
	[[]] -> []
	[[value]] -> [(env,value, input)])

--Create a parser with inside the value fetched from the matrix in position j k
readMatrixVariable :: String -> Int -> Int -> Parser Float
readMatrixVariable name j k = P (\env input -> case searchMatrixVariable env name j k of
	[[]] -> []
	[[value]] -> [(env,value, input)])
	
--Utilities for updating variables:
--Return a parser with a modified environment.
updateEnv :: Variable -> Parser String
updateEnv var = P (\env input -> case input of 
	xs -> [((modifyEnv env var),"",xs)])

--Modify an existing variable or adds a new one.
modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x:xs) newVar = if (name x) == (name newVar) then [newVar] ++ xs else [x] ++ modifyEnv xs newVar

--modify the program adding an instruction (a)
modifyProgram :: String -> Parser String
modifyProgram a = P (\env inp -> [(env, "" ,a++inp)])

createArray :: Int -> [Float]
createArray 0 = []
createArray n = (0):(createArray (n-1))

createMatrix :: Int -> Int -> [[Float]]
createMatrix 1 m= [createArray m]
createMatrix n m= (createArray m):(createMatrix (n-1) m)

--Update value in array
updateArray ::[[Float]] -> Int -> Float ->[Float]
updateArray [[]] n s = []
updateArray [(x:xs)] 0 s = s:xs
updateArray [(x:xs)] n s = x:(updateArray [xs] (n-1) s)

--Update value in a matrix
updateMatrix ::[[Float]] -> Int -> Int -> Float ->[[Float]]
updateMatrix [[]] n m s = [[]]
updateMatrix (xs:ys) 0 m s = (updateArray [xs] m s):ys
updateMatrix (xs:ys) n m s = xs:(updateMatrix ys (n-1) m s)

------------------------------------------------------------------------
-----------------Primitive Parsers--------------------------------------
------------------------------------------------------------------------

--Function which return a parser that give out a with value x if p is satisfied or an empty parse otherwise.
--We can see here the do notation in action:
sat :: (Char -> Bool) -> Parser Char
sat p = do { x <- item;
	if p x then return x else empty;}
	
--Now we can apply several parsers for specific condition using differently haskell's boolean function:
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

--This check if the parsed character is a specific character:
char :: Char -> Parser Char
char x = sat (== x)

--This check if the parsed string is a specific string:
string :: String -> Parser String
string [] = return []
string (x:xs) = do {
	char x;
	string xs;
	return (x:xs);
	}

--This parse for a variable identifier, they must began with a lowercase character:
ident :: Parser String
ident = do {
	x <- lower;
	xs <- many alphanum;
	return (x:xs);
	}

--Parse for spaces:
space :: Parser ()
space = do {
	many (sat isSpace);
	return ();
	}

--This parse for boolean giving in output a boolean value:
bools :: Parser Bool
bools = do {
	b <- string "vero";
	return (stringToBool b);
	} <|> do {
	b <- string "falso";
	return (stringToBool b);
	}

--This parse for floats:
unsignfloat :: Parser String
unsignfloat = do {
	xs <- some digit;
	dot <- string ".";
	dec <- some digit;
	return (xs++dot++dec);
	}

minfloat :: Parser String
minfloat = do {
	char '-';
	f <- unsignfloat;
	return ("-"++f);
	}

floatnum :: Parser String
floatnum = unsignfloat <|> minfloat

--For natural and integer number we still consider them float for generalization.
--This parse for natural numbers.
nat :: Parser String
nat = do {
	xs <- some digit;
	return (xs);
	}
	
--Parse for an integer
int :: Parser String
int = do { 
		char '-';
		n <- nat;
		return ("-"++n);
		}<|> nat
	
--Parse for a generic number
num :: Parser String
num = floatnum <|> int

--Convert a float into an int.
toFloat :: String -> Float
toFloat f = read(f)

--Convert a float into an int.
toInt :: Float -> Int
toInt = floor	

--Array location will be read directly in Int for being compatible with 
--the readArrayVariable and readMatrixVariable parsers.
arrayLoc :: Parser Int
arrayLoc = do {
		a <- aexp;
		return (toInt a);
		}

--Check for a token that is a parser preceded and followed by spaces.
token :: Parser a -> Parser a
token p = do { 
	space;
	v <- p;
	space;
	return v;
	}

--Token specialization:
identifier :: Parser String
identifier = token ident
		
number :: Parser String
number = token num

--Token for Specific string
symbol :: String -> Parser String
symbol xs = token (string xs)

--Convert a string representig a boolean value (in italian) into the proper boolean value in Haskell
stringToBool :: String -> Bool
stringToBool "1" = True
stringToBool "0" = False
stringToBool "vero" = True
stringToBool "falso" = False

floatToBool :: Float -> Bool
floatToBool 1 = True
floatToBool 0 = False

boolToFloat :: Bool -> Float
boolToFloat True = 1
boolToFloat False = 0

boolToString :: Bool -> String
boolToString True = "vero"
boolToString False = "falso"

getDecimal :: Float -> Float
getDecimal x = snd (properFraction x)

isInteger :: Float -> Bool
isInteger x = if (getDecimal x)==0 
			  then True
			  else False
	
------------------------------------------------------------------------
-----------------Parser-------------------------------------------------
------------------------------------------------------------------------
--During program parsing is often necessary to read the program without evaluate its meaning.
--E.g. when it have to skip the then part of a if statement or when it need to repeat the instruction inside a while in order to execute them again.
--Are colled "consume" since read the program without execute anything.

--Boolean operator
myAnd :: Parser String
myAnd = symbol "e"<|>symbol "&"

myOr :: Parser String
myOr = symbol "o"<|>symbol "|"

neg :: Parser String
neg = symbol "neg"<|>symbol "!"

greater :: Parser String
greater = symbol "maggiore di"<|>symbol ">"

greaterOrEqual :: Parser String
greaterOrEqual = symbol "maggiore_uguale di"<|>symbol ">="

less :: Parser String
less = symbol "minore di"<|>symbol "<"

lessOrEqual :: Parser String
lessOrEqual = symbol "minore_uguale di"<|>symbol "<="

equal :: Parser String
equal = symbol "uguale a"<|>symbol "=="

notEqual :: Parser String
notEqual = symbol "diverso da"<|>symbol "!="

--Assigment 
assignSymbol :: Parser String
assignSymbol = symbol "="<|>symbol "vale"

--Variable type
varType :: Parser String
varType = symbol "Num"<|>symbol "Bool"	

--Parse for a generic identifier (variabile, array[i] and matrix[i,j])
complIdent :: Parser String
complIdent = do {
			id <- token ident;
			left <- symbol "[";
			j <- consumeAexp;
			comma <- symbol ",";
			k <- consumeAexp;
			right <- symbol "]";
			return (id++left++j++comma++k++right);
		} <|> do {
			id <- token ident;
			left <- symbol "[";
			j <- consumeAexp;
			right <- symbol "]";
			return (id++left++j++right);
		} <|> identifier

--Parse an arithmetic expression
consumeBrkAexp :: Parser String
consumeBrkAexp = do {symbol "(";
			a <- consumeAexp;
			symbol ")";
			return ("("++a++")");
}

consumeValOrVar :: Parser String
consumeValOrVar = consumeBrkAexp<|>complIdent<|>number

consumeAexp :: Parser String
consumeAexp = do {
	do {
        t <- consumeAterm;
		symbol "+";
		a <- consumeAexp;
		return (t ++"+"++ a);
		}
	<|>
	do {
        t <- consumeAterm;
		symbol "-";
		a <- consumeAexp;
		return (t ++"-"++ a);
		}
	<|>
    do{
        consumeAterm;
        }
	}
	
consumeAterm :: Parser String
consumeAterm = do {
	do {
        f <- consumeAfactor;
		symbol "*";
		t <- consumeAterm;
		return (f ++"*"++ t);
		}
	<|> 
	do{
        f <- consumeAfactor;
		symbol "/";
		t <- consumeAterm;
		return (f ++"/"++ t);
		}
	<|>
    do{
	     consumeAfactor;
        }
	}

consumeAfactor :: Parser String
consumeAfactor = consumeValOrVar

--Parser for simplified operation in natural language
incrementa :: Parser String
incrementa  = 
	do{
		do{
			complId <- complIdent;
			symbol "++";
			symbol ";";
			return (complId ++ "=" ++ complId++" +1;");
		}
		<|>
		do{
			token (string "incrementa");
			complId <- complIdent;		
			symbol ";";			
			return (complId ++ "=" ++ complId++" +1;");
		}
}

decrementa :: Parser String
decrementa  = 
	do{
		do{
			complId <- complIdent;
			symbol "--";
			symbol ";";
			return (complId ++ "=" ++ complId++" -1;");
		}
		<|>
		do{
			symbol "decrementa";
			complId <- complIdent;	
			symbol ";";			
			return (complId ++ "=" ++ complId++" -1;");
		}
}

aggiungi :: Parser String
aggiungi =
	do{
		symbol "aggiungi" ;
		n <- consumeValOrVar;
		symbol "a";
		complId <- complIdent;	
		symbol ";";
		return (complId ++ "=" ++ complId++" + "++n++";");
	}

sottrai :: Parser String
sottrai =
	do{
		symbol "sottrai";
		n <- consumeValOrVar;
		token (string "a");
		complId <- complIdent;	
		symbol ";";
		return (complId ++ "=" ++ complId++" - "++n++";");
	}

moltiplica :: Parser String
moltiplica =
	do{
		symbol "moltiplica";
		complId <- complIdent;
		symbol "per";			
		n <- consumeValOrVar;
		symbol ";";
		return (complId ++ "=" ++ complId++" * "++n++";");
	}
		
dividi :: Parser String
dividi =
	do{
		symbol "dividi";
		complId <- complIdent;
		symbol "per";			
		n <- consumeValOrVar;
		symbol ";";
		return (complId ++ "=" ++ complId++" / "++n++";");
	}				
	
simpleMath :: Parser String
simpleMath = incrementa<|>decrementa<|>aggiungi<|>sottrai<|>moltiplica<|>dividi

--This parse for boolean:
strBools :: Parser String
strBools = do {
	b <- symbol "vero";
	return b;
	} <|> do {
	b <- symbol "falso";
	return b;
	}	

--Parse a Boolean expression
consumeBoolValOrVar :: Parser String
consumeBoolValOrVar = strBools<|>complIdent

consumeCompareBexp :: Parser String
consumeCompareBexp = do {
		b0 <- consumeBexp;
		equal;
		b1 <- consumeCompareBexp;
		return (b0 ++" == "++ b1);
}<|>
	do{
		consumeBexp;
	}

consumeBexp :: Parser String
consumeBexp =  do{
		b0 <- consumeBterm;
		myOr;
		b1 <- consumeBexp;
		return (b0 ++" | "++ b1);
    }
	<|>
	do{
		consumeBterm;
	}

consumeBterm :: Parser String
consumeBterm = 	do{
		f0 <- consumeBfactor;
		myAnd;
		f1 <- consumeBterm;
		return (f0 ++" & "++ f1);
    }
	<|>
	do{
		consumeBfactor;
	}

consumeBfactor:: Parser String
consumeBfactor = do{
		symbol "(";
		b <- consumeCompareBexp;
        symbol ")"; 
		return ("("++b++")");
	}<|>do{
		consumeBcomparison;
    }<|>do{	
		consumeBoolValOrVar
	}
	<|>do{
		neg;
		b <- consumeBfactor; 
		return (" !" ++ b);
	}	

consumeBcomparison:: Parser String
consumeBcomparison = do {
		a0 <- consumeAexp;
		less;
		a1 <- consumeAexp;
		return ( a0 ++ "<" ++ a1);
		}
	<|>
	do {
		a0 <-  consumeAexp;
		lessOrEqual;
		a1 <- consumeAexp;
		return ( a0 ++ "<=" ++ a1);
	}
	<|> 
	do{
		a0 <- consumeAexp;
		greater;
		a1 <- consumeAexp;
		return ( a0 ++ ">" ++ a1);
	}
	<|>
	do {
		a0 <- consumeAexp;
		greaterOrEqual;
		a1 <- consumeAexp;
		return ( a0 ++ ">=" ++  a1);
	}
	<|>
	do{
		a0 <- consumeAexp;
		equal;
		a1 <- consumeAexp;
		return ( a0 ++ "==" ++  a1);
	}
	<|>
	do{
		a0 <- consumeAexp;
		notEqual;
		a1 <- consumeAexp;
		return ( a0 ++ "!=" ++  a1);
	}
	<|>
	do{
		a0 <- consumeAexp;
		symbol "divisibile per";
		a1 <- consumeAexp;
		return ( a0 ++ " divisibile per " ++  a1);
	}

--Parse assignment
consumeAexpAssignment :: String -> Parser String
consumeAexpAssignment "Num" = do{
						ae <- consumeAexp;
						return (ae);
						}
consumeAexpAssignment "Bool" =do{
						be <- consumeCompareBexp;
						return (be);
						} 

--initialization
consumeArrayContentComma :: String -> Parser String
consumeArrayContentComma typ =  do {
						  symbol ",";
						  n <- consumeArrayContent typ;
						  return (","++n);
}

consumeArrayContent :: String -> Parser String
consumeArrayContent typ =do{
						n <- consumeAexpAssignment typ;
						pac <- consumeArrayContentComma typ;
						return (n++pac);
}<|>do{
						n <- consumeAexpAssignment typ;						
						return (n);
}


consumeArray :: String -> Parser String
consumeArray typ = do{
			 symbol "[";
			 ac <- consumeArrayContent typ;
			 symbol "]";
			 return ("["++ac++"]")
}

consumeArrayDirectInitialization :: String -> Parser String
consumeArrayDirectInitialization typ = do {
			assignSymbol;
			v <- consumeArray typ;
			symbol ";";
			return ("="++v++";");
}<|>do {
			symbol ";";
			return (";");
}

consumeMatrixContentComma :: String -> Parser String
consumeMatrixContentComma typ=  do {
						  symbol ",";
						  xsys <- consumeMatrixContent typ;
						  return (","++xsys);
}

consumeMatrixContent :: String -> Parser String
consumeMatrixContent typ =do{
						xs <- consumeArray typ;
						pac <- consumeMatrixContentComma typ;
						return (xs++pac);
}<|>do{
						xs <- consumeArray typ;						
						return (xs);
}

consumeMatrix :: String -> Parser String
consumeMatrix typ = do{
			 symbol "[";
			 mc <- consumeMatrixContent typ;
			 symbol "]";
			 return ("["++mc++"]")
}

consumeMatrixDirectInitialization :: String -> Parser String
consumeMatrixDirectInitialization typ = do {
			assignSymbol;
			v <- consumeMatrix typ;
			symbol ";";
			return ("="++v++";");
}<|>do {
			symbol ";";
			return (";");
}

consumeValueDirectInitialization :: String -> Parser String
consumeValueDirectInitialization typ = do {
			assignSymbol;
			v <- consumeAexpAssignment typ;
			symbol ";";
			return ("="++v++";");
}<|>do {
			symbol ";";
			return (";");
}

consumeVarInitialization :: Parser String
consumeVarInitialization = do{ 
				typ <- varType;
				id <- identifier;
				symbol "[";
				i <- consumeAexp;
				symbol ",";
				j <- consumeAexp;
				symbol "]";
				v <- consumeMatrixDirectInitialization typ;
				return(typ++" "++id++"["++i++","++j++"]"++ v);	
}<|>do{			typ <- varType;
				id <- identifier;
				symbol "[";
				i <- consumeAexp;
				symbol "]";				
				v <- consumeArrayDirectInitialization typ;
				return(typ++" "++id++"["++i++"]"++ v);
}<|>do{
				typ <- varType;
				id <- identifier;
				v <- consumeValueDirectInitialization typ;
				return(typ++" "++id++ v);
}

--assignment
--here we cant even know if the assignment is of the correct type because we haven't really create the variable, this check is made during interpretation
consumAssignment :: Parser String
consumAssignment = do{
					ci<-complIdent;
					assignSymbol;
					xp <- consumeCompareBexp;
					symbol ";";
					return(ci++"="++xp++";");					
}<|>do{
					ci<-complIdent;
					assignSymbol;
					xp <- consumeAexp;
					symbol ";";
					return(ci++"="++xp++";");					
}
		
consumeifThenElse :: Parser String
consumeifThenElse = do{ 
				symbol "se";
				symbol "(";
				b <- consumeCompareBexp;
				symbol ")";
				symbol "allora";
				symbol "{";
				p0 <- consumeProgram;
				symbol "}";
				do{
					symbol "altrimenti";
					symbol "{";
					p1 <- consumeProgram;
					symbol "}";
					return (" se (" ++ b ++ ") allora{ " ++ p0 ++ " } " ++ " altrimenti { " ++ p1 ++ " } ");
				}<|>do{
					return (" se (" ++ b ++ ") allora{ " ++ p0 ++ " } ");
					}
				}

consumeWhile :: Parser String
consumeWhile = do{
	symbol "fin_quando";
	symbol "(";
	b <- consumeCompareBexp;
	symbol ")";
	symbol "ripeti";
	symbol "{";
	p <- consumeProgram;
	symbol "}";
	return("fin_quando (" ++ b ++ ") ripeti{" ++ p ++ "}");
}

consumeCommand :: Parser String
consumeCommand = consumeVarInitialization<|>consumAssignment<|>simpleMath<|>consumeifThenElse<|>consumeWhile

consumeProgram :: Parser String
consumeProgram = do{ 
			do{ c <- consumeCommand; 
				p <- consumeProgram;
				return (c ++ p);}<|>do{
			c <- consumeCommand;
			return c;}
		}
------------------------------------------------------------------------
-----------------Interpreter--------------------------------------------
------------------------------------------------------------------------
--Parse a generic identifier (variabile, array[i] and matrix[i,j]) and give out the value inside it.
var :: Parser Float
var =
	do{
		id <- identifier;
		symbol "[";
		j <- arrayLoc;
		symbol ",";
		k <- arrayLoc;
		symbol "]";
		t <- readMatrixVariable id j k;
		return t;
	}<|> 
	do{
		id <- identifier;
		symbol "[";
		j <- arrayLoc;
		symbol "]";
		t <- readArrayVariable id j;
		return t;
	}<|>
	do{
		id <- identifier;
		t <- readVariable id;
		return t;
	}

--Parse for arithmetic expression inside brackets
brkAexp :: Parser Float
brkAexp = do {symbol "(";
			a <- aexp;
			symbol ")";
			return (a);
}

--Parse a number and convert it into Float
numberFloat :: Parser Float
numberFloat = do {
        f <- number;
		return (read f);
		}

--Parser for arithmetic expression 
aexp :: Parser Float
aexp = do {
	do {
        t <- aterm;
		symbol "+";
		a <- aexp;
		return (t + a);
		}
	<|>
	do {
        t <- aterm;
		symbol "-";
		a <- aexp;
		return (t - a);
		}
	<|>
    do{
        aterm;
        }
	}
	
aterm :: Parser Float
aterm = do {
	do {
        f <- afactor;
		symbol "*";
		t <- aterm;
		return (f * t);
		}
	<|> 
	do{
        f <- afactor;
		symbol "/";
		t <- aterm;
		return (f / t);
		}
	<|>
    do{
	     afactor;
        }
	}

afactor :: Parser Float
afactor = brkAexp<|>var<|>numberFloat

performSimpleMath :: Parser String
performSimpleMath = do {
					instr <- simpleMath;
					modifyProgram (instr);
}

--Boolean Expression and comparison:
boolVar :: Parser Bool
boolVar =
	do{
		id <- identifier;
		symbol "[";
		j <- arrayLoc;
		symbol ",";
		k <- arrayLoc;
		symbol "]";
		b <- readMatrixVariable id j k;
		return (floatToBool b);
	}<|> 
	do{
		id <- identifier;
		symbol "[";
		j <- arrayLoc;
		symbol "]";
		b <- readArrayVariable id j;
		return (floatToBool b);
	}<|>
	do{
		id <- identifier;
		b <- readVariable id;
		return (floatToBool b);
	}

boolValOrVar :: Parser Bool
boolValOrVar = bools<|>boolVar

compareBexp :: Parser Bool
compareBexp = do{
		b0 <- bexp;
		equal;
		b1 <- compareBexp;
		return (b0 == b1);
    }<|>bexp;


bexp :: Parser Bool
bexp = do{
	do{
		b0 <- bterm;
		myOr;
		b1 <- bexp;
		return (b0 || b1);
    }
	<|>
	do{
		bterm;
	}
}

bterm :: Parser Bool
bterm = do{
	do{
		f0 <- bfactor;
		myAnd;
		f1 <- bterm;
		return (f0 && f1);
    }
	<|>
	do{
		bfactor;
	}
}

bfactor:: Parser Bool
bfactor = 
	do{
		symbol "(";
		b <- compareBexp;
        symbol ")"; 
		return b;
	}
	<|>
	do{
		bcomparison;
    }
	<|>
	boolValOrVar
	<|>
	do{
		neg;
		b <- bfactor; 
		return (not b);
	}		

bcomparison:: Parser Bool
bcomparison = do{
	do {
		a0 <- aexp;
		less;
		a1 <- aexp;
		return ( a0 < a1);
		}
	<|>
	do {
		a0 <-  aexp;
		lessOrEqual;
		a1 <- aexp;
		return ( a0 <= a1);
	}
	<|> 
	do{
		a0 <- aexp;
		greater;
		a1 <- aexp;
		return ( a0 > a1);
	}
	<|>
	do {
		a0 <- aexp;
		greaterOrEqual;
		a1 <- aexp;
		return ( a0 >= a1);
	}
	<|>
	do{
		a0 <- aexp;
		equal;
		a1 <- aexp;
		return ( a0 == a1);
	}
	<|>
	do{
		a0 <- aexp;
		notEqual;
		a1 <- aexp;
		return ( a0 /= a1);
	}<|>
	do{
		a0 <- aexp;
		symbol "divisibile per";
		a1 <- aexp;
		return ( (isInteger a0)&&(isInteger a1)&&((mod (toInt a0) (toInt a1)) == 0));
	}
}

fltBool :: Parser Float
fltBool = do {
	b <- compareBexp;
	return (boolToFloat b);
	}


aexpAssignment :: String -> Parser Float
aexpAssignment "Num" = do{
						ae <- aexp;
						return (ae);
} 
aexpAssignment "Bool" = do{
						ae <- fltBool;
						return (ae);
} 
aexpAssignment _ = empty

evaluateArrayContentComma :: String -> Int -> Parser [Float]
evaluateArrayContentComma typ i =  do {
						  symbol ",";
						  n <- evaluateArrayContent typ (i);
						  return (n);
}

evaluateArrayContent :: String -> Int -> Parser [Float]
evaluateArrayContent typ 1 = do{
						n <- aexpAssignment typ;			
						return ([n]);
}
evaluateArrayContent typ i = do{
						n <- aexpAssignment typ;
						pac <- evaluateArrayContentComma typ (i-1);
						return (n:pac);
}

evaluateArray :: String -> Int -> Parser [Float]
evaluateArray typ i = do{
			 symbol "[";
			 ac <- evaluateArrayContent typ i;
			 symbol "]";
			 return (ac)
}

arrayDirectInitialization :: String -> Int -> Parser [[Float]]
arrayDirectInitialization typ i = do {
			assignSymbol;
			v <- evaluateArray typ i;
			symbol ";";
			return [v];
}<|>do {
			symbol ";";
			return [createArray i];
}

evaluateMatrixContentComma :: String -> Int -> Int -> Parser [[Float]]
evaluateMatrixContentComma typ i j =  do {
						  symbol ",";
						  xsys <- evaluateMatrixContent typ (i) j;
						  return (xsys);
}

evaluateMatrixContent :: String -> Int -> Int -> Parser [[Float]]
evaluateMatrixContent typ 1 j = do{
						xs <- evaluateArray typ j;					
						return ([xs]);
}
evaluateMatrixContent typ i j = do{
						xs <- evaluateArray typ j;
						pac <- evaluateMatrixContentComma typ (i-1) j;
						return (xs:pac);
}

evaluateMatrix :: String -> Int -> Int -> Parser [[Float]]
evaluateMatrix typ i j = do{
			 symbol "[";
			 mc <- evaluateMatrixContent typ i j;
			 symbol "]";
			 return (mc)
}

matrixDirectInitialization :: String -> Int -> Int -> Parser [[Float]]
matrixDirectInitialization typ i j = do {
			assignSymbol;
			v <- evaluateMatrix typ i j;
			symbol ";";
			return v;
}<|>do {
			symbol ";";
			return (createMatrix i j);
}

valueDirectInitialization :: String -> Parser [[Float]]
valueDirectInitialization typ = do {
			assignSymbol;
			v <- aexpAssignment typ;
			symbol ";";
			return [[v]];
}<|>do {
			symbol ";";
			return [[0]];
}

varInitialization :: Parser String
varInitialization = do{
				typ <- varType;
				id <- identifier;
				symbol "[";
				i <- arrayLoc;
				symbol ",";
				j <- arrayLoc;
				symbol "]";
				v <- matrixDirectInitialization typ i j;
				updateEnv Variable{name=id,vtype=typ, loctype="Matrice",  value= v};
}<|>do{
				typ <- varType;
				id <- identifier;
				symbol "[";
				i <- arrayLoc;
				symbol "]";				
				v <- arrayDirectInitialization typ i;
				updateEnv Variable{name=id,vtype=typ, loctype="Vettore",  value= v};
}<|>do{
				typ <- varType;
				id <- identifier;
				v <- valueDirectInitialization typ;
				updateEnv Variable{name=id,vtype=typ, loctype="Variable",  value= v};
}

assignment :: Parser String
assignment = do{
				id <- identifier;
				symbol "[";
				i <- arrayLoc;
				symbol ",";
				j <- arrayLoc;
				symbol "]";
				assignSymbol;
				typ <- readVariableType id;
				v <- aexpAssignment typ;
				symbol ";";
				m <- readLocation id;				
				lt <- readVariableLocType id;
				updateEnv Variable{name=id,vtype=typ, loctype=lt, value= (updateMatrix m i j v)};
}<|>do{
				id <- identifier;
				symbol "[";
				i <- arrayLoc;
				symbol "]";
				assignSymbol;
				typ <- readVariableType id;
				v <- aexpAssignment typ;
				symbol ";";
				a <- readLocation id;
				lt <- readVariableLocType id;
				updateEnv Variable{name=id,vtype=typ, loctype=lt, value= [(updateArray a i v)]};
}<|>do{
				id <- identifier;				
				assignSymbol;
				typ <- readVariableType id;
				v <- aexpAssignment typ;
				symbol ";";
				lt <- readVariableLocType id;
				updateEnv Variable{name=id,vtype=typ, loctype=lt, value= [[v]]};
}

ifThenElse :: Parser String
ifThenElse = do{ symbol "se";
				symbol "(";
				b <- compareBexp;
				symbol ")";
				symbol "allora";
				symbol "{";
				if (b) then do{
					program;
					symbol "}";
					do{
						symbol "altrimenti";
						symbol "{";
						consumeProgram;
						symbol "}";
						return "";}<|>do{
						return "";}
					}
				else do{ consumeProgram;
						 symbol "}";
						 do{
							 symbol "altrimenti";
							 symbol "{";
							 program;
							 symbol "}";
							 return "";
						 }<|>do{
							 return "";
						 }
				}
			}
-------------------------------------			
----old implementation of while------
-------------------------------------
-- while :: Parser String
-- while = do {
	-- w <- consumeWhile;
	-- modifyProgram w;
	-- symbol "fin_quando";
	-- b <- bexp;
	-- symbol "ripeti";
	-- symbol "{";
	-- if (b) then do{
		-- program;
		-- symbol "}";
		-- modifyProgram w;
		-- while;
	-- } else do{
		-- consumeProgram;
		-- symbol "}";
		-- return "";
	-- }
-- }
-------------------------------------
-------------------------------------
-------------------------------------

executeWhile :: String -> String -> Parser String
executeWhile p b = do {
	modifyProgram (b++" ");
	bv <- compareBexp;
	if (bv) then do{
	    --I add the character '}' in order to not read the whole program.
		modifyProgram (p++"}");
		program;
		symbol "}";
		executeWhile p b;
	} else do{
		return "";
	}
}

while :: Parser String
while = do {
	symbol "fin_quando";
	symbol "(";
	b <- consumeCompareBexp;
	symbol ")";
	symbol "ripeti";
	symbol "{";
	p <- consumeProgram;
	symbol "}";	
	executeWhile p b;
	return "";
}

command :: Parser String
command = varInitialization<|>assignment<|>performSimpleMath<|>ifThenElse<|>while

program :: Parser String
program = 
 do {
  command;
  program;
 }
 <|>
 command
---------------------------------------------------------------------------------------
-----------------------------------GUI Implementation----------------------------------
---------------------------------------------------------------------------------------
logo :: IO String
logo = do {              
 putStrLn "................(@@@@@@&................";
 putStrLn "...............@@@.@@@@*@@..............";
 putStrLn "...............%@@////(@@@..............";
 putStrLn "................%@@(//&@&%..............";
 putStrLn "..............&@         @@.............";
 putStrLn ".............&@           @@............";
 putStrLn "............@@             @@...........";
 putStrLn "...........&@%            .@@@..........";
 putStrLn "...........@@*            .%@@..........";
 putStrLn "..............,,.    ....,@&&%..........";
 putStrLn ". ........ ..*****(**.******......  ....";
 putStrLn ",------. ,--.,--.  ,--. ,----.   ,--. ,--.";
 putStrLn "|  .--. '`--'|  ,'.|  |'  .-./   |  | |  |";
 putStrLn "|  '--' |,--.|  |' '  ||  | .---.|  | |  |";
 putStrLn "|  | --' |  ||  | `   |'  '--'  |'  '-'  '";
 putStrLn "`--'     `--'`--'  `--' `------'  `-----'";
 putStrLn "Programming in Natural lanGUage.";
 putStrLn "Un linguaggio di programmazione di Antonio Raffaele Iacovazzi.";
 putStrLn "";
 putStrLn "Comandi:";
 putStrLn "'esci': per uscire da PiNGU.";
 putStrLn "'stampa': per verificare il contenuto di una variabile.";
 putStrLn "'file': per caricare un file (estensione.pingu) da eseguire.";
 putStrLn "'pulisci': per pulire la memoria.";
 putStrLn "";
 putStrLn "Inserisci una riga di codice, o un comando.";
 menu [];
}

getCode :: [(Env, String, String)] -> String
getCode [(_, parsedString, _)] = parsedString
getCode [] = ""

showCode :: [(Env, String, String)] -> String
-- The entire input string has been consumed
showCode [(_, parsedString, "")] =
 "Ottimo! Codice completamente eseguibile:\n\n" ++ parsedString ++ "\n"++"Procedo con l'elaborazione.\n\n"
-- The input string has not been completely consumed
showCode [(_, parsedString, notParsedString)] = "Codice da eseguire: \n\n" ++ parsedString ++ "\n\n" ++ "Errore, codice non eseguibile per problemi di sintassi: '" ++ notParsedString ++ "'\n"
--Total failure
showCode [] = "Errore sintattico! Impossibile elaborare il programma.\n"

printValueByType :: Float -> String -> String
printValueByType v "Bool" = show(boolToString (floatToBool(v)))
printValueByType v "Num" = show(v)

printLocation :: [[Float]] -> String -> String
printLocation [] _ = "Variabile inesistente"
printLocation [[]] _ = ""
printLocation [[x]] typ = (printValueByType x typ)
printLocation [x:xs] typ = (printValueByType x typ) ++ "," ++ (printLocation [xs] typ)
printLocation (xs:ys) typ = (printLocation [xs] typ) ++ "\n" ++(printLocation ys typ)

printVariable :: Env -> String -> String
printVariable env n = printLocation (searchLocation env n)(searchVariableType env n)

printEnv :: Env -> String
printEnv [] = ""
printEnv (x:xs) = (loctype x)++" "++(name x)++":\n"++(printVariable (x:xs) (name x))++"\n"++(printEnv xs)

showMem :: [(Env, String, String)] -> String
showMem [(env, _, _)] = printEnv env
showMem [] = "Errore durante l'esecuzione del codice, forse hai fatto riferimento a una variabile non inizializzata?\nIn alternativa potresti aver associato a una variabile un valore non coerente con il suo tipo.\n"

saveMem :: [(Env, String, String)] -> Env
saveMem [(env, _, _)] = env
saveMem [] = []

menu :: Env -> IO String
menu env = do {
 putStr "PiNGU> ";
 hFlush stdout;
 input <- getLine;
 case input of
	"esci"   -> return "Ciao! Ti aspetto per programmare ancora insieme :)"
	"pulisci"-> menu []
	"stampa" -> do
				putStrLn "Inserisci il nome della variabile da stampare:"
				var <- getLine
				putStrLn (printVariable env var)
				menu env
	"file"   -> do
				putStrLn "Inserisci il nome del file .pingu da interpretare:"
				fn <- getLine;
				fileContent <- readFile fn
				putStrLn "Il contenuto del file è:"
				putStrLn fileContent
				let pgrm = parse consumeProgram env fileContent
				putStrLn (showCode pgrm)
				let pgrmCode = getCode pgrm
				let mem = parse program env pgrmCode
				putStrLn (showMem mem)
				menu (saveMem mem)
	_        -> do
				let pgrm = parse consumeProgram env input
				putStrLn (showCode pgrm)
				let pgrmCode = getCode pgrm
				let mem = parse program env pgrmCode
				putStrLn (showMem mem)
				menu (saveMem mem)
}

main = logo;