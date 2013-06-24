----------------------------------------------------------------------
--     Another Lua Parser And Compiler Compiler Attempt :           --
--A simple set of parsing combinators for recursive descent parsing.--
----------------------------------------------------------------------

local Alpacca = {}
Alpacca.__index = Alpacca

--return setmetatable(returner, Alpacca)

-- this function marks a parser as being a token.
-- Tokens are like any other parser, except they
-- invoke the whitespace parser before they parse
-- the token, and several standard walkers/actions
-- treat them specially

function Alpacca:token()
   self.isToken = true
   return self
end

-- this marks a parser as being both a token and an operator
-- the distinction mostly matters during creation of parse
-- trees

function Alpacca:operator(arity,assoc)
   self.isToken    = true
   self.isOperator = true
   self.arity      = arity
   self.assoc      = assoc
   return self
end

-- marks a parser as being groupers like parenthesis

function Alpacca:openParen()
   self.isToken = true
   self.isOperator = true
   self.isOpenParen = true
   self.arity = 0
   self.assoc = "none"
   return self
end

function Alpacca:closeParen()
   self.isToken = true
   self.isOperator = true
   self.isCloseParen = true
   self.arity = 0
   self.assoc = "none"
   return self
end



-- this walks the parser and sets the priority of
-- the operators.

function Alpacca:wp(n)
   if self.isOperator then
      self.precedence = n
   end
   n=n+1
   
   if self.lhs then self.lhs:wp(n) end
   if self.rhs then self.rhs:wp(n) end
end

function Alpacca:calculateOperatorPrecedence()
   self:wp(0)
end

--------------------
-- event handling --
--------------------

Alpacca.events = {}
Alpacca.events.ENT = true
Alpacca.events.LHS = true
Alpacca.events.RHS = true
Alpacca.events.RTN = true
Alpacca.events.FAIL = true


function Alpacca:fireEvent(event,l,s,p,walker,...)
   local returner
   if walker then
      returner = walker:fireEvent(event,l,s,p,self,walker,...)
   end
   if self.action and self.action[event] then
      return self.action[event](self.action,l,s,p,self,walker,...)
   end
end

--  for handling whitespace in front of Tokens.  
--- this is mostly for convenience sake: we could just define
--- all tokens as being preceeded by a whitespace parser.  

function Alpacca:accountForWhiteSpace(s,p)
   local success
   if Alpacca.whiteSpaceParser and self.isToken then 
      success,s,p = Alpacca.whiteSpaceParser:pass(s,p)
   end
   return s,p
end



-------------------------------------------------
-- attaching an action to part of a parse tree --
-------------------------------------------------

-- for actions/captures on rules
function Alpacca:__call (f)
   self.action = f
   if self.action.install then
      self.action:install(self)
   end
   return self
end


-------------------------
-- parsing combinators --
-------------------------

-- this one does nothing.  Useful for overridable defaults and for
-- rounding out combinators like (_ * _) which should be unary

function Alpacca.noOp()
   local returner = {}
   returner.op = "noOp"
   
   function returner:pass(s,p,walker)
      local l = {}
      self:fireEvent("ENT",l,s,p,walker)
      return true,s,p,self:fireEvent("RTN",l,s,p,walker)
   end
   
   return setmetatable(returner, Alpacca)
end


-- to be used in conjunction with the ^ operator
-- to indicate which tokens are operators and what
-- their asssociativity is
function Alpacca.leftAssociative()
   local returner = {}
   returner.op = "noOp"
   
   function returner:pass(s,p,walker)
      local l = {}
      self:fireEvent("ENT",l,s,p,walker)
      return true,s,p,self:fireEvent("RTN",l,s,p,walker)
   end
   
   return setmetatable(returner, Alpacca)
end


function Alpacca.rightAssociative()
   local returner = {}
   returner.op = "noOp"
   
   function returner:pass(s,p,walker)
      local l = {}
      self:fireEvent("ENT",l,s,p,walker)
      return true,s,p,self:fireEvent("RTN",l,s,p,walker)
   end
   
   return setmetatable(returner, Alpacca)
end



-- used for resolving forward declarations
function Alpacca.forward()
   local returner = {}
   returner.op = "fwd"
   
   function returner:setParser(fwdParser)
      self.fwdParser = fwdParser
   end
   
   function returner:pass(s,p,walker)
      return self.fwdParser:pass(s,p,walker)
   end
   
   return setmetatable(returner, Alpacca)
end


function Alpacca.literal(str)
   local returner = {}
   returner.op     =  "literal"
   returner.value  =  str
   returner.delta  =  string.len(str)-1
   
   function returner:pass(s,p,walker)
      s,p = self:accountForWhiteSpace(s,p)
      local l = {}
      self:fireEvent("ENT",l,s,p,walker)
      
      local newEnd = p+self.delta
      if string.byte(s,p,newEnd) 
	 == 
      string.byte(self.value,1,-1) then
	 return true,s,newEnd+1,
	 self:fireEvent("RTN",l,s,newEnd+1,walker)
      else
	 self:fireEvent("FAIL",l,s,p,walker)
	 return false,s,p
      end
   end
   
   return setmetatable(returner, Alpacca)
end

-- This one is used for all the character parsers, we just
-- pass in the apropos function predicate to select the correct characte
function Alpacca.programmableChar (F)
   local returner = {
      op = "progChar",
      pass=function (self, s,p, walker)
	 local a
	 s,p = self:accountForWhiteSpace(s,p)
	 local l={}
	 self:fireEvent("ENT",l,s,p,walker)
	 
	 local sb=string.byte(s,p,p)
	 if sb and F(sb) then
	    
	    return true,s,p+1,
	    self:fireEvent("RTN",l,s,p+1,walker)
	 else
	    self:fireEvent("FAIL",l,s,p,walker)
	    return false,s,p
	 end
      end
   }
   
   return setmetatable(returner, Alpacca)
end


-- accept any char
function Alpacca.makeAnyCharTester()
   return function (ch)
      return true
   end
end

function Alpacca.anyChar () 
   return Alpacca.programmableChar(Alpacca.makeAnyCharTester())
end



-- tests whether the character is a numeric digit
function Alpacca.makeDigitTester()
   local a_0 = string.byte("0",1,1)
   local a_9 = string.byte("9",1,1)
   
   return function (ch)
      return (a_0 <= ch and ch <= a_9) 
   end
end

function Alpacca.digitChar ()
   return Alpacca.programmableChar(Alpacca.makeDigitTester())
end


-- tests whether the character is a alpha character or not
function Alpacca.makeAlphaTester()
   local a_a = string.byte("a",1,1)
   local a_z = string.byte("z",1,1)
   local a_A = string.byte("A",1,1)
   local a_Z = string.byte("Z",1,1)
   
   return function (ch)
      return ((a_a <= ch and ch <= a_z) or (a_A <= ch and ch <= a_Z))
   end
end

function Alpacca.alphaChar()
   return  Alpacca.programmableChar(Alpacca.makeAlphaTester())
end


-- tests whether the character is a alphanumeric or not
function Alpacca.makeAlphaNumericTester()
   local a_a = string.byte("a",1,1)
   local a_z = string.byte("z",1,1)
   local a_A = string.byte("A",1,1)
   local a_Z = string.byte("Z",1,1)
   local a_0 = string.byte("0",1,1)
   local a_9 = string.byte("9",1,1)
   
   return function (ch)
      return ((a_a <= ch and ch <= a_z) 
		 or (a_A <= ch and ch <= a_Z)
		 or (a_0 <= ch and ch <= a_9))
   end
end

function Alpacca.alphaNumericChar()
   return Alpacca.programmableChar(Alpacca.makeAlphaNumericTester())
end


-- sequential operator. Just glues two parsers together into one parser

function Alpacca.__concat (lhs,rhs)
   local returner = {
      op = "seq",
      pass=function (self, s,p, walker)
	 local succeed,a
	 s,p = self:accountForWhiteSpace(s,p)
	 local l={}
	 self:fireEvent("ENT",l,s,p,walker)
	 
	 local saveP = p
	 
	 succeed,s,p,a = lhs:pass(s,p,walker)
	 if not succeed then 
	    self:fireEvent("FAIL",l,s,saveP,walker)
	    return false,s,saveP
	 end
	 self:fireEvent("LHS",l,s,p,walker,a)
	 
	 succeed,s,p,a  = rhs:pass(s,p,walker)
	 if not succeed then 
	    self:fireEvent("FAIL",l,s,saveP,walker)
	    return false,s,saveP
	 end
	 self:fireEvent("RHS",l,s,p,walker,a)
	 
	 return true,s,p, self:fireEvent("RTN",l,s,p,walker)
      end
   }   
   
   -- set up tree structure
   returner.lhs=lhs
   lhs.parent = returner
   
   returner.rhs=rhs
   rhs.parent = returner
   
   return setmetatable(returner, Alpacca)
end

function Alpacca.__pow (lhs,rhs)
   local returner = {
      op = "pow",
      pass=function (self, s,p, walker)
	 local succeed,a
	 s,p = self:accountForWhiteSpace(s,p)
	 local l={}
	 self:fireEvent("ENT",l,s,p,walker)
	 
	 local saveP = p
	 
	 succeed,s,p,a = lhs:pass(s,p,walker)
	 if not succeed then 
	    self:fireEvent("FAIL",l,s,saveP,walker)
	    return false,s,saveP
	 end
	 self:fireEvent("LHS",l,s,p,walker,a)
	 
	 succeed,s,p,a  = rhs:pass(s,p,walker)
	 if not succeed then 
	    self:fireEvent("FAIL",l,s,saveP,walker)
	    return false,s,saveP
	 end
	 self:fireEvent("RHS",l,s,p,walker,a)
	 
	 return true,s,p, self:fireEvent("RTN",l,s,p,walker)
      end
   }   
   
   -- set up tree structure
   returner.lhs=lhs
   lhs.parent = returner
   
   returner.rhs=rhs
   rhs.parent = returner
   
   return setmetatable(returner, Alpacca)
end


-- Kleene plus operator
function Alpacca.__add (lhs,rhs)
   local returner = {
      op = "plus",
      pass=function (self, s,p, walker)
	 s,p = self:accountForWhiteSpace(s,p)
	 local saveP = p
	 local succeed,a
	 
	 local l={}
	 self:fireEvent("ENT",l,s,p,walker)
	 succeed,s,p,a = lhs:pass(s,p,walker)
	 
	 if not succeed then
	    self:fireEvent("FAIL",l,s,p,walker)
	    return false,s,p
	 end
	 while succeed do
	    self:fireEvent("LHS",l,s,p,walker,a)
	    succeed,s,p,a  = lhs:pass(s,p,walker)
	 end
	 
	 succeed,s,p,a = rhs:pass(s,p,walker)
	 if succeed then 
	    self:fireEvent("RHS",l,s,p,walker,a)
	    return succeed,s,p,self:fireEvent("RTN",l,s,p,walker)
	 end
	 
	 self:fireEvent("FAIL",l,s,saveP,walker)
	 return false,s,saveP
      end
   }
   
   --set up tree structure
   returner.lhs=lhs
   lhs.parent = returner
   
   returner.rhs=rhs
   rhs.parent = returner
   
   return setmetatable(returner, Alpacca)
end


-- we overload the * operator for the kleene-star construct.
function Alpacca.__mul (lhs,rhs)
   local returner = {
      op="star",
      pass=function (self, s,p, f)
	 s,p = self:accountForWhiteSpace(s,p)
	 local succeed,a
	 local saveP = p
	 
	 local l={}
	 self:fireEvent("ENT",l,s,p,f)
	 
	 succeed,s,p,a = lhs:pass(s,p,f)
	 while succeed do
	    self:fireEvent("LHS",l,s,p,f,a)
	    succeed,s,p,a = lhs:pass(s,p,f)
	 end
	 
	 succeed,s,p,a = rhs:pass(s,p,f)
	 if succeed then
	    self:fireEvent("RHS",l,s,p,f,a)
	    return succeed,s,p,self:fireEvent("RTN",l,s,p,f)
	 end
	 return false,s,saveP
      end
   }
   
   --now link up the parsers into the tree
   returner.lhs=lhs
   lhs.parent = returner
   
   returner.rhs=rhs
   rhs.parent = returner
   
   return setmetatable(returner, Alpacca)
end


-- nondeterministic choice, basically or.
function Alpacca.__div (lhs,rhs)
   local returner = {
      op = "div",
      
      pass = function (self, s,p, f)
	 local succeed,a
	 s,p = self:accountForWhiteSpace(s,p)
	 
	 local l={}
	 l.tryLHS=true
	 l.tryRHS=true
	 
	 self:fireEvent("ENT",l,s,p,f)
	 
	 if l and l.tryLHS then
	    succeed, s,p,a  = lhs:pass(s,p,f)
	    if succeed then
	       self:fireEvent("LHS",l,s,p,f,a)
	       --	       print("divLevel=" .. f.divLevel .. " bt:1")
	       return succeed,s,p, self:fireEvent("RTN",l,s,p,f)
	    end
	 end
	 
	 if l and l.tryRHS then
	    succeed,s,p,a = rhs:pass(s,p,f)
	    if succeed then
	       self:fireEvent("RHS",l,s,p,f,a)
--	       print("divLevel=" .. f.divLevel .. " bt:2")
	       return succeed,s,p, self:fireEvent("RTN",l,s,p,f)
	    end
	 end
---	 print("divLevel=" .. f.divLevel .. " bt:3")
	 
	 self:fireEvent("FAIL",l,s,p,f)
	 return false,s,p 
      end
   }
   
   --set up tree structure
   returner.lhs=lhs
   lhs.parent = returner
   
   returner.rhs=rhs
   rhs.parent = returner
   
   return setmetatable(returner, Alpacca)
end

--the default whitespace parser
Alpacca.whiteSpaceParser = (( Alpacca.literal(" ")
			    / Alpacca.literal("\t") 
			    / Alpacca.literal("\n") 
			    / Alpacca.literal("\r")
                            )*       Alpacca.noOp())


------------------------------------ 

function Alpacca:printParseTree(t)
   print("printing parse tree")
   Alpacca:printParseTree1(0,t)
   print("done printing parse tree")
end

function Alpacca:printParseTree1(I,a)
   local tabString=""
   for i=1,I do
      tabString = tabString .. " "
   end
   
   if a and a.rhs then
      self:printParseTree1(I+7,a.rhs)
   end
   
   if a and a.op and a.value then print(tabString..a.op.." : "..a.value) end
   if a and a.op and not a.value then print(tabString..a.op) end
   
   if a and a.lhs then
      if a.lhs.kids then
	 for i,v in ipairs(a.lhs.kids) do
	    self:printParseTree1(I+7,v)
	 end
      else
	 self:printParseTree1(I+7,a.lhs)
      end
   end
end




----------------------
-- Standard Actions --
----------------------

function Alpacca.parseTreeGenerator(rtn)
   local returner={}
   
   returner.output = rtn
   
   returner.installed=false
   
   returner.entCount=1
   
   returner.tokenStack={}
   returner.tokenStackIndex=0
   
   returner.operatorStack={}
   returner.operatorStackIndex=0
   
   function returner:pushTokenStack(t)
      self.tokenStackIndex=self.tokenStackIndex+1
      self.tokenStack[self.tokenStackIndex] = t
   end
   
   function returner:topTokenStack()
      return self.tokenStack[self.tokenStackIndex]
   end
   
   function returner:popTokenStack()
      self.tokenStackIndex = self.tokenStackIndex -1
   end
   
   function returner:dumpTokenStack()
      for i,v in ipairs(self.tokenStack) do
	 print(i..":"..v.token)
      end
   end
   
   function returner:pushOperatorStack(t)
      self.operatorStackIndex=self.operatorStackIndex+1
      self.operatorStack[self.operatorStackIndex] = t
   end
   
   function returner:topOperatorStack()
      return self.operatorStack[self.operatorStackIndex]
   end
   
   function returner:popOperatorStack()
      self.operatorStackIndex = self.operatorStackIndex -1
   end
   
   function returner:isOperatorStackEmpty()
      return 0==self.operatorStackIndex
   end
   
   function returner:dumpOperatorStack()
      for i,v in ipairs(self.operatorStack) do
	 print(i.." :"..v.token..
		  " arity="..v.parser.arity..
		  " assoc="..v.parser.assoc..
	          " prec ="..v.parser.precedence)
      end
   end
   
   function returner:dumpEntry(entry)
      print("entry: " .. entry.token)
   end
   
   function returner:handleToken(tokenString,tokenParser)
      -- follows http://csis.pace.edu/~wolf/CS122/infix-postfix.htm
      
      -- data structure used to hold token info
      local entry = {
	 token=tokenString,
	 parser=tokenParser
      }
      
      -- non-operators go straight to output stack
      if not tokenParser.isOperator then
	 self:pushTokenStack(entry)
	 self:dumpEntry(entry)
	 print("r1")
	 return
      end
      
      -- If the operand stack is empty, the operand
      -- goes directly to the operand stack
      if self:isOperatorStackEmpty() then
	 self:pushOperatorStack(entry)
	 self:dumpEntry(entry)
	 print("r2")
	 return
      end
      
      -- if the incomming operator is a right paren
      -- then pop the stack (and push onto output stack)
      -- until you find a left paren.  Pop that paren
      -- and discard both parens
      if entry.parser.isCloseParen then
	 while (not self:isOperatorStackEmpty()
		   and 
		not self:topOperatorStack().parser.isOpenParen) do
	    self:pushTokenStack(self:topOperatorStack())
	    self:popOperatorStack()
	 end
	 if not self:isOperatorStackEmpty() then
	    self:popOperatorStack() -- get rid of lparen
	 end
	 self:dumpEntry(entry)
	 print("r4")
	 return
      end
      
      -- if either the top operator on the stack, OR the 
      -- incomming operator is a left paren, push the
      -- incomming operator on the stack
      if (self:topOperatorStack().parser.isOpenParen
	     or
	  entry.parser.isOpenParen) then
	 self:pushOperatorStack(entry)
	 self:dumpEntry(entry)
	 print("r3")
	 return
      end
      
      -- if the incomming operator has higher precidence 
      -- than the operator on the top of the operator 
      -- stack, then push it on the stack
      if(entry.parser.precedence
	 > self:topOperatorStack().parser.precedence) then
	 self:pushOperatorStack(entry)
	 self:dumpEntry(entry)
	 print("r5")
	 return
      end
      
      -- if incomming operator has equal precedence 
      -- with top of the stack, then use associativity.  


      if entry.parser.precedence
	 == self:topOperatorStack().parser.precedence then
      
	    --if right associative, pop top of stack and 
	    -- send to output stack
	    if "right"==entry.parser.assoc then
	       self:pushTokenStack(self:topOperatorStack())
	       self:popOperatorStack()
	       self:pushOperatorStack(entry)
	       self:dumpEntry(entry)
	       print("r6")
	       return
	    end
      
	    -- if left associative then just push onto
	    -- the top of the stack
	    if "left" ==entry.parser.assoc then
	       self:pushOperatorStack(entry)
	       self:dumpEntry(entry)
	       print("r6")
	       return
	    end
      end
      
      -- if incomming symbols has lower precedence than the 
      -- top of stack, move top of operator stack to output
      -- stack, and rerun this whole shebang.
      if(entry.parser.precedence
	 < self:topOperatorStack().parser.precedence) then
	 self:pushTokenStack(self:topOperatorStack())
	 self:popOperatorStack()
	 self:handleToken(tokenString, tokenParser)
      end
   end
   
   function returner:createParseTreeFromStack()
      print("assembling parse tree")
      for i,entry in ipairs(self.tokenStack) do
	 if not entry.parser.isOperator then
	    local node = {
	       op = "token",
	       value = entry.token
	    }
	    self:pushOperatorStack(node)
	 else
	    local node = {
	       op = entry.token
	    }
	    print("pulled op")
	    Alpacca:printParseTree(node)
	    for a = 1,entry.parser.arity do
	       local arg = self:topOperatorStack()
	       self:popOperatorStack()
	       
	       
	       if not node.rhs then 
		  node.rhs = arg
	       else
		  node.lhs = arg
	       end
	    end
	    self:pushOperatorStack(node)
	 end
      end
      self.output.value = self:topOperatorStack()
   end
   
   function returner:ENT(l,s,p,parser,walker)
      if parser.isToken then
	 l.saveP = p
      end
      
      if parser.installedAction == self then 
	 self.entCount = self.entCount + 1
      end
      
      if not self.installed then
	 walker:installAction(self)
	 parser.installedAction = self
	 parser.action = nil
	 returner.installed=true
      end
   end
   
   function returner:RTN(l,s,p,parser,walker)
      if parser.isToken then
	 print("rtn token: " .. string.sub(s,l.saveP,p-1))
	 self:handleToken(string.sub(s,l.saveP,p-1),parser)
      end
      
      if parser.installedAction == self then 
	 self.entCount = self.entCount - 1
	 
	 if 0 < self.entCount then return end
	 
	 parser.action=self
	 walker:removeAction(self)
	 parser.installedAction = nil
	 self.installed=nil
	 
	 while not self:isOperatorStackEmpty() do
	    self:pushTokenStack(self:topOperatorStack())
	    self:popOperatorStack()
	 end
	 
	 print("token stack")
	 self:dumpTokenStack()
	 print("operator stack")
	 self:dumpOperatorStack()
	 
	 self:createParseTreeFromStack()
      end
      
   end
   
   return returner
end


function Alpacca:parseTreeTokenAction ()
   local returner = {}
   
   returner.name = "parseTreeTokenAction"
   
   function returner:ENT(l,s,p,parser)
      l.saveP = p
   end
   
   function returner:RTN(l,s,p,parser)
      local r = {}
      r.op = "token"
      r.value = string.sub(s,l.saveP,p-1)
      
      return r
   end
   
   return returner
end

function Alpacca:parseTreeSeqAction ()
   local returner={}
   
   returner.name = "parseTreePowAction"
   
   function returner:LHS(l,s,p,parser,a)
      l.lhs = a
   end
   
   function returner:RHS(l,s,p,parser,a)
      l.rhs = a
   end
   
   function returner:RTN(l,s,p,parser)
      local r = {}
      r.op = "seq"
      r.lhs = l.lhs
      r.rhs = l.rhs
      return r
   end
   
   return returner
end

function Alpacca:parseTreePowAction ()
   local returner={}
   
   returner.name = "parseTreePowAction"
   
   function returner:LHS(l,s,p,parser,a)
      l.lhs = a
   end
   
   function returner:RHS(l,s,p,parser,a)
      l.rhs = a
   end
   
   function returner:RTN(l,s,p,parser)
      local r = {}
      r.op = "^"
      r.lhs = l.lhs
      r.rhs = l.rhs
      return r
   end
   
   return returner
end

function Alpacca:parseTreePlusAction ()
   local returner={}
   
   returner.name = "parseTreePlusAction"
   
   function returner:LHS(l,s,p,parser,a)
      if not l.lhs then
	 l.lhs = {}
	 l.lhs.op = "+"
	 l.lhs.kids = {}
	 l.i = 0
      end
      l.i = l.i+1
      l.lhs.kids[l.i] = a
   end
   
   function returner:RHS(l,s,p,parser,a)
      l.rhs = a
   end
   
   function returner:RTN(l,s,p,parser)
      local r = {}
      r.op = "plus"
      r.lhs = l.lhs
      r.rhs = l.rhs
      return r
   end
   
   return returner
end

function Alpacca:parseTreeStarAction ()
   local returner={}
   
   returner.name = "parseTreeStarAction"
   
   function returner:LHS(l,s,p,parser,a)
      if not l.lhs then
	 l.lhs = {}
	 l.lhs.op = "*"
	 l.lhs.kids = {}
	 l.i = 0
      end
      l.i = l.i+1
      l.lhs.kids[l.i] = a
   end
   
   function returner:RHS(l,s,p,parser,a)
      l.rhs = a
   end
   
   function returner:RTN(l,s,p,parser)
      local r = {}
      r.op = "star"
      r.lhs = l.lhs
      r.rhs = l.rhs
      return r
   end
   
   return returner
end

function Alpacca:parseTreeDivAction ()
   local returner={}
   
   returner.name = "parseTreeDivAction"
   
   function returner:LHS(l,s,p,parser,a)
      l.lhs = a
   end
   
   function returner:RHS(l,s,p,parser,a)
      l.rhs = a
   end
   
   function returner:RTN(l,s,p,parser)
      if l.lhs then return l.lhs end
      if l.rhs then return l.rhs end
   end
   
   return returner
end



function Alpacca:parseTreeAction ()
   local returner = {}

   returner.name = "parseTreeAction"
   
   function returner:install(parser)
      --install the apropos action
      if parser.isToken then
	 parser.action = Alpacca:parseTreeTokenAction()
	 return
      end
      
      if "seq"==parser.op then
	 parser.action = Alpacca:parseTreeSeqAction()
      end
      
      if "pow"==parser.op then
	 parser.action = Alpacca:parseTreePowAction()
      end
      
      if "plus"==parser.op then
	 parser.action = Alpacca:parseTreePlusAction()
      end
      
      if "star"==parser.op then
	 parser.action = Alpacca:parseTreeStarAction()
      end
      
      if "div"==parser.op then
	 parser.action = Alpacca:parseTreeDivAction()
      end
   
   
      -- recurse
      
      if parser.lhs then
	 self:install(parser.lhs)
      end
      
      if parser.rhs then
	 self:install(parser.rhs)
      end
      
   end

   return returner
end



-- captures and back references

-- Action functor for captures
function Alpacca.capture (results)
   local returner={}
   
   returner.results=results
   
   -- event handlers for the insertion of this
   -- into the walker
   function returner:ENT(l,s,p,parser)
      self.startP = p
      self.results.value = ""
   end
   
   function returner:RTN(l,s,p,parser)
      self.results.value = string.sub(s,self.startP,p-1)
   end
   
   return returner
end



------------------
-- control flow --
------------------

-- The branch cache is used to ensure that the
-- actions are only called once, and only on
-- succeeding paths

function BranchCache ()
   local returner = {}
   
   returner.cache      = {}
   returner.divLevels  = {}
   returner.index      = 1
   
   function returner:reserveBranchPoint(divLevel)
      self.cache[self.index] = 0
      self.divLevels[self.index] = divLevel
      local returner= self.index
      self.index = self.index + 1
      return returner
   end
   
   function returner:storeBranchPoint(bpIndex, bp)
      self.cache[bpIndex] = bp
   end
   
   function returner:rewindCache()
      self.index = 1
   end
   
   function returner:getBp()
      while(self.cache[self.index]==3) do
	 self.index = self.index+1
      end
      local r = self.cache[self.index]
      self.index = self.index+1
      return r
   end
   
   function returner:dump()
      for i = 1,self.index-1 do
	 print("["..i.."] = "..self.divLevels[i]..":"..self.cache[i])
      end
   end
   
   return returner
end

--- abstract pass
--- this runs the parser without triggering any 
--- actions, it just (1) figures whether the 
--- parser has actions or not and (2) records
--- which branches are taken
function Alpacca.abstractPassWalker (inputBranchCache)
   local returner = {}
   
   returner.hasActions = false
   returner.branchCache = inputBranchCache
   returner.divLevel = 0
   returner.buildingCache = true
   
   function returner:fireEvent(event,l,s,p,parser)
      self[event](self,l,s,p,parser)
   end
   
   function returner:ENT(l,s,p,parser)
      if parser.op == "div" then
	 self.divLevel = self.divLevel+1
	 local bp = self.branchCache:reserveBranchPoint(self.divLevel)
	 l.bpIndex = bp
      end
   end
   
   function returner:LHS(l,s,p,parser)
      -- update the branch cache, if this is a choice
      if parser.op == "div" then
	 self.branchCache:storeBranchPoint(l.bpIndex, 1)
      end
   end
   
   function returner:RHS(l,s,p,parser)
      -- update the branch cache, if this is a choice
      if parser.op == "div" then
	 self.branchCache:storeBranchPoint(l.bpIndex, 2)
      end
   end
   
   function returner:FAIL(l,s,p,parser)
      -- update the branch cache, if this is a choice
      if parser.op == "div" then
	 self.branchCache:storeBranchPoint(l.bpIndex, 3)
	 
	 self.divLevel = self.divLevel-1
      end
   end
   
   function returner:RTN(l,s,p,parser)
      if parser.op == "div" then
      	 self.divLevel = self.divLevel-1
      end
      
      if parser.action or parser.saveAction  then 
      	 self.hasActions = true
      end
   end
   
   return returner
end

-- action pass.  Once the abstract pass has gone through,
-- if it finds any actions, we run again, but this time
-- using the branch cache info to make the run quasi-deterministic.

function ActionPassWalker(inputBranchCache)
   local returner = {}
   
   returner.divLevel = 0
   
   returner.useBranchCache = true
   returner.branchCache = inputBranchCache
   
   returner.handlers = {}
   for event,i in pairs(Alpacca.events) do
      returner.handlers[event] = {}
   end
   
   function returner:fireEvent(event,l,s,p,parser,walker)
      for F,i in pairs(self.handlers[event]) do
	 F[event](F,l,s,p,parser,walker)
      end
   end
   
   function returner:ENT(l,s,p,parser)
      if parser.op == "div" then
	 self.divLevel = self.divLevel+1
	 
	 l.bp = self.branchCache:getBp()
	 
	 if 1==l.bp then l.tryRHS = false end
	 if 2==l.bp then l.tryLHS = false end
	 if 3==l.bp then l.tryLHS,l.tryRHS = false,false end
      end
   end
   
   function returner:FAIL(l,s,p,parser)
      if parser.op == "div" then
	 self.divLevel = self.divLevel-1
      end
   end
   
   function returner:RTN(l,s,p,parser)
      if parser.op == "div" then
      	 self.divLevel = self.divLevel-1
      end
   end
   
   function returner:installAction(A)
      for event,i in pairs(Alpacca.events) do
	 if A[event] then 
	    self.handlers[event][A]=true
	 end
      end
   end
   
   function returner:removeAction(A)
      for event,i in pairs(Alpacca.events) do
	 self.handlers[event][A]=nil
      end
   end
   
   returner:installAction(returner)
   
   return returner
end

---------------------------
function Alpacca:turnOffActions ()
   if not self.action then return end
   self.saveAction = self.action
   self.action = nil
end

function Alpacca:turnOnActions ()
   if not self.saveAction then return end
   self.action = self.saveAction
   self.saveAction = nil
end

function Alpacca:walkParser(...)
   local arg = {...}
   for i,F in ipairs(arg) do
      F(self)
   end
   
   local tryLhs = (self.lhs and self.lhs:walkParser(...))
   local tryRhs = (self.rhs and self.rhs:walkParser(...))
end

----------------------------


function Alpacca:parse(s,d)
   -- first pass
   print("first pass")
   
   self:calculateOperatorPrecedence()
   
   self:walkParser(self.turnOffActions)
   
   branchCache = BranchCache()
   local abstractWalker = self.abstractPassWalker(branchCache)
   local succeed, s,p  = self:pass(s,d,abstractWalker)
   
   if not succeed or not abstractWalker.hasActions then
      return succeed,s,p
   end
   
   -- second pass
   print("second pass")
   
   branchCache:rewindCache()
   
   self:walkParser(self.turnOnActions)
   local actionPassWalker = ActionPassWalker(branchCache)
   
   local succeed,s,p,a = self:pass(s,d,actionPassWalker)
   
   return succeed,s,p,a
end


-- handy parser abbrieveiations
local li    =  Alpacca.literal    -- literal parser
local nop   =  Alpacca.noOp       -- no op parser
local rch   =  Alpacca.rangeChar  -- begining or end of a char range
local digit =  Alpacca.digitChar
local alpha =  Alpacca.alphaChar
local any   =  Alpacca.anyChar
local alphaNumeric =  Alpacca.alphaNumericChar

--actions
local pta = Alpacca:parseTreeAction()
local ptg = Alpacca:parseTreeGenerator()
local cap = Alpacca.capture

return Alpacca
