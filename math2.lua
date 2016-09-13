lpeg = require("lpeg")
dumptable = require("pl.pretty").dump

function makeparser()
   local V,C,P,R,Ct,S = lpeg.V,lpeg.C,lpeg.P,lpeg.R,lpeg.Ct,lpeg.S

   local space = lpeg.S(" \n\t")^0
   local int = (C(P"-"^-1 * R"09"^1) * space) / function(txt) return {"num",val=tonumber(txt),props={}} end
   local var = C(R"az"+R"AZ") * space / function(txt) return {"var",name=txt,props={}} end
   local parens = S("(") * space * V"lowest" * S(")") * space
      / function(math) math.props.input_paren = "()" return math end

   function sym(cd,name) return {"sym",sym=cd.."."..name,props={}} end
   function apply2(cd,name) local s=sym(cd,name); return function(a1,a2) return {"apply",head=s,args={a1,a2},props={}} end end

   local parse_table = {
      [100] = {{typ="infixl", info="*", op=S("*"), sem=apply2("arith1","times")},
	       {typ="infixl", info="/", op=S("/"), sem=apply2("arith1","divide")}},
      [90] = {{typ="infixl", info="+", op=S("+"), sem=apply2("arith1","plus")},
	      {typ="infixl", info="-", op=S("-"), sem=apply2("arith1","minus")}},
   }

   local priorities = {}
   for k,v in pairs(parse_table) do
      table.insert(priorities,k)
   end
   table.sort(priorities)

   local lpeg_table = {"root", 
     root = V"lowest" * -1,
     lowest = V(priorities[1]),
     atom = int + var + parens,
   }

   for i,pri in ipairs(priorities) do
      local nextpri = priorities[i+1]
      if nextpri==nil then nextpri = "atom" end
      local nextnonterm = lpeg.V(nextpri)

      local row = parse_table[pri]

      function K(x) return function() return x end end

      typ = row[1].typ -- TODO: make sure all entries have the same type in this row
      if typ=="infixl" then
	 local op = nil
	 for i,entry in ipairs(row) do
	    local op2 = (entry.op * space) / K(entry.sem)
	    if op==nil then op=op2 else op=op+op2 end
	 end
	 function iter(arg)
	    local result = arg[1]
	    for i=2,#arg,2 do
	       result = arg[i](result,arg[i+1])
	    end
	    return result
	 end
	 local rule = lpeg.Ct(nextnonterm * (op * nextnonterm)^0) / iter
	 lpeg_table[pri] = rule
      else
	 error("unknown rule type "..typ.." in parser table at priority "..tostring(i).." (rule info is "..row[1].info..")")
      end
   end
   
   return lpeg.P(lpeg_table)
end

function dump(math)
   if math==nil then error("dump called with nil argument",2) end
   local typ = math[1]
   if typ=="apply" then
      dump(math.head)
      io.write("(")
      for i,arg in ipairs(math.args) do
	 if i>1 then io.write(",") end
	 dump(arg)
      end
      io.write(")")
   elseif typ=="sym" then
      io.write(math.sym)
   elseif typ=="num" then
      io.write(tostring(math.val))
   elseif typ=="var" then
      io.write("$"..math.name)
   else
      io.write("???????")
      require("pl.pretty").dump(math)
   end
   if next(math.props)~=nil then
      io.write("{")
      for k,v in pairs(math.props) do
	 io.write(tostring(k) .. " -> " .. tostring(v) .. ",") end
      io.write("}")
   end
end		

function tex_infix2_renderer(op)
   return function(_,args)
      if #args~=2 then error("expect 2 args") end
      return args[1].." "..op.." "..args[2]
   end
end

function tex_default_renderer(op,args)
   return "[nyi: "..op.."]"
end

function tex_macro_renderer(op)
   return function(_,args) 
      local res = op
      for i,a in ipairs(args) do
	 res = res .. "{" .. a .. "}"
      end
      return res
   end
end

tex_renderers = {
   ["arith1.plus"] = tex_infix2_renderer("+"),
   ["arith1.times"] = tex_infix2_renderer("*"),
   ["arith1.divide"] = tex_macro_renderer("\\frac"),
   ["arith1.minus"] = tex_infix2_renderer("-"),
   default = tex_default_renderer,
}

tex_priority = {
   ["arith1.plus"] = 90,
   ["arith1.minus"] = 90,
   ["arith1.times"] = 100,
   ["arith1.divide"] = 0,
}

-- Adds a parenthesis if priority <= pri
function addparens(pri,math)
   if math[1]~="apply" then return end
   local mypri = tex_priority[math.head.sym]

   if mypri == 0 then
      addparens(0,math.args[1])
      addparens(0,math.args[2])
      return
   end

   if pri==nil then error("nyi") end
   if mypri <= pri then math.props.parens = true end
   addparens(mypri-1,math.args[1])
   addparens(mypri,math.args[2])
end


function totex(math)
   if math.props.tex~=nil then return end
   local typ = math[1]

   if typ=="num" then
      math.props.tex = tostring(math.val)
   elseif typ=="apply" then
      if math.head[1] ~= "sym" then
	 error("apply without sym-head: nyi") end
      local sym = math.head.sym
      local renderer = tex_renderers[sym]
      if renderer==nil then renderer = tex_renderers["default"] end
      local texargs = {}
      for _,a in ipairs(math.args) do totex(a); table.insert(texargs,a.props.tex) end
      math.props.tex = renderer(sym,texargs)
   elseif typ=="var" then
      math.props.tex = math.name
   else
      math.props.tex = "?"..tostring(typ).."?"
   end

   if math.props.parens then
      math.props.tex = "("..math.props.tex..")" end
end

mathparser = makeparser()

function parsemath(math)
   return lpeg.match(mathparser,math)
end

function test() 
   local math = parsemath("c*(c/2)")
   dump(math)
   print()
   addparens(0,math)
   dump(math)
   print()
   totex(math)
   print(math.props.tex)
end

test()

