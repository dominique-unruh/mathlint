lpeg = require("lpeg")


function makeparser()
   local V,C,P,R,Ct,S = lpeg.V,lpeg.C,lpeg.P,lpeg.R,lpeg.Ct,lpeg.S

   local space = lpeg.S(" \n\t")^0
   local int = (C(P"-"^-1 * R"09"^1) * space) / function(txt) return {"num",val=tonumber(txt)} end
   local var = C(R"az"+R"AZ") * space / function(txt) return {"var",name=txt} end

   local ops = {
      ["+"] = {"arith1","plus"},
      ["-"] = {"arith1","minus"},
      ["*"] = {"arith1","times"},
      ["/"] = {"arith1","divide"},
   }

   function op(a1,op,a2)
      local op2 = ops[op]
      local sym = {"sym",cd=op2[1],name=op2[2]}
      return {"apply", head=sym, args={a1,a2}}
   end

   return lpeg.P {"root",
     root = V"math" * -1,
     math = V"addexp",
     addexp = (V"mulexp" * C(S("+-")) * space * V"addexp") / op
            + V"mulexp",
     mulexp = (V"atom" * C(S("*/")) * space * V"atom") / op
            + V"atom",
     atom = int + var
   }
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
      io.write(math.cd.."."..math.name)
   elseif typ=="num" then
      io.write(tostring(math.val))
   elseif typ=="var" then
      io.write("$"..math.name)
   else
      require("pl.pretty").dump(math)
   end
end		

mathparser = makeparser()

function parsemath(math)
   return lpeg.match(mathparser,math)
end

dump(parsemath("x*2+3"))
