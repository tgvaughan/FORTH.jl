module forth

RS = Array{Int64, 1}(1024)
RSP = 0

PS = Array{Int64, 1}(1024)
PSP =0

IP = 0
W = 0
X = 0

jmp = 0

primitives = Array{Expr,1}()
memory = Array{Int64,1}(64*1024)
LATEST = 0
HERE = 1

# Intperpreter state

STATE = 0

# Current radix

BASE = 10

# Stack manipulation macros

function pushRS(val::Int64)
    global RSP
    RS[RSP += 1] = val
end

function popRS()
    global RSP
    val = RS[RSP]
    RSP -= 1
    return val
end

function pushPS(val::Int64)
    global PSP
    PS[PSP += 1] = val
end

function popPS()
    global PSP
    val = PS[PSP]
    PSP -= 1
    return val
end

# Primitive creation functions

function defPrim(name::AbstractString, expr::Expr)
    global HERE, LATEST

    memory[HERE] = LATEST
    LATEST = HERE
    HERE += 1

    memory[HERE] = length(name); HERE += 1
    memory[HERE:(HERE+length(name)-1)] = [Int(c) for c in name]; HERE += length(name)

    push!(primitives, expr)
    memory[HERE] = -length(primitives)
    codeword = HERE
    HERE += 1

    return codeword
end

function defVar(name::AbstractString, var::Expr)
    defPrim(name, Expr(:call, :pushPS, var))
end

# Threading Primitives

NEXT = defPrim("NEXT", :(begin
    W = memory[IP]
    IP += 1
    X = memory[W]
    jmp = X
end))

DOCOL = defPrim("DOCOL", :(begin
    pushRS(IP)
    IP = W + 1
    jmp = NEXT
end))

EXIT = defPrim("EXIT", :(begin
    IP = popRS()
    jmp = NEXT
end))


# Basic forth primitives

DROP = defPrim("DROP", :(begin
    popPS()
end))

SWAP = defPrim("SWAP", :(begin
    PS[PSP], PS[PSP-1] = PS[PSP-1], PS[PS]
end))

DUP = defPrim("DUP", :(begin
    pushPS(PS[PSP])
end))

LIT = defPrim("LIT", :(begin
    pushPS(memory[IP])
    IP += 1
end))

# Memory primitives

STORE = defPrim("!", :(begin
    addr = popPS()
    dat = popPS()
    memory[addr] = dat
end))

FETCH = defPrim("@", :(begin
    addr = popPS()
    pushPS(memory[addr])
end))

ADDSTORE = defPrim("+!", :(begin
    addr = popPS()
    toAdd = popPS()
    memory[addr] += toAdd
end))

SUBSTORE = defPrim("-!", :(begin
    addr = popPS()
    toSub = popPS()
    memory[addr] -= toSub
end))


# Built-in variables

defVar("STATE", :STATE)
defVar("HERE", :HERE)
defVar("LATEST", :LATEST)
defVAR("PSP", :PSP)
defVAR("BASE", :BASE)

# Constants



# VM loop
jmp = NEXT
function runVM()
    while true
        eval(primitives[-memory[jmp]])
    end
end

end
