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

# Stack manipulation functions

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

# Primitive creation and calling functions

function defPrim(name::AbstractString, expr::Expr)
    global HERE, LATEST

    memory[HERE] = LATEST
    LATEST = HERE
    HERE += 1

    memory[HERE] = length(name); HERE += 1
    memory[HERE:(HERE+length(name)-1)] = [Int(c) for c in name]; HERE += length(name)

    push!(primitives, expr)
    memory[HERE] = -length(primitives)
    HERE += 1

    return -length(primitives)
end

defVar(name::AbstractString, var::Symbol) = defPrim(name, quote
    pushPS($var)
    jmp = NEXT
end)

defConst(name::AbstractString, val::Int64) = defPrim(name, quote
    pushPS($val)
    jmp = Next
end)

callPrim(addr::Int64) = eval(primitives[-addr])

# Threading Primitives

NEXT = defPrim("NEXT", quote
    W = memory[IP]
    IP += 1
    X = memory[W]
    jmp = X
end)

DOCOL = defPrim("DOCOL", quote
    pushRS(IP)
    IP = W + 1
    jmp = NEXT
end)

EXIT = defPrim("EXIT", quote
    IP = popRS()
    jmp = NEXT
end)


# Basic forth primitives

DROP = defPrim("DROP", quote
    popPS()
    jmp = NEXT
end)

SWAP = defPrim("SWAP", quote
    PS[PSP], PS[PSP-1] = PS[PSP-1], PS[PS]
    jmp = NEXT
end)

DUP = defPrim("DUP", quote
    pushPS(PS[PSP])
    jmp = NEXT
end)

LIT = defPrim("LIT", quote
    pushPS(memory[IP])
    IP += 1
    jmp = NEXT
end)

# Memory primitives

STORE = defPrim("!", quote
    addr = popPS()
    dat = popPS()
    memory[addr] = dat
    jmp = NEXT
end)

FETCH = defPrim("@", quote
    addr = popPS()
    pushPS(memory[addr])
    jmp = NEXT
end)

ADDSTORE = defPrim("+!", quote
    addr = popPS()
    toAdd = popPS()
    memory[addr] += toAdd
    jmp = NEXT
end)

SUBSTORE = defPrim("-!", quote
    addr = popPS()
    toSub = popPS()
    memory[addr] -= toSub
    jmp = NEXT
end)


# Built-in variables

defVar("STATE", :STATE)
defVar("HERE", :HERE)
defVar("LATEST", :LATEST)
defVAR("BASE", :BASE)

# Constants

defConst("VERSION", 1)
defConst("DOCOL", DOCOL)

# Return Stack

TOR = defPrim(">R", quote
    pushRS(popPS())
    jmp = NEXT
end)

FROMR = defPrim("R>", quote
    pushPS(popRS())
end)

RSPFETCH = defPrim("RSP@", quote
    pushPS(RSP)
    jmp = NEXT
end)

RSPSTORE = defPrim("RSP!", quote
    RSP = popPS()
    jmp = NEXT
end)

RDROP = defPrim("RDROP", quote
    popRS()
    jmp = NEXT
end)

# Parameter Stack

PSPFETCH = defPrim("PSP@", quote
    pushPS(PSP)
    jmp = NEXT
end)

PSPSTORE = defPrim("PSP!", quote
    PSP = popPS()
    jmp = NEXT
end)

# I/O

KEY = defPrim("KEY", quote

    jmp = NEXT
end)

EMIT = defPrim("EMIT", quote

    jmp = NEXT
end)

WORD = defPrim("WORD", quote

    jmp = NEXT
end)

NUMBER = defPrim("NUMBER", quote

    jmp = NEXT
end)

#### VM loop ####
jmp = NEXT
function runVM()
    while true
        callPrim(jmp)
    end
end

end
