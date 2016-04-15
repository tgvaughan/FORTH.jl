module forth

# VM memory size
size_memory = 640*1024

# Buffer sizes
size_RS = 1024  # Return stack size
size_PS = 1024  # Parameter stack size
size_TIB = 4096 # Terminal input buffer size

# VM registers
RSP = 0 # Return stack pointer
PSP =0  # Parameter/data stack pointer
IP = 0  # Instruction pointer
W = 0   # Working register
X = 0   # Extra register

RSP0 = 1
PSP0 = RSP0 + size_RS
here = PSP0 + size_PS + size_TIB  # location of bottom of dictionary
latest = 0 # no previous definition

# The following array constitutes the memory of the VM. It has the following geography:
#
# memory = +-----------------------+
#          | Return Stack          |
#          +-----------------------+
#          | Parameter Stack       |
#          +-----------------------+
#          | Terminal Input Buffer |
#          +-----------------------+
#          | Dictionary            |
#          +-----------------------+
#
# Note that all words (user-defined, primitive, variables, etc) are included in
# the dictionary.
#
# Simple linear addressing is used with one exception: references to primitive code
# blocks, which are represented as anonymous functions, appear the negative index
# into the primitives array which contains only these functions.

memory = Array{Int64,1}(size_memory)
primitives = Array{Function,1}()


# Stack manipulation functions

function pushRS(val::Int64)
    global RSP
    memory[RSP+=1] = val
end

function popRS()
    global RSP
    val = memory[RSP]
    RSP -= 1
    return val
end

function pushPS(val::Int64)
    global PSP
    memory[PSP += 1] = val
end

function popPS()
    global PSP
    val = PS[PSP]
    PSP -= 1
    return val
end

# Primitive creation and calling functions

function defPrim(name::AbstractString, f::Function)
    global latest, here

    memory[here] = latest
    latest = here
    here += 1

    memory[here] = length(name); here += 1
    memory[here:(here+length(name)-1)] = [Int(c) for c in name]; here += length(name)

    push!(primitives, f)
    memory[here] = -length(primitives)
    here += 1

    return -length(primitives)
end

callPrim(addr::Int64) = primitives[-addr]()

function defVar(name::AbstractString, val::Int64)
    global latest, here

    memory[here] = latest
    latest = here
    here += 1

    memory[here] = length(name); here += 1
    memory[here:(here+length(name)-1)] = [Int(c) for c in name]; here += length(name)

    push!(primitives, () -> begin

    end)

    pushPS($var)
    jmp = NEXT
end

# Threading Primitives

NEXT = defPrim("NEXT", () -> begin
    W = memory[IP]
    IP += 1
    X = memory[W]
    return X
end)

DOCOL = defPrim("DOCOL", () -> begin
    pushRS(IP)
    IP = W + 1
    return NEXT
end)

EXIT = defPrim("EXIT", () -> begin
    IP = popRS()
    return NEXT
end)


# Basic forth primitives

DROP = defPrim("DROP", () -> begin
    popPS()
    return NEXT
end)

SWAP = defPrim("SWAP", () -> begin
    PS[PSP], PS[PSP-1] = PS[PSP-1], PS[PS]
    return NEXT
end)

DUP = defPrim("DUP", () -> begin
    pushPS(PS[PSP])
    return NEXT
end)

LIT = defPrim("LIT", () -> begin
    pushPS(memory[IP])
    IP += 1
    return NEXT
end)

# Memory primitives

STORE = defPrim("!", quote
    addr = popPS()
    dat = popPS()
    memory[addr] = dat
    return NEXT
end)

FETCH = defPrim("@", quote
    addr = popPS()
    pushPS(memory[addr])
    return NEXT
end)

ADDSTORE = defPrim("+!", quote
    addr = popPS()
    toAdd = popPS()
    memory[addr] += toAdd
    return NEXT
end)

SUBSTORE = defPrim("-!", quote
    addr = popPS()
    toSub = popPS()
    memory[addr] -= toSub
    return NEXT
end)


# Built-in variables

defVar("STATE", :state)
defVar("HERE", :here)
defVar("LATEST", :latest)
defVar("BASE", :base)

# Constants

defConst("VERSION", 1)
defConst("DOCOL", DOCOL)

# Return Stack

TOR = defPrim(">R", () -> begin
    pushRS(popPS())
    return NEXT
end)

FROMR = defPrim("R>", () -> begin
    pushPS(popRS())
    return NEXT
end)

RSPFETCH = defPrim("RSP@", () -> begin
    pushPS(RSP)
    return NEXT
end)

RSPSTORE = defPrim("RSP!", () -> begin
    RSP = popPS()
    return NEXT
end)

RDROP = defPrim("RDROP", () -> begin
    popRS()
    return NEXT
end)

# Parameter Stack

PSPFETCH = defPrim("PSP@", () -> begin
    pushPS(PSP)
    return NEXT
end)

PSPSTORE = defPrim("PSP!", () -> begin
    PSP = popPS()
    return NEXT
end)

# I/O

defConst("TIB", tib)
defVar("#TIB", :numtib)
defVar(">IN", :toin)

KEY = defPrim("KEY", () -> begin
    if toin >= numtib

    end

    return NEXT
end)

EMIT = defPrim("EMIT", () -> begin

    return NEXT
end)

WORD = defPrim("WORD", () -> begin

    return NEXT
end)

NUMBER = defPrim("NUMBER", () -> begin

    return NEXT
end)

#### VM loop ####
function runVM()
    jmp = NEXT
    while (jmp = callPrim(jmp)) != 0 end
end

end
