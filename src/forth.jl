module forth

# VM memory size
size_memory = 640*1024

# Buffer sizes
size_SysVar = 16 #
size_RS = 1024   # Return stack size
size_PS = 1024   # Parameter stack size
size_TIB = 4096  # Terminal input buffer size

# VM registers
type Reg
    RSP::Int64  # Return stack pointer
    PSP::Int64  # Parameter/data stack pointer
    IP::Int64   # Instruction pointer
    W::Int64    # Working register
    X::Int64    # Extra register
end

# The following array constitutes the memory of the VM. It has the following geography:
#
# memory = +-----------------------+
#          | Built-in Variables    |
#          +-----------------------+
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
# the dictionary. Additionally, all 
#
# Simple linear addressing is used with one exception: references to primitive code
# blocks, which are represented as anonymous functions, appear the negative index
# into the primitives array which contains only these functions.

memory = Array{Int64,1}(size_memory)
primitives = Array{Function,1}()

# Built-in variables

nextVarAddr = 1
RSP0 = nextVarAddr; nextVarAddr += 1
PSP0 = nextVarAddr; nextVarAddr += 1
HERE = nextVarAddr; nextVarAddr += 1
LATEST = nextVarAddr; nextVarAddr += 1

memory[RSP0] = size_BIVar               # bottom of RS
memory[PSP0] = memory[RSP0] + size_RS   # bottom of PS
TIB = memory[PSP0] + size_PS            # address of terminal input buffer
memory[HERE] = TIB + size_TIB           # location of bottom of dictionary
memory[LATEST] = 0                      # no previous definition


# Stack manipulation functions

function pushRS(reg::Reg, val::Int64)
    memory[reg.RSP+=1] = val
end

function popRS(reg::Reg)
    val = memory[reg.RSP]
    reg.RSP -= 1
    return val
end

function pushPS(reg::Reg, val::Int64)
    memory[reg.PSP += 1] = val
end

function popPS(reg::Reg)
    val = memory[reg.PSP]
    reg.PSP -= 1
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

callPrim(reg::Reg, addr::Int64) = primitives[-addr](reg)

function defSysVar(name::AbstractString, varAddr::Int64)
    global latest, here

    memory[here] = latest
    latest = here
    here += 1

    memory[here] = length(name); here += 1
    memory[here:(here+length(name)-1)] = [Int(c) for c in name]; here += length(name)

    push!(primitives, eval(:((reg::Reg) -> begin
        pushPS(reg, $(varAddr))
        return NEXT
    end)))
    memory[here] = -length(primitives)
    here += 1

    return varAddr
end

defConst(name::AbstractString, val::Int64) = defSysVar(name, val)

# Threading Primitives

NEXT = defPrim("NEXT", (reg) -> begin
    reg.W = memory[reg.IP]
    reg.IP += 1
    X = memory[reg.W]
    return X
end)

DOCOL = defPrim("DOCOL", (reg) -> begin
    pushRS(reg, reg.IP)
    reg.IP = reg.W + 1
    return NEXT
end)

EXIT = defPrim("EXIT", (reg) -> begin
    reg.IP = popRS(reg)
    return NEXT
end)


# Basic forth primitives

DROP = defPrim("DROP", (reg) -> begin
    popPS(reg)
    return NEXT
end)

SWAP = defPrim("SWAP", (reg) -> begin
    memory[reg.PSP], memory[reg.PSP-1] = memory[reg.PSP-1], memory[reg.PSP]
    return NEXT
end)

DUP = defPrim("DUP", (reg) -> begin
    pushPS(reg, memory[reg.PSP])
    return NEXT
end)

LIT = defPrim("LIT", (reg) -> begin
    pushPS(reg, memory[reg.IP])
    reg.IP += 1
    return NEXT
end)

# Memory primitives

STORE = defPrim("!", (reg) -> begin
    addr = popPS(reg)
    dat = popPS(reg)
    memory[addr] = dat
    return NEXT
end)

FETCH = defPrim("@", (reg) -> begin
    addr = popPS(reg)
    pushPS(reg, memory[addr])
    return NEXT
end)

ADDSTORE = defPrim("+!", (reg) -> begin
    addr = popPS(reg)
    toAdd = popPS(reg)
    memory[addr] += toAdd
    return NEXT
end)

SUBSTORE = defPrim("-!", (reg) -> begin
    addr = popPS(reg)
    toSub = popPS(reg)
    memory[addr] -= toSub
    return NEXT
end)


# Built-in variables


# Constants

defConst("VERSION", 1)
defConst("DOCOL", DOCOL)

# Return Stack

TOR = defPrim(">R", (reg) -> begin
    pushRS(reg, popPS(reg))
    return NEXT
end)

FROMR = defPrim("R>", (reg) -> begin
    pushPS(reg, popRS(reg))
    return NEXT
end)

RSPFETCH = defPrim("RSP@", (reg) -> begin
    pushPS(reg, RSP)
    return NEXT
end)

RSPSTORE = defPrim("RSP!", (reg) -> begin
    RSP = popPS(reg)
    return NEXT
end)

RDROP = defPrim("RDROP", (reg) -> begin
    popRS(reg)
    return NEXT
end)

# Parameter Stack

PSPFETCH = defPrim("PSP@", (reg) -> begin
    pushPS(reg, PSP)
    return NEXT
end)

PSPSTORE = defPrim("PSP!", (reg) -> begin
    PSP = popPS(reg)
    return NEXT
end)

# I/O

defConst("TIB", tib)
defVar("#TIB", :numtib)
defVar(">IN", :toin)

KEY = defPrim("KEY", (reg) -> begin
    if toin >= numtib

    end

    return NEXT
end)

EMIT = defPrim("EMIT", (reg) -> begin

    return NEXT
end)

WORD = defPrim("WORD", (reg) -> begin

    return NEXT
end)

NUMBER = defPrim("NUMBER", (reg) -> begin

    return NEXT
end)

#### VM loop ####
function runVM(reg::Reg)
    jmp = NEXT
    while (jmp = callPrim(reg, jmp)) != 0 end
end

end
