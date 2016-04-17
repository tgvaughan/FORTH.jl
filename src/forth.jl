module forth

# VM mem size
size_mem = 640*1024

# Buffer sizes
size_BIVar = 16 #
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
# mem = +-----------------------+
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
# the dictionary.
#
# Simple linear addressing is used with one exception: references to primitive code
# blocks, which are represented as anonymous functions, appear the negative index
# into the primitives array which contains only these functions.

mem = Array{Int64,1}(size_mem)
primitives = Array{Function,1}()

# Built-in variables

nextVarAddr = 1
RSP0 = nextVarAddr; nextVarAddr += 1
PSP0 = nextVarAddr; nextVarAddr += 1
TIB = nextVarAddr; nextVarAddr += 1
HERE = nextVarAddr; nextVarAddr += 1
LATEST = nextVarAddr; nextVarAddr += 1

mem[RSP0] = size_BIVar               # bottom of RS
mem[PSP0] = mem[RSP0] + size_RS      # bottom of PS
mem[TIB] = mem[PSP0] + size_PS            # address of terminal input buffer
mem[HERE] = mem[TIB] + size_TIB           # location of bottom of dictionary
mem[LATEST] = 0                      # no previous definition

# Stack manipulation functions

function pushRS(reg::Reg, val::Int64)
    mem[reg.RSP+=1] = val
end

function popRS(reg::Reg)
    val = mem[reg.RSP]
    reg.RSP -= 1
    return val
end

function pushPS(reg::Reg, val::Int64)
    mem[reg.PSP += 1] = val
end

function popPS(reg::Reg)
    val = mem[reg.PSP]
    reg.PSP -= 1
    return val
end

# Primitive creation and calling functions

function createHeader(name::AbstractString)
    mem[mem[HERE]] = mem[LATEST]
    mem[LATEST] = mem[HERE]
    mem[HERE] += 1

    mem[mem[HERE]] = length(name); mem[HERE] += 1
    mem[mem[HERE]:(mem[HERE]+length(name)-1)] = [Int(c) for c in name]; mem[HERE] += length(name)
end

function defPrim(name::AbstractString, f::Function)
    createHeader(name)

    push!(primitives, f)
    mem[mem[HERE]] = -length(primitives)
    mem[HERE] += 1

    return -length(primitives)
end

callPrim(reg::Reg, addr::Int64) = primitives[-addr](reg)

defExistingVar(name::AbstractString, varAddr::Int64) = defPrim(name, eval(:((reg) -> begin
    pushPS(reg, $(varAddr))
    return NEXT
end)))

defConst(name::AbstractString, val::Int64) = defPrim(name, eval(:((reg) -> begin
    pushPS(reg, $(val))
    return NEXT
end)))

function defNewVar(name::AbstractString, initial::Int64)
    createHeader(name)
    
    varAddr = mem[HERE] + 1
    push!(primitives, eval(:((reg) -> begin
        pushPS(reg, $(varAddr))
        return NEXT
    end)))
    mem[mem[HERE]] = -length(primitives); mem[HERE] += 1

    mem[mem[HERE]] = inital; mem[HERE] += 1

    return varAddr
end

# Threading Primitives

NEXT = defPrim("NEXT", (reg) -> begin
    reg.W = mem[reg.IP]
    reg.IP += 1
    X = mem[reg.W]
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
    mem[reg.PSP], mem[reg.PSP-1] = mem[reg.PSP-1], mem[reg.PSP]
    return NEXT
end)

DUP = defPrim("DUP", (reg) -> begin
    pushPS(reg, mem[reg.PSP])
    return NEXT
end)

LIT = defPrim("LIT", (reg) -> begin
    pushPS(reg, mem[reg.IP])
    reg.IP += 1
    return NEXT
end)

# Memory primitives

STORE = defPrim("!", (reg) -> begin
    addr = popPS(reg)
    dat = popPS(reg)
    mem[addr] = dat
    return NEXT
end)

FETCH = defPrim("@", (reg) -> begin
    addr = popPS(reg)
    pushPS(reg, mem[addr])
    return NEXT
end)

ADDSTORE = defPrim("+!", (reg) -> begin
    addr = popPS(reg)
    toAdd = popPS(reg)
    mem[addr] += toAdd
    return NEXT
end)

SUBSTORE = defPrim("-!", (reg) -> begin
    addr = popPS(reg)
    toSub = popPS(reg)
    mem[addr] -= toSub
    return NEXT
end)


# Built-in variables

defExistingVar("HERE", HERE)
defExistingVar("LATEST", LATEST)
defExistingVar("PSP0", PSP0)
defExistingVar("RSP0", RSP0)
defNewVar("STATE", 0)
defNewVar("BASE", 10)

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

#defConst("TIB", tib)
#defVar("#TIB", :numtib)
#defVar(">IN", :toin)
#
#KEY = defPrim("KEY", (reg) -> begin
#    if toin >= numtib
#
#    end
#
#    return NEXT
#end)
#
#EMIT = defPrim("EMIT", (reg) -> begin
#
#    return NEXT
#end)
#
#WORD = defPrim("WORD", (reg) -> begin
#
#    return NEXT
#end)
#
#NUMBER = defPrim("NUMBER", (reg) -> begin
#
#    return NEXT
#end)
#
#### VM loop ####
#function runVM(reg::Reg)
#    jmp = NEXT
#    while (jmp = callPrim(reg, jmp)) != 0 end
#end

# Debugging tools

function coredump(startAddr::Int64; count::Int64 = 16, cellsPerLine::Int64 = 8)
    chars = Array{Char,1}(cellsPerLine)

    for i in 0:(count-1)
        addr = startAddr + i
        if i%cellsPerLine == 0
            print("$addr:")
        end

        print("\t$(mem[addr]) ")

        if (mem[addr]>=32 && mem[addr]<176)
            chars[i%cellsPerLine + 1] = Char(mem[addr])
        else
            chars[i%cellsPerLine + 1] = '.'
        end

        if i%cellsPerLine == cellsPerLine-1
            println(string("\t", ASCIIString(chars)))
        end
    end
end

end
