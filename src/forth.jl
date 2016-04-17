module forth

# VM mem size
size_mem = 640*1024

# Buffer sizes
size_BIVar = 16 #
size_RS = 1024   # Return stack size
size_PS = 1024   # Parameter stack size
size_TIB = 4096  # Terminal input buffer size

# The mem array constitutes the memory of the VM. It has the following geography:
#
# mem = +-----------------------+
#       | Built-in Variables    |
#       +-----------------------+
#       | Return Stack          |
#       +-----------------------+
#       | Parameter Stack       |
#       +-----------------------+
#       | Terminal Input Buffer |
#       +-----------------------+
#       | Dictionary            |
#       +-----------------------+
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
HERE = nextVarAddr; nextVarAddr += 1
LATEST = nextVarAddr; nextVarAddr += 1

mem[RSP0] = size_BIVar               # bottom of RS
mem[PSP0] = mem[RSP0] + size_RS      # bottom of PS
TIB = mem[PSP0] + size_PS            # address of terminal input buffer
mem[HERE] = TIB + size_TIB           # location of bottom of dictionary
mem[LATEST] = 0                      # no previous definition

# VM registers
type Reg
    RSP::Int64  # Return stack pointer
    PSP::Int64  # Parameter/data stack pointer
    IP::Int64   # Instruction pointer
    W::Int64    # Working register
    X::Int64    # Extra register
end
reg = Reg(mem[RSP0], mem[PSP0], 0, 0, 0)

# Stack manipulation functions

function pushRS(val::Int64)
    mem[reg.RSP+=1] = val
end

function popRS()
    val = mem[reg.RSP]
    reg.RSP -= 1
    return val
end

function pushPS(val::Int64)
    mem[reg.PSP += 1] = val
end

function popPS()
    val = mem[reg.PSP]
    reg.PSP -= 1
    return val
end

# Primitive creation and calling functions

function createHeader(name::AbstractString, flags::Int64)
    mem[mem[HERE]] = mem[LATEST]
    mem[LATEST] = mem[HERE]
    mem[HERE] += 1

    mem[mem[HERE]] = length(name) + flags; mem[HERE] += 1
    mem[mem[HERE]:(mem[HERE]+length(name)-1)] = [Int(c) for c in name]; mem[HERE] += length(name)
end

function defPrim(name::AbstractString, f::Function; flags::Int64=0)
    createHeader(name, flags)

    push!(primitives, f)
    mem[mem[HERE]] = -length(primitives)
    mem[HERE] += 1

    return -length(primitives)
end

callPrim(addr::Int64) = primitives[-addr]()

function defExistingVar(name::AbstractString, varAddr::Int64; flags::Int64=0)
    defPrim(name, eval(:(() -> begin
        pushPS($(varAddr))
        return NEXT
    end)))
end

function defConst(name::AbstractString, val::Int64; flags::Int64=0)
    defPrim(name, eval(:(() -> begin
        pushPS($(val))
        return NEXT
    end)))
end

function defNewVar(name::AbstractString, initial::Int64; flags::Int64=0)
    createHeader(name, flags)
    
    varAddr = mem[HERE] + 1
    push!(primitives, eval(:(() -> begin
        pushPS($(varAddr))
        return NEXT
    end)))
    mem[mem[HERE]] = -length(primitives); mem[HERE] += 1

    mem[mem[HERE]] = initial; mem[HERE] += 1

    return varAddr
end

# Threading Primitives

NEXT = defPrim("NEXT", () -> begin
    reg.W = mem[reg.IP]
    reg.IP += 1
    X = mem[reg.W]
    return X
end)

DOCOL = defPrim("DOCOL", () -> begin
    pushRS(reg.IP)
    reg.IP = reg.W + 1
    return NEXT
end)

EXIT = defPrim("EXIT", () -> begin
    reg.IP = popRS()
    return NEXT
end)


# Basic forth primitives

DROP = defPrim("DROP", () -> begin
    popPS()
    return NEXT
end)

SWAP = defPrim("SWAP", () -> begin
    mem[reg.PSP], mem[reg.PSP-1] = mem[reg.PSP-1], mem[reg.PSP]
    return NEXT
end)

DUP = defPrim("DUP", () -> begin
    pushPS(mem[reg.PSP])
    return NEXT
end)

LIT = defPrim("LIT", () -> begin
    pushPS(mem[reg.IP])
    reg.IP += 1
    return NEXT
end)

# Memory primitives

STORE = defPrim("!", () -> begin
    addr = popPS()
    dat = popPS()
    mem[addr] = dat
    return NEXT
end)

FETCH = defPrim("@", () -> begin
    addr = popPS()
    pushPS(mem[addr])
    return NEXT
end)

ADDSTORE = defPrim("+!", () -> begin
    addr = popPS()
    toAdd = popPS()
    mem[addr] += toAdd
    return NEXT
end)

SUBSTORE = defPrim("-!", () -> begin
    addr = popPS()
    toSub = popPS()
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

defConst("TIB", TIB)
NUMTIB = defNewVar("#TIB", 0)
TOIN = defNewVar(">IN", TIB)

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

function dumpPS()
    count = reg.PSP - mem[PSP0]

    if count > 0
        print("<$count>")
        for i in (mem[PSP0]+1):reg.PSP
            print(" $(mem[i])")
        end
        println()
    else
        println("Parameter stack empty")
    end
end

end
