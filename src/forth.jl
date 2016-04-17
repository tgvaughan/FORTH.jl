module forth

# VM mem size
size_mem = 640*1024

# Buffer sizes
size_RS = 1024   # Return stack size
size_PS = 1024   # Parameter stack size
size_TIB = 1096  # Terminal input buffer size

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

mem[RSP0] = nextVarAddr              # bottom of RS
mem[PSP0] = mem[RSP0] + size_RS      # bottom of PS
TIB = mem[PSP0] + size_PS            # address of terminal input buffer
mem[HERE] = TIB + size_TIB           # location of bottom of dictionary
mem[LATEST] = 0                      # no previous definition

DICT = mem[HERE] # Save bottom of dictionary as constant

# VM registers
type Reg
    RSP::Int64  # Return stack pointer
    PSP::Int64  # Parameter/data stack pointer
    IP::Int64   # Instruction pointer
    W::Int64    # Working register
    X::Int64    # Extra register
end
reg = Reg(mem[RSP0], mem[PSP0], 0, 0, 0)

# Stack manipulation

type StackUnderflow <: Exception end

getRSDepth() = reg.RSP - mem[RSP0]
getPSDepth() = reg.PSP - mem[PSP0]

function ensurePSDepth(depth::Int64)
    if getPSDepth()<depth
        throw(StackUnderflow())
    end
end

function ensureRSDepth(depth::Int64)
    if getRSDepth()<depth
        throw(StackUnderflow())
    end
end

function pushRS(val::Int64)
    mem[reg.RSP+=1] = val
end

function popRS()
    ensureRSDepth(1)

    val = mem[reg.RSP]
    reg.RSP -= 1
    return val
end

function pushPS(val::Int64)
    mem[reg.PSP += 1] = val
end

function popPS()
    ensurePSDepth(1)

    val = mem[reg.PSP]
    reg.PSP -= 1
    return val
end

# Primitive creation and calling functions

function createHeader(name::AbstractString, flags::Int64)
    mem[mem[HERE]] = mem[LATEST]
    mem[LATEST] = mem[HERE]
    mem[HERE] += 1

    mem[mem[HERE]] = length(name) | flags; mem[HERE] += 1
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

function defConst(name::AbstractString, val::Int64; flags::Int64=0)
    defPrim(name, eval(:(() -> begin
        pushPS($(val))
        return NEXT
    end)))

    return val
end

function defWord(name::AbstractString, wordAddrs::Array{Int64,1}; flags::Int64=0)
    createHeader(name, flags)

    addr = mem[HERE]
    mem[mem[HERE]] = DOCOL
    mem[HERE] += 1

    for wordAddr in wordAddrs
        mem[mem[HERE]] = wordAddr
        mem[HERE] += 1
    end

    return addr
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
    a = popPS()
    b = popPS()
    pushPS(a)
    pushPS(b)
    return NEXT
end)

DUP = defPrim("DUP", () -> begin
    pushPS(mem[reg.PSP])
    return NEXT
end)

OVER = defPrim("OVER", () -> begin
    ensurePSDepth(2)
    pushPS(mem[reg.PSP-1])
    return NEXT
end)

ROT = defPrim("ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(a)
    pushPS(c)
    pushPS(b)
    return NEXT
end)

NROT = defPrim("-ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(c)
    return NEXT
end)

TWODROP = defPrim("2DROP", () -> begin
    popPS()
    popPS()
    return NEXT
end)

TWODUP = defPrim("2DUP", () -> begin
    ensurePSDepth(2)
    a = mem[reg.PSP-1]
    b = mem[reg.PSP]
    pushPS(a)
    pushPS(b)
    return NEXT
end)

TWOSWAP = defPrim("2SWAP", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    d = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(c)
    pushPS(d)
    return NEXT
end)

QDUP = defPrim("?DUP", () -> begin
    ensurePSDepth(1)
    val = mem[reg.PSP]
    if val != 0
        pushPS(val)
    end
    return NEXT
end)

INCR = defPrim("1+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 1
    return NEXT
end)

DECR = defPrim("1-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 1
    return NEXT
end)

ADD = defPrim("+", () -> begin
    a = popPS()
    b = popPS()
    pushPS(a+b)
    return NEXT
end)

SUB = defPrim("-", () -> begin
    a = popPS()
    b = popPS()
    pushPS(b-a)
    return NEXT
end)

MUL = defPrim("*", () -> begin
    a = popPS()
    b = popPS()
    pushPS(a*b)
    return NEXT
end)

DIVMOD = defPrim("/MOD", () -> begin
    a = popPS()
    b = popPS()
    q,r = divrem(b,a)
    pushPS(r)
    pushPS(q)
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
STATE = defNewVar("STATE", 0)
BASE = defNewVar("BASE", 10)

# Constants

defConst("VERSION", 1)
defConst("DOCOL", DOCOL)
defConst("DICT", DICT)
F_IMMED = defConst("F_IMMED", 128)
F_HIDDEN = defConst("F_HIDDEN", 256)
F_LENMASK = defConst("F_LENMASK", 127)

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
    pushPS(reg.RSP)
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
    pushPS(reg.PSP)
    return NEXT
end)

PSPSTORE = defPrim("PSP!", () -> begin
    PSP = popPS()
    return NEXT
end)

# I/O

defConst("TIB", TIB)
NUMTIB = defNewVar("#TIB", 0)
TOIN = defNewVar(">IN", 0)

KEY = defPrim("KEY", () -> begin
    if mem[TOIN] >= mem[NUMTIB]
        mem[TOIN] = 0
        line = readline()
        mem[NUMTIB] = length(line)
        mem[TIB:(TIB+mem[NUMTIB]-1)] = [Int64(c) for c in collect(line)]
    end

    pushPS(mem[TIB + mem[TOIN]])
    mem[TOIN] += 1

    return NEXT
end)

EMIT = defPrim("EMIT", () -> begin
    print(Char(popPS()))
    return NEXT
end)

WORD = defPrim("WORD", () -> begin
    
    c = -1

    skip_to_end = false
    while true

        callPrim(KEY)
        c = Char(popPS())

        if c == '\\'
            skip_to_end = true
            continue
        end

        if skip_to_end
            if c == '\n'
                skip_to_end = false
            end
            continue
        end

        if c == ' ' || c == '\t'
            continue
        end

        break
    end

    wordAddr = mem[HERE]
    offset = 0

    while true
        mem[wordAddr + offset] = Int64(c)
        offset += 1

        callPrim(KEY)
        c = Char(popPS())

        if c == ' ' || c == '\t' || c == '\n'
            break
        end
    end

    wordLen = offset

    pushPS(wordAddr)
    pushPS(wordLen)

    return NEXT
end)

NUMBER = defPrim("NUMBER", () -> begin

    wordLen = popPS()
    wordAddr = popPS()

    s = ASCIIString([Char(c) for c in mem[wordAddr:(wordAddr+wordLen-1)]])

    try
        pushPS(parse(Int64, s, mem[BASE]))
        pushPS(0)
    catch
        pushPS(1) # Error indication
    end

    return NEXT
end)

# Dictionary searches

FIND = defPrim("FIND", () -> begin

    wordLen = popPS()
    wordAddr = popPS()
    word = ASCIIString([Char(c) for c in mem[wordAddr:(wordAddr+wordLen-1)]])

    latest = mem[LATEST]
    
    while latest>0
        lenAndFlags = mem[latest+1]
        len = lenAndFlags & F_LENMASK
        hidden = (lenAndFlags & F_HIDDEN) == F_HIDDEN

        if hidden || len != wordLen
            latest = mem[latest]
            continue
        end
        
        thisAddr = latest+2
        thisWord = ASCIIString([Char(c) for c in mem[thisAddr:(thisAddr+len-1)]])

        if thisWord == word
            break
        end
    end

    pushPS(latest)

    return NEXT
end)

TOCFA = defPrim(">CFA", () -> begin

    addr = popPS()
    lenAndFlags = mem[addr+1]
    len = lenAndFlags & F_LENMASK

    pushPS(addr + 2 + len)

    return NEXT
end)

TODFA = defWord(">DFA", [TOCFA, INCR, EXIT])

#### VM loop ####
function runVM()
    jmp = NEXT
    while (jmp = callPrim(jmp)) != 0 end
end

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

function printPS()
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
