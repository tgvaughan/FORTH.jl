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

# Handy functions for adding/retrieving strings to/from memory.

getString(addr::Int64, len::Int64) = ASCIIString([Char(c) for c in mem[addr:(addr+len-1)]])
function putString(str::ASCIIString, addr::Int64)
    mem[addr:(addr+length(str)-1)] = [Int64(c) for c in str]
end

# Primitive creation and calling functions

function createHeader(name::AbstractString, flags::Int64)
    mem[mem[HERE]] = mem[LATEST]
    mem[LATEST] = mem[HERE]
    mem[HERE] += 1

    mem[mem[HERE]] = length(name) | flags; mem[HERE] += 1
    putString(name, mem[HERE]); mem[HERE] += length(name)
end

function defPrim(name::AbstractString, f::Function; flags::Int64=0)
    createHeader(name, flags)

    codeWordAddr = mem[HERE]
    push!(primitives, f)
    mem[codeWordAddr] = -length(primitives)
    mem[HERE] += 1

    return codeWordAddr
end

callPrim(addr::Int64) = primitives[-addr]()

function defExistingVar(name::AbstractString, varAddr::Int64; flags::Int64=0)
    defPrim(name, eval(:(() -> begin
        pushPS($(varAddr))
        return mem[NEXT]
    end)))
end

function defNewVar(name::AbstractString, initial::Int64; flags::Int64=0)
    createHeader(name, flags)
    
    codeWordAddr = mem[HERE]
    varAddr = mem[HERE] + 1
    push!(primitives, eval(:(() -> begin
        pushPS($(varAddr))
        return mem[NEXT]
    end)))
    mem[mem[HERE]] = -length(primitives); mem[HERE] += 1

    mem[mem[HERE]] = initial; mem[HERE] += 1

    return varAddr, codeWordAddr
end

function defConst(name::AbstractString, val::Int64; flags::Int64=0)
    defPrim(name, eval(:(() -> begin
        pushPS($(val))
        return mem[NEXT]
    end)))

    return val
end

function defWord(name::AbstractString, wordAddrs::Array{Int64,1}; flags::Int64=0)
    createHeader(name, flags)

    addr = mem[HERE]
    mem[mem[HERE]] = mem[DOCOL]
    mem[HERE] += 1

    for wordAddr in wordAddrs
        mem[mem[HERE]] = wordAddr
        mem[HERE] += 1
    end

    return addr
end

# Threading Primitives (inner interpreter)

NEXT = defPrim("NEXT", () -> begin
    reg.W = mem[reg.IP]
    reg.IP += 1
    return mem[reg.W]
end)

DOCOL = defPrim("DOCOL", () -> begin
    pushRS(reg.IP)
    reg.IP = reg.W + 1
    return mem[NEXT]
end)

EXIT = defPrim("EXIT", () -> begin
    reg.IP = popRS()
    return mem[NEXT]
end)

# Basic forth primitives

DROP = defPrim("DROP", () -> begin
    popPS()
    return mem[NEXT]
end)

SWAP = defPrim("SWAP", () -> begin
    a = popPS()
    b = popPS()
    pushPS(a)
    pushPS(b)
    return mem[NEXT]
end)

DUP = defPrim("DUP", () -> begin
    pushPS(mem[reg.PSP])
    return mem[NEXT]
end)

OVER = defPrim("OVER", () -> begin
    ensurePSDepth(2)
    pushPS(mem[reg.PSP-1])
    return mem[NEXT]
end)

ROT = defPrim("ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(a)
    pushPS(c)
    pushPS(b)
    return mem[NEXT]
end)

NROT = defPrim("-ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(c)
    return mem[NEXT]
end)

TWODROP = defPrim("2DROP", () -> begin
    popPS()
    popPS()
    return mem[NEXT]
end)

TWODUP = defPrim("2DUP", () -> begin
    ensurePSDepth(2)
    a = mem[reg.PSP-1]
    b = mem[reg.PSP]
    pushPS(a)
    pushPS(b)
    return mem[NEXT]
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
    return mem[NEXT]
end)

QDUP = defPrim("?DUP", () -> begin
    ensurePSDepth(1)
    val = mem[reg.PSP]
    if val != 0
        pushPS(val)
    end
    return mem[NEXT]
end)

INCR = defPrim("1+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 1
    return mem[NEXT]
end)

DECR = defPrim("1-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 1
    return mem[NEXT]
end)

INCR2 = defPrim("2+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 2
    return mem[NEXT]
end)

DECR2 = defPrim("2-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 2
    return mem[NEXT]
end)

ADD = defPrim("+", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a+b)
    return mem[NEXT]
end)

SUB = defPrim("-", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a-b)
    return mem[NEXT]
end)

MUL = defPrim("*", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a*b)
    return mem[NEXT]
end)

DIVMOD = defPrim("/MOD", () -> begin
    b = popPS()
    a = popPS()
    q,r = divrem(a,b)
    pushPS(r)
    pushPS(q)
    return mem[NEXT]
end)

EQU = defPrim("=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a==b ? -1 : 0)
    return mem[NEXT]
end)

NEQU = defPrim("<>", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a!=b ? -1 : 0)
    return mem[NEXT]
end)

LT = defPrim("<", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<b ? -1 : 0)
    return mem[NEXT]
end)

GT = defPrim(">", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>b ? -1 : 0)
    return mem[NEXT]
end)

LE = defPrim("<=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<=b ? -1 : 0)
    return mem[NEXT]
end)

GE = defPrim(">=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>=b ? -1 : 0)
    return mem[NEXT]
end)

ZEQU = defPrim("0=", () -> begin
    pushPS(popPS() == 0 ? -1 : 0)
    return mem[NEXT]
end)

ZNEQU = defPrim("0<>", () -> begin
    pushPS(popPS() != 0 ? -1 : 0)
    return mem[NEXT]
end)

ZLT = defPrim("0<", () -> begin
    pushPS(popPS() < 0 ? -1 : 0)
    return mem[NEXT]
end)

ZGT = defPrim("0>", () -> begin
    pushPS(popPS() > 0 ? -1 : 0)
    return mem[NEXT]
end)

ZLE = defPrim("0<=", () -> begin
    pushPS(popPS() <= 0 ? -1 : 0)
    return mem[NEXT]
end)

ZGE = defPrim("0>=", () -> begin
    pushPS(popPS() >= 0 ? -1 : 0)
    return mem[NEXT]
end)

AND = defPrim("AND", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a & b)
    return mem[NEXT]
end)

OR = defPrim("OR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a | b)
    return mem[NEXT]
end)

XOR = defPrim("XOR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a $ b)
    return mem[NEXT]
end)

INVERT = defPrim("INVERT", () -> begin
    pushPS(~popPS())
    return mem[NEXT]
end)

# Literals

LIT = defPrim("LIT", () -> begin
    pushPS(mem[reg.IP])
    reg.IP += 1
    return mem[NEXT]
end)

# Memory primitives

STORE = defPrim("!", () -> begin
    addr = popPS()
    dat = popPS()
    mem[addr] = dat
    return mem[NEXT]
end)

FETCH = defPrim("@", () -> begin
    addr = popPS()
    pushPS(mem[addr])
    return mem[NEXT]
end)

ADDSTORE = defPrim("+!", () -> begin
    addr = popPS()
    toAdd = popPS()
    mem[addr] += toAdd
    return mem[NEXT]
end)

SUBSTORE = defPrim("-!", () -> begin
    addr = popPS()
    toSub = popPS()
    mem[addr] -= toSub
    return mem[NEXT]
end)


# Built-in variables

HERE_CFA = defExistingVar("HERE", HERE)
LATEST_CFA = defExistingVar("LATEST", LATEST)
PSP0_CFA = defExistingVar("PSP0", PSP0)
RSP0_CFA = defExistingVar("RSP0", RSP0)
STATE, STATE_CFA = defNewVar("STATE", 0)
BASE, BASE_CFA = defNewVar("BASE", 10)

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
    return mem[NEXT]
end)

FROMR = defPrim("R>", () -> begin
    pushPS(popRS())
    return mem[NEXT]
end)

RSPFETCH = defPrim("RSP@", () -> begin
    pushPS(reg.RSP)
    return mem[NEXT]
end)

RSPSTORE = defPrim("RSP!", () -> begin
    RSP = popPS()
    return mem[NEXT]
end)

RDROP = defPrim("RDROP", () -> begin
    popRS()
    return mem[NEXT]
end)

# Parameter Stack

PSPFETCH = defPrim("PSP@", () -> begin
    pushPS(reg.PSP)
    return mem[NEXT]
end)

PSPSTORE = defPrim("PSP!", () -> begin
    PSP = popPS()
    return mem[NEXT]
end)

# I/O

defConst("TIB", TIB)
NUMTIB, NUMTIB_CFA = defNewVar("#TIB", 0)
TOIN, TOIN_CFA = defNewVar(">IN", 0)

KEY = defPrim("KEY", () -> begin
    if mem[TOIN] >= mem[NUMTIB]
        mem[TOIN] = 0
        line = readline()
        mem[NUMTIB] = length(line)
        putString(line, TIB)
    end

    pushPS(mem[TIB + mem[TOIN]])
    mem[TOIN] += 1

    return mem[NEXT]
end)

EMIT = defPrim("EMIT", () -> begin
    print(Char(popPS()))
    return mem[NEXT]
end)

WORD = defPrim("WORD", () -> begin
    
    c = -1

    skip_to_end = false
    while true

        callPrim(mem[KEY])
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

        callPrim(mem[KEY])
        c = Char(popPS())

        if c == ' ' || c == '\t' || c == '\n'
            break
        end
    end

    wordLen = offset

    pushPS(wordAddr)
    pushPS(wordLen)

    return mem[NEXT]
end)

NUMBER = defPrim("NUMBER", () -> begin

    wordLen = popPS()
    wordAddr = popPS()

    s = getString(wordAddr, wordLen)

    try
        pushPS(parse(Int64, s, mem[BASE]))
        pushPS(0)
    catch
        pushPS(1) # Error indication
    end

    return mem[NEXT]
end)

# Dictionary searches

FIND = defPrim("FIND", () -> begin

    wordLen = popPS()
    wordAddr = popPS()
    word = getString(wordAddr, wordLen)

    latest = LATEST
    
    i = 0
    while (latest = mem[latest]) > 0
        lenAndFlags = mem[latest+1]
        len = lenAndFlags & F_LENMASK
        hidden = (lenAndFlags & F_HIDDEN) == F_HIDDEN

        if hidden || len != wordLen
            continue
        end
        
        thisAddr = latest+2
        thisWord = getString(thisAddr, len)

        if thisWord == word
            break
        end
    end

    pushPS(latest)

    return mem[NEXT]
end)

TOCFA = defPrim(">CFA", () -> begin

    addr = popPS()
    lenAndFlags = mem[addr+1]
    len = lenAndFlags & F_LENMASK

    pushPS(addr + 2 + len)

    return mem[NEXT]
end)

TODFA = defWord(">DFA", [TOCFA, INCR, EXIT])

# Compilation

CREATE = defPrim("CREATE", () -> begin

    wordLen = popPS()
    wordAddr = popPS()
    word = getString(wordAddr, wordLen)

    createHeader(word, 0)

    return mem[NEXT]
end)

COMMA = defPrim(",", () -> begin
    mem[mem[HERE]] = popPS()
    mem[HERE] += 1

    return mem[NEXT]
end)

LBRAC = defPrim("[", () -> begin
    mem[STATE] = 0
    return mem[NEXT]
end, flags=F_IMMED)

RBRAC = defPrim("]", () -> begin
    mem[STATE] = 1
    return mem[NEXT]
end, flags=F_IMMED)

HIDDEN = defPrim("HIDDEN", () -> begin
    addr = popPS() + 1
    mem[addr] = mem[addr] $ F_HIDDEN
    return mem[NEXT]
end)

HIDE = defWord("HIDE",
    [WORD,
    FIND,
    HIDDEN,
    EXIT])

COLON = defWord(":",
    [WORD,
    CREATE,
    LIT, DOCOL, COMMA,
    LATEST, FETCH, HIDDEN,
    RBRAC,
    EXIT])

SEMICOLON = defWord(";",
    [LIT, EXIT, COMMA,
    LATEST, FETCH, HIDDEN,
    LBRAC,
    EXIT], flags=F_IMMED)

IMMEDIATE = defPrim("IMMEDIATE", () -> begin
    lenAndFlagsAddr = mem[LATEST] + 1
    mem[lenAndFlagsAddr] = mem[lenAndFlagsAddr] $ F_IMMED
    return mem[NEXT]
end, flags=F_IMMED)

TICK = defWord("'", [WORD, FIND, TOCFA, EXIT])

# Branching

BRANCH = defPrim("BRANCH", () -> begin
    reg.IP += mem[reg.IP]
    return mem[NEXT]
end)

ZBRANCH = defPrim("0BRANCH", () -> begin
    if (popPS() == 0)
        reg.IP += mem[reg.IP]
    else
        reg.IP += 1
    end

    return mem[NEXT]
end)

# Strings

LITSTRING = defPrim("LITSTRING", () -> begin
    len = mem[reg.IP]
    reg.IP += 1
    pushPS(reg.IP)
    pushPS(len)
    reg.IP += len

    return mem[NEXT]
end)

TELL = defPrim("TELL", () -> begin
    len = popPS()
    addr = popPS()
    str = getString(addr, len)
    print(str)
    return mem[NEXT]
end)

# Outer interpreter

INTERPRET = defPrim("INTERPRET", () -> begin

    callPrim(mem[WORD])
    callPrim(mem[TWODUP])
    callPrim(mem[FIND])

    wordAddr = mem[reg.PSP]

    if wordAddr>0
        # Word in dictionary

        isImmediate = (mem[wordAddr+1] & F_IMMED) != 0
        callPrim(mem[TOCFA])

        callPrim(mem[ROT]) # get rid of extra copy of word string details
        popPS()
        popPS()

        if mem[STATE] == 0 || isImmediate
            # Execute!
            return mem[popPS()]
        else
            # Append CFA to dictionary
            callPrim(mem[COMMA])
        end
    else
        # Not in dictionary, assume number

        popPS()
        callPrim(mem[NUMBER])

        if popPS() != 0
            println("Parse error!")
            return mem[NEXT]
        end

        if mem[STATE] == 0
            # Number already on stack!
        else
            # Append literal to dictionary
            pushPS(LIT)
            callPrim(mem[COMMA])
            callPrim(mem[COMMA])
        end
    end

    return mem[NEXT]
end)

QUIT = defWord("QUIT",
    [RSP0_CFA, RSPSTORE,
    INTERPRET,
    BRANCH,-2])

#### VM loop ####
function runVM()
    jmp = mem[NEXT]
    while (jmp = callPrim(jmp)) != 0 end
end

# Debugging tools

function dump(startAddr::Int64; count::Int64 = 100, cellsPerLine::Int64 = 10)
    chars = Array{Char,1}(cellsPerLine)

    lineStartAddr = cellsPerLine*div((startAddr-1),cellsPerLine) + 1
    endAddr = startAddr + count - 1

    q, r = divrem((endAddr-lineStartAddr+1), cellsPerLine)
    numLines = q + (r > 0 ? 1 : 0)

    i = lineStartAddr
    for l in 1:numLines
        print(i,":")

        for c in 1:cellsPerLine
            if i >= startAddr && i <= endAddr
                print("\t",mem[i])
                if mem[i]>=32 && mem[i]<128
                    chars[c] = Char(mem[i])
                else
                    chars[c] = '.'
                end
            else
                print("\t")
                chars[c] = ' '
            end

            i += 1
        end

        println("\t", ASCIIString(chars))
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

function printRS()
    count = reg.RSP - mem[RSP0]

    if count > 0
        print("<$count>")
        for i in (mem[RSP0]+1):reg.RSP
            print(" $(mem[i])")
        end
        println()
    else
        println("Return stack empty")
    end
end

end
