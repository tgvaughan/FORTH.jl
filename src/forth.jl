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
# blocks, which are represented as anonymous functions, appear as negative indicies
# into the primitives array which contains these functions.

mem = Array{Int64,1}(size_mem)
primitives = Array{Function,1}()
primNames = Array{ASCIIString,1}()

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
end
reg = Reg(mem[RSP0], mem[PSP0], 0, 0)

# Stack manipulation functions

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

function defPrim(f::Function; name="nameless")
    push!(primitives, f)
    push!(primNames, replace(replace(name, "\004", "EOF"), "\n", "\\n"))

    return -length(primitives)
end

callPrim(addr::Int64) = primitives[-addr]()

# Word creation

function createHeader(name::AbstractString, flags::Int64)
    mem[mem[HERE]] = mem[LATEST]
    mem[LATEST] = mem[HERE]
    mem[HERE] += 1

    mem[mem[HERE]] = length(name) | flags; mem[HERE] += 1
    putString(name, mem[HERE]); mem[HERE] += length(name)
end

function defPrimWord(name::AbstractString, f::Function; flags::Int64=0)
    createHeader(name, flags)

    codeWordAddr = mem[HERE]
    mem[codeWordAddr] = defPrim(f, name=name)
    mem[HERE] += 1

    return codeWordAddr
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

# Variable creation

function defExistingVar(name::AbstractString, varAddr::Int64; flags::Int64=0)

    defPrimWord(name, eval(:(() -> begin
        pushPS($(varAddr))
        return NEXT
    end)))
end

function defNewVar(name::AbstractString, initial::Int64; flags::Int64=0)
    createHeader(name, flags)
    
    codeWordAddr = mem[HERE]
    varAddr = mem[HERE] + 1

    f = eval(:(() -> begin
        pushPS($(varAddr))
        return NEXT
    end))

    mem[mem[HERE]] = defPrim(f, name=name); mem[HERE] += 1
    mem[mem[HERE]] = initial; mem[HERE] += 1

    return varAddr, codeWordAddr
end

function defConst(name::AbstractString, val::Int64; flags::Int64=0)
    defPrimWord(name, eval(:(() -> begin
        pushPS($(val))
        return NEXT
    end)))

    return val
end

# Threading Primitives (inner interpreter)

NEXT = defPrim(() -> begin
    reg.W = mem[reg.IP]
    reg.IP += 1
    return mem[reg.W]
end, name="NEXT")

DOCOL = defPrim(() -> begin
    pushRS(reg.IP)
    reg.IP = reg.W + 1
    return NEXT
end, name="DOCOL")

EXIT = defPrimWord("EXIT", () -> begin
    reg.IP = popRS()
    return NEXT
end)

# Basic forth primitives

DROP = defPrimWord("DROP", () -> begin
    popPS()
    return NEXT
end)

SWAP = defPrimWord("SWAP", () -> begin
    a = popPS()
    b = popPS()
    pushPS(a)
    pushPS(b)
    return NEXT
end)

DUP = defPrimWord("DUP", () -> begin
    ensurePSDepth(1)
    pushPS(mem[reg.PSP])
    return NEXT
end)

OVER = defPrimWord("OVER", () -> begin
    ensurePSDepth(2)
    pushPS(mem[reg.PSP-1])
    return NEXT
end)

ROT = defPrimWord("ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(a)
    pushPS(c)
    pushPS(b)
    return NEXT
end)

NROT = defPrimWord("-ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(c)
    return NEXT
end)

TWODROP = defPrimWord("2DROP", () -> begin
    popPS()
    popPS()
    return NEXT
end)

TWODUP = defPrimWord("2DUP", () -> begin
    ensurePSDepth(2)
    a = mem[reg.PSP-1]
    b = mem[reg.PSP]
    pushPS(a)
    pushPS(b)
    return NEXT
end)

TWOSWAP = defPrimWord("2SWAP", () -> begin
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

QDUP = defPrimWord("?DUP", () -> begin
    ensurePSDepth(1)
    val = mem[reg.PSP]
    if val != 0
        pushPS(val)
    end
    return NEXT
end)

INCR = defPrimWord("1+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 1
    return NEXT
end)

DECR = defPrimWord("1-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 1
    return NEXT
end)

INCR2 = defPrimWord("2+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 2
    return NEXT
end)

DECR2 = defPrimWord("2-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 2
    return NEXT
end)

ADD = defPrimWord("+", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a+b)
    return NEXT
end)

SUB = defPrimWord("-", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a-b)
    return NEXT
end)

MUL = defPrimWord("*", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a*b)
    return NEXT
end)

DIVMOD = defPrimWord("/MOD", () -> begin
    b = popPS()
    a = popPS()
    q,r = divrem(a,b)
    pushPS(r)
    pushPS(q)
    return NEXT
end)

EQU = defPrimWord("=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a==b ? -1 : 0)
    return NEXT
end)

NEQU = defPrimWord("<>", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a!=b ? -1 : 0)
    return NEXT
end)

LT = defPrimWord("<", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<b ? -1 : 0)
    return NEXT
end)

GT = defPrimWord(">", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>b ? -1 : 0)
    return NEXT
end)

LE = defPrimWord("<=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<=b ? -1 : 0)
    return NEXT
end)

GE = defPrimWord(">=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>=b ? -1 : 0)
    return NEXT
end)

ZEQU = defPrimWord("0=", () -> begin
    pushPS(popPS() == 0 ? -1 : 0)
    return NEXT
end)

ZNEQU = defPrimWord("0<>", () -> begin
    pushPS(popPS() != 0 ? -1 : 0)
    return NEXT
end)

ZLT = defPrimWord("0<", () -> begin
    pushPS(popPS() < 0 ? -1 : 0)
    return NEXT
end)

ZGT = defPrimWord("0>", () -> begin
    pushPS(popPS() > 0 ? -1 : 0)
    return NEXT
end)

ZLE = defPrimWord("0<=", () -> begin
    pushPS(popPS() <= 0 ? -1 : 0)
    return NEXT
end)

ZGE = defPrimWord("0>=", () -> begin
    pushPS(popPS() >= 0 ? -1 : 0)
    return NEXT
end)

AND = defPrimWord("AND", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a & b)
    return NEXT
end)

OR = defPrimWord("OR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a | b)
    return NEXT
end)

XOR = defPrimWord("XOR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a $ b)
    return NEXT
end)

INVERT = defPrimWord("INVERT", () -> begin
    pushPS(~popPS())
    return NEXT
end)

# Literals

LIT = defPrimWord("LIT", () -> begin
    pushPS(mem[reg.IP])
    reg.IP += 1
    return NEXT
end)

# Memory primitives

STORE = defPrimWord("!", () -> begin
    addr = popPS()
    dat = popPS()
    mem[addr] = dat
    return NEXT
end)

FETCH = defPrimWord("@", () -> begin
    addr = popPS()
    pushPS(mem[addr])
    return NEXT
end)

ADDSTORE = defPrimWord("+!", () -> begin
    addr = popPS()
    toAdd = popPS()
    mem[addr] += toAdd
    return NEXT
end)

SUBSTORE = defPrimWord("-!", () -> begin
    addr = popPS()
    toSub = popPS()
    mem[addr] -= toSub
    return NEXT
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

TOR = defPrimWord(">R", () -> begin
    pushRS(popPS())
    return NEXT
end)

FROMR = defPrimWord("R>", () -> begin
    pushPS(popRS())
    return NEXT
end)

RSPFETCH = defPrimWord("RSP@", () -> begin
    pushPS(reg.RSP)
    return NEXT
end)

RSPSTORE = defPrimWord("RSP!", () -> begin
    RSP = popPS()
    return NEXT
end)

RDROP = defPrimWord("RDROP", () -> begin
    popRS()
    return NEXT
end)

# Parameter Stack

PSPFETCH = defPrimWord("PSP@", () -> begin
    pushPS(reg.PSP)
    return NEXT
end)

PSPSTORE = defPrimWord("PSP!", () -> begin
    PSP = popPS()
    return NEXT
end)

# Working Register

WFETCH = defPrimWord("W@", () -> begin
    pushPS(reg.W)
    return NEXT
end)

WSTORE = defPrimWord("W!", () -> begin
    reg.W = popPS()
    return NEXT
end)

# I/O

sources = Array{Any,1}()
currentSource() = sources[length(sources)]

defConst("TIB", TIB)
NUMTIB, NUMTIB_CFA = defNewVar("#TIB", 0)
TOIN, TOIN_CFA = defNewVar(">IN", 0)
EOF = defConst("EOF", 4)

KEY = defPrimWord("KEY", () -> begin
    if mem[TOIN] >= mem[NUMTIB]
        mem[TOIN] = 0

        if !eof(currentSource())
            line = readline(currentSource())
            mem[NUMTIB] = length(line)
            putString(line, TIB)
        else
            mem[NUMTIB] = 1
            mem[TIB] = EOF
        end
    end

    pushPS(mem[TIB + mem[TOIN]])
    mem[TOIN] += 1

    return NEXT
end)

EMIT = defPrimWord("EMIT", () -> begin
    print(Char(popPS()))
    return NEXT
end)

WORD = defPrimWord("WORD", () -> begin

    eof_char = Char(EOF)
    c = eof_char

    skip_to_end = false
    while true

        callPrim(mem[KEY])
        c = Char(popPS())

        if c == '\\'
            skip_to_end = true
            continue
        end

        if skip_to_end
            if c == '\n' || c == eof_char
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

    if c == '\n' || c == eof_char
        # Treat newline as a special word

        mem[wordAddr + offset] = Int64(c)
        pushPS(wordAddr)
        pushPS(1)
        return NEXT
    end

    while true
        mem[wordAddr + offset] = Int64(c)
        offset += 1

        callPrim(mem[KEY])
        c = Char(popPS())

        if c == ' ' || c == '\t' || c == '\n' || c == eof_char
            # Rewind KEY
            mem[TOIN] -= 1
            break
        end
    end

    wordLen = offset

    pushPS(wordAddr)
    pushPS(wordLen)

    return NEXT
end)

NUMBER = defPrimWord("NUMBER", () -> begin

    wordLen = popPS()
    wordAddr = popPS()

    s = getString(wordAddr, wordLen)

    try
        pushPS(parse(Int64, s, mem[BASE]))
        pushPS(0)
    catch
        pushPS(1) # Error indication
    end

    return NEXT
end)

# Dictionary searches

FIND = defPrimWord("FIND", () -> begin

    wordLen = popPS()
    wordAddr = popPS()
    word = lowercase(getString(wordAddr, wordLen))

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
        thisWord = lowercase(getString(thisAddr, len))

        if lowercase(thisWord) == lowercase(word)
            break
        end
    end

    pushPS(latest)

    return NEXT
end)

TOCFA = defPrimWord(">CFA", () -> begin

    addr = popPS()
    lenAndFlags = mem[addr+1]
    len = lenAndFlags & F_LENMASK

    pushPS(addr + 2 + len)

    return NEXT
end)

TODFA = defWord(">DFA", [TOCFA, INCR, EXIT])

# Branching

BRANCH = defPrimWord("BRANCH", () -> begin
    reg.IP += mem[reg.IP]
    return NEXT
end)

ZBRANCH = defPrimWord("0BRANCH", () -> begin
    if (popPS() == 0)
        reg.IP += mem[reg.IP]
    else
        reg.IP += 1
    end

    return NEXT
end)

# Compilation

CREATE = defPrimWord("CREATE", () -> begin

    wordLen = popPS()
    wordAddr = popPS()
    word = getString(wordAddr, wordLen)

    createHeader(word, 0)

    return NEXT
end)

COMMA = defPrimWord(",", () -> begin
    mem[mem[HERE]] = popPS()
    mem[HERE] += 1

    return NEXT
end)

LBRAC = defPrimWord("[", () -> begin
    mem[STATE] = 0
    return NEXT
end, flags=F_IMMED)

RBRAC = defPrimWord("]", () -> begin
    mem[STATE] = 1
    return NEXT
end, flags=F_IMMED)

HIDDEN = defPrimWord("HIDDEN", () -> begin
    addr = popPS() + 1
    mem[addr] = mem[addr] $ F_HIDDEN
    return NEXT
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
    LATEST_CFA, FETCH, HIDDEN,
    RBRAC,
    EXIT])

SEMICOLON = defWord(";",
    [LIT, EXIT, COMMA,
    LATEST_CFA, FETCH, HIDDEN,
    LBRAC,
    EXIT], flags=F_IMMED)

IMMEDIATE = defPrimWord("IMMEDIATE", () -> begin
    lenAndFlagsAddr = mem[LATEST] + 1
    mem[lenAndFlagsAddr] = mem[lenAndFlagsAddr] $ F_IMMED
    return NEXT
end, flags=F_IMMED)

TICK = defWord("'",
    [STATE_CFA, FETCH, ZBRANCH, 7,
    FROMR, DUP, INCR, TOR, FETCH, EXIT,
    WORD, FIND, TOCFA, EXIT])

# Strings

LITSTRING = defPrimWord("LITSTRING", () -> begin
    len = mem[reg.IP]
    reg.IP += 1
    pushPS(reg.IP)
    pushPS(len)
    reg.IP += len

    return NEXT
end)

TELL = defPrimWord("TELL", () -> begin
    len = popPS()
    addr = popPS()
    str = getString(addr, len)
    print(str)
    return NEXT
end)

# Outer interpreter

EXECUTE = defPrimWord("EXECUTE", () -> begin
    reg.W = popPS()
    return mem[reg.W]
end)

INTERPRET = defPrimWord("INTERPRET", () -> begin

    callPrim(mem[WORD])

    wordName = getString(mem[reg.PSP-1], mem[reg.PSP])
    #println("... ", replace(replace(wordName, "\004", "EOF"), "\n", "\\n"), " ...")

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
            return callPrim(mem[EXECUTE])
        else
            # Append CFA to dictionary
            callPrim(mem[COMMA])
        end
    else
        # Not in dictionary, assume number

        popPS()

        callPrim(mem[NUMBER])

        if popPS() != 0
            println("Parse error at word: '$wordName'")
            return NEXT
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

    return NEXT
end)

QUIT = defWord("QUIT",
    [RSP0_CFA, RSPSTORE,
    INTERPRET,
    BRANCH,-2])

BYE = defPrimWord("BYE", () -> begin
    return 0
end)

NL = defPrimWord("\n", () -> begin
    if mem[STATE] == 0 && currentSource() == STDIN
        println(" ok")
    end
    return NEXT
end, flags=F_IMMED)

INCLUDE = defPrimWord("INCLUDE", () -> begin
    callPrim(mem[WORD])
    wordLen = popPS()
    wordAddr = popPS()
    word = getString(wordAddr, wordLen)

    push!(sources, open(word, "r"))

    # Clear input buffer
    mem[NUMTIB] = 0

    return NEXT
end)

EOF_WORD = defPrimWord("\x04", () -> begin
    if currentSource() != STDIN
        close(currentSource())
    end

    pop!(sources)

    if length(sources)>0
        return NEXT
    else
        return 0
    end
end, flags=F_IMMED)

# Odds and Ends

CHAR = defPrimWord("CHAR", () -> begin
    callPrim(mem[WORD])
    wordLen = popPS()
    wordAddr = popPS()
    word = getString(wordAddr, wordLen)
    pushPS(Int64(word[1]))

    return NEXT
end)

#### VM loop ####
function run()
    # Begin with STDIN as source
    push!(sources, STDIN)

    # Start with IP pointing to first instruction of outer interpreter
    reg.IP = QUIT + 1

    # Primitive processing loop.
    # Everyting else is simply a consequence of this loop!
    jmp = NEXT
    while jmp != 0
        try
            #println("Evaluating prim ", jmp," ", primNames[-jmp])
            jmp = callPrim(jmp)

        catch ex
            if isa(ex, StackUnderflow)
                println("Stack underflow!")

                mem[NUMTIB] = 0
                reg.IP = QUIT + 1
                jmp = NEXT
            else
                rethrow(ex)
            end
        end
    end
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

DOT = defPrimWord(".", () -> begin
    print(popPS())
    return NEXT
end)

#DOTS = defPrimWord(".s", () -> begin
#    printPS()
#    return NEXT
#end)

DUMP = defPrimWord("DUMP", () -> begin
    count = popPS()
    addr = popPS()

    dump(addr, count=count)

    return NEXT
end)

end
