module forth

# VM mem size
size_mem = 1000000 # 1 mega-int

# Buffer sizes
size_RS = 1000   # Return stack size
size_PS = 1000   # Parameter stack size
size_TIB = 1000  # Terminal input buffer size

# Memory arrays
mem = Array{Int64,1}(size_mem)
primitives = Array{Function,1}()
primNames = Array{ASCIIString,1}()

# Built-in variables

nextVarAddr = 1
H = nextVarAddr; nextVarAddr += 1       # Next free memory address
LATEST = nextVarAddr; nextVarAddr += 1  # LFA of latest word in systetm dict

RSP0 = nextVarAddr                  # bottom of RS
PSP0 = RSP0 + size_RS               # bottom of PS
TIB = PSP0 + size_PS                # address of terminal input buffer
mem[H] = TIB + size_TIB             # location of bottom of dictionary
mem[LATEST] = 0                     # no previous definition

DICT = mem[H] # Save bottom of dictionary as constant

# VM registers
type Reg
    RSP::Int64  # Return stack pointer
    PSP::Int64  # Parameter/data stack pointer
    IP::Int64   # Instruction pointer
    W::Int64    # Working register
end
reg = Reg(RSP0, PSP0, 0, 0)

# Stack manipulation functions

function ensurePSDepth(depth::Int64)
    if reg.PSP - PSP0 < depth
        error("Parameter stack underflow.")
    end
end

function ensureRSDepth(depth::Int64)
    if reg.RSP - RSP0 < depth
        error("Return stack underflow.")
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
    push!(primNames, replace(name, "\004", "EOF"))

    return -length(primitives)
end

function callPrim(addr::Int64)
    if addr >=0 || -addr>length(primitives)
        error("Attempted to execute non-existent primitive at address $addr.")
    else
        primitives[-addr]()
    end
end
getPrimName(addr::Int64) = primNames[-addr]

# Word creation functions

F_LENMASK = 31
F_IMMED = 32
F_HIDDEN = 64
NFA_MARK = 128

function createHeader(name::AbstractString, flags::Int64)
    mem[mem[H]] = mem[LATEST]
    mem[LATEST] = mem[H]
    mem[H] += 1

    mem[mem[H]] = length(name) | flags | NFA_MARK; mem[H] += 1
    putString(name, mem[H]); mem[H] += length(name)
end

function defPrimWord(name::AbstractString, f::Function; flags::Int64=0)
    createHeader(name, flags)

    codeWordAddr = mem[H]
    mem[codeWordAddr] = defPrim(f, name=name)
    mem[H] += 1

    return codeWordAddr
end

function defWord(name::AbstractString, wordAddrs::Array{Int64,1}; flags::Int64=0)
    createHeader(name, flags)

    addr = mem[H]
    mem[mem[H]] = DOCOL
    mem[H] += 1

    for wordAddr in wordAddrs
        mem[mem[H]] = wordAddr
        mem[H] += 1
    end

    return addr
end

# Variable creation functions

function defExistingVar(name::AbstractString, varAddr::Int64; flags::Int64=0)

    defPrimWord(name, eval(:(() -> begin
        pushPS($(varAddr))
        return NEXT
    end)))
end

function defNewVar(name::AbstractString, initial::Array{Int64,1}; flags::Int64=0)
    createHeader(name, flags)
    
    codeWordAddr = mem[H]
    varAddr = mem[H] + 1

    mem[mem[H]] = DOVAR; mem[H] += 1
    mem[mem[H]:(mem[H]+length(initial)-1)] = initial; mem[H] += length(initial)

    return varAddr, codeWordAddr
end

defNewVar(name::AbstractString, initial::Int64; flags::Int64=0) =
    defNewVar(name, [initial]; flags=flags)

function defConst(name::AbstractString, val::Int64; flags::Int64=0)
    createHeader(name, flags)

    codeWordAddr = mem[H]

    mem[mem[H]] = DOCON; mem[H] += 1
    mem[mem[H]] = val; mem[H] += 1

    return codeWordAddr
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

DOVAR = defPrim(() -> begin
    pushPS(reg.W + 1)
    return NEXT
end, name="DOVAR")

DOCON = defPrim(() -> begin
    pushPS(mem[reg.W + 1])
    return NEXT
end, name="DOVAR")

EXIT_CFA = defPrimWord("EXIT", () -> begin
    reg.IP = popRS()
    return NEXT
end)

# Dictionary entries for core built-in variables, constants

H_CFA = defExistingVar("H", H)
LATEST_CFA = defExistingVar("LATEST", LATEST)

PSP0_CFA = defConst("PSP0", PSP0)
RSP0_CFA = defConst("RSP0", RSP0)

defConst("DOCOL", DOCOL)
defConst("DOCON", DOCON)
defConst("DOVAR", DOVAR)

defConst("DICT", DICT)
defConst("MEMSIZE", size_mem)

F_IMMED_CFA = defConst("F_IMMED", F_IMMED)
F_HIDDEN_CFA = defConst("F_HIDDEN", F_HIDDEN)
F_LENMASK_CFA = defConst("F_LENMASK", F_LENMASK)
NFA_MARK_CFA = defConst("NFA_MARK", NFA_MARK)

# Basic forth primitives

DROP_CFA = defPrimWord("DROP", () -> begin
    popPS()
    return NEXT
end)

SWAP_CFA = defPrimWord("SWAP", () -> begin
    a = popPS()
    b = popPS()
    pushPS(a)
    pushPS(b)
    return NEXT
end)

DUP_CFA = defPrimWord("DUP", () -> begin
    ensurePSDepth(1)
    pushPS(mem[reg.PSP])
    return NEXT
end)

OVER_CFA = defPrimWord("OVER", () -> begin
    ensurePSDepth(2)
    pushPS(mem[reg.PSP-1])
    return NEXT
end)

ROT_CFA = defPrimWord("ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(c)
    return NEXT
end)

NROT_CFA = defPrimWord("-ROT", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    pushPS(a)
    pushPS(c)
    pushPS(b)
    return NEXT
end)


TWODROP_CFA = defPrimWord("2DROP", () -> begin
    popPS()
    popPS()
    return NEXT
end)

TWODUP_CFA = defPrimWord("2DUP", () -> begin
    ensurePSDepth(2)
    a = mem[reg.PSP-1]
    b = mem[reg.PSP]
    pushPS(a)
    pushPS(b)
    return NEXT
end)

TWOSWAP_CFA = defPrimWord("2SWAP", () -> begin
    a = popPS()
    b = popPS()
    c = popPS()
    d = popPS()
    pushPS(b)
    pushPS(a)
    pushPS(d)
    pushPS(c)
    return NEXT
end)

TWOOVER_CFA = defPrimWord("2OVER", () -> begin
    ensurePSDepth(4)
    a = mem[reg.PSP-3]
    b = mem[reg.PSP-2]
    pushPS(a)
    pushPS(b)
    return NEXT
end)

QDUP_CFA = defPrimWord("?DUP", () -> begin
    ensurePSDepth(1)
    val = mem[reg.PSP]
    if val != 0
        pushPS(val)
    end
    return NEXT
end)

INCR_CFA = defPrimWord("1+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 1
    return NEXT
end)

DECR_CFA = defPrimWord("1-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 1
    return NEXT
end)

INCR2_CFA = defPrimWord("2+", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] += 2
    return NEXT
end)

DECR2_CFA = defPrimWord("2-", () -> begin
    ensurePSDepth(1)
    mem[reg.PSP] -= 2
    return NEXT
end)

ADD_CFA = defPrimWord("+", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a+b)
    return NEXT
end)

SUB_CFA = defPrimWord("-", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a-b)
    return NEXT
end)

MUL_CFA = defPrimWord("*", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a*b)
    return NEXT
end)

DIVMOD_CFA = defPrimWord("/MOD", () -> begin
    b = popPS()
    a = popPS()
    q,r = divrem(a,b)
    pushPS(r)
    pushPS(q)
    return NEXT
end)

TWOMUL_CFA = defPrimWord("2*", () -> begin
    pushPS(popPS() << 1)
    return NEXT
end)

TWODIV_CFA = defPrimWord("2/", () -> begin
    pushPS(popPS() >> 1)
    return NEXT
end)

EQ_CFA = defPrimWord("=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a==b ? -1 : 0)
    return NEXT
end)

NE_CFA = defPrimWord("<>", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a!=b ? -1 : 0)
    return NEXT
end)

LT_CFA = defPrimWord("<", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<b ? -1 : 0)
    return NEXT
end)

GT_CFA = defPrimWord(">", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>b ? -1 : 0)
    return NEXT
end)

LE_CFA = defPrimWord("<=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a<=b ? -1 : 0)
    return NEXT
end)

GE_CFA = defPrimWord(">=", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a>=b ? -1 : 0)
    return NEXT
end)

ZE_CFA = defPrimWord("0=", () -> begin
    pushPS(popPS() == 0 ? -1 : 0)
    return NEXT
end)

ZNE_CFA = defPrimWord("0<>", () -> begin
    pushPS(popPS() != 0 ? -1 : 0)
    return NEXT
end)

ZLT_CFA = defPrimWord("0<", () -> begin
    pushPS(popPS() < 0 ? -1 : 0)
    return NEXT
end)

ZGT_CFA = defPrimWord("0>", () -> begin
    pushPS(popPS() > 0 ? -1 : 0)
    return NEXT
end)

ZLE_CFA = defPrimWord("0<=", () -> begin
    pushPS(popPS() <= 0 ? -1 : 0)
    return NEXT
end)

ZGE_CFA = defPrimWord("0>=", () -> begin
    pushPS(popPS() >= 0 ? -1 : 0)
    return NEXT
end)

AND_CFA = defPrimWord("AND", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a & b)
    return NEXT
end)

OR_CFA = defPrimWord("OR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a | b)
    return NEXT
end)

XOR_CFA = defPrimWord("XOR", () -> begin
    b = popPS()
    a = popPS()
    pushPS(a $ b)
    return NEXT
end)

INVERT_CFA = defPrimWord("INVERT", () -> begin
    pushPS(~popPS())
    return NEXT
end)

# Literals

LIT_CFA = defPrimWord("LIT", () -> begin
    pushPS(mem[reg.IP])
    reg.IP += 1
    return NEXT
end)

# Memory primitives

STORE_CFA = defPrimWord("!", () -> begin
    addr = popPS()
    dat = popPS()
    mem[addr] = dat
    return NEXT
end)

FETCH_CFA = defPrimWord("@", () -> begin
    addr = popPS()
    pushPS(mem[addr])
    return NEXT
end)

ADDSTORE_CFA = defPrimWord("+!", () -> begin
    addr = popPS()
    toAdd = popPS()
    mem[addr] += toAdd
    return NEXT
end)

SUBSTORE_CFA = defPrimWord("-!", () -> begin
    addr = popPS()
    toSub = popPS()
    mem[addr] -= toSub
    return NEXT
end)


# Return Stack

TOR_CFA = defPrimWord(">R", () -> begin
    pushRS(popPS())
    return NEXT
end)

FROMR_CFA = defPrimWord("R>", () -> begin
    pushPS(popRS())
    return NEXT
end)

RFETCH_CFA = defPrimWord("R@", () -> begin
    pushPS(mem[reg.RSP])
    return NEXT
end)

RSPFETCH_CFA = defPrimWord("RSP@", () -> begin
    pushPS(reg.RSP)
    return NEXT
end)

RSPSTORE_CFA = defPrimWord("RSP!", () -> begin
    reg.RSP = popPS()
    return NEXT
end)

RDROP_CFA = defPrimWord("RDROP", () -> begin
    popRS()
    return NEXT
end)

# Parameter Stack

PSPFETCH_CFA = defPrimWord("PSP@", () -> begin
    pushPS(reg.PSP)
    return NEXT
end)

PSPSTORE_CFA = defPrimWord("PSP!", () -> begin
    reg.PSP = popPS()
    return NEXT
end)

# Working Register

WFETCH_CFA = defPrimWord("W@", () -> begin
    pushPS(reg.W)
    return NEXT
end)

WSTORE_CFA = defPrimWord("W!", () -> begin
    reg.W = popPS()
    return NEXT
end)

# I/O

sources = Array{Any,1}()
currentSource() = sources[length(sources)]

EOF_CFA = defPrimWord("\x04", () -> begin
    if currentSource() != STDIN
        close(pop!(sources))
        return NEXT
    else
        return 0
    end
end)

EMIT_CFA = defPrimWord("EMIT", () -> begin
    print(Char(popPS()))
    return NEXT
end)

function raw_mode!(mode::Bool)
    if ccall(:jl_tty_set_mode, Int32, (Ptr{Void}, Int32), STDIN.handle, mode) != 0
        throw("FATAL: Terminal unable to enter raw mode.")
    end
end

function getKey()
    raw_mode!(true)
    byte = readbytes(STDIN, 1)[1]
    raw_mode!(false)

    if byte == 0x0d
        return 0x0a
    elseif byte == 127
        return 0x08
    else
        return byte
    end
end

KEY_CFA = defPrimWord("KEY", () -> begin
    pushPS(Int(getKey()))
    return NEXT
end)

function getLineFromSTDIN()
    line = ""
    while true
        key = Char(getKey())

        if key == '\n'
            print(" ")
            return ASCIIString(line)

        elseif key == '\x04'
            if isempty(line)
                return string("\x04")
            end

        elseif key == '\b'
            if !isempty(line)
                line = line[1:length(line)-1]
                print("\b \b")
            end

        elseif key == '\e'
            # Strip ANSI escape sequence
            nextKey = Char(getKey())
            if nextKey == '['
                while true
                    nextKey = Char(getKey())
                    if nextKey >= '@' || nextKey <= '~'
                        break
                    end
                end
            end

        else
            print(key)
            line = string(line, key)
        end
    end
end

SPAN, SPAN_CFA = defNewVar("SPAN", 0)
EXPECT_CFA = defPrimWord("EXPECT", () -> begin
    maxLen = popPS()
    addr = popPS()

    if currentSource() == STDIN
        line = getLineFromSTDIN()
    else
        if !eof(currentSource())
            line = chomp(readline(currentSource()))
        else
            line = "\x04" # eof
        end
    end

    mem[SPAN] = min(length(line), maxLen)
    putString(line[1:mem[SPAN]], addr)

    return NEXT
end)

BASE, BASE_CFA = defNewVar("BASE", 10)
NUMBER_CFA = defPrimWord("NUMBER", () -> begin
    wordAddr = popPS()+1
    wordLen = mem[wordAddr-1]

    s = getString(wordAddr, wordLen)

    pushPS(parse(Int64, s, mem[BASE]))

    return NEXT
end)

# Dictionary searches

TOCFA_CFA = defPrimWord(">CFA", () -> begin

    addr = popPS()
    lenAndFlags = mem[addr+1]
    len = lenAndFlags & F_LENMASK

    pushPS(addr + 2 + len)

    return NEXT
end)

TOBODY_CFA = defWord(">BODY", [INCR_CFA, EXIT_CFA])

FIND_CFA = defPrimWord("FIND", () -> begin

    countedAddr = popPS()
    wordAddr = countedAddr + 1
    wordLen = mem[countedAddr]
    word = lowercase(getString(wordAddr, wordLen))

    latest = LATEST
    lenAndFlags = 0
    
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

    if latest > 0
        pushPS(latest)
        callPrim(mem[TOCFA_CFA])
        if (lenAndFlags & F_IMMED) == F_IMMED
            pushPS(1)
        else
            pushPS(-1)
        end
    else
        pushPS(countedAddr)
        pushPS(0)
    end

    return NEXT
end)


# Branching

BRANCH_CFA = defPrimWord("BRANCH", () -> begin
    reg.IP += mem[reg.IP]
    return NEXT
end)

ZBRANCH_CFA = defPrimWord("0BRANCH", () -> begin
    if (popPS() == 0)
        reg.IP += mem[reg.IP]
    else
        reg.IP += 1
    end

    return NEXT
end)

# Strings

LITSTRING_CFA = defPrimWord("LITSTRING", () -> begin
    len = mem[reg.IP]
    reg.IP += 1
    pushPS(reg.IP)
    pushPS(len)
    reg.IP += len

    return NEXT
end)

TYPE_CFA = defPrimWord("TYPE", () -> begin
    len = popPS()
    addr = popPS()
    str = getString(addr, len)
    print(str)
    return NEXT
end)

# Interpreter/Compiler-specific I/O

TIB_CFA = defConst("TIB", TIB)
NUMTIB, NUMTIB_CFA = defNewVar("#TIB", 0)
TOIN, TOIN_CFA = defNewVar(">IN", 0)

QUERY_CFA = defWord("QUERY",
    [TIB_CFA, LIT_CFA, 160, EXPECT_CFA,
    SPAN_CFA, FETCH_CFA, NUMTIB_CFA, STORE_CFA,
    LIT_CFA, 0, TOIN_CFA, STORE_CFA,
    EXIT_CFA])

WORD_CFA = defPrimWord("WORD", () -> begin
    delim = popPS()

    # Chew up initial occurrences of delim
    while (mem[TOIN]<mem[NUMTIB] && mem[TIB+mem[TOIN]] == delim)
        mem[TOIN] += 1
    end

    countAddr = mem[H]
    addr = mem[H]+1

    # Start reading in word
    count = 0
    while (mem[TOIN]<mem[NUMTIB])
        mem[addr] = mem[TIB+mem[TOIN]]
        mem[TOIN] += 1

        if (mem[addr] == delim)
            break
        end

        count += 1
        addr += 1
    end

    # Record count
    mem[countAddr] = count
    pushPS(countAddr)

    return NEXT
end)

# Compilation

STATE, STATE_CFA = defNewVar("STATE", 0)

COMMA_CFA = defPrimWord(",", () -> begin
    mem[mem[H]] = popPS()
    mem[H] += 1

    return NEXT
end)

HERE_CFA = defWord("HERE",
    [H_CFA, FETCH_CFA, EXIT_CFA])

HEADER_CFA = defPrimWord("HEADER", () -> begin
    wordAddr = popPS()+1
    wordLen = mem[wordAddr-1]
    word = getString(wordAddr, wordLen)

    createHeader(word, 0)

    return NEXT
end)

CREATE_CFA = defWord("CREATE",
    [LIT_CFA, 32, WORD_CFA, HEADER_CFA,
    LIT_CFA, DOVAR, COMMA_CFA,
    EXIT_CFA])

DODOES = defPrim(() -> begin
    pushRS(reg.IP)
    reg.IP = popPS()
    pushPS(reg.W + 1)
    return NEXT
end, name="DODOES")

DOES_HELPER_CFA = defPrimWord("(DOES>)", () -> begin

    pushPS(mem[LATEST])
    callPrim(mem[TOCFA_CFA])
    cfa = popPS()

    runtimeAddr = popPS()

    mem[cfa] = defPrim(eval(:(() -> begin
        pushPS($(runtimeAddr))
        return DODOES
    end)), name="doesPrim")

    return NEXT
end, flags=F_IMMED)

DOES_CFA = defWord("DOES>",
    [LIT_CFA, LIT_CFA, COMMA_CFA, HERE_CFA, LIT_CFA, 3, ADD_CFA, COMMA_CFA,
    LIT_CFA, DOES_HELPER_CFA, COMMA_CFA, LIT_CFA, EXIT_CFA, COMMA_CFA, EXIT_CFA],
    flags=F_IMMED)

LBRAC_CFA = defPrimWord("[", () -> begin
    mem[STATE] = 0
    return NEXT
end, flags=F_IMMED)

RBRAC_CFA = defPrimWord("]", () -> begin
    mem[STATE] = 1
    return NEXT
end, flags=F_IMMED)

HIDDEN_CFA = defPrimWord("HIDDEN", () -> begin
    lenAndFlagsAddr = mem[LATEST] + 1
    mem[lenAndFlagsAddr] = mem[lenAndFlagsAddr] $ F_HIDDEN
    return NEXT
end)

COLON_CFA = defWord(":",
    [LIT_CFA, 32, WORD_CFA,
    HEADER_CFA,
    LIT_CFA, DOCOL, COMMA_CFA,
    HIDDEN_CFA,
    RBRAC_CFA,
    EXIT_CFA])

SEMICOLON_CFA = defWord(";",
    [LIT_CFA, EXIT_CFA, COMMA_CFA,
    HIDDEN_CFA,
    LBRAC_CFA,
    EXIT_CFA], flags=F_IMMED)

IMMEDIATE_CFA = defPrimWord("IMMEDIATE", () -> begin
    lenAndFlagsAddr = mem[LATEST] + 1
    mem[lenAndFlagsAddr] = mem[lenAndFlagsAddr] $ F_IMMED
    return NEXT
end, flags=F_IMMED)

# Outer Interpreter

EXECUTE_CFA = defPrimWord("EXECUTE", () -> begin
    reg.W = popPS()
    return mem[reg.W]
end)

INTERPRET_CFA = defWord("INTERPRET",
    [LIT_CFA, 32, WORD_CFA, # Read next space-delimited word

    DUP_CFA, FETCH_CFA, ZE_CFA, ZBRANCH_CFA, 3,
        DROP_CFA, EXIT_CFA, # Exit if TIB is exhausted

    STATE_CFA, FETCH_CFA, ZBRANCH_CFA, 24,
        # Compiling
        FIND_CFA, QDUP_CFA, ZBRANCH_CFA, 13,

            # Found word. 
            LIT_CFA, -1, EQ_CFA, INVERT_CFA, ZBRANCH_CFA, 4,

                # Immediate: Execute!
                EXECUTE_CFA, BRANCH_CFA, -26,

                # Not immediate: Compile!
                COMMA_CFA, BRANCH_CFA, -29,

            # No word found, parse number
            NUMBER_CFA, LIT_CFA, LIT_CFA, COMMA_CFA, COMMA_CFA, BRANCH_CFA, -36,
        
       # Interpreting
        FIND_CFA, QDUP_CFA, ZBRANCH_CFA, 5,

            # Found word. Execute!
            DROP_CFA, EXECUTE_CFA, BRANCH_CFA, -44,

            # No word found, parse number and leave on stack
            NUMBER_CFA, BRANCH_CFA, -47,
    EXIT_CFA])

PROMPT_CFA = defPrimWord("PROMPT", () -> begin
    if (mem[STATE] == 0 && currentSource() == STDIN)
        println(" ok")
    end

    return NEXT
end)

QUIT_CFA = defWord("QUIT",
    [LIT_CFA, 0, STATE_CFA, STORE_CFA,
    LIT_CFA, 0, NUMTIB_CFA, STORE_CFA,
    RSP0_CFA, RSPSTORE_CFA,
    QUERY_CFA,
    INTERPRET_CFA, PROMPT_CFA,
    BRANCH_CFA,-4])

ABORT_CFA = defWord("ABORT",
    [PSP0_CFA, PSPSTORE_CFA, QUIT_CFA])

BYE_CFA = defPrimWord("BYE", () -> begin
    println("\nBye!")
    return 0
end)

# File I/O

INCLUDE_CFA = defPrimWord("INCLUDE", () -> begin
    pushPS(32)
    callPrim(mem[WORD_CFA])
    wordAddr = popPS()+1
    wordLen = mem[wordAddr-1]
    word = getString(wordAddr, wordLen)

    fname = word
    if !isfile(fname)
        fname = Pkg.dir("forth","src",word)
        if !isfile(fname)
            error("No file named $word found in current directory or package source directory.")
        end
    end
    push!(sources, open(fname, "r"))

    # Clear input buffer
    mem[NUMTIB] = 0

    return NEXT
end)


#### VM loop ####

initialized = false
initFileName = nothing
if isfile("lib.4th")
    initFileName = "lib.4th"
elseif isfile(Pkg.dir("forth","src", "lib.4th"))
    initFileName = Pkg.dir("forth","src","lib.4th")
end

function run(;initialize=true)
    # Begin with STDIN as source
    push!(sources, STDIN)

    global initialized, initFileName
    if !initialized && initialize
        if initFileName != nothing
            print("Including definitions from $initFileName...")
            push!(sources, open(initFileName, "r"))
            initialized = true
        else
            println("No library file found. Only primitive words available.")
        end
    end

    # Start with IP pointing to first instruction of outer interpreter
    reg.IP = QUIT_CFA + 1

    # Primitive processing loop.
    # Everyting else is simply a consequence of this loop!
    jmp = NEXT
    while jmp != 0
        try
            #println("Entering prim $(getPrimName(jmp))")
            jmp = callPrim(jmp)

        catch ex
            showerror(STDOUT, ex)
            println()

            while !isempty(sources) && currentSource() != STDIN
                close(pop!(sources))
            end

            # QUIT
            reg.IP = ABORT_CFA + 1
            jmp = NEXT
        end
    end
end

# Debugging tools

TRACE_CFA = defPrimWord("TRACE", () -> begin
    println("reg.W: $(reg.W) reg.IP: $(reg.IP)")
    print("PS: "); printPS()
    print("RS: "); printRS()
    print("[paused]")
    readline()

    return NEXT
end)

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
    count = reg.PSP - PSP0

    if count > 0
        print("<$count>")
        for i in (PSP0+1):reg.PSP
            print(" $(mem[i])")
        end
        println()
    else
        println("Parameter stack empty")
    end
end

function printRS()
    count = reg.RSP - RSP0

    if count > 0
        print("<$count>")
        for i in (RSP0+1):reg.RSP
            print(" $(mem[i])")
        end
        println()
    else
        println("Return stack empty")
    end
end

DUMP = defPrimWord("DUMP", () -> begin
    count = popPS()
    addr = popPS()

    println()
    dump(addr, count=count)

    return NEXT
end)

end
