module forth

instream = STDIN

currentLine = ""
currentPos = 0

function readPattern(pattern::Regex)

    if currentPos<1 || currentPos>length(currentLine)
        if eof(instream)
            return ""
        else
            global currentLine = readline(instream)
            global currentPos = 1
        end
    end

    m = match(pattern, currentLine[currentPos:length(currentLine)])
    if m != nothing
        global currentPos += length(m.match)
        return m.match
    else
        return ""
    end
end

readSpaces() = readPattern(r"^([ \t]*)")
readWord() = readPattern(r"^([^\s]+)")
readNewline() = readPattern(r"^(\n)")
readRestOfLine() = readPattern(r"^([^\n]*)")

word = ""
function getWordOrNewline()
    global word = readWord()
    if word == ""
        global word = readNewline()
    end
end

modes = Dict{AbstractString,Function}()
mode = ""

dict = Dict{AbstractString, Function}()
dict["%J"] = () -> begin
    rol = readRestOfLine()
    println("Evaluating '$rol'")
    eval(parse(rol))
end

function interpretPrimitive()
    if haskey(dict, word)
        dict[word]()
        return true
    else
        return false
    end
end
interpretNonPrimitive() = false
interpretNumber() = false

modes["interpret"] = () -> begin
    getWordOrNewline()

    if ! (interpretPrimitive() ||
        interpretNonPrimitive() ||
        interpretNumber())
        println("Error: unknown word '$word'.")
    end
end

function repl()

    global mode = "interpret"
    idx = 1
    while mode != "stop"
        modes[mode]()
    end
end

# Bootstrapping interpreter

firstProg = """%J dict["\\n"] = () -> nothing
%J dict["\\n"] = () -> nothing
%J dict[""] = () -> global mode = "stop"
%J global DS = []
%J global RS = []
"""

instream = IOBuffer(firstProg)
repl()

end
