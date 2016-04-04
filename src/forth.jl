module forth

currentLine = ""
currentPos = 0

function nextLine()
    if eof(STDIN)
        return false
    else
        currentLine = readLine()
        currentPos = 1
        return true
    end
end

function readPattern(pattern::Regex)
    m = match(pattern, currentLine)
    pos += length(m.match)
    return m.match
end

modes = Dict{AbstractString,Function}()
mode = ""

function interpretPrimitive()
    if haskey(dict, word)
        dict[word]()
        return true
    else
        return false
end
interpretNonPrimitive() = false
interpretNumber() = false

modes["interpret"] = () -> begin
    getWord()
    
    if ! (interpretPrimitive() ||
        interpretNonPrimitive() ||
        interpretNumber())
        println("Error: unknown word '$word'.")
    end
end

function repl()

    mode = "interpret"
    while mode != "stop"
        modes[mode]()
    end
end

end
