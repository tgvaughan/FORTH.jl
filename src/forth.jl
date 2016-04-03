module FORTH

function input(prompt::AbstractString="")
    print(prompt)
    strip(readline)
end

dictCore = Set{AbstractString}([
    "bye"
])

function coreEval(word::AbstractString)
    if word == "bye"
end

function repl()

    while true
        line = input()

    end
end

end
