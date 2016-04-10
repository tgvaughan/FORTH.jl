module forth


RS = Array{Int64, 1}(1024)
RSP = 1

PS = Array{Int64, 1}(1024)
PSP = 1

IP = 0
W = 0
X = 0

jmp = nothing

primitives = Array{Expr,1}()
memory = Array{Int64,1}()
headers = Array{Tuple{AbstractString, Int64},1}()

function addPrim(name::AbstractString, expr::Expr)
    push!(primitives, expr)
    push!(memory, -length(primitives))
    push!(headers, (name, length(memory)))
    
    return expr
end

NEXT = addPrim("next", :(begin
    W = memory[IP]
    IP += 1
    X = memory[W]
    jmp = primitives[-X]
end))

DOCOL = addPrim("docol", :(begin
    push!(RS, IP)
    IP = W + 1
    jmp = NEXT
end))

EXIT = addPrim("exit", :(begin
    IP = pop!(RS)
    jmp = NEXT
end))




# VM loop
jmp = NEXT
function runVM()
    while true
        eval(jmp)
    end
end

end
