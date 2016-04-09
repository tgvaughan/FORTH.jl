module forth

RS = Array{Int64, 1}()
DS = Array{Int64, 1}()

primitives = Array{Expr,1}()
memory = Array{Int,1}()
headers = Array{Tuple{AbstractString, Int},1}

function addPrim(name::AbstractString, expr::Expr)
    push!(primitives, expr)
    push!(memory, -length(primitives))
    push!(headers, length(memory))
end

addPrim("docol", :(begin

end))

end
