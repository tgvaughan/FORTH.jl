module forth

RS = Array{Int64, 1}()
DS = Array{Int64, 1}()

primitives = Array{Expr,1}()



type Definition
    name::AbstractString
    data::Array{Int64,1}
end


end
