using Pkg

using DifferentialEquations
using CSV
using DataFrames
using Distributions
using LinearAlgebra
using Tables
using Dates
using Random
using Plots

 

cd("/Users/katherinedixon/Documents/StuffINeed/_Research/_DFTM_2023/Synchrony/_julia")

testing_df = DataFrame(CSV.File("_data/jtest.csv", missingstring = "NA"))
norm_val = 0.1

testing_df = dropmissing(testing_df)

trap_data = testing_df.trap_mean

replace!(trap_data,0=>1e-3)

const μ = 0.55
const δ = 0.083
const k = Int(20)
const ν = 0.5
const C = 2.0

function onestrain_SEIR(du,u,p,t)
    ν, δ, k, C, S0, μ = p

    du[1] = -ν*u[1]*u[2 + k]*((u[1]/S0)^(C^2)) # S
    du[2] = ν*u[1]*u[2 + k]*((u[1]/S0)^(C^2)) - k*δ*u[2]

    for i in 2:k
        du[2 + i - 1] = (k*δ*u[2 + i - 2]) - k*δ*u[2 + i - 1]                      # E1 second
    end

    du[2 + k] = k*δ*u[2 + k - 1] - μ*u[2 + k]                        # P1
    du[3 + k] = k*δ*u[2 + k - 1]                                            # psum1
    nothing
end

function run_ode(S0, p0)
    e = zeros(k)

    u0 = hcat(S0, transpose(e),p0, 0.0)
    p = (ν, δ, k, C, S0, μ)

    num_eq = length(u0)

    tspan = (0.0,70.0)
    prob = ODEProblem{true}(onestrain_SEIR,u0,tspan,p)
    sol = solve(prob, Tsit5(), abstol = 1e-8, reltol = 1e-8, isoutofdomain=(u,p,t) -> any(x -> x < 0, u), save_everystep = false, verbose = false)

    return(sol,num_eq)

end

function inter_annual(S_end, I_end, ϕ, λ)

    Nt1 = λ*S_end

    Zt1 = ϕ*I_end #+ gamma*Z1old

    ow_list = [Nt1, Zt1]

    return ow_list
end

function run_simulation(S_end,I_end,lambda)

    over_wintering = inter_annual(S_end,I_end,7.4,lambda)

    output = run_ode(over_wintering[1],over_wintering[2])
    
    simulation = output[1]
    num_equations = output[2]
    
    end_t = length(simulation.t)::Int64

    new_S_end = simulation[1,end_t]::Float64
    new_I_end = simulation[num_equations,end_t]::Float64

    return(new_S_end,new_I_end)

end


l_seq = -4:0.05:2

p2_end = 0.1
s2_store = zeros(length(trap_data)-1)
p1_store = zeros(length(trap_data)-1)
lambda_store = zeros(length(trap_data)-1)
min_store = zeros(length(trap_data)-1)

for i in 1:(length(trap_data)-1)

    s2_pred = zeros(length(l_seq))
    p2_pred = zeros(length(l_seq))
    diff_s2 = zeros(length(l_seq))

    s1_end = trap_data[i]*norm_val
    s2_end = trap_data[i + 1]*norm_val
    p1_end = p2_end

    for j in 1:length(l_seq)

        lamb = 10^l_seq[j]
        sim_op = run_simulation(s1_end,p1_end,lamb)

        s2_pred[j] = sim_op[1]
        p2_pred[j] = sim_op[2]
        diff_s2[j] = abs(sim_op[1] - s2_end)

    end

    min_store[i] = findmin(diff_s2)[1]
    min_ind = findmin(diff_s2)[2]

    lambda_store[i] = 10^l_seq[min_ind]
    p2_end = p2_pred[min_ind]


end

plot(lambda_store)

plot(min_store)