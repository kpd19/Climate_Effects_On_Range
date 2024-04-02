using Pkg

#Pkg.add("Optim")

using DifferentialEquations
using CSV
using DataFrames
using Distributions
using LinearAlgebra
using Tables
using Dates
using Random
using Plots
using Peaks
using Optim

function findpeaks(
    y :: AbstractVector{T},
    x :: AbstractVector{S} = collect(1:length(y))
    ;
    min_height :: T = minimum(y),
    min_prom :: T = zero(y[1]),
    min_dist :: S = zero(x[1]),
    threshold :: T = zero(y[1]),
   ) where {T <: Real, S}

    dy = diff(y)

    peaks = in_threshold(dy, threshold)

    yP = y[peaks]
    peaks = with_prominence(y, peaks, min_prom)

    #minimal height refinement
    peaks = peaks[y[peaks] .> min_height]
    yP = y[peaks]

    peaks = with_distance(peaks, x, y, min_dist)

    peaks = sort(peaks)

    return(peaks, y[peaks])
end

function in_threshold(dy :: AbstractVector{T}, threshold :: T,) where {T <: Real}

    peaks = 1:length(dy) |> collect

    k = 0
    for i = 2:length(dy)
        if dy[i] <= -threshold && dy[i-1] >= threshold
            k += 1
            peaks[k] = i
        end
    end
    peaks[1:k]
end

function with_prominence(
          y :: AbstractVector{T},
          peaks :: AbstractVector{Int},
          min_prom::T,
         ) where {T <: Real}

    #minimal prominence refinement
    peaks[prominence(y, peaks) .> min_prom]
end


function prominence(y::AbstractVector{T}, peaks::AbstractVector{Int}) where {T <: Real}
    yP = y[peaks]
    proms = zero(yP)

    for (i, p) in enumerate(peaks)
        lP, rP = 1, length(y)
        for j = (i-1):-1:1
            if yP[j] > yP[i]
                lP = peaks[j]
                break
            end
        end
        ml = minimum(y[lP:p])
        for j = (i+1):length(yP)
            if yP[j] > yP[i]
                rP = peaks[j]
                break
            end
        end
        mr = minimum(y[p:rP])
        ref = max(mr,ml)
        proms[i] = yP[i] - ref
    end

    proms
end


function with_distance(
    peaks :: AbstractVector{Int},
    x :: AbstractVector{S},
    y :: AbstractVector{T},
    min_dist::S,
    ) where {T <: Real, S}

    peaks2del = zeros(Bool, length(peaks))
    inds = sortperm(y[peaks], rev=true)
    permute!(peaks, inds)
    for i = 1:length(peaks)
        for j = 1:(i-1)
            if abs(x[peaks[i]] - x[peaks[j]]) <= min_dist
                if !peaks2del[j]
                    peaks2del[i] = true
                end
            end
        end
    end

    peaks[.!peaks2del]
end

cd("/Users/katherinedixon/Documents/StuffINeed/_Research/_DFTM_2023/Synchrony/_julia")

model_preds = DataFrame(CSV.File("_data/gam_predictions.csv"))

num_pops = length(model_preds.manual_id)

global μ = 0.55
global δ = 0.083
global k = Int(20)
#global sig = 0.5

global max_gam = maximum(model_preds.pred_gam)
global min_gam = minimum(model_preds.pred_gam)

function onestrain_SEIR(du,u,p,t)
    ν, δ, k, C, S0, μ = p

    du[1] = -ν*abs(u[1])*u[2 + k]*((abs(u[1])/S0)^(C^2)) # S
    du[2] = ν*abs(u[1])*u[2 + k]*((abs(u[1])/S0)^(C^2)) - k*δ*u[2]

    for i in 2:k
        du[2 + i - 1] = (k*δ*u[2 + i - 2]) - k*δ*u[2 + i - 1]                      # E1 second
    end

    du[2 + k] = k*δ*u[2 + k - 1] - μ*u[2 + k]                        # P1
    du[3 + k] = k*δ*u[2 + k - 1]                                            # psum1
    nothing
end

function run_ode(S0, p0, C)
    e = zeros(k)

    u0 = hcat(S0, transpose(e),p0, 0.0)
    p = (ν, δ, k, C, S0, μ)

    num_eq = length(u0)

    tspan = (0.0,70.0)
    prob = ODEProblem{true}(onestrain_SEIR,u0,tspan,p)
    sol = solve(prob, Tsit5(), abstol = 1e-8, reltol = 1e-8, isoutofdomain=(u,p,t) -> any(x -> x < 0, u), save_everystep = false, verbose = false)

    return(sol,num_eq)

end

function inter_annual(S_end, I_end, Zold, N_old, ϕ, γ, σ,  λ, a, b)

    en = rand(Normal(0,σ),1)
    stoch = exp(en[1])

    Nt1 = (stoch*λ*S_end)*(1 - (2*a*b*N_old)/(b^2 + N_old^2))

    Zt1 = ϕ*I_end + γ*Zold

    ow_list = [Nt1, Zt1]

    return ow_list
end


function get_lambda(k1,k2,amp)

    lambda = -k2*log(amp/k1)
    
    return lambda
end


function sqerror(betas, X, Y)
    err = 0.0
    for i in 1:length(X)
        pred_i = X[i]*betas[1] + betas[2]
        err += (Y[i] - pred_i)^2
    end
    return err
end

function run_simulation_single(phi::Float64, lambda::Float64, C_val::Float64, sigma, a::Float64, gen::Int64, zpass::Vector{Float64},spass::Vector{Float64})

    S_pop = zeros(num_pops, gen + 1)::Matrix{Float64}
    Z_pop = zeros(num_pops, gen + 1)::Matrix{Float64}

    S_end = zeros(num_pops, gen + 1)::Matrix{Float64}
    Z_end = zeros(num_pops, gen + 1)::Matrix{Float64}

    lambda_df = zeros(num_pops)::Vector{Float64}
    ampl_mod = zeros(num_pops)::Vector{Float64}

    S_pop[:,1] = spass
    Z_pop[:,1] = zpass

    op_cut = zeros(num_pops, 201)::Matrix{Float64}

    for p in 1:num_pops

        for t in 1:gen

            output = run_ode(S_pop[p,t],Z_pop[p,t], C_val)
            
            simulation = output[1]
            num_equations = output[2]
            
            end_t = length(simulation.t)::Int64
        
            S_end[p,t] = simulation[1,end_t]::Float64
            Z_end[p,t] = simulation[num_equations,end_t]::Float64

            over_wintering = inter_annual(S_end[p,t],Z_end[p,t],Z_pop[p,t],S_pop[p,t], phi, 0.05, sigma, lambda, a,b) 

            S_pop[p, t + 1]  = over_wintering[1]
            Z_pop[p, t + 1]  = over_wintering[2]

        end

        op_cut[p,:] = S_end[p,:][(gen - 200):gen]

        # sd_per = std(diff(peaks))
        # cv_per = sd_per/per_pred

    end

    return(op_cut) #S_pop because it starts at overwintering

end


initS_test = rand(LogNormal(-1.5,0.75),num_pops)::Vector{Float64}
initZ_test = rand(LogNormal(-1.5,0.75),num_pops)::Vector{Float64}


phi = 10.5
lambda = 20.1
C = 0.86
sigma = 0.5

global ν = 8
global a = 0.0
global b = 0.5

# function run_simulation_single(phi::Float64, lambda::Float64, C::Float64, a::Float64, gen::Int64, zpass::Vector{Float64},spass::Vector{Float64})
get_inits = run_simulation_single(phi, lambda, C, sigma, a, 400, initS_test,initZ_test)

peaks2, vals2 = findpeaks(get_inits[1,:],min_height = maximum(get_inits[1,:])*0.1,min_dist = 3)
proms = prominence(get_inits[1,:],peaks2)

mean(vals2)
mean(proms)
mean(diff(peaks2))


plot(1:201, get_inits[1,:])
#scatter!(peaks,vals .- minimum(get_inits[1,:]))
scatter!(peaks2,vals2)
savefig("_plots/peaks_nu1.9_phi10.pdf")

print("done")

y = get_inits[1,:]
peaks = peaks2

yP = y[peaks]
proms = zero(yP)

enumerate(peaks)

for (i, p) in enumerate(peaks)
    lP, rP = 1, length(y)
    for j = (i-1):-1:1
        if yP[j] > yP[i]
            lP = peaks[j]
            break
        end
    end
    ml = minimum(y[lP:p])
    for j = (i+1):length(yP)
        if yP[j] > yP[i]
            rP = peaks[j]
            break
        end
end
mr = minimum(y[p:rP])
ref = max(mr,ml)
proms[i] = yP[i] - ref
end
