using Pkg

#Pkg.add("Peaks")
Pkg.add("Optim")

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

cd("/Users/katherinedixon/Documents/StuffINeed/_Research/_DFTM_2023/Synchrony/_julia")

model_preds = DataFrame(CSV.File("_data/gam_predictions.csv"))

num_pops = length(model_preds.manual_id)

global μ = 0.55
global δ = 0.083
global k = Int(20)
global ν = 0.5
#global C = 0.86
#global phi = 30.0
global a = 0.0
global b = 0.14
global sig = 0.5

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

function run_simulation(phi::Float64, a::Float64, k1::Float64, k2::Float64, gen::Int64, zpass::Vector{Float64},spass::Vector{Float64},ampl_pred::Vector{Float64}, obs_data::Vector{Float64})

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

        test = (1/k1)*(ampl_pred[p])

        if test > 0 
            lambda_df[p] = get_lambda(k1,k2,ampl_pred[p])
        else 
            lambda_df[p] = 0
        end

    end

    if sum(lambda_df .>= 1) == length(lambda_df)

        for p in 1:num_pops

            for t in 1:gen

                # if S_pop[p,t] <= 0 
                #     println("Spop is less than zero")
                # end

                # if Z_pop[p,t] <= 0 
                #     println("Zpop is less than zero")
                # end

                output = run_ode(S_pop[p,t],Z_pop[p,t])
                
                simulation = output[1]
                num_equations = output[2]
                
                end_t = length(simulation.t)::Int64
            
                S_end[p,t] = simulation[1,end_t]::Float64
                Z_end[p,t] = simulation[num_equations,end_t]::Float64

                over_wintering = inter_annual(S_end[p,t],Z_end[p,t],Z_pop[p,t],S_pop[p,t], phi, 0.05, sig, lambda_df[p], a,b) 

                S_pop[p, t + 1]  = over_wintering[1]
                Z_pop[p, t + 1]  = over_wintering[2]

            end

            op_cut[p,:] = S_end[p,:][(gen - 200):gen]

            peaks, vals = findmaxima(op_cut[p,:])

            #per_pred = mean(diff(peaks))
            ampl_mod[p] = mean(vals)

            # sd_per = std(diff(peaks))
            # cv_per = sd_per/per_pred

        end
        
        res = optimize(m -> sqerror(m, ampl_mod,obs_data), [0.1], LBFGS())
        
        m_pred = Optim.minimizer(res)[1]
        sum_sq = Optim.minimum(res)

        ampl_conv = ampl_mod./m_pred

        new_spass = S_pop[:,gen]
        new_zpass = Z_pop[:,gen]

    else 
    
    new_spass = spass
    new_zpass = zpass

    #op_cut = zeros(num_pops, 201)::Matrix{Float64}
    ampl_conv = zeros(num_pops)::Vector{Float64}
    ampl_mod = zeros(num_pops)::Vector{Float64}


    m_pred = -Inf
    sum_sq = -Inf

    end

    return(ampl_mod,ampl_conv,lambda_df,m_pred, sum_sq, new_spass,new_zpass) #S_pop because it starts at overwintering

end

k2_test = 1.0:10.0:400.0
k1_test = 1.0:10.0:51.0 # k1 must be greater than m*amp+b

#a_test =  0:0.055:0.99 # [0.1,0.56,0.97] #
#phi_test = 5:5:100 # [15,30,45,60,75]# 
#length(lambda_test)*length(a_test)*length(phi_test)
num_sims = length(k1_test)*length(k2_test)

initS_test = rand(LogNormal(-1.5,0.75),num_pops)::Vector{Float64}
initZ_test = rand(LogNormal(-1.5,0.75),num_pops)::Vector{Float64}

get_inits = run_simulation(phi, a, 100.0, 50.0, 1000, initS_test,initZ_test,model_preds.pred_gam, model_preds.mean_ampl)


function sqerror_mm(betas, X, Y)
    err = 0.0
    for i in 1:length(X)
        pred_i = (X[i]*betas[1]*betas[2])/(betas[3] + betas[2]*X[i])
        err += (Y[i] - pred_i)^2
    end
    return err
end

res = optimize(p -> sqerror(p, get_inits[1],model_preds.mean_ampl), [0.1,0.1])

res = optimize(p -> sqerror_mm(p, get_inits[1],model_preds.mean_ampl), [0.1,0.1,0.1])

Optim.minimizer(res)[1]
Optim.minimizer(res)[2]
Optim.minimizer(res)[3]
Optim.minimum(res)

plot(scatter(get_inits[1].*193 .-108, model_preds.pred_gam))


initS = zeros(num_pops, num_sims)::Matrix{Float64}
initZ = zeros(num_pops, num_sims)::Matrix{Float64}

initS[:,1] = get_inits[6]
initZ[:,1] = get_inits[7]

gns = 1000

ll_data = DataFrame()
sims_data = DataFrame()

n = 1

for k1 in k1_test
    for k2 in k2_test 
        for m in m_test
            for int in int_test
        
                op = run_simulation(phi, a, k1, k2, m, int, 400, initS_test, initZ_test,model_preds.pred_gam, model_preds.mean_ampl)

                n = n + 1

                initS[:,n] = op[4]
                initZ[:,n] = op[5]

                temp_df = DataFrame(ampl_mod = op[1], lambda = op[3], sse = op[2], k1 = k1, k2 = k2, m = m, int = int, a = a, phi = phi, manual_id = model_preds.manual_id)

                #temp_sims = DataFrame(op[6], t = (gen - 200):gen, k1 = k1, k2 = k2, m = m, int = int, a = a, phi = phi)

                ll_data = vcat(ll_data,temp_df)


    end
    println(lambda_test[lt])
end

dir = "_output/"
name = "all_test"

peaks_info = "peaks_lambda_"*name*".csv"
CSV.write(dir*peaks_info, ll_data, header = true)

sim_info = "sims_lambda_"*name*".csv"
CSV.write(dir*sim_info, sims_data, header = true)
