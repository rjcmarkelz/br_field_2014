data{
    int<lower=1> N_geno;
    int<lower=1> N;
    real bolt[N];
    real trt[N];
    int geno[N];
}
parameters{
    vector[N_geno] bg_geno;
    vector[N_geno] a_geno;
    real a;
    real bg;
    vector<lower=0>[2] sigma_geno;
    real<lower=0> sigma;
    corr_matrix[2] Rho;
}
transformed parameters{
    vector[2] v_a_genobg_geno[N_geno];
    vector[2] Mu_abg;
    cov_matrix[2] SRS_sigma_genoRho;
    for ( j in 1:N_geno ) {
        v_a_genobg_geno[j,1] <- a_geno[j];
        v_a_genobg_geno[j,2] <- bg_geno[j];
    }
    for ( j in 1:2 ) {
        Mu_abg[1] <- a;
        Mu_abg[2] <- bg;
    }
    SRS_sigma_genoRho <- quad_form_diag(Rho,sigma_geno);
}
model{
    vector[N] mu;
    Rho ~ lkj_corr( 2 );
    sigma ~ cauchy( 0 , 2 );
    sigma_geno ~ cauchy( 0 , 2 );
    bg ~ normal( 0 , 100 );
    a ~ normal( 0 , 100 );
    v_a_genobg_geno ~ multi_normal( Mu_abg , SRS_sigma_genoRho );
    for ( i in 1:N ) {
        mu[i] <- a_geno[geno[i]] + bg_geno[geno[i]] * trt[i];
    }
    bolt ~ normal( mu , sigma );
}
generated quantities{
    vector[N] mu;
    real dev;
    dev <- 0;
    for ( i in 1:N ) {
        mu[i] <- a_geno[geno[i]] + bg_geno[geno[i]] * trt[i];
    }
    dev <- dev + (-2)*normal_log( bolt , mu , sigma );
}