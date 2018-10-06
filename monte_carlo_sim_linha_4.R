
# função que simula ipc futuro
sim_ipc <- function(ipc_0 = 1.1, ipc_realizado) {
  if (is.na(ipc_realizado)) {
    # inflação anual, t-student, 7 df, com drift de .05
    ipc_base <- 1 +rt(29, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    ipc <- cumprod(c(ipc_base, ipc_o))

  }
  return(ipc)
}

# função que simula igpm futuro
sim_igpm <- function(igpm_0 = 1.1, igpm_realizado) {
  if (is.na(igpm_realizado)) {
    # inflação anual, t-student, 7 df, com drift de .05
    igpm_base <- 1 + rt(29, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    igpm <- cumprod(c(igpm_base, igpm_o))
  }
  return(igpm)
}

# função que calcula o preço da tarifa para cada ano
price_ticket_line4 <- function(num_years = 30, t_0 = 2.14, ipc_0 = 1.1,
                                 ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, igpm_realizado=NA
                                 ) {
  
  ano <- 1:num_years
  
  ipc <- sim_ipc(ipc_realizado=ipc_realizado)
  igpm <- sim_igpm(igpm_realizado=igpm_realizado)
  
  rt <- numeric()
  priceTicket <- numeric()
  
  
  for ( i in 1:num_years) {
    if(ano[i] <= 15) {
      priceTicket[i] <- t_0 * (a*(igpm[i]/igpm_o ) + b*(ipc[i]/ipc_o))
    } else {
      priceTicket[i] <- priceTicket[i-1] * ipc[i]/ipc[i-1]
    }
  }
  return(priceTicket)
}


# por enquanto vou assumir que projetado igual a realizado

# função que calcula receita tarifária
ticket_revenue <- function(num_years = 30, t_0 = 2.14, ipc_0 = 1.1,
                           ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, igpm_realizado=NA,
                           num_passengers_contract, qualityAdjustment=1) {
  
  perc_pass_exclusive_contract <- c(rep(.05, 4), rep(.1, 26))
  perc_pass_integrated_contract <- 1 - perc_pass_exclusivo_contrato
  
  priceTicket <- price_ticket_line4()

  numPassengersIntegration <- num_passengers_contract*perc_pass_exclusive_contract
  numPassengersExclusive <- num_passengers_contract*perc_pass_integrated_contract
  
  rt <- (numPassengersExclusive * priceTicket + numPassengersIntegration *.5*priceTicket)* qualityAdjustment
  return(rt)
}

## testando a função
num_passengers_contract <- c(196860, 204204, 211822, 214415, 284490,
                             264251,	271691,	295706,	278154,	274183,	
                             274183,	274183,	274183,	274183,	274183,	
                             274183,	274183,	274183,	274183,	274183,	
                             274183,	274183,	274183,	274183,	274183,	
                             274183,	274183,	274183,	274183,	274183)

ticket_revenue(num_passengers_contract = num_passengers_contract)

# pro futuro: incluir quality adjsutment
# 
# qualityAdjustment = .8 + .1*lqs + .1*lqm
# 
# lqs = .3*ISU + .2*INT + .15*TMP + .1*IAL + .1* ICL + .5*ICO +.5*IVA + .5*IRG


