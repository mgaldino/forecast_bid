
# função que simula ipc futuro
sim_ipc <- function(ipc_0 = 1.1, ipc_realizado) {
  if (is.na(ipc_realizado)) {
    # inflação anual, t-student, 7 df, com drift de .05
    ipc_base <- 1 +rt(29, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    ipc <- cumprod(c(ipc_base, ipc_0))

  }
  return(ipc)
}

# função que simula igpm futuro
sim_igpm <- function(igpm_0 = 1.1, igpm_realizado) {
  if (is.na(igpm_realizado)) {
    # inflação anual, t-student, 7 df, com drift de .05
    igpm_base <- 1 + rt(29, 7)/75 + .05 # simular melhor depois. arima, algo assim, simples.
    igpm <- cumprod(c(igpm_base, igpm_0))
  }
  return(igpm)
}

# função que gera num passagenrs

gen_num_passengers <- function (sensibilidade = 1) {
  demanda_projetada <- c(196860, 204204, 211822, 214415, 284490,
                         264251,	271691,	295706,	278154,	274183,	
                         274183,	274183,	274183,	274183,	274183,	
                         274183,	274183,	274183,	274183,	274183,	
                         274183,	274183,	274183,	274183,	274183,	
                         274183,	274183,	274183,	274183,	274183)
  
  
  perc_pass_exclusive_contract <- c(rep(.05, 4), rep(.1, 26))
  perc_pass_integrated_contract <- 1 - perc_pass_exclusive_contract
  
  demanda_real <- demanda_projetada * sensibilidade
  
  numPassengersExclusive <- demanda_real * perc_pass_exclusive_contract
  numPassengersIntegrated <- demanda_real * perc_pass_integrated_contract
  
  return(list(numPassengersExclusive, numPassengersIntegrated, demanda_projetada, demanda_real))
}
 
# função que calcula o preço da tarifa (aka tr) para cada ano
gen_price_ticket_line4 <- function(num_years, t_0 = 2.14, ipc_0 = 1.1,
                                 ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA,
                               igpm_realizado=NA, sensibilidade = 1) {
  # esta função  calcula a TR: tarifa de remuneração
  ano <- 1:num_years
  
  ipc <- sim_ipc(ipc_realizado=ipc_realizado)
  igpm <- sim_igpm(igpm_realizado=igpm_realizado)
  
  rt <- numeric()
  priceTicket <- numeric()
  
  
  for ( i in 1:num_years) {
    if(ano[i] <= 15) {
      priceTicket[i] <- t_0 * (a*(igpm[i]/ipgm_0 ) + b*(ipc[i]/ipc_0))
    } else {
      priceTicket[i] <- priceTicket[i-1] * ipc[i]/ipc[i-1]
    }
  }
  return(priceTicket)
}

# funçãoque calcula MD
gen_ajuste_demanda <- function(sensibilidade = 1, num_years,
                               t_0,
                               ipc_0 , ipgm_0 ,
                               a,b, ipc_realizado ,
                               igpm_realizado ) {
  
  stopifnot(.6 <= sensibilidade && sensibilidade <= 1.4)
  
  price_ticket <- gen_price_ticket_line4(num_years, t_0 = t_0,
                                         ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                         a= a,b = b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado)
  passengers <- gen_num_passengers()
  dp <- passengers[[3]]
  dr <- dp*sensibilidade
  pe <- passengers[[1]]
  pi <- passengers[[2]]
  aux <- .5*pi*price_ticket/(pi +pe) + price_ticket*pe / (pe +pi)
  
  
  if(.6 <= sensibilidade && sensibilidade <= .8) {
    md <- .06*dp + .9*(.8*dp - dr)*aux 
  }
  
  if(.8 < sensibilidade && sensibilidade <= .9) {
    md <- .06*(.9*dp - dr)*aux
  }
  
  if(.9 < sensibilidade && sensibilidade <= 1.1) {
    md <- 0
  }
  
  if(1.1 < sensibilidade && sensibilidade <= 1.2) {
    md <- .06*(dr - 1.1*dp)*aux
  }
  
  if(1.2 < sensibilidade && sensibilidade <= 1.4) {
    md <- (.06*dp + .9*(dr - 1.2*dp))*aux
  }
  
  return(md)
  
}


# função que calcula ti
gen_implicit_ticket_revenue <- function (numPassengersExclusive, 
                                     numPassengersIntegrated, price_ticket,
                                     num_years, t_0 = 2.14, ipc_0 = 1.1,
                                     ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, 
                                     igpm_realizado=NA, sensibilidade) {
  
  ano <- 1:num_years
  incid_period <- numeric()

  for ( i in ano) {
    if ( ano[i] <= 3) {
      incid_period[i] <- 0
    }
    if ( ano[i] == 4) {
      incid_period[i] <- .5
    }
    if ( ano[i] > 4 && ano[i] <= 11) {
      incid_period[i] <- 1
    }
    if ( ano[i] > 11) {
      incid_period[i] <- 0
    }
  }
  
  md <- gen_ajuste_demanda(sensibilidade, num_years, t_0 = t_0,
                           ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                           a= a,b = b, ipc_realizado = ipc_realizado,
                           igpm_realizado = igpm_realizado)
  
  ti <- (md*incid_period * numPassengersExclusive  +  price_ticket * numPassengersIntegrated + price_ticket*numPassengersIntegrated*.5)/(numPassengersExclusive + numPassengersIntegrated*.5 )
  return(ti)

}

# função que calcula receita tarifária
gen_ticket_revenue <- function(num_years = 30, t_0 = 2.14, ipc_0 = 1.1,
                           ipgm_0 = 1.1, a=.5, b=.5, ipc_realizado=NA, igpm_realizado=NA,
                           qualityAdjustment=1, sensibilidade = 1) {

  num_pass <- gen_num_passengers(sensibilidade = sensibilidade)
  
  numPassengersExclusive <- num_pass[[1]]
  numPassengersIntegrated <- num_pass[[2]]
  demanda_projetada <- num_pass[[3]]
  demanda_real <- num_pass[[4]]
  
  price_ticket <- gen_price_ticket_line4(num_years = num_years, t_0 = t_0, 
                                         ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                         a =a,b= b, ipc_realizado = ipc_realizado,
                                         igpm_realizado = igpm_realizado,
                                         sensibilidade = sensibilidade)

  implicit_ticket_rev <- gen_implicit_ticket_revenue(numPassengersExclusive = numPassengersExclusive,
                                                     numPassengersIntegrated = numPassengersIntegrated,
                                                     price_ticket = price_ticket,
                                                     num_years = num_years, t_0 = t_0, 
                                                     ipc_0 = ipc_0, ipgm_0 = ipgm_0,
                                                     a =a,b= b, ipc_realizado = ipc_realizado,
                                                     igpm_realizado = igpm_realizado,
                                                     sensibilidade = sensibilidade)

  rt <- (numPassengersExclusive * implicit_ticket_rev + numPassengersIntegrated *.5*implicit_ticket_rev)* qualityAdjustment
  return(rt)
}

## testando a função
# num_years = 30
# t_0 = 2.14
# ipc_0 = 1.1
# ipgm_0 = 1.1
# a=.5
# b=.5
# ipc_realizado=NA
# igpm_realizado=NA
# qualityAdjustment=1
# sensibilidade = 1

gen_ticket_revenue(sensibilidade = 1.3)

implicit_ticket_rev
