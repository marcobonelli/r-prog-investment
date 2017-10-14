no_risk_investment = function(r, n_periods){
# pacote FinCal deve ser instalado previamente  
  library("FinCal")
  
  stocks_list = readLines("Acoes.txt") 
  
  tax = r
  
# declaração da matriz para armazenamento da solução final  
  out_value = matrix(nrow = length(stocks_list), ncol = 3)
  rownames(out_value, do.NULL = FALSE)
  rownames(out_value) = stocks_list
  colnames(out_value, do.NULL = FALSE)
  colnames(out_value) = c("Investment", "NPV value", "IRR value")
  
# declaração da matriz para armazenamento dos dados de dividendos e disponibilidade de ações
  database = matrix(nrow = length(stocks_list), ncol = 2)
  rownames(database, do.NULL = FALSE)
  rownames(database) = stocks_list
  colnames(database, do.NULL = FALSE)
  colnames(database) = c("Dividends($)", "Availability(nº)")
  
# declaração da matriz para armazenamento do retorno referente às cotação das ações
  cash_flow = matrix(nrow = n_periods, ncol = length(stocks_list))
  colnames(cash_flow, do.NULL = FALSE)
  colnames(cash_flow) = stocks_list
  
# inicializa matriz de dividendos e disponibilidade
  database[, "Dividends($)"] = as.double(readLines("Dividendos.txt"))
  database[, "Availability(nº)"] = as.integer(readLines("Disponibilidade.txt"))
  
  for(stocks in stocks_list){
    
# realiza leitura das cotações periodicas da ações e dos periodos em que as cotações ocorreram
    quotation_list = as.double(readLines(paste(stocks, ".cot", sep = "")))
    period_list = as.integer(readLines(paste(stocks, ".per", sep = "")))
    
# inicializa matriz de retornos com os valores existentes, em seus respectivos períodos
    for(line in c(1:length(period_list)))
      cash_flow[period_list[line] + 1, stocks] = quotation_list[line] * database[stocks, "Availability(nº)"] * (database[stocks, "Dividends($)"] / 100)

# inicializa investimento inicial e retorno de capital    
    cash_flow[1, stocks] = (-1) * database[stocks, "Availability(nº)"] * quotation_list[length(quotation_list)]
    cash_flow[length(cash_flow[, stocks]), stocks] = database[stocks, "Availability(nº)"] * quotation_list[1]
    
# atribui valor nulo para os períodos cujo qual não existem retornos
    cash_flow[is.na(cash_flow[, stocks]), stocks] = 0.00
    
# realiza cálculo do VPL e TIR, respectivamente
    out_value[stocks, "NPV value"] = npv(r = tax, cf = cash_flow[, stocks])
    out_value[stocks, "IRR value"] = irr(cf = cash_flow[, stocks])
    
  }
  
# inicializa investimento inicial na matriz de saída
  out_value[, "Investment"] = cash_flow[1, ]

# retorna matriz de saída com informações sobre os investimentos
  return (out_value)
  
}

all_combinations = function(array, n){
# pacote hier.part deve ser instalado previamente
  require(hier.part)
  
# gera todas as opções de investimento não nulo
  m_auxiliary = combos(n)$binary
  
# ajuste matriz de investimento
  possible_options = matrix(0, nrow = length(m_auxiliary[, 1]) + 1, ncol = (n + 1))
  
# ajusta investimento nulo
  for(i in c(1:length(m_auxiliary[, 1])))
    for(j in c(1:length(m_auxiliary[1, ])))
      possible_options[i + 1, j + 1] = m_auxiliary[i, j]

# retorna opções de investimento 
  return (possible_options)
}

portfolio_choose = function(budget = -200000, array = c("GOAU4", "SMLE3", "CPLE6", "ELET3", "BRKM5"), n = 5, r = 0.00022509, n_periods = 283){
# chama funções de calculo das informações do investimento e das possibilidades de investimento
  return_values = no_risk_investment(r, n_periods)
  options_investment = all_combinations(array, n)

# calcula desembolso necessário para cada opção de investimento 
  for(i in c(1:length(options_investment[, 1]))){
    options_investment[i, 1] = options_investment[i, 2:(n + 1)] %*% return_values[, 1]
# anula investimentos que ultrapassam limite de orçamento
    if(options_investment[i, 1] < budget)
      options_investment[i, 1] = NA
  }

# ordena investimento segundo o retorno esperado  
  options_investment = options_investment[do.call(order, as.data.frame(options_investment)), ]
  
# retorna o conjunto de investimentos de maior vpl acumulado
  return (options_investment[1, ])
}