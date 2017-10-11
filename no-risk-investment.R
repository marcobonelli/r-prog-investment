no_risk_investment = function(r = 0.00022509, n_periods = 283){
  
  library("FinCal")
  
  stocks_list = readLines("Acoes.txt") 
  
  tax = r
  
  database = matrix(nrow = length(stocks_list), ncol = 2)
  rownames(database, do.NULL = FALSE)
  rownames(database) = stocks_list
  colnames(database, do.NULL = FALSE)
  colnames(database) = c("Dividendos($)","Disponibilidade(nº)")
  
  cash_flow = matrix(nrow = n_periods, ncol = length(stocks_list))
  colnames(cash_flow, do.NULL = FALSE)
  colnames(cash_flow) = stocks_list
  
  database[, "Dividendos($)"] = as.double(readLines("Dividendos.txt"))
  database[, "Disponibilidade(nº)"] = as.integer(readLines("Disponibilidade.txt"))
  
  for(stocks in stocks_list){
    
    quotation_list = as.double(readLines(paste(stocks, ".cot", sep = "")))
    period_list = as.integer(readLines(paste(stocks, ".per", sep = "")))
    
    for(line in c(1:length(period_list)))
      cash_flow[period_list[line] + 1, stocks] = quotation_list[line] * database[stocks, "Disponibilidade(nº)"] * (database[stocks, "Dividendos($)"] / 100)
    
    cash_flow[1, stocks] = (-1) * database[stocks, "Disponibilidade(nº)"] * quotation_list[length(quotation_list)]
    cash_flow[length(cash_flow[, stocks]), stocks] = database[stocks, "Disponibilidade(nº)"] * quotation_list[1]
    
    cash_flow[is.na(cash_flow[, stocks]), stocks] = 0.00
    
    npc_ = npv(r = tax, cf = cash_flow[, stocks])
    irr_ = irr(cf = cash_flow[, stocks])
    
    print(paste(stocks, "-> npv($) =", npc_))
    print(paste(stocks, "-> irr(%) =", irr_ * 100))
    
  }
  
}