#Association Rules: Market basket Analysis, Affinity Analysis and recommendation engines
#If antecedent, then consequent.  Antecedent and consequent item sets are disjoint, completely separate
#Complexity grows exponentially with number of items.
#Support of item set: % of all transactions containing that item set 
#Confidence: the % of antecedent transactions that also have the consequent item set: P(conseqent | antecedent)
#Lift = confidence / benchmark confidence =P(consequent | antecedent) / P(consequent), Lift>1 means better than random
#Benchmark confidence = P(consequent)= # transactions with consequent / # of all transactions
#
Input Dataframe must have user/transactions on rows and items in columns
library(arules) ## a-rules package for association rules
## only associations with support > 0.01 and confidence > .50, this rules out rare sets 
rules <- apriori(df,parameter=list(support=.01,confidence=.5)) 
inspect(rules)
## let's filter by lift > 5. 
## Among those associations with support > 0.01 and confidence > .50, 
## only show those with lift > 5
inspect(subset(rules, subset=lift > 5)) 

df <- lapply(df,unique)     ## remove item duplicates
#df <- as(playlist,"transactions") ??
itemFrequency(df) 
itemFrequencyPlot(df,support=.08,cex.names=1.5) 




