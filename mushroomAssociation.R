# Mia Belli
# 12/15/23
# ASSOCIATION OF EDIBLE AND POISONOUS MUSHROOMS

#### LOADING PACKAGES & DATA - ASSOCIATION ####
# For Association
install.packages("arules")
install.packages("arulesViz") #has visualization functions for association rules

library(arules)
library(arulesViz)

# load data - association dataset
data(package='arules') # all the datasets in the arules packages
data("Mushroom")
help("Mushroom")

#### DATA EXPLORATION - ASSOCIATION ####
arules::summary(Mushroom) # 8124 mushrooms (transactions), 114 columns (items)
inspect(head(Mushroom))
inspect(tail(Mushroom))

size(head(Mushroom)) #transaction width or the size (number of items) of the first 6 itemsets
size(tail(Mushroom)) #the number of items in the last 6 itemsets (last 6 transactions/records)

LIST(head(Mushroom)) #list view of the first 6 transactions with items numbered
LIST(tail(Mushroom)) #list view of the last 6 transactions with items numbered

# frequent itemsets
frequent.itemsets <- eclat(Mushroom) #default support is 10%-20%
frequent.itemsets # 340050 itemsets
frequent.itemsets <- eclat(Mushroom, parameter = list(support=.6)) #60% min support
frequent.itemsets # 51 itemsets
inspect(head(frequent.itemsets, 10)) # getting first 10 itemsets bc computer can't load 2 million

# sorting frequent itemsets in ascending order with decreasing=FALSE >> shows top 10 most frequent itemsets
frequent.itemsets.support <- sort(frequent.itemsets, by = "support", decreasing = FALSE)
inspect(head(frequent.itemsets.support,10)) #the 10 itemsets with the min support of 60ish%

#visualize the most frequent items by using itemfrequencyPlot().
itemFrequencyPlot(Mushroom) #too many - filter the results
itemFrequencyPlot(Mushroom, topN = 10) #top 10 most frequent items
itemFrequencyPlot(Mushroom, topN=10, type = "absolute") #by adding type="absolute" >> shows the count of each frequent item

#### GENERATE ASSOCIATION RULES ####
rules <- apriori(Mushroom, parameter = list(support=.6, confidence=.75))
rules
inspect(rules)
# 108 rules created
# most lifts for each rule are > 1

# sorting rules by support
rules.support <- sort(rules, by="support")
inspect(rules.support)

# keeping rules with only a lift > 1
rules.lift <- subset(rules, lift>1)
rules.lift
inspect(rules.lift)

#visualize the rules using the graph technique
plot(rules.lift) #scatter plot
plot(rules.lift, method = 'graph')

#### TARGET SPECIFIC LHS OR RHS ITEMSETS ####
# Poisonous Mushroom Rules
rules.poison.rhs <- apriori(Mushroom, parameter = list(support=.6, confidence=.75, minlen=2),
                            appearance = list(default = "lhs", rhs='Class=poisonous'))
rules.poison.rhs # 0 rules
inspect(rules.poison.rhs)

rules.poison.rhs <- apriori(Mushroom, parameter = list(support=.4, confidence=.75, minlen=2),
                            appearance = list(default = "lhs", rhs='Class=poisonous'))
rules.poison.rhs # 2 rules
inspect(rules.poison.rhs)


# Edible Mushroom Rules
rules.edible.rhs <- apriori(Mushroom, parameter = list(support=.6, confidence=.75, minlen=2),
                            appearance = list(default = "lhs", rhs='Class=edible'))
rules.edible.rhs # 0 rules
inspect(rules.edible.rhs)

rules.edible.rhs <- apriori(Mushroom, parameter = list(support=.4, confidence=.75, minlen=2),
                            appearance = list(default = "lhs", rhs='Class=edible'))
rules.edible.rhs # 4 rules
inspect(rules.edible.rhs)