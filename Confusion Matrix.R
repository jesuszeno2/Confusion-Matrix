# Jesus Zeno


breast=read.csv(file.choose(),header=TRUE)
# Attach funciton allows us to not have to call its name
attach(breast)

model2=glm(as.factor(BenignMalignant)~ClumpThickness+CellSizeUniformity+CellShapeUniformity
           +MarginalAdhesion+SingleEpithelialCell+BareNuclei+
             BlandChromatin+NormalNucleoli+Mitoses,
           family=binomial(link="logit"))

# This undoes the logistic regression.
# predict function takes the model and applies it to the dataset. 
breast$prediction2=exp(predict(model2,breast))/1+exp(predict(model2,breast))

# take the value that comes out of it. The probability >.5 is malignant then 
# turn into 4. If <.5 then it's benign and turn it into 2. 
for(i in 1:699){
  if(breast$prediction2[i]>0.5){
    breast$prediction2[i]=4
  }
  else(breast$prediction2[i] = 2)
}

table(as.factor(breast$BenignMalignant),as.factor(breast$prediction2))
# This gets 443 benign ones correct. 15 benign wrong. 3 malignant wrong. 
# 238 malignant correct. 

# We will be treating malignant (score of 4) as true positive (TP), 
# benign (score of 2) as true negative (TN), actually malignant (positive) 
# while testing benign (negative) as (FN), and actually benign (negative) while 
# testing malignant (positive) as false negative (FP).
TP = 238
FN = 3
FP = 15
TN = 443

# We want sensitivity, specificity, precision, and negative predictive value
# to be as close to 1 as possible since that means they are mirroring their
# environment. 

# Sensitivity. Tells us how many malignant cases it catches out of all the 
# actually malignant ones aka condition positive (P). 
TPR = (TP/(TP+FN))
TPR
# TPR is .987 and close to 1 which is good. 98.7% of malignant cases are 
# caught.


# Specificity. Tells us how many benign cases it can catch out of all the 
# actually benign ones aka condition negative (N)
TNR = (TN/(TN+FP))
TNR
# TNR is .967 and close to 1 which is good. 96.7% of the benign cases are
# caught. 

# Precision. Tells us the confidence in a positive result. Liklihood of 
# a positive test (malignant) that a patient actually has it. 
PPV = (TP/(TP+FP))
PPV
# PPV is .94 and close to 1 which is good. 94% confident that a malignant
# test result means the patient actually has malignant tumor. 

# Negative predictive value. Tells us the confidence in a negative result. 
NPV = (TN/(TN+FN))
NPV
# NPV is .993 and close to 1 which is good. 99.3% confident that benign
# test result means the patient is actually benign. .7% of patients are
# unaware they are malignant. 

# These next four will be compliments of the previous four so we want them to
# be as close to zero as possible. 

# Miss rate/ False negative rate. 
# Person is actually positive, but tests negative.
FNR = (FN/(FN+TP))
FNR
# FNR is .012 which is close to 0 and good. 1.2% of individuals with a result
# saying the mass is benign while it is actually malignant. 

# Fallout rate/False positive rate. 
# Penalty we are paying for trying to prevent missed positive cases. 
FPR = (FP/(FP+TN))
FPR
# FPR is .032 which is close to 0 and good. 3.2% of individuals with malignant
# mass were misses with testing. 

# False discovery rate. Declared wrongly to be positive out of all cases
# declared positive. 
FDR = (FP/(FP+TP))
FDR
# FDR is .059 which is close to 0 and good. 5.9% of people were wrongly
# diagnosed positive out of all the cases that were declared positive. 

# False omission rate. Declared wrongly negative out of all the cases
# declared negative. 
FOR = (FN/(FN+TN))
FOR
# FOR is .006 which is close to 0 and good. Out of all the cases declared
# negative, only .6% of them were wrongly diagnosed. 

# Prevalence threshold. How often we find the condition in the world.
# The number of positives aka the rate of the testing group actually 
# being malignant. Used formula that was simplified and uses numbers
# from previously calculated numbers. 
PT = (sqrt(FPR)/(sqrt(TPR)+sqrt(FPR)))
PT
# PT is .154. We would likely find 15.4% of people in the world with a 
# malignant mass. 

# Threat score. Taking into account all the things that would be bad for
# society. In this case, we are taking into account having a malignant tumor
# and testing positive. Having the condition is a threat. We are also 
# considering the instances where the test doesn't match what the patient 
# actually has. Balances things that are an actual threat against things 
# that are perceived as threats. FP is a perceived threat and FN is letting
# the threat escape. TS is the likelihood of the threat compared to all the 
# measures that have something to do with a threat. 
TS = (TP/(TP+FN+FP))
TS
# TS is .929. Takes into consideration all the TPs and misdiagnoses and finds
# that 92.9% of them are a threat. 


# Accuracy. Generally the ability to replicate the the actual known value.
# Also has to do with being able to get the value at the measurement that
# is used. What's the likelihood that the test is correct. 
ACC = ((TP+TN)/(TP+TN+FP+FN))
ACC
# ACC is .974. The test has a 97.4% likelihood that it is correct. 

# Balanced accuracy. Takes the ability to get the test right in the positive
# and negative sense. Then averages them out. Deals with things that are
# disproportionately likely to occur or not. If a random person walks in for 
# a screening, there should be more raw negative (benign) tests that are 
# TN than there are raw positive (malignant) that are TP. 
BA = ((TPR+TNR)/2)
BA
# BA is .977. If a random person walks in for a screening, the average of
# the positive test and negative test being correct is 97.7%.

# F1 score. Measure that tries to balance the precision and sensitivity measurement.
# Ability to say patient is malignant and actually be malignant while 
# the test being malignant and the patient actually being malignant. We 
# want the F1 score to be close to 1. 
F_1 = ((2*TP)/((2*TP)+FP+FN))
F_1
# F_1 is .963 and is close to 1 which is good. This tells us we have a 96.3%
# chance to say mass is malignant and actually be malignant while 
# the test being malignant and the mass actually being malignant. 

# Mathews correlation coefficient. Compares category to category. It analyzes
# how the the truths and faults perform. Behaves like Pearson R correlation
# coefficient. If the answer is close to 1 they are associated (the positives
# and negatives go together perfectly). If it is -1 the negatives are the
# positives and it's reversed. If it is 0, then they are 50/50 across the board
# in terms of getting it right or wrong. Is this prediction serviceable in the 
# long-term. 
MCC = (((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
MCC
# MCC is .994 and is close to 1 which means the positives and the negatives
# go together quite well. This testing prediction is serviceable long-term. 
