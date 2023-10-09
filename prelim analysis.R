# PC
elaia2 <- read.csv("C:/Users/simon/Documents/myrepo/deidentified_elaia2_data_missing_NaN3.csv")

# MACOS
elaia2 <- read.csv("/Users/simon/Library/CloudStorage/GoogleDrive-simonite410@gmail.com/My Drive/Academic/Year 4/PHAR_423/myrepo/deidentified_elaia2_data_missing_NaN3.csv")

vent_icu <- subset(elaia2, elaia2$ventorder == 1 & elaia2$icu_at_rand == 1)

average_providers_vent_icu <- mean(vent_icu$num_provider, na.rm = TRUE)

icu_no_vent <- subset(elaia2, elaia2$ventorder == 0 & elaia2$icu_at_rand == 1)

average_providers_icu_no_vent <- mean(icu_no_vent$num_provider, na.rm = TRUE)

print(average_providers_vent_icu)
print(average_providers_icu_no_vent)


vent_no_icu <- subset(elaia2, elaia2$ventorder == 1 & elaia2$icu_at_rand == 0)

average_providers_vent_no_icu <- mean(vent_no_icu$num_provider, na.rm = TRUE)

no_icu_no_vent <- subset(elaia2, elaia2$ventorder == 0 & elaia2$icu_at_rand == 0)

average_providers_no_icu_no_vent <- mean(no_icu_no_vent$num_provider, na.rm = TRUE)

print(average_providers_vent_no_icu)
print(average_providers_no_icu_no_vent)



tra_icu <- subset(elaia2, elaia2$transfuserbc == 1 & elaia2$icu_at_rand == 1)

average_providers_tra_icu <- mean(tra_icu$num_provider, na.rm = TRUE)

icu_no_tra <- subset(elaia2, elaia2$transfuserbc == 0 & elaia2$icu_at_rand == 1)

average_providers_icu_no_tra <- mean(icu_no_tra$num_provider, na.rm = TRUE)

print(average_providers_tra_icu)
print(average_providers_icu_no_tra)

tra_no_icu <- subset(elaia2, elaia2$transfuserbc == 1 & elaia2$icu_at_rand == 0)

average_providers_tra_no_icu <- mean(tra_no_icu$num_provider, na.rm = TRUE)

no_icu_no_tra <- subset(elaia2, elaia2$transfuserbc == 0 & elaia2$icu_at_rand == 0)

average_providers_no_icu_no_tra <- mean(no_icu_no_tra$num_provider, na.rm = TRUE)

print(average_providers_tra_no_icu)
print(average_providers_no_icu_no_tra)


renal.consult.no.icu.bad.outcome <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$icu_at_rand == 0 & elaia2$composite_outcome == 1)
no.renal.consult.no.icu.bad.outcome <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$icu_at_rand == 0 & elaia2$composite_outcome == 1)
renal.consult.no.icu.good.outcome <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$icu_at_rand == 0 & elaia2$composite_outcome == 0)
no.renal.consult.no.icu.good.outcome <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$icu_at_rand == 0 & elaia2$composite_outcome == 0)

averenalbad <- mean(renal.consult.no.icu.bad.outcome$num_provider, na.rm = TRUE)
avenorenalbad <- mean(no.renal.consult.no.icu.bad.outcome$num_provider, na.rm = TRUE)
averenalgood <- mean(renal.consult.no.icu.good.outcome$num_provider, na.rm = TRUE)
avenorenalgood <- mean(no.renal.consult.no.icu.good.outcome$num_provider, na.rm = TRUE)


# duration of aki in patients with each exi score with and without renal consult

onerenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$elx_score_pmhx <= 4)
onenorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$elx_score_pmhx <= 4)
fiverenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$elx_score_pmhx >= 5 & elaia2$elx_score_pmhx <=9)
fivenorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$elx_score_pmhx >= 5 & elaia2$elx_score_pmhx <=9)
tenrenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$elx_score_pmhx >= 10 & elaia2$elx_score_pmhx <=14)
tennorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$elx_score_pmhx >= 10 & elaia2$elx_score_pmhx <=14)
fifrenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$elx_score_pmhx >= 15)
fifnorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$elx_score_pmhx >= 15)

ave1r <- mean(onerenal$duration_of_aki, na.rm = TRUE)
ave1 <- mean(onenorenal$duration_of_aki, na.rm = TRUE)
ave5r <- mean(fiverenal$duration_of_aki, na.rm = TRUE)
ave5 <- mean(fivenorenal$duration_of_aki, na.rm = TRUE)
ave10r <- mean(tenrenal$duration_of_aki, na.rm = TRUE)
ave10 <- mean(tennorenal$duration_of_aki, na.rm = TRUE)
ave15 <-mean(fifnorenal$duration_of_aki, na.rm = TRUE)
ave15r <- mean(fifrenal$duration_of_aki, na.rm = TRUE)


#sofa scores
Sonerenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$sofa_at_rand <= 4)
Sonenorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$sofa_at_rand <= 4)
Sfiverenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$sofa_at_rand >= 5 & elaia2$sofa_at_rand <=9)
Sfivenorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$sofa_at_rand >= 5 & elaia2$sofa_at_rand <=9)
Stenrenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$sofa_at_rand >= 10 & elaia2$sofa_at_rand <=14)
Stennorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$sofa_at_rand >= 10 & elaia2$sofa_at_rand <=14)
Sfifrenal <- subset(elaia2, elaia2$prior_cs72 == 1 & elaia2$sofa_at_rand >= 15)
Sfifnorenal <- subset(elaia2, elaia2$prior_cs72 == 0 & elaia2$sofa_at_rand >= 15)

Save1r <- mean(Sonerenal$duration_of_aki, na.rm = TRUE)
Save1 <- mean(Sonenorenal$duration_of_aki, na.rm = TRUE)
Save5r <- mean(Sfiverenal$duration_of_aki, na.rm = TRUE)
Save5 <- mean(Sfivenorenal$duration_of_aki, na.rm = TRUE)
Save10r <- mean(Stenrenal$duration_of_aki, na.rm = TRUE)
Save10 <- mean(Stennorenal$duration_of_aki, na.rm = TRUE)
Save15 <-mean(Sfifnorenal$duration_of_aki, na.rm = TRUE)
Save15r <- mean(Sfifrenal$duration_of_aki, na.rm = TRUE)



# inpatient consult

ionerenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$elx_score_pmhx <= 4)
ionenorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$elx_score_pmhx <= 4)
ifiverenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$elx_score_pmhx >= 5 & elaia2$elx_score_pmhx <=9)
ifivenorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$elx_score_pmhx >= 5 & elaia2$elx_score_pmhx <=9)
itenrenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$elx_score_pmhx >= 10 & elaia2$elx_score_pmhx <=14)
itennorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$elx_score_pmhx >= 10 & elaia2$elx_score_pmhx <=14)
ififrenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$elx_score_pmhx >= 15)
ififnorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$elx_score_pmhx >= 15)

iave1r <- mean(ionerenal$duration_of_aki, na.rm = TRUE)
iave1 <- mean(ionenorenal$duration_of_aki, na.rm = TRUE)
iave5r <- mean(ifiverenal$duration_of_aki, na.rm = TRUE)
iave5 <- mean(ifivenorenal$duration_of_aki, na.rm = TRUE)
iave10r <- mean(itenrenal$duration_of_aki, na.rm = TRUE)
iave10 <- mean(itennorenal$duration_of_aki, na.rm = TRUE)
iave15 <-mean(ififnorenal$duration_of_aki, na.rm = TRUE)
iave15r <- mean(ififrenal$duration_of_aki, na.rm = TRUE)



iSonerenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$sofa_at_rand <= 4)
iSonenorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$sofa_at_rand <= 4)
iSfiverenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$sofa_at_rand >= 5 & elaia2$sofa_at_rand <=9)
iSfivenorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$sofa_at_rand >= 5 & elaia2$sofa_at_rand <=9)
iStenrenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$sofa_at_rand >= 10 & elaia2$sofa_at_rand <=14)
iStennorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$sofa_at_rand >= 10 & elaia2$sofa_at_rand <=14)
iSfifrenal <- subset(elaia2, elaia2$renalconsult14 == 1 & elaia2$sofa_at_rand >= 15)
iSfifnorenal <- subset(elaia2, elaia2$renalconsult14 == 0 & elaia2$sofa_at_rand >= 15)

iSave1r <- mean(iSonerenal$duration_of_aki, na.rm = TRUE)
iSave1 <- mean(iSonenorenal$duration_of_aki, na.rm = TRUE)
iSave5r <- mean(iSfiverenal$duration_of_aki, na.rm = TRUE)
iSave5 <- mean(iSfivenorenal$duration_of_aki, na.rm = TRUE)
iSave10r <- mean(iStenrenal$duration_of_aki, na.rm = TRUE)
iSave10 <- mean(iStennorenal$duration_of_aki, na.rm = TRUE)
iSave15 <-mean(iSfifnorenal$duration_of_aki, na.rm = TRUE)
iSave15r <- mean(iSfifrenal$duration_of_aki, na.rm = TRUE)


icuset <- subset(elaia2, elaia2$icu_at_rand == 1)

boxplot(icuset$num_provider ~ icuset$ventorder,
        col = 'steelblue',
        main = 'Providers by vent order in the ICU',
        xlab = 'ventilator',
        ylab = 'providers')
 #mean(icuset$num_provider, icuset)


boxplot(elaia2$num_provider ~ elaia2$elx_score_pmhx,
        col = 'steelblue',
        main = 'Providers by vent order in the ICU',
        xlab = 'ventilator',
        ylab = 'providers')

# boxplot(elaia2$num_provider ~ elaia2$)


# proportion of patients with good outcomes for each value of sofa

renalset <- subset(elaia2, elaia2$prior_cs72 == 1)

mean(renalset$duration_of_aki)




# phi <- sqrt(chisq.test(table(dat))$statistic / length(dat$var1))

# Print the Phi coefficient
phi


boxplot(elaia2$num_provider ~ elaia2$composite_outcome,
        col = 'steelblue',
        main = 'Providers by bad outcome',
        xlab = 'bad outcome',
        ylab = 'providers')


library(Hmisc)

# Assuming your dataset is stored in a variable called 'data'
# correlations <- rcorr(elaia2)

# Access the correlation matrix
# cor_matrix <- correlations$r

# Access the p-values
# p_values <- correlations$P

sofaset <- subset(elaia2, elaia2$icu_at_rand == 1)

# sqrt(chisq.test(table(sofaset$, sofaset$composite_outcome))$statistic / length(sofaset$composite_outcome))


table(sofaset$composite_outcome)


ppv_icu <- subset(elaia2, elaia2$icu_at_rand == 1 & elaia2$diastolic_max14 >= 95 & elaia2$spo2_at_rand <= 90)
ppv_no_icu <- subset(elaia2, elaia2$icu_at_rand == 0 & elaia2$diastolic_max14 >= 95 & elaia2$spo2_at_rand <= 90)

npv_icu <- subset(elaia2, elaia2$icu_at_rand == 1 & elaia2$diastolic_max14 <= 120 & elaia2$spo2_at_rand >= 80)
npv_no_icu <- subset(elaia2, elaia2$icu_at_rand == 0 & elaia2$diastolic_max14 <= 120 & elaia2$spo2_at_rand >= 80)

print(mean(ppv_icu$composite_outcome))
print(mean(ppv_no_icu$composite_outcome))
print(mean(npv_icu$composite_outcome))
print(mean(npv_no_icu$composite_outcome))


npv_icu <- subset(elaia2, elaia2$icu_at_rand == 1 & elaia2$diastolic_max14 <= 120 & elaia2$spo2_at_rand >= 80)
npv_no_icu <- subset(elaia2, elaia2$ward_at_rand == 1 & elaia2$diastolic_max14 <= 100 & elaia2$spo2_at_rand >= 80)

print(mean(ppv_icu$composite_outcome))
print(mean(ppv_no_icu$composite_outcome))
print(mean(npv_icu$composite_outcome))
print(mean(npv_no_icu$composite_outcome))

sickpatients <- subset(elaia2, elaia2$diastolic_max14 >= 110 & elaia2$spo2_at_rand <= 82)



sickmatrix <- table(sickpatients$icu_at_rand,sickpatients$composite_outcome)
overallmatrix <- table(elaia2$icu_at_rand, elaia2$composite_outcome)


sickmatrix

library(mosaic)

oddsRatio(overallmatrix, quiet=F)

# 1/output

oddsRatio(sickmatrix, quiet = F)
1/1.553
1/1.015

# send to ICU prediction

dia_spo2_outside_ICU_RR <- 1/relrisk(sickmatrix)

chisq.test(sickmatrix)

relrisk(sickmatrix,quiet = F)


