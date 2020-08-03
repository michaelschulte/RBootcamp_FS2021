
crime = read_csv('_sessions/PlottingII/1_Data/crime.csv')

verbrechen = crime %>%
  gather(verbrechen, haeufigkeit, c(murders, robberies, assaults, burglaries, larcenies, autoTheft, arsons)) %>%
  select(communityname, state, population, PopDens, householdsize, medIncome, pctWRetire, PctPopUnderPov, PctUnemployed,
         PctUsePubTrans, verbrechen, haeufigkeit) %>%
  rename(gemeinde = communityname,
         staat = state,
         bevoelkerung = population,
         bevoelkerung_dichte = PopDens,
         haushalt_groesse = householdsize,
         median_einkommen = medIncome,
         prozent_pension = pctWRetire,
         prozent_armut = PctPopUnderPov,
         prozent_arbeitslos = PctUnemployed,
         prozent_metro = PctUsePubTrans) %>%
  mutate(verbrechen = case_when(
    verbrechen == 'arsons' ~ 'brandstiftung',
    verbrechen == 'assaults' ~ 'körperverletzung',
    verbrechen == 'autoTheft' ~ 'autodiebstahl',
    verbrechen == 'burglaries' ~ 'einbruch',
    verbrechen == 'larcenies' ~ 'diebstahl',
    verbrechen == 'murders' ~ 'mord',
    verbrechen == 'assaults' ~ 'körperverletzung',
    verbrechen == 'robberies' ~ 'raubüberfall',
  ))

write_csv(verbrechen,'_sessions/PlottingII/1_Data/verbrechen.csv')
