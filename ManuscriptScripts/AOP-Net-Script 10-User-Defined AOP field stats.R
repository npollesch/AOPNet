
### IMPORTANT: this script relies on objects created in other scripts. Please run the following other scripts to create the required objects:
###   1) "AOP-Net-1-XML Parse.R"                to create raw data files


#Directories
workingDir<-"C:\\Users\\obrienja\\Documents\\GitHub\\AOPWiki\\R_files_Jason\\"


# Convert XML data to an R list for easier manipulation
xList<-as_list(xData)


# identify locations of KE, KER and AOP data in the list
KE_loc<-which(names(xList$data)=="key-event")
KER_loc<-which(names(xList$data)=="key-event-relationship")
AOP_loc<-which(names(xList$data)=="aop")


### Statistics on Key Event fields

# List all KE fields
keFields<-vector()
for(i in 1:length(KE_loc)){
  keFields<-c(keFields, names(xList$data[KE_loc[i]][[1]]))
  keFields<-unique(keFields)
}

keFields
# [1] "title"                                       "short-name"                                 
# [3] "biological-organization-level"               "description"                                
# [5] "measurement-methodology"                     "evidence-supporting-taxonomic-applicability"
# [7] "organ-term"                                  "applicability"                              
# [9] "biological-events"                           "references"                                 
# [11] "source"                                      "cell-term"                                  
# [13] "key-event-stressors" 

# Total number of KEs
numKEs<-length(KE_loc)

# KE ids
KE_ref_id<-vector()
for(i in 1:length(KE_loc)){
  KE_ref_id<-c(KE_ref_id, attr(xList$data[KE_loc[i]][[1]],"id"))
}
KE_id<-keID$ID[match(KE_ref_id,keID$ref)]


# KEs with titles
KE_title<-vector()
for(i in KE_loc){
  KE_title<-c(KE_title,length(xList$data[i][[1]]$title[[1]]))
}
nKE_title<-sum(KE_titles) # 911
pKE_title<-nKE_title/numKEs*100 # 100%


# KEs with short-name
KE_sn<-vector()
for(i in KE_loc){
  KE_sn<-c(KE_sn,length(xList$data[i][[1]]$"short-name"[[1]]))
}
nKE_sn<-sum(KE_sn) # 911
pKE_sn<-nKE_sn/numKEs*100 # 100%


# KEs with biological organization level
KE_bol<-vector()
for(i in KE_loc){
  KE_bol<-c(KE_bol,xList$data[i][[1]]$"biological-organization-level"[[1]])
}
KE_bol<-KE_bol=="Molecular"|KE_bol=="Cellular"|KE_bol=="Tissue"|KE_bol=="Organ"|KE_bol=="Individual"|KE_bol=="Population"
KE_bol<-as.numeric(KE_bol)
nKE_bol<-sum(KE_bol)
pKE_bol<-nKE_bol/numKEs*100


# KEs with description
KE_des<-vector()
for(i in KE_loc){
  KE_des<-c(KE_des,length(xList$data[i][[1]]$"description"))
}
nKE_des<-sum(KE_des)
pKE_des<-nKE_des/numKEs*100


# KEs with measurement-methodology
KE_mm<-vector()
for(i in KE_loc){
  KE_mm<-c(KE_mm,length(xList$data[i][[1]]$"measurement-methodology"))
}
nKE_mm<-sum(KE_mesLen)
pKE_mm<-nKE_mm/numKEs*100


# KEs with evidence-supporting-taxonomic-applicability
KE_esta<-vector()
for(i in KE_loc){
  KE_esta<-c(KE_esta,length(xList$data[i][[1]]$"evidence-supporting-taxonomic-applicability"))
}
nKE_esta<-sum(KE_esta)
pKE_esta<-nKE_esta/numKEs*100


# KEs with organ term
KE_ot<-vector()
for(i in KE_loc){
  KE_ot<-c(KE_ot,length(xList$data[i][[1]]$"organ-term"))
}
KE_ot<-as.numeric(KE_ot==3)
nKE_ot<-sum(KE_ot)
pKE_ot<-nKE_ot/numKEs*100


# KEs with cell term
KE_ct<-vector()
for(i in KE_loc){
  KE_ct<-c(KE_ct,length(xList$data[i][[1]]$"cell-term"))
}
KE_ct<-as.numeric(KE_ct==3)
nKE_ct<-sum(KE_ct)
pKE_ct<-nKE_ct/numKEs*100


# KEs with cell term OR organ term
KE_otct<-as.numeric(KE_ot|KE_ct)
nKE_otct<-sum(KE_otct)
pKE_otct<-nKE_otct/numKEs*100


# KEs with taxonomic applicability data
KE_tax<-vector()
for(i in 1:length(KE_loc)){
  KE_tax<-c(KE_tax, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="taxonomy"))
}
KE_tax<-as.numeric(KE_tax)
nKE_tax<-sum(KE_tax)
pKE_tax<-nKE_tax/numKEs*100


# KEs with sex applicability data
KE_sex<-vector()
for(i in 1:length(KE_loc)){
  KE_sex<-c(KE_sex, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="sex"))
}
KE_sex<-as.numeric(KE_sex)
nKE_sex<-sum(KE_sex)
pKE_sex<-nKE_sex/numKEs*100


# KEs with life-stage applicability data
KE_ls<-vector()
for(i in 1:length(KE_loc)){
  KE_ls<-c(KE_ls, any(attr(xList$data[KE_loc[i]][[1]]$"applicability", "names")=="life-stage"))
}
KE_ls<-as.numeric(KE_ls)
nKE_ls<-sum(KE_ls)
pKE_ls<-nKE_ls/numKEs*100


# All KE fields
KE_field_stats<-cbind(KE_title, KE_sn, KE_bol, KE_otct, KE_des, KE_mm, KE_tax, KE_sex, KE_ls, KE_esta)
row.names(KE_field_stats)<-KE_id

# Save results
# write.table(KE_field_stats, file=paste(workingDir, "KE_field_stats.txt"), sep="\t")


### Statistics Key Event Relationship fields

# List all KER fields
kerFields<-vector()
for(i in 1:length(KER_loc)){
  kerFields<-c(kerFields, names(xList$data[KER_loc[i]][[1]]))
  kerFields<-unique(kerFields)
}
kerFields
# [1] "title"                                       "description"                                
# [3] "weight-of-evidence"                          "quantitative-understanding"                 
# [5] "taxonomic-applicability"                     "evidence-supporting-taxonomic-applicability"
# [7] "references"                                  "source"                                     
# [9] "creation-timestamp"                          "last-modification-timestamp"   


# total number of KERs
numKERs<-length(KER_loc)

# KER ids
KER_ref_id<-vector()
for(i in 1:length(KER_loc)){
  KER_ref_id<-c(KER_ref_id, attr(xList$data[KER_loc[i]][[1]],"id"))
}
KER_id<-kerID$ID[match(KER_ref_id,kerID$ref)]


# KERs with titles
KER_title<-vector()
for(i in 1:length(KER_loc)){
  KER_title<-c(KER_title,length(xList$data[KER_loc[i]][[1]]$title))
}
KER_title<-as.numeric(KER_title==2)
nKER_title<-sum(KER_title)
pKER_title<-nKER_title/numKERs*100


# KERs with descriptions
KER_des<-vector()
for(i in 1:length(KER_loc)){
  KER_des<-c(KER_des,length(xList$data[KER_loc[i]][[1]]$"description"))
}
nKER_des<-sum(KER_des)
pKER_des<-nKER_des/numKERs*100


# KERs with biological plausibility
KER_bp<-vector()
for(i in 1:length(KER_loc)){
  KER_bp<-c(KER_bp,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"biological-plausibility"))
}
nKER_bp<-sum(KER_bp)
pKER_bp<-nKER_bp/numKERs*100


# KERs with emperical-support
KER_es<-vector()
for(i in 1:length(KER_loc)){
  KER_es<-c(KER_es,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"emperical-support-linkage"))
}
nKER_es<-sum(KER_es)
pKER_es<-nKER_es/numKERs*100


# KERs with uncertainties and inconsistencies
KER_ui<-vector()
for(i in 1:length(KER_loc)){
  KER_ui<-c(KER_ui,length(xList$data[KER_loc[i]][[1]]$"weight-of-evidence"$"uncertainties-or-inconsistencies"))
}
nKER_ui<-sum(KER_ui)
pKER_ui<-nKER_ui/numKERs*100


# KERs with quantitative understanding
KER_qu<-vector()
for(i in 1:length(KER_loc)){
  KER_qu<-c(KER_qu,length(xList$data[KER_loc[i]][[1]]$"quantitative-understanding"$description))
}
nKER_qu<-sum(KER_qu)
pKER_qu<-nKER_qu/numKERs*100


# KERs with taxonomic applicability data
KER_tax<-vector()
for(i in 1:length(KER_loc)){
  KER_tax<-c(KER_tax, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="taxonomy"))
}
KER_tax<-as.numeric(KER_tax)
nKER_tax<-sum(KER_tax)
pKER_tax<-nKER_tax/numKERs*100


# KERs with sex applicability data
KER_sex<-vector()
for(i in 1:length(KER_loc)){
  KER_sex<-c(KER_sex, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="sex"))
}
KER_sex<-as.numeric(KER_sex)
nKER_sex<-sum(KER_sex)
pKER_sex<-nKER_sex/numKERs*100


# KERs with life-stage applicability data
KER_ls<-vector()
for(i in 1:length(KER_loc)){
  KER_ls<-c(KER_ls, any(attr(xList$data[KER_loc[i]][[1]]$"taxonomic-applicability", "names")=="life-stage"))
}
KER_ls<-as.numeric(KER_ls)
nKER_ls<-sum(KER_ls)
pKER_ls<-nKER_ls/numKERs*100


# KERs with evidence-supporting-taxonomic-applicability
KER_esta<-vector()
for(i in 1:length(KER_loc)){
  KER_esta<-c(KER_esta,length(xList$data[KER_loc[i]][[1]]$"evidence-supporting-taxonomic-applicability"))
}
nKER_esta<-sum(KER_esta)
pKER_esta<-nKER_esta/numKERs*100


# All KE fields
KER_field_stats<-cbind(KER_title,KER_des,KER_bp,KER_es,KER_qu,KER_ui,KER_tax,KER_sex,KER_ls,KER_esta)
row.names(KER_field_stats)<-KER_id

# Save results
# write.table(KER_field_stats, file=paste(workingDir, "KER_field_stats.txt"), sep="\t")




### Statistics on AOP fields

# list aop fields
aopFields<-vector()
for(i in 1:length(AOP_loc)){
  aopFields<-c(aopFields, names(xList$data[AOP_loc[i]][[1]]))
  aopFields<-unique(aopFields)
}
aopFields
# [1] "title"                       "short-name"                  "authors"                     "status"                     
# [5] "oecd-project"                "abstract"                    "molecular-initiating-event"  "adverse-outcome"            
# [9] "key-event-relationships"     "essentiality-support"        "key-events"                  "applicability"              
# [13] "overall-assessment"          "potential-applications"      "references"                  "source"                     
# [17] "creation-timestamp"          "last-modification-timestamp" "aop-stressors"               "background"  

# Total number of AOPs
numAOPs<-length(AOP_loc)

# AOP ids
AOP_ref_id<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ref_id<-c(AOP_ref_id, attr(xList$data[AOP_loc[i]][[1]],"id"))
}
AOP_id<-aopID$ID[match(AOP_ref_id,aopID$ref)]


# AOPs with title
AOP_title<-vector()
for(i in 1:length(AOP_loc)){
  AOP_title<-c(AOP_title, length(xList$data[AOP_loc[i]][[1]]$title[[1]]))
}
nAOP_title<-sum(AOP_title)
pAOP_title<-nAOP_title/numAOPs*100


# AOPs with short name
AOP_sn<-vector()
for(i in 1:length(AOP_loc)){
  AOP_sn<-c(AOP_sn,length(xList$data[AOP_loc[i]][[1]]$"short-name"[[1]]))
}
nAOP_sn<-sum(AOP_sn)
pAOP_sn<-nAOP_sn/numAOPs*100


# AOP authors
AOP_au<-vector()
for(i in 1:length(AOP_loc)){
  AOP_au<-c(AOP_au,length(xList$data[AOP_loc[i]][[1]]$"authors"))
}
nAOP_au<-sum(AOP_au)
pAOP_au<-nAOP_au/numAOPs*100


#AOP abstract
AOP_ab<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ab<-c(AOP_ab,length(xList$data[AOP_loc[i]][[1]]$"abstract"))
}
nAOP_ab<-sum(AOP_ab)
pAOP_ab<-nAOP_ab/numAOPs*100


#AOP MIE
AOP_mie<-vector()
for(i in 1:length(AOP_loc)){
  AOP_mie<-c(AOP_mie,length(xList$data[AOP_loc[i]][[1]]$"molecular-initiating-event"))
}
nAOP_mie<-sum(AOP_mie)
pAOP_mie<-nAOP_mie/numAOPs*100


#AOP evidence supporting mie
AOP_esm<-vector()
for(i in 1:length(AOP_loc)){
  AOP_esm<-c(AOP_esm,length(xList$data[AOP_loc[i]][[1]]$"molecular-initiating-event"$`evidence-supporting-chemical-initiation`))
}
nAOP_esm<-sum(AOP_esm)
pAOP_esm<-nAOP_esm/numAOPs*100


#AOP adverse outcome
AOP_ao<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ao<-c(AOP_ao,length(xList$data[AOP_loc[i]][[1]]$'adverse-outcome'))
}
nAOP_ao<-sum(AOP_ao)
pAOP_ao<-nAOP_ao/numAOPs*100


#AOP adverse outcome examples
AOP_aoe<-vector()
for(i in 1:length(AOP_loc)){
  AOP_aoe<-c(AOP_aoe,length(xList$data[AOP_loc[i]][[1]]$'adverse-outcome'$'examples'))
}
nAOP_aoe<-sum(AOP_aoe)
pAOP_aoe<-nAOP_aoe/numAOPs*100


#AOP KERs
AOP_ker<-vector()
for(i in 1:length(AOP_loc)){
  if(length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships")==1){
    if(length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"[[1]])==1){
      AOP_ker<-c(AOP_ker,0)      
    }else{
      AOP_ker<-c(AOP_ker,length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"))
    }
  }else{
    AOP_ker<-c(AOP_ker,length(xList$data[AOP_loc[i]][[1]]$"key-event-relationships"))
  }
}
AOP_hasker<-as.numeric(AOP_ker>0)
nAOP_ker<-sum(AOP_hasker)
pAOP_ker<-nAOP_ker/numAOPs*100


#AOP KEs
AOP_ke<-vector()
for(i in 1:length(AOP_loc)){
  if(any(names(xList$data[AOP_loc[i]][[1]]$"key-events")=="key-event")){
    AOP_ke<-c(AOP_ke,length(xList$data[AOP_loc[i]][[1]]$"key-events"))
  }else{
    AOP_ke<-c(AOP_ke,0)
  }
}
AOP_haske<-as.numeric(AOP_ke>0)
nAOP_ke<-sum(AOP_haske)
pAOP_ke<-nAOP_ke/numAOPs*100


#AOP essentiality support
AOP_es<-vector()
for(i in 1:length(AOP_loc)){
  AOP_es<-c(AOP_es,length(xList$data[AOP_loc[i]][[1]]$"essentiality-support"))
}
nAOP_es<-sum(AOP_es>0)
pAOP_es<-nAOP_es/numAOPs*100


#AOP domain of applicability, taxonomic
## the XML file seems to mbe missing all taxonomic applicability data for all AOPs


#AOP domain of applicability, sex
AOP_sex<-vector()
for(i in 1:length(AOP_loc)){
  AOP_sex<-c(AOP_sex,any(names(xList$data[AOP_loc[i]][[1]]$"applicability")=="sex"))
}
AOP_sex<-as.numeric(AOP_sex)
nAOP_sex<-sum(AOP_sex)
pAOP_sex<-nAOP_sex/numAOPs*100


#AOP domain of applicability, life-stage
AOP_ls<-vector()
for(i in 1:length(AOP_loc)){
  AOP_ls<-c(AOP_ls,any(names(xList$data[AOP_loc[i]][[1]]$"applicability")=="life-stage"))
}
AOP_ls<-as.numeric(AOP_ls)
nAOP_ls<-sum(AOP_ls)
pAOP_ls<-nAOP_ls/numAOPs*100


#AOP overall assessment, description
AOP_oad<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oad<-c(AOP_oad,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$description))
}
nAOP_oad<-sum(AOP_oad)
pAOP_oad<-nAOP_oad/numAOPs*100


#AOP overall assessment, applicability
AOP_oaa<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaa<-c(AOP_oaa,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$applicability))
}
nAOP_oaa<-sum(AOP_oaa)
pAOP_oaa<-nAOP_oaa/numAOPs*100


#AOP overall assessment, essentiality summary
AOP_oae<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oae<-c(AOP_oae,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`key-event-essentiality-summary`))
}
nAOP_oae<-sum(AOP_oae)
pAOP_oae<-nAOP_oae/numAOPs*100


#AOP overall assessment, WOE summary
AOP_oaw<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaw<-c(AOP_oaw,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`weight-of-evidence-summary`))
}
nAOP_oaw<-sum(AOP_oaw)
pAOP_oaw<-nAOP_oaw/numAOPs*100


#AOP overall assessment, quantitative summary
AOP_oaq<-vector()
for(i in 1:length(AOP_loc)){
  AOP_oaq<-c(AOP_oaq,length(xList$data[AOP_loc[i]][[1]]$"overall-assessment"$`quantitative-considerations`))
}
nAOP_oaq<-sum(AOP_oaq)
pAOP_oaq<-nAOP_oaq/numAOPs*100


#AOP potential applications
AOP_pa<-vector()
for(i in 1:length(AOP_loc)){
  AOP_pa<-c(AOP_pa,length(xList$data[AOP_loc[i]][[1]]$`potential-applications`))
}
nAOP_pa<-sum(AOP_pa)
pAOP_pa<-nAOP_pa/numAOPs*100


#AOP stressors
AOP_st<-vector()
for(i in 1:length(AOP_loc)){
  AOP_st<-c(AOP_st,length(xList$data[AOP_loc[i]][[1]]$"aop-stressors"))
}
AOP_st<-as.numeric(AOP_st>0)
nAOP_st<-sum(AOP_st)
pAOP_st<-nAOP_st/numAOPs*100


#AOP background
AOP_bg<-vector()
for(i in 1:length(AOP_loc)){
  AOP_bg<-c(AOP_bg,length(xList$data[AOP_loc[i]][[1]]$"background"))
}
nAOP_bg<-sum(AOP_bg)
pAOP_bg<-nAOP_bg/numAOPs*100


# All AOP fields
AOP_field_stats<-cbind(AOP_title,AOP_sn,AOP_au,AOP_ab,AOP_bg,AOP_haske,AOP_hasker,AOP_mie,AOP_esm,AOP_ao,AOP_aoe,AOP_es,AOP_sex,AOP_ls,AOP_pa,AOP_st,AOP_oad,AOP_oaa,AOP_oae,AOP_oaw,AOP_oaq)
row.names(AOP_field_stats)<-AOP_id

# Save results
# write.table(AOP_field_stats, file=paste(workingDir, "AOP_field_stats.txt"), sep="\t")



