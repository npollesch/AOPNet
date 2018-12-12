library(xml2)

fName<-"data/aop-wiki-xml-2018-10-01.xml"

xData<-read_xml(fName)
xData<-xml_ns_strip(xData)


### Ref ID to AOPwiki ID

keID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

kerID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

aopID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)


### Key event (KE) Data

keData<-data.frame(
  ID=keID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event"), "id"),keID$ref)],
  title=xml_text(xml_find_all(xData, "/data/key-event/title")),
  LOBO=xml_text(xml_find_all(xData, "/data/key-event/biological-organization-level")),
  stringsAsFactors=FALSE
)


### Key event relationship (KER) Data

kerData<-data.frame(
  ID=kerID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event-relationship"), "id"),kerID$ref)],
  KEup=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/upstream-id")),keID$ref)],
  KEdown=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/downstream-id")),keID$ref)],
  stringsAsFactors=FALSE
)


### AOP data

# OECD status: not all aops have an "oecd-status" xml tag, so must us "if" to return NA when missing
oecdStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("oecd-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"oecd-status")))
  }else{
    return("not specified")
  }
})

# SAAOP status: not all aops have an "saaop-status" xml tag, so must us "if" to return NA when missing
saaopStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("saaop-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"saaop-status")))
  }else{
    return("not specified")
  }
})

# MIEs: more than one MIE possible per aop, so must return list
mies<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("molecular-initiating-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})

# AOs: more than one AO possible per aop, so must return list
aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("adverse-outcome"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})


# KEs: more than one KE possible per aop, so must return list
kes<-lapply(xml_find_all(xData, "/data/aop/key-events"),FUN=function(x){
  if("key-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])
  }else{
    return(NULL)
  }
})

# KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
# So must return data frame of KERs
kers<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
  if("relationship"%in%xml_name(xml_children(x))){
    return(data.frame(
      ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
      adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
      quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
      woe=xml_text(xml_find_all(x, "relationship/evidence")),
      stringsAsFactors=FALSE
    ))
  }else{
    return(NULL)
  }
})

# add kes and MIE/AO designation (which is AOP-specific) for each KER in kers data.frame
for(i in 1:length(kers)){
 if(length(kers[[i]])>0){
  KEup<-kerData$KEup[match(kers[[i]]$ID,kerData$ID)]
  KEDup<-sapply(KEup, FUN=function(x){
    if(x%in%mies[[i]]){
      return("MIE")
    }else{
      if(x%in%aos[[i]]){
        return("AO")
      }else{
        return("KE")
      }
    }
  })

  KEdown<-kerData$KEdown[match(kers[[i]]$ID,kerData$ID)]
  KEDdown<-sapply(KEdown, FUN=function(x){
    if(x%in%mies[[i]]){
      return("MIE")
    }else{
      if(x%in%aos[[i]]){
        return("AO")
      }else{
        return("KE")
      }
    }
  })

  kers[[i]]<-data.frame(
     ID=kers[[i]]$ID,
     KEup=KEup,
     KEDup=KEDup,
     KEdown=KEdown,
     KEDdown=KEDdown,
     adjacency=kers[[i]]$adjacency,
     quant=kers[[i]]$quant,
     woe=kers[[i]]$woe,
     row.names=NULL,
     stringsAsFactors = FALSE
  )
 }
}


aopData<-data.frame(
  ID=aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"),aopID$ref)],
  oecdStatus=oecdStatus,
  saaopStatus=saaopStatus,
  mies=I(mies),
  aos=I(aos),
  kes=I(kes),
  kers=I(kers),
  stringsAsFactors=FALSE
)

