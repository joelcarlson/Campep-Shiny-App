##Get and format data

library(sjPlot); library(ggplot2); library(reshape); library(plyr); library(rCharts)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

############Data loading functions#################
getStats <- function(){
    stats <- read.csv("C:/Users/Administrator/Desktop/CAMPEP Pet Project/CAMPEP Stats.csv", stringsAsFactors=FALSE)
    stats$pct.acc <- format(ifelse(stats$Applicants > 0, 100*stats$Offers/stats$Applicants, 0),digits=2)
    stats$pct.res <- format(100*stats$Residency/stats$Graduated, digits=2)
    colnames(stats) <- c("School.Name", "Website", "Campep.Degrees", 
                     "Year", "Degree.Type", "# Applicants", "Offers", 
                     "Matriculated", "# Graduated", "Residency", 
                     "Clinical" ,"Advanced Degrees", "Industry", "Academic", 
                     "Other", "Notes", "Address", "Latitude", "Longitude", "% Accepted", "% Obtain Residency")
    stats$School.Name <- as.factor(stats$School.Name)
    return(stats)
    }

getAddresses <- function(){
    Addresses <- read.csv("C:/Users/Administrator/Desktop/CAMPEP Pet Project/Addresses.csv", stringsAsFactors=FALSE) 
    return(Addresses)
    }
    
getSchoolNames <- function(){
    stats <- getStats()
    return(data.frame(School.Name = stats$School.Name, Degree.Type = stats$Degree.Type))
    }
    


      
    
################Define acc function for plotter ###################   
acc <- function(SchoolName = "Columbia University"){
    stats <- getStats()
    #Exception if school is not in full data list, but has address/accreditation
    if(length(stats[stats$School.Name == SchoolName, "Degree.Type"]) > 0){
    vals <- stats[stats$School.Name == SchoolName, ]
    #Find data for degree type matching first level
    vals <- vals[vals$Degree.Type == levels(as.factor(vals$Degree.Type))[1],]
    vals <- vals[vals$Year == max(vals$Year),]
    return(c(as.character(vals$Year), as.numeric(vals$"% Accepted"), vals$Degree.Type, vals$"# Applicants"))
    }
    else{
    return(c(rep(" ", 4)))
    }    
} 

   

##############map plotter #####################
plotMap <- function(){
    Addresses <- getAddresses()
    #Load new map
    map <- Leaflet$new()
    #Set map coordinates to center roughly on USA
    map$setView(c(37.45, -93.85), zoom = 3)
    #map$set(width = 60)
    map$tileLayer(provider = 'OpenStreetMap.Mapnik')
    #Loop over each school name and create a marker for each with info from acc() (defined in Analysis.r)
    for(i in 1:length(Addresses$School.Name)){
        map$marker(c(Addresses$Latitude[i], Addresses$Longitude[i]), 
                bindPopup = paste("<p>",  Addresses$School.Name[i], " </p>",
                                  "<p>", acc(Addresses$School.Name[i])[1], "</p>", #Date
                                  "<p>", "# Applied:", acc(Addresses$School.Name[i])[4],  "(",acc(Addresses$School.Name[i])[3], ")",  "</p>",
                                  "<p>", "% Accepted:", acc(Addresses$School.Name[i])[2], "</p>"
                                 )
                )}
return(map)}


#########Graphs########

plot.enrol <- function(School = "Columbia University", Degree.Type = "MSc"){
    stats <- getStats()
    enrol.stats <- stats[stats$School.Name == School & stats$Degree.Type == Degree.Type, c("Degree.Type", "Year", "# Applicants", "Offers", "Matriculated")]
    enrol.stats <- melt(enrol.stats, id=c("Degree.Type", "Year"))
    ##highcharts line plot:
    chart <- hPlot(value ~ Year, data=enrol.stats, group="variable")
    chart$xAxis(type="category")
    chart$yAxis(floor=0)
    chart$title(text = School)
    chart$subtitle(text = Degree.Type)
    return(chart)
    #Alternative bar plot
    #ggplot(enrol.stats, aes(Year, value)) + geom_bar(aes(fill=variable), position = "dodge", stat="identity") + scale_fill_manual(values=cbPalette)
    }

    
#plot.jobs is same as plot.enrol except it gives job information.  There should be a check.button in the app to select jobs included    
plot.jobs <- function(School = "Columbia University", Degree.Type = "MSc"){
    stats <- getStats()
    job.stats <- stats[stats$School.Name == School & stats$Degree.Type == Degree.Type, c("Degree.Type", "Year", "# Graduated", "Residency", "Clinical", "Advanced Degrees", "Industry", "Academic", "Other")]
    job.stats <- melt(job.stats, id=c("Degree.Type", "Year"))
    chart <- hPlot(value ~ Year, data=job.stats, group="variable", type=c("column", rep("line",6)))
    chart$xAxis(type="category")
    chart$yAxis(floor=0)
    chart$title(text = School)
    chart$subtitle(text = Degree.Type)
    return(chart)
    #Alernative bar plot
    #ggplot(job.stats, aes(Year, value)) + 
    #             geom_bar(aes(fill=variable), position = "dodge", stat="identity") + 
    #            scale_fill_manual(values=cbPalette) 
    }

##Unused##    
#plot.all returns a plot of # of applicants, or any column for all institutions by year and degree type
plot.all <- function(agg = "# Applicants"){
    stats <- getStats()
    aggdata <- aggregate(stats[,agg], by=list(stats$Year, stats$Degree.Type), sum)
    chart <- hPlot(x ~ Group.1, data=aggdata, group="Group.2")
    return(chart)
    }
    
    
########Statistics########### 
#applicant.percent <- function(SchoolName = "Carleton University") {  
#agg <- aggregate(stats[stats$SchoolName,"# Applicants"], by=list(stats[SchoolName, "Year"], stats[SchoolName,"Degree.Type"]), sum)    
#    agg
#    }
    
percents <- function(SchoolName = "Carleton University", DegreeType = "MSc") {  
    stats <- getStats()
    stats <- stats[stats$School.Name == SchoolName & stats$Degree.Type == DegreeType, c("Year","Degree.Type", "% Accepted", "% Obtain Residency")]
    colnames(stats) <- c("Year", "Degree Type", "% Accepted", "% Obtain Residency")
    return(stats)    
} 
    