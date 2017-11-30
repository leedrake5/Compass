library(shiny)
library(ggplot2)
library(pbapply)
library(reshape2)
library(dplyr)
library(data.table)
library(DT)
library(gridExtra)
library(rhandsontable)
library(Cairo)
library(broom)
library(shinyjs)
library(formattable)
library(markdown)
library(rmarkdown)



options(shiny.maxRequestSize=9000000*1024^2)

options(warn=-1)
assign("last.warning", NULL, envir = baseenv())


shinyServer(function(input, output, session) {
    
    output$selectcal <- renderUI({
        
        if(input$manualoverride==TRUE){
            radioButtons('whichinstrument', label="Choose Instrument", choices=c("T4S2480", "T4S2481"), selected="T4S2480")
        } else {
            p()
        }
        
    })
    
    observeEvent(is.null(input$loadvaldata)==FALSE, {
        

            
        
        
        fullValSpectra <- reactive({
            
            
            withProgress(message = 'Processing Data', value = 0, {
                
                inFile <- input$loadvaldata
                if (is.null(inFile)) return(NULL)
                temp = inFile$name
                temp <- gsub(".csv", "", temp)
                id.seq <- seq(1, 2048,1)
                
                n <- length(temp)*id.seq
                
                myfiles.x = pblapply(inFile$datapath, read_csv_filename_x)
                
                
                
                myfiles.y = pblapply(inFile$datapath, read_csv_filename_y)
                
                
                
                
                xrf.x <- data.frame(id.seq, myfiles.x)
                colnames(xrf.x) <- c("ID", temp)
                xrf.y <- data.frame(id.seq, myfiles.y)
                colnames(xrf.y) <- c("ID", temp)
                
                
                xrf.x <- data.table(xrf.x)
                xrf.y <- data.table(xrf.y)
                
                
                energy.m <- xrf.x[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
                cps.m <- xrf.y[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = ID]
                
                
                spectra.frame <- data.frame(energy.m$value, cps.m$value, cps.m$variable)
                colnames(spectra.frame) <- c("Energy", "CPS", "Spectrum")
                data <- spectra.frame
                
                
                incProgress(1/n)
                Sys.sleep(0.1)
            })
            
            data
        })
        
        

        
        
        myValData <- reactive({
            
                fullValSpectra()
            
            
        })
        
        

        valdata <- myValData()

        
        
        output$contents2 <- renderTable({
            
            
            
            myValData()
            
        })
        

        
        calVariables <- reactive({
            

               T4S2480.cal$Intensities

            
            
        })
        
        calValElements <- reactive({
            ls(T4S2480.cal[[6]])

        })
        
        calVariableElements <- reactive({
            variables <- calVariables()
            variableelements <- ls(variables)
            variableelements
        })
        
        
        
  
        
        
        tableInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            spectra.line.list <- lapply(valelements, function(x) elementGrab(element.line=x, data=val.data))
            element.count.list <- lapply(spectra.line.list, '[', 2)
            
            
            
            spectra.line.vector <- as.numeric(unlist(element.count.list))
            
            dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(valelements))
            
            spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)
            
            colnames(spectra.line.frame) <- c("Spectrum", valelements)
            
            spectra.line.frame <- as.data.frame(spectra.line.frame)
        
            spectra.line.frame
            
            val.line.table <- data.table(spectra.line.frame[, c("Spectrum", valelements), drop = FALSE])
            
            
            
                
                val.line.table


        })



        
        
        fullInputValCounts <- reactive({
            valelements <- calValElements()
            variableelements <- calVariableElements()
            val.data <- myValData()
            
            spectra.line.list <- lapply(variableelements, function(x) elementGrab(element.line=x, data=val.data))
            element.count.list <- lapply(spectra.line.list, `[`, 2)
            
            
            spectra.line.vector <- as.numeric(unlist(element.count.list))
            
            dim(spectra.line.vector) <- c(length(spectra.line.list[[1]]$Spectrum), length(variableelements))
            
            spectra.line.frame <- data.frame(spectra.line.list[[1]]$Spectrum, spectra.line.vector)
            
            colnames(spectra.line.frame) <- c("Spectrum", variableelements)
            
            spectra.line.frame <- as.data.frame(spectra.line.frame)
            
            val.line.table <- spectra.line.frame[c("Spectrum", variableelements)]
            
            
            
            val.line.table
        })

        
        
        output$myvaltable1 <- renderDataTable({
            
            fullInputValCounts()
            
        })




        
        
        
 
 T4S2481InputValQuant <- reactive({
     
     count.table.first <- data.frame(fullInputValCounts())
     the.cal <- T4S2481.cal[[6]]
     elements.cal <- calValElements()
     elements <- elements.cal[!is.na(match(elements.cal, ls(count.table.first)))]
     variables <- calVariableElements()
     valdatafirst <- myValData()
     
     valdatafirst$Instrument <- substr(valdatafirst$Spectrum, 0, 7)

     
     if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2481")==TRUE){
         valdata <- subset(valdatafirst, Instrument=="T4S2481")
     }else if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2481")==FALSE){
             valdata <- NA
     }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){
         valdata <- valdatafirst
     }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){
         valdata <- NA
     }
     
     count.table.first$Instrument <- substr(count.table.first$Spectrum, 0, 7)
     
     if(input$manualoverride==FALSE && any(count.table.first$Instrument=="T4S2481")==TRUE){
         count.table <- dplyr::filter(count.table.first, Instrument %in% "T4S2481")
     }else if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2481")==FALSE){
         count.table <- NA
     }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){
         count.table <- count.table.first
     }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){
         count.table <- NA
     }
     
     if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){count.table$Instrument <- "T4S2481"}
     
     
     if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){valdata$Instrument <- "T4S2481"}

     
     
        predicted.list <- pblapply(elements, function (x)
            if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x)
                )
            } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.tc.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x
                    )
                )
            } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                predict(
                    object=the.cal[[x]][[2]],
                        newdata=simple.comp.prep(
                            data=valdata,
                            spectra.line.table=as.data.frame(
                                count.table
                                ),
                            element.line=x,
                            norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                            norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                            )
                )
            } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                 predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.simp.prep(
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                        )
                 )
            } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.tc.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                )
            } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.comp.prep(
                        data=valdata,
                        spectra.line.table=as.data.frame(
                            count.table
                            ),
                        element.line=x,
                        slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                        intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                        norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                        norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                        )
                )
            }
            )
            
            
            

            
            if(is.na(valdata)==FALSE){predicted.vector <- unlist(predicted.list)}
            
            if(is.na(valdata)==FALSE){dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))}
            
            if(is.na(valdata)==FALSE){predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)}
            
            if(is.na(valdata)==FALSE){colnames(predicted.frame) <- c("Spectrum", elements)}
            
            if(is.na(valdata)==FALSE){predicted.data.table <- data.table(predicted.frame)}
            #predicted.values <- t(predicted.values)
            if(is.na(valdata)==FALSE){predicted.data.table}else{NA}
 
 })
            
            
            
            T4S2480InputValQuant <- reactive({
                
                count.table.first <- data.frame(fullInputValCounts())
                the.cal <- T4S2480.cal[[6]]
                elements.cal <- calValElements()
                elements <- elements.cal[!is.na(match(elements.cal, ls(count.table.first)))]
                variables <- calVariableElements()
                valdatafirst <- myValData()
                
                valdatafirst$Instrument <- substr(valdatafirst$Spectrum, 0, 7)

                
                if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2480")==TRUE){
                    valdata <- dplyr::filter(valdatafirst, Instrument %in% "T4S2480")
                }else if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2480")==FALSE){
                    valdata <- NA
                }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){
                    valdata <- valdatafirst
                }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){
                    valdata <- NA
                }
                
                count.table.first$Instrument <- substr(count.table.first$Spectrum, 0, 7)

                if(input$manualoverride==FALSE && any(count.table.first$Instrument=="T4S2480")==TRUE){
                    count.table <- dplyr::filter(count.table.first, Instrument %in% "T4S2480")
                }else if(input$manualoverride==FALSE && any(valdatafirst$Instrument=="T4S2480")==FALSE){
                    count.table <- NA
                }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){
                    count.table <- count.table.first
                }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){
                    count.table <- NA
                }
                
                if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){count.table$Instrument <- "T4S2480"}

                
                if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){valdata$Instrument <- "T4S2480"}

                
                
                
                predicted.list <- pblapply(elements, function (x)
                if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=general.prep(
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x)
                    )
                } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==2) {
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.tc.prep(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x
                    )
                    )
                } else if(the.cal[[x]][[1]]$CalTable$CalType!=3 && the.cal[[x]][[1]]$CalTable$NormType==3) {
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=simple.comp.prep(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                    )
                    )
                } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==1){
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.simp.prep(
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x,
                    slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                    )
                } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==2){
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.tc.prep(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x,
                    slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept
                    )
                    )
                } else if(the.cal[[x]][[1]]$CalTable$CalType==3 && the.cal[[x]][[1]]$CalTable$NormType==3){
                    predict(
                    object=the.cal[[x]][[2]],
                    newdata=lucas.comp.prep(
                    data=valdata,
                    spectra.line.table=as.data.frame(
                    count.table
                    ),
                    element.line=x,
                    slope.element.lines=the.cal[[x]][[1]][2]$Slope,
                    intercept.element.lines=the.cal[[x]][[1]][3]$Intercept,
                    norm.min=the.cal[[x]][[1]][1]$CalTable$Min,
                    norm.max=the.cal[[x]][[1]][1]$CalTable$Max
                    )
                    )
                }
                )
                
                
           
           
        if(is.na(valdata)==FALSE){predicted.vector <- unlist(predicted.list)}
        
        if(is.na(valdata)==FALSE){dim(predicted.vector) <- c(length(count.table$Spectrum), length(elements))}
        
        if(is.na(valdata)==FALSE){predicted.frame <- data.frame(count.table$Spectrum, predicted.vector)}
        
        if(is.na(valdata)==FALSE){colnames(predicted.frame) <- c("Spectrum", elements)}

        if(is.na(valdata)==FALSE){predicted.data.table <- data.table(predicted.frame)}
        #predicted.values <- t(predicted.values)
        if(is.na(valdata)==FALSE){predicted.data.table}else{NA}
        
        
    
})

tableInputValQuant <- reactive({
    
    if(input$manualoverride==FALSE && is.na(T4S2480InputValQuant())==FALSE && is.na(T4S2481InputValQuant())==TRUE){
            T4S2480InputValQuant()
    }else if(input$manualoverride==FALSE && is.na(T4S2480InputValQuant())==TRUE && is.na(T4S2481InputValQuant())==FALSE){
            T4S2481InputValQuant()
    }else if(input$manualoverride==FALSE && is.na(T4S2480InputValQuant())==FALSE && is.na(T4S2481InputValQuant())==FALSE){
            rbind(T4S2480InputValQuant(), T4S2481InputValQuant())
    }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2480"){
        T4S2480InputValQuant()
    }else if(input$manualoverride==TRUE && input$whichinstrument=="T4S2481"){
        T4S2481InputValQuant()
    }
    
    
})

moleculeEstimate <- reactive({
    
    elemental.results <- tableInputValQuant()
    
    MgCl.fromMg <- elemental.results[,"Mg.K.alpha"]*((fluorescence.lines["Mg", "AtomicWeight"]+fluorescence.lines["Cl", "AtomicWeight"])/fluorescence.lines["Mg", "AtomicWeight"])
    
    MgCl.fromCl <- elemental.results[,"Cl.K.alpha"]*((fluorescence.lines["Mg", "AtomicWeight"]+fluorescence.lines["Cl", "AtomicWeight"])/fluorescence.lines["Cl", "AtomicWeight"])
    
    MgCl.fromMean <- rowMeans(data.frame(MgCl.fromMg, MgCl.fromCl))

    SO4 <- elemental.results[,"S.K.alpha"]*((fluorescence.lines["S", "AtomicWeight"]+fluorescence.lines["O", "AtomicWeight"]*4)/fluorescence.lines["S", "AtomicWeight"])
    
    molecule.frame <- data.frame(MgCl.fromMg, MgCl.fromCl, MgCl.fromMean, SO4)
    colnames(molecule.frame) <- c("MgCl.Mg", "MgCl.Cl", "MgCl.Mean", "SO4")
    rownames(molecule.frame) <- elemental.results$Spectrum
    molecule.frame
    
})
        
        output$myvaltable2 <- renderDataTable({
            
            tableInputValQuant()
            
        })
        
        output$moleculetable <- renderDataTable({
            
            moleculeEstimate()
            
        })
        
        # valtest <- lapply(valelements, function(x) predict(calsList[[x]], as.data.frame(val.line.table[x])))
        
        output$downloadValData <- downloadHandler(
        filename = function() { paste(input$calname, "_ValData", '.csv', sep='', collapse='') },
        content = function(file
        ) {
            write.csv(tableInputValQuant(), file)
        }
        )
        
        


    })


 })





