library("shiny")
library(bigrquery)

library(devtools)
devtools::install_github("hadley/httr")
devtools::install_github("hadley/bigrquery")
devtools::install_github("s-u/PKI") #for SHA256
library(httr)

endpoint <- oauth_endpoints("google")
secrets <- jsonlite::fromJSON("bigrquery-token.json") #change if necessary
scope <- "https://www.googleapis.com/auth/bigquery"

#Convert secrets$private_key from whatever it comes as from Google to RSA
#openssl must be installed
#I'm sure there is a more elegant way to do this
fileConn<-file("temp")
writeLines(secrets$private_key, fileConn)
close(fileConn)
system("openssl rsa -in temp -out temp2")
secrets$private_key <- scan("temp2",what="character",sep="\n")
system("rm temp")
system("rm temp2")

token <- httr::oauth_service_token(endpoint, secrets, scope)
bigrquery::set_access_cred(token)
##path <- "~/.bigrquery-token.json"
##has_auth <- file.exists(path)

##skip_if_no_auth <- function() {
##  if (!has_auth) {
##      skip("Authentication not available")
##  }
##}

##if (has_auth)
##set_service_token(path)

shinyServer <- function(input, output, session){

        output$piePlot <- renderPlot({
            if (input$button1 == 0)
            return()

isolate({
            #Set Project parameters
            projectid <- input$pid
            databasename <- input$dbname
            #Set names to student data table, university ranking table and company ranking table
            studentdatatable <- input$sdtable
            univranktable <- input$urtable
            compranktable <- input$crtable
            #Set field names
            #university name field in student data table
            field_univ_student <- "univ"
            #university name field in university ranking table
            field_univ_univ <- "univ_name"
            #company name field in student data table
            field_comp_student <- "comp"
            #company name field in company ranking table
            field_comp_comp <- "comp_name"
            #year in student data table
            field_year_student <- "eyear"
            #year in university ranking table
            field_year_univ <- "uryear"
            #year in company ranking table
            field_year_comp <- "cryear"
            #university rank in university ranking table
            field_rank_univ <- "univ_rank"
            #company rank in company ranking table
            field_rank_comp <- "comp_rank"
            #package field in student data table
            field_package_student <- "package"
            #qscore field in student data table
            field_qscore_student <- "q_score"
            #id field in student data table
            field_id_student <- "id"
            #year for which the piechart is to be generated
            vyear = input$EvalYear
            
            project <- projectid
            
            sql <- paste("SELECT StDt.", field_id_student, ", UnRk.", field_rank_univ, " FROM (SELECT * FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt JOIN EACH (SELECT * FROM [", projectid, ":", databasename, ".", univranktable, "]) AS UnRk ON StDt.", field_univ_student, " = UnRk.", field_univ_univ, " AND StDt.", field_year_student, " = UnRk.", field_year_univ, ") WHERE StDt.", field_year_student, " = ", vyear, sep="")
            x = query_exec(sql, project = project)
            sql1 <- paste("SELECT StDt.", field_id_student, ", CpRk.", field_rank_comp, " FROM (SELECT * FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt JOIN (SELECT * FROM [", projectid, ":", databasename, ".", compranktable, "]) AS CpRk ON StDt.", field_comp_student, " = CpRk.", field_comp_comp, " AND StDt.", field_year_student, " = CpRk.", field_year_comp, ") WHERE StDt.", field_year_student, " = ", vyear, sep="")
            y = query_exec(sql1, project = project)
            sql2 <- paste("SELECT StDt.", field_id_student, ", StDt.", field_package_student, ", StDt.", field_year_student, " FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt WHERE StDt.", field_package_student, " IS NOT NULL AND StDt.", field_year_student, " = ", vyear, sep="")
            z = query_exec(sql2, project = project)
            sql3 <- paste("SELECT MAX(", field_rank_univ, "), MAX(", field_year_univ, ") FROM [", projectid, ":", databasename, ".", univranktable, "] WHERE ", field_year_univ, " = ", vyear, sep="")
            a = query_exec(sql3, project = project)
            sql4 <- paste("SELECT MAX(", field_rank_comp, "), MAX(", field_year_comp, ") FROM [", projectid, ":", databasename, ".", compranktable, "] WHERE ", field_year_comp, " = ", vyear, sep="")
            b = query_exec(sql4, project = project)
            sql5 <- paste("SELECT MAX(", field_package_student, "), MAX(", field_year_student, ") FROM [", projectid, ":", databasename, ".", studentdatatable, "] WHERE ", field_year_student, " = ", vyear, sep="")
            c = query_exec(sql5, project = project)
            sql6 <- paste("SELECT MIN(", field_package_student, "), MAX(", field_year_student, ") FROM [", projectid, ":", databasename, ".", studentdatatable, "] WHERE ", field_year_student, " = ", vyear, sep="")
            d = query_exec(sql6, project = project)
            sql7 <- paste("SELECT StDt.", field_id_student, ", StDt.", field_year_student, " , StDt.", field_qscore_student, " FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt WHERE StDt.", field_year_student, " = ", vyear, sep="")
            e = query_exec(sql7, project = project)
            rate_univ = 9/(1-a[1,1])
            offset_univ = 1 - (a[1,1] * rate_univ)
            x['qs'] <- NA
            x$ StDt_q_score<- (x$UnRk_univ_rank * rate_univ) + offset_univ
            rate_comp = 4/(1-b[1,1])
            offset_comp = 1 - (b[1,1] * rate_comp)
            y['qs_comp'] <- 0
            y$ qs_comp <- (y$CpRk_comp_rank * rate_comp) + offset_comp
            r = c[1,1]
            q = d[1,1]
            p = r - q
            rate_pack = 5/p
            offset_pack = -(d[1,1] * rate_pack)
            z['qs_pack'] <- 0
            z$qs_pack <- (z$StDt_package * rate_pack) + offset_pack
            fieldid = paste("StDt_", field_id_student, sep="")
            fieldqscore = paste("StDt_", field_qscore_student, sep="")
            u = merge(y, z, by=fieldid, all = TRUE)
            u[is.na(u)] <- 0
            u[fieldqscore] <- 0
            u$StDt_q_score <- u$qs_comp + u$qs_pack
            common <- intersect(names(x), names(u))
            qf = rbind(x[,common], u[,common])
            qfinal = merge(qf, e, by=fieldid, all = TRUE)
            qfinal[is.na(qfinal)] <- 0
            qfinal[fieldqscore] <- 0
            qfinal$StDt_q_score <- qfinal$StDt_q_score.x + qfinal$StDt_q_score.y
            qfinal <- qfinal[order(qfinal$StDt_q_score), ]
            aa <- 0
            av <- 0
            ba <- 0
            avg_aa <- 0
            avg_av <- 0
            avg_ba <- 0
            for(i in 1:length(qfinal$StDt_q_score)){
                if(qfinal$StDt_q_score[i] <= 4.5){
                    ba = ba + 1
                    avg_ba = avg_ba + qfinal$StDt_q_score[i]
                }
                if(qfinal$StDt_q_score[i] > 4.5 & qfinal$StDt_q_score[i] < 7.5){
                    av = av + 1
                    avg_av = avg_av + qfinal$StDt_q_score[i]
                }
                if(qfinal$StDt_q_score[i] >= 7.5){
                    aa = aa + 1
                    avg_aa = avg_aa + qfinal$StDt_q_score[i]
                }
            }
            avg_aa <- round(avg_aa/aa, 2)
            avg_av <- round(avg_av/av, 2)
            avg_ba <- round(avg_ba/ba, 2)
            B <- c(aa, av, ba)
            percentlabels <- round(100*B/sum(B), 1)
            pielabels <- paste(percentlabels,"%",sep="")
            cols = rainbow(length(pielabels))
            fname <- paste("Piechart for Quality Score Breakup", vyear)
            pie(B, main=fname, col=cols, labels= pielabels, cex=0.8, radius = 1.0)
            legend("topright", c("Above Average (AA)", "Average(AV)", "Below Average(BA)"), cex=0.8, fill=cols)
            ptext <- paste("Average Quality Score Values: AA = ", avg_aa, ", AA = ", avg_av, ", BA = ", avg_ba)
            mtext(ptext, side = 1)
    
})
    })
      
        output$linePlot <- renderPlot({

        if (input$button2 == 0)
        return()
        
        isolate({
            #Set Project parameters
            projectid <- input$pid
            databasename <- input$dbname
            #Set names to student data table, university ranking table and company ranking table
            studentdatatable <- input$sdtable
            univranktable <- input$urtable
            compranktable <- input$crtable
            #Set field names
            #university name field in student data table
            field_univ_student <- "univ"
            #university name field in university ranking table
            field_univ_univ <- "univ_name"
            #company name field in student data table
            field_comp_student <- "comp"
            #company name field in company ranking table
            field_comp_comp <- "comp_name"
            #year in student data table
            field_year_student <- "eyear"
            #year in university ranking table
            field_year_univ <- "uryear"
            #year in company ranking table
            field_year_comp <- "cryear"
            #university rank in university ranking table
            field_rank_univ <- "univ_rank"
            #company rank in company ranking table
            field_rank_comp <- "comp_rank"
            #package field in student data table
            field_package_student <- "package"
            #qscore field in student data table
            field_qscore_student <- "q_score"
            #id field in student data table
            field_id_student <- "id"
            
            project <- projectid
            
            sql8 <- paste("SELECT StDt.", field_year_student, " FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt GROUP BY StDt.", field_year_student, " ORDER BY StDt.", field_year_student, " DESC", sep="")
            f = query_exec(sql8, project = project)
            avgagg <- matrix(nrow = 3, ncol = nrow(f))
        for(j in 1:nrow(f)){
            vyear = f[j, 1]
            sql <- paste("SELECT StDt.", field_id_student, ", UnRk.", field_rank_univ, " FROM (SELECT * FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt JOIN EACH (SELECT * FROM [", projectid, ":", databasename, ".", univranktable, "]) AS UnRk ON StDt.", field_univ_student, " = UnRk.", field_univ_univ, " AND StDt.", field_year_student, " = UnRk.", field_year_univ, ") WHERE StDt.", field_year_student, " = ", vyear, sep="")
            x = query_exec(sql, project = project)
            sql1 <- paste("SELECT StDt.", field_id_student, ", CpRk.", field_rank_comp, " FROM (SELECT * FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt JOIN (SELECT * FROM [", projectid, ":", databasename, ".", compranktable, "]) AS CpRk ON StDt.", field_comp_student, " = CpRk.", field_comp_comp, " AND StDt.", field_year_student, " = CpRk.", field_year_comp, ") WHERE StDt.", field_year_student, " = ", vyear, sep="")
            y = query_exec(sql1, project = project)
            sql2 <- paste("SELECT StDt.", field_id_student, ", StDt.", field_package_student, ", StDt.", field_year_student, " FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt WHERE StDt.", field_package_student, " IS NOT NULL AND StDt.", field_year_student, " = ", vyear, sep="")
            z = query_exec(sql2, project = project)
            sql3 <- paste("SELECT MAX(", field_rank_univ, "), MAX(", field_year_univ, ") FROM [", projectid, ":", databasename, ".", univranktable, "] WHERE ", field_year_univ, " = ", vyear, sep="")
            a = query_exec(sql3, project = project)
            sql4 <- paste("SELECT MAX(", field_rank_comp, "), MAX(", field_year_comp, ") FROM [", projectid, ":", databasename, ".", compranktable, "] WHERE ", field_year_comp, " = ", vyear, sep="")
            b = query_exec(sql4, project = project)
            sql5 <- paste("SELECT MAX(", field_package_student, "), MAX(", field_year_student, ") FROM [", projectid, ":", databasename, ".", studentdatatable, "] WHERE ", field_year_student, " = ", vyear, sep="")
            c = query_exec(sql5, project = project)
            sql6 <- paste("SELECT MIN(", field_package_student, "), MAX(", field_year_student, ") FROM [", projectid, ":", databasename, ".", studentdatatable, "] WHERE ", field_year_student, " = ", vyear, sep="")
            d = query_exec(sql6, project = project)
            sql7 <- paste("SELECT StDt.", field_id_student, ", StDt.", field_year_student, " , StDt.", field_qscore_student, " FROM [", projectid, ":", databasename, ".", studentdatatable, "] As StDt WHERE StDt.", field_year_student, " = ", vyear, sep="")
            e = query_exec(sql7, project = project)
            rate_univ = 9/(1-a[1,1])
            offset_univ = 1 - (a[1,1] * rate_univ)
            x['qs'] <- NA
            x$ StDt_q_score<- (x$UnRk_univ_rank * rate_univ) + offset_univ
            rate_comp = 4/(1-b[1,1])
            offset_comp = 1 - (b[1,1] * rate_comp)
            y['qs_comp'] <- 0
            y$ qs_comp <- (y$CpRk_comp_rank * rate_comp) + offset_comp
            r = c[1,1]
            q = d[1,1]
            p = r - q
            rate_pack = 5/p
            offset_pack = -(d[1,1] * rate_pack)
            z['qs_pack'] <- 0
            z$qs_pack <- (z$StDt_package * rate_pack) + offset_pack
            fieldid = paste("StDt_", field_id_student, sep="")
            fieldqscore = paste("StDt_", field_qscore_student, sep="")
            u = merge(y, z, by=fieldid, all = TRUE)
            u[is.na(u)] <- 0
            u[fieldqscore] <- 0
            u$StDt_q_score <- u$qs_comp + u$qs_pack
            common <- intersect(names(x), names(u))
            qf = rbind(x[,common], u[,common])
            qfinal = merge(qf, e, by=fieldid, all = TRUE)
            qfinal[is.na(qfinal)] <- 0
            qfinal[fieldqscore] <- 0
            qfinal$StDt_q_score <- qfinal$StDt_q_score.x + qfinal$StDt_q_score.y
            qfinal <- qfinal[order(qfinal$StDt_q_score), ]
            aa <- 0
            av <- 0
            ba <- 0
            avg_aa <- 0
            avg_av <- 0
            avg_ba <- 0
            for(i in 1:length(qfinal$StDt_q_score)){
                if(qfinal$StDt_q_score[i] <= 4.5){
                    ba = ba + 1
                    avg_ba = avg_ba + qfinal$StDt_q_score[i]
                }
                if(qfinal$StDt_q_score[i] > 4.5 & qfinal$StDt_q_score[i] < 7.5){
                    av = av + 1
                    avg_av = avg_av + qfinal$StDt_q_score[i]
                }
                if(qfinal$StDt_q_score[i] >= 7.5){
                    aa = aa + 1
                    avg_aa = avg_aa + qfinal$StDt_q_score[i]
                }
            }
            avg_aa <- round(avg_aa/aa, 2)
            avg_av <- round(avg_av/av, 2)
            avg_ba <- round(avg_ba/ba, 2)
            
            avgagg[1, j] <- avg_aa
            avgagg[2, j] <- avg_av
            avgagg[3, j] <- avg_ba
        }

        myear <- matrix(nrow = 1, ncol = nrow(f))
        for(j in 1:nrow(f)){
                myear[1, j] = f[j, 1]
        }
        plot(myear, avgagg[1,], type="b",ylim=c(0,10),col="black", xlab="Year",ylab="Quality Score", main="Line Chart for Quality Score")
        grid()
        lines(myear, avgagg[2,], lwd=2, col="red")
        lines(myear, avgagg[3,], lwd=2, col="blue")
        legend("topright", c("Above Average (AA)", "Average(AV)", "Below Average(BA)"), cex=0.5, col=c("black","red","blue"), text.col=c("black","red","blue"), lty=1,lwd=2,pch=4,bty="n", inset=0.01)

        })
    })
    output$textop <- renderText({
        "Uses BigQuery and R Programming"
    })
    output$textopx <- renderText({
        "*Please enter all the required values."
    })
    
}