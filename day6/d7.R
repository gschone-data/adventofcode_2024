input<- read.delim("~/avent_2024/input7.data",sep=" ",header=F)
input$test0=rowsum(input,na.rm = T,group = )
##test si chiffre > full multiple alors ligne out
##test si chiffre < full adition alors ligne out
input$V1<-sub(":","",input$V1)
input$V1<-as.numeric(input$V1)


input$product <- apply(input[, 2:13], 1, function(x) prod(x, na.rm = TRUE))
input$test_product<-input$V1>input$product

input$sum<-apply(input[, 2:13], 1, function(x) sum(x, na.rm = TRUE))
input$test_sum<-input$V1>=input$sum

input[,.N,test_sum]
input[test_sum==F]
559+675+88+71+656
