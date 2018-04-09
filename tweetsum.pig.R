ui <- fluidPage(
titlePanel("TweetSum : Your automated news summarizer"),
sidebarLayout(
sidebarPanel(
textInput(inputId = "trend", label = "Type a trend to summarize"),
actionButton("do", "Search")
),
mainPanel(
h2('Step 1: The most tweeted keywords cloud'),
plotOutput("cloud"),
hr(),
h2('Step 2: The top 10 keywords for summarization'),
textOutput("frequency"),
hr(),
h2('Step 3: The links used'),
textOutput("links"),
hr(),
h2('Step 4: The summary'),
htmlOutput("summary")
)
)
)

server <- function(input, output) {
freq <- reactive({
trend1<- searchTwitter(input$trend, n=200,lang="en",resultType="recent")
trend_text <- sapply(trend1,function(x) x$getText())
trend_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trend_text)
trend_text <- gsub("@\\w+", "", trend_text)
trend_text <- gsub("[[:punct:]]", "", trend_text)
trend_text <- gsub("[[:digit:]]", "", trend_text)
trend_text <- gsub("http\\w+", "", trend_text)
trend_text <- gsub("[ \t]{2,}", "", trend_text)
trend_text <- gsub("^\\s+|\\s+$", "", trend_text)
trend_text <- gsub("#\\w+", "", trend_text)

out <- capture.output(trend_text)
cat("", out, file="web.txt", append=FALSE)
mytf1 <- textfile("web.txt")
mycorpus1 <- corpus(mytf1)
mydfm <- dfm(mycorpus1, ignoredFeatures = c("rt","t.co","https","amp","u","n","j","still","will","should","could","would","im","v","we","i","623c","613c","643c","633c","653c","383c","393c",stopwords("english")), removeNumbers= TRUE, stem= FALSE, removeTwitter=TRUE)
a=topfeatures(mydfm,10)
print(a)
a=names(topfeatures(mydfm,10))
print(a)
})
links <- reactive({
search.term<-input$trend
getGoogleURL <- function(search.term, domain = '.co.uk', quotes=TRUE) {
search.term <- gsub(' ', '%20', search.term)
if(quotes) search.term <- paste('%22', search.term, '%22', sep='')
getGoogleURL <- paste('http://www.google', domain, '/search?q=',
search.term, sep='')
}
getGoogleLinks <- function(google.url) {
doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                             (2.10.0)"))
   html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                          (...){})
   nodes <- getNodeSet(html, "//h3[@class='r']//a")
   return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}
quotes <- "FALSE"
search.url <- getGoogleURL(search.term=search.term, quotes=quotes)
llinks <- getGoogleLinks(search.url)


llinks<-gsub('/url\\?q=','',sapply(strsplit(llinks[as.vector(grep('url',llinks))],split='&'),'[',1))
for(i in 1:10)
{
if(grepl("twitter",llinks[i])==TRUE){
llinks[i]<-as.integer(0)}
if(grepl("facebook",llinks[i])==TRUE){
llinks[i]<-as.integer(0)}
if(grepl("instagram",llinks[i])==TRUE){
llinks[i]<-as.integer(0)}
if(grepl("youtube",llinks[i])==TRUE){
llinks[i]<-as.integer(0)}
}
nlinks<-array()
j<-as.integer(1)
for(i in 1:10)
{
if(grepl("0",llinks[i])==FALSE){
nlinks[j]<-llinks[i]
j=j+1
}
}
paste(nlinks)
})
sum <- reactive({
trend1<- searchTwitter(input$trend, n=200,lang="en",resultType="recent")
trend_text <- sapply(trend1,function(x) x$getText())
trend_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trend_text)
trend_text <- gsub("@\\w+", "", trend_text)
trend_text <- gsub("[[:punct:]]", "", trend_text)
trend_text <- gsub("[[:digit:]]", "", trend_text)
trend_text <- gsub("http\\w+", "", trend_text)
trend_text <- gsub("[ \t]{2,}", "", trend_text)
trend_text <- gsub("^\\s+|\\s+$", "", trend_text)
trend_text <- gsub("#\\w+", "", trend_text)

out <- capture.output(trend_text)
cat("", out, file="web.txt", append=FALSE)
mytf1 <- textfile("web.txt")
mycorpus1 <- corpus(mytf1)
mydfm <- dfm(mycorpus1, ignoredFeatures = c("rt","t.co","https","amp","u","n","j","still","will","should","could","would","im","v","we","i","623c","613c","643c","633c","653c","383c","393c",stopwords("english")), removeNumbers= TRUE, stem= FALSE)
a=names(topfeatures(mydfm,10))

search.term<-input$trend
getGoogleURL <- function(search.term, domain = '.co.uk', quotes=TRUE) {
search.term <- gsub(' ', '%20', search.term)
if(quotes) search.term <- paste('%22', search.term, '%22', sep='')
getGoogleURL <- paste('http://www.google', domain, '/search?q=',
search.term, sep='')
}
getGoogleLinks <- function(google.url) {
doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                             (2.10.0)"))
   html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                          (...){})
   nodes <- getNodeSet(html, "//h3[@class='r']//a")
   return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}
quotes <- "FALSE"
search.url <- getGoogleURL(search.term=search.term, quotes=quotes)
llinks <- getGoogleLinks(search.url)


llinks<-gsub('/url\\?q=','',sapply(strsplit(llinks[as.vector(grep('url',llinks))],split='&'),'[',1))
print(llinks)
abcxyz<-'abcxyz'
for(i in 1:10)
{
if(grepl("twitter",llinks[i])==TRUE){
llinks[i]<-abcxyz}
if(grepl("facebook",llinks[i])==TRUE){
llinks[i]<-abcxyz}
if(grepl("instagram",llinks[i])==TRUE){
llinks[i]<-abcxyz}
if(grepl("youtube",llinks[i])==TRUE){
llinks[i]<-abcxyz}
}
nlinks<-array()
j<-as.integer(1)
for(i in 1:10)
{
if(grepl("abcxyz",llinks[i],fixed=TRUE)==FALSE){
nlinks[j]<-llinks[i]
j=j+1
}
}
print(nlinks)

html <- getURL(nlinks[], followlocation = TRUE)
doc = htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
out1 <- capture.output(plain.text)
cat("", out1, file="hopes.txt", append=FALSE)
tx  <- readLines("hopes.txt")
tx2  <- gsub(pattern = "   ", replace = "", x = tx)
writeLines(tx2, con="hopes.txt")
tx  <- readLines("hopes.txt")
tx5  <- gsub(pattern = "\\\n", replace = "", x = tx)
writeLines(tx5, con="hopes.txt")
tx  <- readLines("hopes.txt")
tx6  <- gsub(pattern = "\"", replace = "", x = tx)
writeLines(tx6, con="hopes.txt")
tx  <- readLines("hopes.txt")
tx7  <- gsub(pattern = "\\\\", replace = "", x = tx)
writeLines(tx7, con="hopes.txt")
tx  <- readLines("hopes.txt")
tx8  <- gsub(pattern = "\\[.*?\\]", replace = "", x = tx)
writeLines(tx8, con="hopes.txt")
tx  <- readLines("hopes.txt")
tx3  <- gsub(pattern = ".  ", replace = ". ", x = tx, fixed=TRUE)
writeLines(tx3, con="hopes.txt")

mapfile <- textfile("C:/Users/Chaitali/Documents/hopes.txt")
mapcorpus <- corpus(mapfile)
b<-segment(mapcorpus,what="sentences")
count<-ndoc(b)
WikiSent=toLower(tokenize(b))
for(i in 1:count)
{
countarr[i]=0
}

for(i in 1:count){
nw<-as.integer(length(WikiSent[i]))
text<-WikiSent[i]
nk<-length(text)
for(k in 1:nk){
if(grepl(a[1], text[k], fixed=TRUE)==TRUE){
countarr[i]=countarr[i]+3}
for(j in 2:10){
if(grepl(a[j], text[k], fixed=TRUE)==TRUE){
countarr[i]=countarr[i]+1
}
}
}
if((ntoken(b[i]))>50){
countarr[i]=0}
}
print(countarr)
dd<-data.frame(matrix(nrow=count,ncol=1))
for( i in 1:count)
{
dd[i,]<-countarr[i]
}

for(i in 1:count)
{
dd["index"] <- i
}

dd["index"] <- (1:count)
out2 <- capture.output(texts(b))
cat("", out2, file="b.txt", append=FALSE)
dd <- arrange(dd,desc(matrix.nrow...count..ncol...1.),index)
print(length(WikiSent[1]))
print(WikiSent[1])
HTML(paste(b[dd[1:5,2]],sep='<br/>'))
})
wcloud <- reactive({
trend1<- searchTwitter(input$trend, n=200,lang="en",resultType="recent")
trend_text <- sapply(trend1,function(x) x$getText())
trend_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", trend_text)
trend_text <- gsub("@\\w+", "", trend_text)
trend_text <- gsub("[[:punct:]]", "", trend_text)
trend_text <- gsub("[[:digit:]]", "", trend_text)
trend_text <- gsub("http\\w+", "", trend_text)
trend_text <- gsub("[ \t]{2,}", "", trend_text)
trend_text <- gsub("^\\s+|\\s+$", "", trend_text)
trend_text <- gsub("#\\w+", "", trend_text)

out <- capture.output(trend_text)
cat("", out, file="web.txt", append=FALSE)
mytf1 <- textfile("web.txt")
mycorpus1 <- corpus(mytf1)
mydfm <- dfm(mycorpus1, ignoredFeatures = c("rt","t.co","https","amp","u","n","j","still","will","should","could","would","im","v","we","i","623c","613c","643c","633c","653c","383c","393c",stopwords("english")), removeNumbers= TRUE, removeTwitter= TRUE, stem= FALSE)
a=names(topfeatures(mydfm,10))
plot(mydfm, colors = RColorBrewer::brewer.pal(8,"Dark2"))
})



observeEvent(input$do, {
output$frequency <-renderText({
freq()
})
output$summary <- renderUI({
sum()
})
output$links <- renderText({
links()
})
output$cloud <- renderPlot({
wcloud()
})
}
)
}
shinyApp(ui = ui, server = server)