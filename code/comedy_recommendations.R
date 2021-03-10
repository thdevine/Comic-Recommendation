#This Script is written by Thomas Devine 02-21-2021
#   Disclaimers: 
#           1.   content, quotes, and discussion stem from Stephan J Kaplan's work found 
#                written about in an article here https://stephenjkaplan.github.io/2020/09/18/standup-comedy-recommender/
#                and code written by him found here https://github.com/stephenjkaplan/standup-comedy-recommender/tree/master/analysis.
#                I DO NOT HAVE THE same approach to (pre)processing, however, it's worth mentioning that some steps are 
#                unavoidable and shared (like removing foreign-language monologues).
#           2.   I independently thought of doing this subject and took a different, albeit function similar approach. 
#                I only include his work since it's the only other project I've found on this subject so it's worth 
#                disclosing it for posterity's sake.
#   DIFFERENCES between Kaplan's work and mine
#                I differ in many areas, most notably, including non-special monologues, external genres for topics modeling, outside lists of
#                comedians within preset genres (for comparisons), extrapolating for recommendations, etc.
#                In general, I'm adding an extra supervised stage to allow for more filtering at the final stage of production of the app (showcasing the project). 
#-----------------------------------------------------------------------------------------------------------------
#LINKS
#   DATA: scrapsfromtheloft.com/stand-up-comedy-scripts/
#   Kaplan: stephenjkaplan.github.io/2020/09/18/standup-comedy-recommender/, github.com/stephenjkaplan/standup-comedy-recommender, https://github.com/stephenjkaplan/standup-comedy-recommender/tree/master/analysis
#   other: www.analyticsvidhya.com/blog/2019/08/how-to-remove-stopwords-text-normalization-nltk-spacy-gensim-python/, https://cran.r-project.org/web/packages/tm/tm.pdf
#-----------------------------------------------------------------------------------------------------------------
#CLEAN data (transcript errors: 4 duplicated and 5 password-protected <==> 361 total)
    #1. tokenization (+ cleaning)
    #2. stemming
    #3. lemmatization
    #1. Clean the text by removing punctuation, non-essential parts like the person introducing the comedian, removing numbers, removing line breaks, crazy noises and expressions like, and common English stop words.
    #2. stem words, then lemmatize words to reduce all forms of words to their base or lemma (eg changing/changed to chang). The purpose of this is to extract the core meaning and topics out of the text rather than pay attention to how words are being used in the context of a sentence. The Python NLTK library was very useful for this step and several of the previous steps.
    #3. Vectorized the entire corpus. In general, this means converting the long strings of text to tabular format, where each column is a word, each row represents a document in the corpus (also known as a doc-term matrix), and each cell contains a numerical representation of the words frequency of use or some other metric.
        # Analyze content of data
            #count vectorization: 
            #tf-idf vectorization: tutorials.datasciencedojo.com/text-analytics-with-r-tf-idf/
            #
            #Latent semantic analysis: 
            #Linear Direlect Analysis:
            #Non-negative_matrix_factorization (NMF): (wiki quote)"...or where the data are provided in streaming fashion. One such use is for collaborative filtering in recommendation systems, where there may be many users and many items to recommend, and it would be inefficient to recalculate everything when one user or one item is added to the system. The cost function for optimization in these cases may or may not be the same as for standard NMF, but the algorithms need to be rather different..."
#-----------------------------------------------------------------------------------------------------------------
# IMPORTANT:
#       Running the code for the first time demands you uncomment the section that saves parsed data to .rds files.
#       To find that section do the following: ctrl+f, then "***"
#   PAPERS:
#       Efficient NMF for NLP: http://proceedings.mlr.press/v28/arora13.html
#       Computing NMF: https://www.cs.cornell.edu/courses/cs6241/2020sp/readings/Arora-2016-NMF.pdf

#Clear stuff first; set working directory
rm(list=ls()); gc(); options(digits = 22)
options(scipen=999) # turn off scientific notation
setwd("C:/Users/tdevine/Box Sync/learning/projects")

# Make functions; 
readInPackages = function(packages){
    #for reading in packages
    packsInstalling <- packages[!packages %in% installed.packages()]
    for(lib in packsInstalling) install.packages(lib, dependencies = TRUE)
    sapply(packages, require, character=TRUE)
}

# Load packages       
packages <- c(#Data wrangling
               "tidyverse",#"data.table",
              #web-scraping
              "rvest",
              #Text mining, NLP-related; https://cran.r-project.org/web/views/NaturalLanguageProcessing.html
              "tm", #https://cran.r-project.org/web/packages/tm/tm.pdf
              "tidytext", #for document term matrix stuff
              "openNLP",
                # "cleanNLP", #for cleaning corpus, https://cran.r-project.org/web/packages/cleanNLP/cleanNLP.pdf
                #Analysis packages
                "lsa", #for latent semantic analysis (https://cran.r-project.org/web/packages/lsa/lsa.pdf)
                "lda", #for linear direlect analysis (https://cran.r-project.org/web/packages/lda/lda.pdf)
                "topicmodels",                       #https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
                    #CTM(), LDA()
                "stm", #for structural topics models, esp words -to- topic matches (https://cran.r-project.org/web/packages/stm/stm.pdf)
                    #findThoughts(); findTopics(); manyTopics(); searchK() [this fn runs selectModel() seen below]
                    #selectModel(); stm() 
              #Graphing and visualization
               "ggplot2","wordcloud",
              #progress bar
              "pbapply"#,
              #connects Python
                # "reticulate"
              )
    readInPackages(packages)
        #List known Command Conflicts
            # -- Conflicts ------------------------------------------ tidyverse_conflicts() --
            #     x dplyr::filter() masks stats::filter()
            # x dplyr::lag()    masks stats::lag()
#-----------------------------------------------------------------------------------------
    # 1. parse the name of the urls text to get the name title 
    # 2. find the beginning of the monologue within each url
    # 3. check if people singin within
    # -remove/transform:
    #       carriage returns, lowercase, stopwords
    
#NEXT, PARSE TEXT TO GET CORPUS
    #Get the URLS
        html_data = read_html("https://scrapsfromtheloft.com/stand-up-comedy-scripts/") #get all text
        links <- html_data %>% html_nodes("a")
        urls <- links %>% html_attr("href")
    #Remove irrelevant entries (most are not URLS)
        urls = urls[-(438:length(urls))] %>%.[-(1:67)]; #from end, then from beginning
            #370 urls
    #Make a function to get titles
        getHeaders = function(urls) urls %>% read_html() %>% html_nodes("h1") %>% html_text();
    #Make a function to get body/monologue
        getBody = function(urls) urls %>% read_html() %>% html_nodes("body") %>% html_text(); 
    #Clean Titles; some of them dont follow format "first last: title (year)", 
        #find which: note "comedy central presents" and "carlin at carnegie", "louis c.k.: 2017", 
        #"sincerely louis CK", "[101] "doug stanhope on babies and abortion"
        #"sarah silverman: saturday night live monologue season 40 | episode 2 | 10/04/2014"
        #93, 266, 132, 148 (transcr), 276
        
        ##THE following two commands should be ran the first time 
        # titles0= unname(pbsapply(urls,function(x){getHeaders(x)  }))       #***
        # write.table(titles0,"titles.txt", row.names=F, col.names=F)        #***
        ####
        titles0= unlist(unname(read.table("titles.txt", header = F, sep = "\t"))) 
        titles=tolower(titles0)%>%# make lowercase
                sapply(.,function(x)gsub('(\\)).*' ,"\\1",x)) %>%unname(.)%>% #removes everything after ")", but keeps ")", remove names on list
                sapply(.,function(x)gsub('(\\[).*' ,"",x)) %>% unname(.) %>%      #removes everything after "[", including "["
                sapply(.,function(x)gsub('(protected\\:)' ,"",x)) %>%unname(.)%>% #removes everything after "[", including "["
                    unname(titles2.1) %>%
                sapply(.,function(x)gsub('full tra.*' ,"",x)) %>% unname(.) %>%   #removes everything after " full", including "full"
                sapply(.,function(x)gsub(' transcript' ,"",x)) %>% unname(.) %>%  #remove " transcript"
                sapply(.,function(x)gsub('[.,\"]' ,"",x)) %>% unname(.) %>%  # remove punctuation (eg.: ",", ".", "'")
                sapply(.,function(x)gsub("[^[:alnum:]\\:]", " ", x)) %>% unname(.) %>%  # remove punctuation ")", "-", 
                trimws(.) #remove white space at front and tail
        
        #need names for files, here we just remove colons so no errors in saving with names script name
            filenames = sapply(titles,function(x)gsub(":", "", x));
                filenames=unname(filenames);tail(filenames,30) ; 
                "four files are duplicated: "; filenames[which(duplicated(filenames))]
        #split based on most common format, "first last: title (year)"; note "comedy central presents" and "carlin at carnegie"
            titles1 = stringi::stri_split_fixed(titles,":",n = 2); #split titles at first ":", previous ...=sapply(titles3.4, function(x) strsplit(x,"\\:"))
                # tail(titles4,60); #sum(sapply(titles4, function(x)length(x))>2) #since we have one show with 2 ":"
                
            #address titles that didn't split (i.e., not typical format), note, not all atypical formats are here
                t1 = titles1[sapply(titles1, function(x)length(x))==1]
            
            #address double element vector (ideally, that fit format with year in second element)
                t2 = titles1[sapply(titles1, function(x)length(x))==2]
                    #titles and year
                    t2.ty = c(); i=1
                    while(i < length(t2)){
                        t2.ty[i] <- t2[[i]][2]; i=i+1
                    }
                    t2.ty=trimws(t2.ty)
        "done trimming titles ONLY";

    #--------------------------------------------------------------------------------------------------------------------------------------
    #Clean text bodies from each url (the monologues)
        # step 0: get mostly the monologue after removing superfluous head and tail stuff
            # # ***(begin) Run this ONCE: save main body of text from each url, save each as the filenames[i]corresponding to the i^th url
            # #INPUTS : urls, filenames, i^th value for file naming niceness (can always remove leading numbers later or have no numbers at all)
            # #OUTPUTS: save data to .TXT file
            # setwd("C:/Users/tdevine/Box Sync/learning/projects/savedTextFiles")
            # get.n.save = function(url,filename){
            #     dat = c(paste0(#first parse p nodes of each url
            #         url %>% read_html() %>% html_nodes("p") %>% html_text() %>%
            #        gsub("[\r\n\t]", "", .)%>%                   #remove carriage returns
            #        iconv(., "latin1", "ASCII", sub="")%>%       #remove emojis, foreign stuff, and odd symbols
            #        .[-((length(.)-9):length(.))],
            #    collapse=" "))
            #     return(write.table(dat,paste0(filename,".txt")))
            # }
            # for(i in 171:length(urls)){
            #     if(i %in% c(95,96,266:268) ){next;} #password protected content <=> cannot be parsed, esp not with format I got going on
            #     else{get.n.save(urls[i],filenames[i])}
            # }
            # setwd("C:/Users/tdevine/Box Sync/learning/projects")
            # "done saving the (parsed) body of each URL for the monologue transcript";
            # #***(END)
        #step 1: transformations (to lowercase, remove stopwords)
            # tolower(.); nchar(a0)

#-------------------------------------------------------------------------------------------
# #Genres #https://en.wikipedia.org/wiki/Comedic_genres#:~:text=Comedic%20genres%20%20%20%20Genre%20%20,John%20Th%20...%20%2013%20more%20rows%20
# knownGenres= c("aggressive humor",    "alternative comedy",    "anecdotal comedy",        "anti-humor",        "black comedy/dark comedy",
#                "blue comedy",        "burlesque",               "character comedy",       "cringe comedy",      "deadpan comedy",        
#                "heritage comedy",     "improvisational comedy", "inside humor",           "insult comedy",      "mockumentary",   
#                "comedy music",        "observational comedy",   "one-line joke",          "physical comedy",    "prop comedy",  
#                "shock humor",          "sitcom",                  "sketch",                "spoof/parody",      "surreal comedy",
#                "satire/topical comedy",                         "ventriloquism",           "wit/word play")

            
    
    
    
    
    
    
    
    
    
    
    
    
    
   
# OLD CODE------------------------------------------------------
#for getting the body before I just used the node "p"
    # setwd("C:/Users/tdevine/Box Sync/learning/projects/savedTextFiles")
    # get.n.save= function(x,filename){
    #     dat = getBody(x)
    #     return(write.table(dat,paste0(filename,".txt")))
    # }
    # for(i in 1:length(urls)){
    #     get.n.save(urls[i],filenames[i])
    # }
    # setwd("C:/Users/tdevine/Box Sync/learning/projects")
        
        #old step 0 under clean body
        # a0 = urls[95]%>% read_html() %>%            #get body (where the important stuff is)
        #     html_nodes("body") %>% html_text() %>%
        #     gsub("[\r\n\t]", "", .)%>%                   #remove carriage returns
        #     iconv(., "latin1", "ASCII", sub="")%>%       #remove emojis, foreign stuff, and odd symbols
        #     gsub("ARTICLEShare.*", "", .)%>%             #remove all characters after a ARTICLEShare (code for sharing via fb, twit, linkedin, etc)
        #     sub(".*smusichistorybookssearchcomedy",      #remove everything before fixed string
        #         "", .)# %>%