<!DOCTYPE html>
<!-- saved from url=(0014)about:internet -->
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta http-equiv="x-ua-compatible" content="IE=9" >

<title>Practical Machine Learning Project</title>

<style type="text/css">
body, td {
   font-family: sans-serif;
   background-color: white;
   font-size: 12px;
   margin: 8px;
}

tt, code, pre {
   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;
}

h1 { 
   font-size:2.2em; 
}

h2 { 
   font-size:1.8em; 
}

h3 { 
   font-size:1.4em; 
}

h4 { 
   font-size:1.0em; 
}

h5 { 
   font-size:0.9em; 
}

h6 { 
   font-size:0.8em; 
}

a:visited {
   color: rgb(50%, 0%, 50%);
}

pre {	
   margin-top: 0;
   max-width: 95%;
   border: 1px solid #ccc;
   white-space: pre-wrap;
}

pre code {
   display: block; padding: 0.5em;
}

code.r, code.cpp {
   background-color: #F8F8F8;
}

table, td, th {
  border: none;
}

blockquote {
   color:#666666;
   margin:0;
   padding-left: 1em;
   border-left: 0.5em #EEE solid;
}

hr {
   height: 0px;
   border-bottom: none;
   border-top-width: thin;
   border-top-style: dotted;
   border-top-color: #999999;
}

@media print {
   * { 
      background: transparent !important; 
      color: black !important; 
      filter:none !important; 
      -ms-filter: none !important; 
   }

   body { 
      font-size:12pt; 
      max-width:100%; 
   }
       
   a, a:visited { 
      text-decoration: underline; 
   }

   hr { 
      visibility: hidden;
      page-break-before: always;
   }

   pre, blockquote { 
      padding-right: 1em; 
      page-break-inside: avoid; 
   }

   tr, img { 
      page-break-inside: avoid; 
   }

   img { 
      max-width: 100% !important; 
   }

   @page :left { 
      margin: 15mm 20mm 15mm 10mm; 
   }
     
   @page :right { 
      margin: 15mm 10mm 15mm 20mm; 
   }

   p, h2, h3 { 
      orphans: 3; widows: 3; 
   }

   h2, h3 { 
      page-break-after: avoid; 
   }
}

</style>





</head>

<body>
<h1>Practical Machine Learning Project</h1>

<p>Here is the code I ran to get my results and what not.</p>

<pre><code>library(caret)
library(randomForest)
library(gbm)
library(plyr)

##########################################
# readin datasets

train &lt;- read.csv(&quot;train.csv&quot;,stringsAsFactors = TRUE)

test.set &lt;- read.csv(&quot;test.csv&quot;)

#checking what was readin
names(train); dim(train); str(train)
test

#wow that&#39;s a lot vars in train!
# I am going to see if I can reduce them

#find the number of missing values in each column
colSums(is.na(train))

# remove my columns that contain all nas.
train2 &lt;- train[ , colSums(is.na(train)) == 0]

colSums(is.na(train2))

#str(train2); view(train2)
# wait a second ! There looks to be a lot of simply blank cells too
colSums(train2==&quot;&quot;)

train3 &lt;- train2[ , !colSums(train2==&quot;&quot;)]

colSums(train3==&quot;&quot;)

## looks like these are the varaibles I will be using
#but dropping X, user_name,  numwindow because the are all identifiable
# dropping time stamps because they are not sensor data

training &lt;- train3[8:60]
rm(train , train2, train3)

#doing same for the test
keep.names &lt;- names(training)
testing &lt;- test.set[names(test.set) %in% keep.names]

test2 &lt;- test.set[ , colSums(is.na(test)) == 0]
test3 &lt;- test2[ , !colSums(test2==&quot;&quot;)]
testing &lt;- test3[8:60]
rm(test2, test3)

## look at intercorrelation among vars - none
descrCor &lt;- cor(training[1:52])
highCorr &lt;- sum(abs(descrCor[upper.tri(descrCor)]) &gt; 0.999)

#double check for uniary columns - my work here is done
nearZeroVar(training[1:52])


################################################
#  Seperate Training and Test by 70/30 
set.seed(12345)

inTrain &lt;- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
train  &lt;- training[inTrain,]
test &lt;- training[-inTrain,]

#################################################
# Trying various types of techinques form the course
#and then will compare the reuslts

# Bootstrapped Decision Tree

modFit &lt;- train(classe ~., method=&quot;rpart&quot;, data=train)
#library(rattle); library(rpart.plot)
#fancyRpartPlot(modFit$finalModel)
a.tree &lt;- predict(modFit, test)

# Boosted Decision Tree

btree.modFit &lt;- train(classe~., method = &quot;gbm&quot;, data =train)

b.tree &lt;- predict(btree.modFit, test)

# Random Forest

ran.modFit &lt;- train(classe~., method = &quot;rf&quot;, data =train)

ran.forest &lt;- predict(ran.modFit, test)

# &quot;Traditional&quot; Stat Models  using Naive Bayes 

modnb = train(classe ~ ., data=training,method=&quot;nb&quot;)

nb &lt;- predict(mobnb, test)

#################################################################
# Compare the results side by side and see how they stack up

confusionMatrix(a.tree, test$classe)
# Decsion Tree is 
## 46.5% accuray 0.31 kappa - random chance is better

confusionMatrix(b.tree, test$classe)
# boosted Tree is
## 95.8% accurate and Kappa is 0.94

confusionMatrix(ran.forest, test$classe)
# Random Forest is
##98.9% accuracy  0.98 kappa

confusionMatrix(nb, test$classe)
# Naive Bayes is
## 75%  accuracy 0.68 kappa

################################################################
# based on the above it seems like  random forest  is the most accurate model
# Compared the to the others it has 98.9% accuracy 

pred &lt;- predict(ran.modFit, testing)

################################################################
# write my answers out - sorry deleted so not to show anyone

answers= paste(pred,sep=&quot;,&quot;)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0(&quot;problem_id_&quot;,i,&quot;.txt&quot;)
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

</code></pre>

</body>

</html>

