# SVM_PROJECT

<h3>About this repository</h3>

This folder contains the work and development of a Shiny WEB application as part of a project concerning SVM (Support Vector Machine) made by 3 students in 2nd year of the <a href="http://www.univ-orleans.fr/deg/masters/ESA/">Master ESA degree</a> at the <a href="http://www.univ-orleans.fr/fr/deg">University of Orléans</a>.

This project was conducted by : Karl MEUNIER, Thibaut RAOUL-JOURDE and Victor YE.

Under the supervision of Mr. Christophe HURLIN and Mr. Jérémy DUDEK.

<h3>Contents</h3>

<p>
We worked on the open source database called <strong>"Credit Card Fraud Detection"</strong> available for free in the <a href="https://www.kaggle.com/janiobachmann/credit-fraud-dealing-with-imbalanced-datasets">Kaggle link</a>.</p>


It is important that credit card companies are able to recognize fraudulent credit card transactions so that customers are not charged for items they have not purchased.

<p>
The data sets include credit card transactions carried out by European cardholders in September 2013.
This data set shows transactions that occurred within two days, where we have <strong>492 frauds out of 284,807 transactions.</strong>
The data set is very unbalanced, with the positive class (fraud) accounting for 0.17% of all transactions.</p>

|            | Frequency |   Ratio     |
|:----------:|:---------:|:-----------:|
| Fraud      |    492    | 0.17%       |
| No fraud   | 284,315    | 99.83%      |

<i>Unfortunately, due to confidentiality issues, we cannot provide the original features and more basic information about the data. The V1, V2, ..., V28 functionalities are the main components obtained with PCA. The only features that have not been transformed with PCA are 'Time' and 'Amount'.</i>


| Variables       | Description |
|:---------------:|:---------------------------------------------------------------------------|
| V1,...,V28      | Main components (anonymous)                                                |
| Time            | Seconds between each transaction and the first transaction in the data set |
| Amount          | Amount of the transaction                                                  |
| Class           | Response variable (1 in case of fraud 0 otherwise)                         |

<strong>Correction and processing done</strong> :


<h3>Using the SVM</h3>
Support vector machines are a set of supervised learning techniques designed to solve classification or regression problems. SVMs are also called wide-margin separators.
<p>
<i>More details about this method is available in the <a href="https://esas.shinyapps.io/SVM_PROJECT/">Shiny Application</a></i>.</p>



&nbsp;


<p> <i> November 2019 </i> </p>
