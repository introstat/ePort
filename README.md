# ePort

**Motivation:**

Instructors may wish to better understand student performance throughout the teaching semester. Generating reports immediately after homework assignment deadlines can allow instructors to assess and improve upon their teaching approaches in a fast response cycle.

Some large introductory courses consist of multiple topics (groups of learning outcomes) that are taught by multiple instructors across multiple sections (groups of students). To accomodate the various ways that student performance can be examined for such courses, reportation generation that can compare within and between topics and sections is necessary.

**Description:** 

The ePort package provides tools for course instructors to generate electronic reports regarding student performance. The tools in this package will be especially beneficial for users who supervise large introductory courses, which often consist of multiple topics (groups of learning outcomes) that are taught by multiple instructors across multiple sections (groups of students).

**There are four general types of reports that can be generated**:

1) One topic and one section.  
This allows course coordinators to determine how well a particular section performed on a particular topic. 

2) One topic between multiple sections.  
This allows course coordinators to quickly determine how well and consistently the multiple sections performed on a topic of interest. This could be particularly insightful in cases where discrepancies in student performance are discovered between sections, especially if different instructors and/or teaching methods are being used across the sections.  

3) One unit (group of topics) one section.  
This allows coordinators to assess student performance across all the learning outcomes of the combined topics that form the unit for a given section.  

4) One unit (group of topics) between multiple sections.  
This allows coordinators to assess student performance across all the learning outcomes of the combined topics that form the unit, and to quantify the consistency of how students perform across sections.

In general, there are short and long versions available for reports. Short versions of reports provide brief summarizations of student performance without regard to individual problems, whereas long versions of reports provide detailed summarizations of student performance for each individual problem in the assignment. Hence, long reports can also be used to confirm the suitability of assigned problems. For instance, in some courses, problems that assess the same learning outcome and are intended to be of equal difficulty levels are grouped into a question set, and each student is assigned a random subset from this set of problems. However, sometimes, an unexpected discrepancy in student performance between problems in a given quesiton set will be discovered, indicating an unintended discrepancy in the clearness or difficulty level of the problems to which students were randomly assigned. This package will allow users to efficiently find and fix such issues.

**Types of summaries and analyses in reports**:

* Mean, standard deviation, five number summary
* Statistical graphics
* Mixed effect models
* Clustering techniques

**Additional file manipulation tools available in this package**:

* Splitting files
* Merging files
* Deidentifying files

**Installation:**

* The latest released version: `install.packages("ePort")`
* The latest development version: `install_github("introstat/ePort")`

**Resources:**

Installation of the package will automatically download a vignette, which contains a more thorough explanation of the available methods, and example code to produce each type of report.

**License:**

GPL