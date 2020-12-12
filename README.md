# Data Visualization Final Project

The goal of this project is to build a visual narrative in the form of a flexdashboard which shows your mastery of data visualization for both exploratory data analysis and to convey information to an audience.

## Requirements

### Dataset

You need to find one or more datasets for the project, and it needs to be approved by the professors. You are welcome to use multiple data sets to add additional layers of analysis/information to your project.

The following are requirements for your dataset, or combination of datasets. They dataset (or combined datasets) must:

* Have both qualitative and quantitative data
* Have a time element
* Have a geospatial element
* Have an text element
* Have the ability to be transformed into a graph

**You may not use any of the datasets in this list:**

* New York City Taxicab
* Airline Delays
* Amazon Reviews
* Iris dataset
* Penguins dataset
* Any datasets used in labs or homework assignments


If you use multiple datasets then you must be able to join or layer them in a way that makes sense. Pick something that interests you.

**Students may use the same dataset(s) with the condition that each student creates unique analysis and visualizations. This is individual worjk. If any visualization or dashboard looks similar to that of another student that is using the same dataset, then the grade will be split.**


## Deliverables in the project

You must create at least one visualization of each of the following types and combine them together in a narrative:

- Exploratory
- Geographical
- Graph/Network
- Time Series
- Text


### Creativity and Design

It is up to you to choose static or dynamic visuals to support your analysis and narrative, and you can use design elements of your liking. You are also welcome to use the language which you are most comfortable with, R or Python. 

You must use excellent design practices and make choices that support them for a clean and professional presentation. All graphics should look professional, and have requisite titles, labels, source references, annotations and other textual elements in regular English; no variable or data names are allowed. They should also be thematically in a style you have created, not in one of the default styles found in `ggplot2`, `ggthemes`, `hrbrthemes`, `matplotlib`, `seaborn` or any other package we have covered in the course. The visualizations should also be understandable on their own, but brief legends are allowed.

**You must develop your own visual theme. No default settings are permitted, period.**

Remember, deafault settings are not usually the best. Make your visualization **both beautiful and useful**. 

**YOUR WORK MUST BE PUBLICATION READY!**


### Submitting the Assignment

The assignment **must** be submitted in the form of a `flexdashboard`. The `flexdashboard` is a R Markdown format that can produce HTML dashboards. See https://rmarkdown.rstudio.com/flexdashboard/ for details and setup. There are several layouts of dashboards you can choose from (see the [Gallery](https://rmarkdown.rstudio.com/flexdashboard/examples.html) and a description of available [layouts](https://rmarkdown.rstudio.com/flexdashboard/layouts.html) for ideas). You must have, as part of the dashboard, a page or a tab titled **About**, which will contain a textual description of the data source, the objectives of the visual analyses, and the software and package versions used for creating the dashboard.

This project will reside in its own Github repository, which is set up to deliver [Github Pages](https://pages.github.com/). This will ensure that your dashboard will have a public URL, that can then be linked as a page in your Visualization portfolio. For this to work, you will have one `index.Rmd` file in the repository, which will be your flexdashboard R Markdown file. When you render this R Markdown file, several other files and folders will be created. You may also have one `img` folder and one `data` folder, so that the dashboard will be self-sufficient. **Note that all data visualizations that will be graded must be generated using scripts**; other images may be included for context and thematic elements, but will not be part of what is graded. 

> As a consequence of this setup, please do not use any data that is proprietary, private or embargoed, since it will be exposed to the public. Also, please acknowledge the sources of any data you do use on the **About** page.


Make sure you commit and push to GitHub **only the files requested above**! Although you can upload files to GitHub using the website, we would prefer if you get in the habit of cloning your repisitory to your local machine, and using git to push back to GitHub using commit messages. 



## Grading Criteria

The project will be graded holistically considering the following:

* The work is complete
* Your visual narrative and analysis are compelling
* All visualizations are labeled properly and correctly with titles, axes and legends
* All output is correct and the code runs and does what it is expected to do
* There is discussion on specifics of the analysis, and analysis decisions are justified
* The visualization choices are justified and are designed for the right audience
* The deliverable is professional and properly formatted, has clean presentation, good design choices, and no spelling/grammatical errors

Grade of A: You will receive full credit if your submission meets all of the above criteria. 

Grade of B: One major deficiency

Grade if C: Two or more major deficiencies

For the purposes of grading, a **deficiency** can mean any of the following:

+ You perform the visualizations and analysis just for the sake of doing them, without thinking through and providing analytical justification
+ The instructions are not followed
+ There are missing sections of the assignment
+ The visualizations are sloppy
+ The visualizations are not labeled or titled properly
+ There is no legend or information to provide context
+ The reader has to try to decipher what is being conveyed in the visualization
+ The overall presentation and/or writing is sloppy
+ There are no comments in your code
+ There are files in the repository other than those requested
+ There are absolute filename links in your code
+ The repository structure is altered in any way
+ Files are named incorrectly

and last, but not least

+ You use Excel for your charts
+ You create a pie chart with 100 categories
+ Your visuals look like those we've seen created by Fox News


