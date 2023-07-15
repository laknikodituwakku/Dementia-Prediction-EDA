# Dementia-Prediction-EDA
•••• This is an exploratory data analysis done by using the dementia prediction dataset from kaggle. And this was done by,

  • Lakni Kodithuwakku
  
  • Ravindu Nishal
  
  • Manul Wickramasinghe
  
This was submitted as a project for a course named "statistical learning" offered by UOC FOS.

•••• What is Dementia?

  • A condition that describes a combination of symptoms that can develop when certain groupings of brain cells stop functioning properly
  
  • Alzheimer's disease is the most common cause of dementia
  
•••• What is our objective ?

• There is presently no known treatment for dementia, early detection can greatly improve a patient's quality of life.

• "To develop a predictive model to accurately identify individuals who are at risk of developing dementia at each visit, based on demographic, medical, and        lifestyle factors, with the aim of improving early diagnosis and treatment outcomes."

  
•••• Description of the data set

  • The Kaggle-obtained Dementia Dataset has 150 subjects aged 60 to 98 and include both men and women.
  
  • Each subject was scanned on two or more visits.
  
  • has 373 observations and 15 variables.
  
  • It also includes demographic and clinical variables such as Years of Education, Socioeconomic status, Mini-Mental State Examination score etc.
  
  • Also, we take the response variable as Group variable.
  
•••• Variables are:

01• MRI ID            Nominal                 MRI Identification Code

02• Subject ID        Nominal                 Patient Identification Code

03• M/F               Nominal Qualitative     Gender of the Patient (M/F)

04• Age               Quantitative            Age of the Patient

05• EDUC              Quantitative            Years of Education

06• SES               Ordinal Qualitative     Socioeconomic status as assessed by the Hollingshead Index from 1 (highest status) to 5 (lowest status)

07• CDR               Ordinal Qualitative     Clinical Dementia Rating (0 = no dementia, 0.5 = questionable or very mild, 1 = mild AD, 2 = moderate AD)

08• MMSE              Ordinal Quantitative    Mini-Mental State Examination score (range is from 0 = worst to 30 = best)

09• eTIV              Quantitative            Estimated total intracranial volume in mm

10• nWBV              Quantitative            Normalized whole-brain volume

11• ASF               Quantitative            Atlas scaling factor

12• MR Delay          Quantitative            Number of days since the previous visit

13• Hand              Nominal Qualitative     Dominant hand of the subject (Right, Left)

14• Visit             Quantitative            Number of the visit of the entry

15• Group             Nominal Qualitative     The group the subject belongs to (Demented, Non-Demented, Converted)

  
•••• Data Pre-processing

• We removed “Hand” variable since it has only right handed data.

• The variables "MRI ID" and "Subject ID" can also be eliminated because they are only needed for identification.

• SES and MMSE both had missing values. To fill these gaps, the Random Selection Imputation approach was utilized.

• Initially we had three levels in our response variable. Then we converted it into two categories.

•••• We used following in our EDA

• Pearson correlation map for quantitative data

• Importance of the predictor variables (using PLS)

• Stacked Barplot of Group by Gender

• Boxplot of Distribution of nWBV by Group

• Boxplot of Distribution of nWBV by Group and Gender

• Stacked Barplot of Group by SES

• Boxplot of Distribution of Years of Education by SES

• Boxplot of Distribution of Years of Education by Group

• Distribution of MMSE by Group
  
•••• Then introduced some Suggestions for Advanced Analysis Techniques.

  
  
  
  
  
  
  
  
  
