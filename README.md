# Managing Anxiety - R34
This R34-funded study targets interpretation bias in anxious adults. For more information about the _Managing Anxiety_ study, see [the MindTrails wiki](https://sites.google.com/a/virginia.edu/mindtrails-wiki/studies/managinganxiety).

You can see the following information in this document:
1. [Goal of this repository](#goal-of-this-repository)
2. [Main data cleaning steps](#main-data-cleaning)
3. [Tables in each session of the study](#managing-anxiety-questionnaire-in-each-session)
4. [List of projects](#list-of-project)
5. [Contact](#contact)


### Goal of this repository
- We try to have a one document to address all the projects related to _R34_ study and major data cleaning issues.
- Each data analysis project has its own folder that contains the code specific to that project.

## Main Data cleaning
## Proposed Main steps of the data cleaning
- [X] Name convention of ID column (participantID)
- [X] Actual IDs
- [X] Remove test and admin accounts
- [X] Check the values of ID
    - Take care of jump in the values of IDs
- [X] labeled IDs based on the timeline of the study (e.g., soft-launch, actual launch)
- [X] Remove duplicate values
    - Delete the same values
    - Keep the very last entry
- [X] Check the missing values
    - Blank, n/a, na, NA
- [X] Check the prefer answer value (e.g., 555, -1)
- [X] Check the value of each measurement
    - Visualize the value range of each column if we have outlier (e.g. R34 has a value that shouldn’t be there)
      - As an example, in the return intention table, we have a negative value without any explanation
      - Solution: fill them out with the null value
- [X] Time conversion based on participants’ timezone
- [X] Column shifting (e.g., sometimes the entry’s value shift in some tables)


### Managing Anxiety questionnaire in each session

- **Eligible**
  - Recent Anxiety symptoms. _DASS-AS_ Table
- **PRE**
  - _Credibility_ Table: Consent to participant
  - _demographic_ Table: Demographic 
  - _MH_ Table: Mental Health History
  - _QOL_ Table: Satisfaction
  - _Recongition Rating_ Table: Completing short stories 
  - _RR_ Table: Completing short stories-continued
  - _BBSIQ_ Table: Why things happen. 
  - _DASS-DS_Table: Mood assessment. 
  - _DD_ Table: Assessment. 
  - _OA_ Table: Anxiety Review. 
  - _Anxiety trigger_ Table: Personal anxiety triggers. 
- **SESSION 1**
  - _SUDS_ Table: How anxious you feel. 
  - _Imaginary prime_ Table: How your imagination. 
  - _Impact_ Table: Impact question. 
  - _first session sentence_ Table: Training stories. 
  - _SUDS_ Table: How anxious you feel. 
  - _CC_ Table: Follow up. 
  - _OA_ Table: Anxiety Review. 
  - _Return Intention_ Table: Return Intention. 
- **SESSION 2**
  - _Imaginary prime_ Table: Use your imagination. 
  - _Impact_ Table: Impact question. 
  - _second session sentence_ Table: Training stories. 
  - _OA_ Table: Anxiety Review. 
  - _Return Intention_ Table: Return Intention. 
- **SESSION 3**
  - _SUDS_ Table: How anxious you feel. 
  - _Imaginary prime_ Table: Use your imagination. 
  - _Impact_ Table: Impact question. 
  - _third session sentence_ Table: Training stories. 
  - _SUDS_ Table: How anxious you feel. 
  - _CC_ Table: Follow up. 
  - _Recongition Rating_ Table: Completing short stories. 
  - _RR_ Table: Completing short stories-continued. 
  - _BBSIQ_ Table: Why things happen. 
  - _QOL_ Table: How satisfied you feel. 
  - _DASS-DS_Table: Mood assessment. 
  - _DD-FU_Table: Assessment. 
  - _OA_ Table: Anxiety Review. 
  - _Return Intention_ Table: Return Intention. 
- **SESSION 4**
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _fourth session sentence_ Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 5**
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _fifth session sentence_ Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 6**
  - How anxious you feel. _SUDS_ Table
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _sixth session sentence_ Table
  - How anxious you feel. _SUDS_ Table
  - Follow up. _CC_ Table
  - Completing short stories. _Recongition Rating_ Table
  - Completing short stories-continued. _RR_ Table
  - Why things happen. _BBSIQ_ Table
  - How satisfied you feel. _QOL_ Table
  - Mood assessment. _DASS-DS_Table
  - Assessment. _DD-FU_Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 7**
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _seventh session sentence_ Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 8**
  - How anxious you feel. _SUDS_ Table
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _third session sentence_ Table
  - How anxious you feel. _SUDS_ Table
  - Follow up. _CC_ Table
  - Completing short stories. _Recongition Rating_ Table
  - Completing short stories-continued. _RR_ Table
  - Why things happen. _BBSIQ_ Table
  - How satisfied you feel. _QOL_ Table
  - Mood assessment. _DASS-DS_Table
  - Assessment. _DD-FU_Table
  - Anxiety Review. _OA_ Table
  - Recent anxiety symptoms. _DASS-AS_ Table.
  - Change in help seeking. _CIHS_ Table.
  - Return Intention. _Return Intention_ Table
  
- **Post**
  - Completing short stories. _Recongition Rating_ Table
  - Completing short stories-continued. _RR_ Table
  - Why things happen. _BBSIQ_ Table
  - How satisfied you feel. _QOL_ Table
  - Mood assessment. _DASS-DS_Table
  - Assessment. _DD-FU_Table
  - Anxiety Review. _OA_ Table
  - Recent anxiety symptoms. _DASS-AS_ Table.
  - Change in help seeking. _CIHS_ Table.
  - Evaluating the program. _MUE_ Table.

- **Complete**

  
## List of project
1. [Main Outcomes](https://github.com/TeachmanLab/R34-Data/tree/master/Main%20Outcomes), Leader: Julie
2. [Credibility](https://github.com/TeachmanLab/R34-Data/tree/master/Credibility), Leader: Nichola
3. Dose-Response Relationships[](), Leader: Jeremy


## Contact
If you are a researcher who wants to contribute to this project, please contact Henry Behan at hb7zz@virginia.edu or Claudia Calicho-Mamani at cpc4tz@virginia.edu. Thanks!
