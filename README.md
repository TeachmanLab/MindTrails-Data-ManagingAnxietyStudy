# Managing Anxiety - R34
This R34-funded study targets interpretation bias in anxious adults. For more information about the _Managing Anxiety_ study, see [the MindTrails wiki](https://sites.google.com/a/virginia.edu/mindtrails-wiki/studies/managinganxiety).

### Goal of this repository
- We try to have a one document to address all the projects related to _R34_ study and major data cleaning issues.
- Each data analysis project has its own folder that contains the code specific to that project.

## Main Data cleaning
Here is the main steps for cleaning the data:
- [x] Exclude participants with ID between **20:420**. _They are spam_. 
- [x] Exclude _Admin_ users.
- [x] Exclude participant that didn't start training. _TaskLog_ entry for those has a _session=PRE_.

You can see the major steps for cleaning the data in the following diagram:
![Consort Diagram](https://github.com/TeachmanLab/R34-Data/blob/master/Consort%20Diagram.png)

### Managing Anxiety questionnaire in each session

- **Eligible**
  - Recent Anxiety symptoms. _DASS-AS_ Table
- **PRE**
  - Consent to participant. _Credibility_ Table
  - Demographic. _demographic_ Table
  - Mental Health History. _MH_ Table
  - Satisfaction. _QOL_ Table
  - Completing short stories. _Recongition Rating_ Table
  - Completing short stories-continued. _RR_ Table
  - Why things happen. _BBSIQ_ Table
  - Mood assessment. _DASS-DS_Table
  - Assessment. _DD_ Table
  - Anxiety Review. _OA_ Table
  - Personal anxiety triggers. _Anxiety trigger_ Table  
- **SESSION 1**
  - How anxious you feel. _SUDS_ Table
  - How your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _first session sentence_ Table
  - How anxious you feel. _SUDS_ Table
  - Follow up. _CC_ Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 2**
  - Use your imagination. _Imaginary prime_ Table
  - Impact question. _Impact_ Table
  - Training stories. _second session sentence_ Table
  - Anxiety Review. _OA_ Table
  - Return Intention. _Return Intention_ Table
- **SESSION 3**
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
  - Return Intention. _Return Intention_ Table
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
3. Dose-Response Relationships



For the data issues history and suggested cleaning procedure, see https://docs.google.com/document/d/1Rh6nzQRT6rce37m8mJGfyAw_aekixrT7t0h3uzEWxpY/edit?usp=sharing


If you are a researcher who wants to contribute to this project, please contact Henry Behan at hb7zz@virginia.edu or Claudia Calicho-Mamani at cpc4tz@virginia.edu. Thanks!
