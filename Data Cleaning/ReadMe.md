# Data Cleaning

**Note: The data cleaning scripts in this folder are out of date due to file loss.**

Table of Contents
1. [Main Data Cleaning Steps](#main-data-cleaning-steps)
2. [Tables at Each Time Point](#tables-at-each-time-point)

## Main Data Cleaning Steps

**TODO: Sonia Baee to clarify which cleaning script(s) do which steps below.**

Run this script to have a clean dataset which takes care of following steps:

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

## Tables at Each Time Point

- **Eligibility**
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
