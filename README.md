
# **Diagnostic Laboratory dashboard**
#### *<a href="https://www.hugzsoubrier.com">Hugo Soubrier</a>*

#### **Context & Aims**

This is a template dashboard based on the one that I have developed during my work at Bernhard Nocht Institute of Tropical Medicine in the *Operational research, Capacity Building and Outbreak response group*. Following the response to the 2021 Ebola Virus Disease (EVD) outbreak in Guinea, capacity building activities were continued and included the implementation of a new data management system in local diagnostic laboratories.

In each of our partner laboratories, new linelists were developed in Excel (previously used by teams on ground). The linelist gathers, centralises, and standardises *Demographic*, *Clinical*, *Sample* and *Tests* data for each of the sample received in the labs. These data could then be analysed and visualised using the present dashboard entirely designed on R shiny.

The aim of the dashboard is to help staff of the laboratories to

1. Quickly visualise, filter and anonymise the linelist data before downloading for sharing with authorities

2. Visualise laboratory activities (number and type of samples received, pathogens tested, median time for testing etc... )

3. Analyse the Laboratory activities specific to one pathogen 

- Testing frequency and result for a selected pathogen
- Rapid access to the lists of positive samples and their geographic origins
- Ct values informations of positive samples

#### **Fake data scenario**

__*Laboratories*__\
The dashboard provides analysis of two fake guinean laboratories datasets (Laboratory 1 & Laboratory 2) between the 1st of January 2022 and the 31st of December 2023. Over this period, Laboratory 1, a bigger reference lab, received 2500 samples from all of the country. Laboratory 2, a smaller remote lab, received 1250 samples, mainly from Forest region of Guinea. 

__*Pathogens*__\
Both laboratory are specialised in **Hemorrhagic Fever viruses** diagnostic (*Ebola Virus*, *Marburg Virus*, *Lassa Virus*, *Yellow Fever Virus*, *Dengue Virus*), both also propose RDT for *Malaria* detection. Due to missing reagents, Laboratory 2 is not able to test Dengue Virus 

__*Tests*__\
To represent the complex reality of diagnostic testing, several tests type were including in the fake datasets. Laboratories can tests using Real-Time RT-PCRs, Gene Xpert technology or Rapid Diagnostic Tests (RDT)

![scenario_tables](https://raw.githubusercontent.com/hugzsoubrier/lab_dashboard_template/main/www/png/scenarios_table.png)



