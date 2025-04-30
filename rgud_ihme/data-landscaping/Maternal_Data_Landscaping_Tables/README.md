## **We chose to stick with the standard GBD Source Counts Strategy**
**1** The 'Total_Source_Counts_Macro' folder contains generated outputs using Strategy 1 (GBD Strategy):  

 [NID + country + year_start + year_end]  
 [NID + region + year_start + year_end]  

• The strategy was implemented as per the GBD strategy, which states collapsing source counts by location(location_id, country, region, etc). Hence, depending on the level of detail, the source counts counted source by country or region. Excluding unique subnationals.  
• This strategy was vetted and discussed with Mae and the Maternal Morbidity Team in July, 2022.  
• As a result, strategy 2(below) was favored for presentation purposes.  

• THIS IS THE GBD STANDARD SOURCE COUNTING STRATEGY! 

**2** The 'Total_Source_Counts_Micro' folder contains generated outputs using Strategy 2 (Includes Subnational counts):  

 [NID + country + ihme_loc_id + year_start + year_end]  
 [NID + region + ihme_loc_id + year_start + year_end]    

• For this strategy, the source counts uses ihme_loc_id as a unique identifier to includes subnationals, as each subnational has a unique ihme_loc_id label. This was decided as to be the best key in order to genera source counts at the most detailed level.  
• Using this strategy would provide you with an Identical source counts figure both at the country and regional level for each country and region.  

<hr style="border:2px solid gray">

## **Maternal Bundle Versions IDs for Landscaping**

**Discussed and Agreed Bundle Versions Cause Groups:**  
	• Maternal Hemorrhage: 20930  
	• Maternal Sepsis and Infection: 20945(Puerperal Sepsis) & 20948(Other Maternal Infections)  
	• Maternal Hypertensive Disorders: 20933(Hypertensive Disorders of Pregnancy) & 20936(Eclampsia) & 21185(Severe Pre-eclampsia)  
	• Maternal Abortion: 20942(Maternal Abortive Outcome)  
	• Ectopic Pregnancy: 20951  
	• Maternal Obstruction: 29876(Obstructed Labour Acute Event) & 21626(Fistula)  

[Maternal Extended Documentation here: "J:\WORK\12_bundle\maternal\Documentation\extended_maternal_documentation\maternal_disorders_extended_doc_2020_MAD.docx"]

<hr style="border:2px solid gray">

## **Data Inputs used to create Maternal Data Landscaping Tables**   

**Christian's Clinical Data Screenings:**  

	"I:\RTs_and_Projects\GBD\Teams\RGUD\Maternal Morbidity Project\1A_Data Landscaping\Incidence_Landscape\Christian_landscape\Maternal_Clinical_Data_Tables_Final.xlsx"  
	
**Laura's SciLit Screenings:**  

	"I:\RTs_and_Projects\GBD\Teams\RGUD\Maternal Morbidity Project\1A_Data Landscaping\Incidence_Landscape\Laura_landscape"  
	
<hr style="border:2px solid gray">

## **The following notes are for Christian's Cliniical Data Screenings Only**  
	
**The following Clinical Data resources were used:**  
	
	1. GBD_2022_clinical_metadata.xlsx  
	2. clinical_source_quality_matrix.xlsx  
	
	> To access these, please reach out to the Clinical team or find them here: "I:\RTs_and_Projects\GBD\Teams\RGUD\Maternal Morbidity Project\1A_Data Landscaping\Incidence_Landscape\Christian_landscape". These sensitive data files, so please don't share. 

**Excluded Columns**  
• The following columns were excluded due to zero counts across all cause-groups: [Community Only] and [Chart Review]  
• These columns were not included in the tables or any of the visualizations.  

**Demographics Included**

[All ages or 10-54yo] - All Clinical Data sources were graded as 1.  
[15-49yo] - Graded as 0 for all Clinical Data sources.  
[Narrower age-group] - Graded as 0 for all Clinical Data sources.  
[National] - 'subnational_detail' from GBD_2022_clinical_metadata.xlsx and ihme-location type level data was used to grade this column. Any admin0 and 'NO' for 'subnational_detail' was graded as 1.  
[Comprehensive subnationals] - 'subnational_detail' from GBD_2022_clinical_metadata.xlsx and ihme-location type level data was used to grade this column. Any NOT:admin0 and 'YES' for 'subnational_detail' was graded as 1.  
[Limited subnationals] - Graded as 0 for all Clinical Data sources.  

**Denominator**

[Live Birth] - Graded as 0 for all Clinical Data sources.  
[Pregnancy] - Graded as 0 for all Clinical Data sources.  
[Woman] - Any sources with Claims were graded as 1. 'facility_type' was used from GBD_2022_clinical_metadata.xlsx  
[Deliveries] - Graded as 0 for all Clinical Data sources.  
[Admissions] - Any Hospital-related data were graded as 1. 'facility_type' was used from GBD_2022_clinical_metadata.xlsx  

**Granularity**

[Age] - All Clinical Data sources were graded as 1.  
[Case Definition] - This was originally tabled to cover both SciLit and Clinical Data. But after further discussions and agreements. This column mainly covers SciLit, as Clinical Data have there own columns(ICD column in the preliminary tables & the Clinical Data section in the final tales). This was graded as 1 if 2 or more case definitions columns had a 1 for a data source. If the data source had 1 or less case definitions, then it would be graded as 0. Each table covers different case definitions, but the same mechanics stay true for this particular column across all cause groups.  

**Setting**

[Any or not specified] - All Clinical Data sources were graded as 1. We agreed that we simply don't know for most/all Clinical Data sources.
[In-facility only] - Graded as 0 for all Clinical Data sources.  

**Case Ascertainment**

[Admin data] - All Clinical Data sources were graded as 1.  
[Self-report] - This was only done for SciLit sources. Not included for Clinical Data Sources.  
[Registry or surveillance] - Graded as 0 for all Clinical Data sources.  

**Case Definition**

This section is for SciLit. Clinical Data was covered by the ICD column in the preliminary tables and the Clinical Data section in the Final tables.  

**Clinical Data**
[ICD9] - Any sources with ICD9 data were graded as 1. 'icd_version' was used from GBD_2022_clinical_metadata.xlsx  
[ICD10] - Any sources with ICD10 data were graded as 1. 'icd_version' was used from GBD_2022_clinical_metadata.xlsx  
[1 Digit] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[2 Digits] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[3 Digits] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[4 Digits] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[5 Digits] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[6 Digits] - 'icd_length' was used from GBD_2022_clinical_metadata.xlsx  
[Multiple Diagnosis] - Any sources with Multi was graded as 1 and any without were graded as 0. 'diagnosis_type' was used from GBD_2022_clinical_metadata.xlsx  
[Linkages, Identifies sources that provide data that allow to de-duplicate by patient(individual) based on multiple diagnosis and admissions. Not necessarily across sources.] - Any sources with YES in 'Patient IDs' from clinical_source_quality_matrix.xlsx were graded 1. Any sources with NO were graded as 0.

**Quick Notes Regarding ICD & Case Definition Clinical Data screenings**  

	1. A deep dive on ICD information was conducted by Christian to explore ICD length, variability, and availability. Along with provided Clinical metadata, case definitions, and data inputs were incorporated accordingly.  
		• We are aware there is strong interest to conduct data deep dives on exactly what ICD codes are provided in each source. Unfortunately this is a large project on its own, and would require more time and personal - Including input from the clinical team.  
	2. Similar to the SciLit case definition screenings, some sources 'contained' case definition data for a cause, but didn't actually exist in the native bundle version for that cause. This was discussed with Mae and Ashley. As per guidelines these sources were included regardles of bundle version origin. As long as they contained or were vetted for a cause, it was perfectly fine to include them for that cause.  
		• Any yellow higlighted rows in any of the sheets on "/Maternal_Clinical_Data_Tables_Final.xlsx" were sources that didn't exist in that causes bundle version data bin, but was identified to potentially have ICD data for that cause.  

<hr style="border:2px solid gray">

## **How were the Maternal Data Landscaping Tables created? (Summary)**

**Steps**
1. Christian(Clinical Data) and Laura(Scientific Literature) both separately worked on their screenings, based on the prescribed dummy table columns from Mae.  
   a. Columns were graded on a logical or binary grading scale, 1 or 0. - This is important(it facilitates the process) for when generating source counts.  
   
2. Using script "Maternal_DataLandscaping_Final.R", all screening sheets, Christian's and Laura's. Are appended and merged with all the maternal bundle_version_id data bins.  
   a. The Case Definition columns are used as the 'key' columns to include all the data sources with the desired Case Definition data for each cause group.  
   b. The Non-Case Definition columns are all other columns that were also processed.  
   
3. In the same script as above, the source_counts() function is sourced in. This function was used to generate the source counts for each of the Case Definition and Non-Case Definition columns from the appended & joined data input.  

4. Once source counts have been created for each of the desired columns, using the same script "Maternal_DataLandscaping_Final.R", all columns are automatically and manually put together.  
   a. For these Maternal Data Landscaping, additional columns and rows were created, such as 'Recent Year' and Zero counts countries that were also incorporated in the script and super important to consider when reviewing the code.  
   b. Please keep in mind that each table is different, containing different number of columns(should contain the same number of rows), so manual tinkering and vetting is important.  
   
5. Once the tables are put together, they were manually colored and formatted accordingly on Microsoft excel.  




























