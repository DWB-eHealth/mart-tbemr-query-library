/*ABOUT
The basic register query creates a register of patients recorded in the EMR by program. For a full description of each veriable, see the codebook. The function of each sub-table used within the main statement is described above each sub-table. */

/*SUB-TABLE:The baseline_disease_site table extracts the disease site from the baseline form. Since the disease site options are multi-select, the sub-table re-codes multiple selections.*/
WITH baseline_disease_site_agg AS ( 
	SELECT 
		blds.patient_program_id,
		blds.id_baseline_template,
		CASE
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Pulmonary' THEN 'Pulmonary'
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Extrapulmonary' THEN 'Extrapulmonary'		
			WHEN string_agg(blds.baseline_disease_site,',' ORDER BY blds.baseline_disease_site) = 'Extrapulmonary,Pulmonary' THEN 'Both'
			ELSE null
		END AS disease_site
	FROM baseline_disease_site AS blds
	GROUP BY blds.patient_program_id, blds.id_baseline_template),

/*SUB-TABLE: The last_facility table extracts the last facility the patient was seeen in from the treatment initiation form, follow up form and hospital admission notification form. If there is not facility in those forms, the registration facility is used.*/
last_facility AS (
	WITH last_facility_sub AS (
		WITH treatment_initiation_facility AS (
			SELECT
				DISTINCT on (ti.patient_program_id) ti.patient_program_id, 
				ti.tuberculosis_drug_treatment_start_date::date AS facility_date,
				ti.ti_treatment_facility_at_start AS facility
			FROM treatment_initiation_template AS ti
			WHERE ti.ti_treatment_facility_at_start is not null
			ORDER BY ti.patient_program_id, ti.tuberculosis_drug_treatment_start_date DESC),
/*Removed table reference from this sub-table because PNG does not use this form.*/
		/*hospital_admission_facililty AS (
			SELECT
				DISTINCT on (hant.patient_program_id) hant.patient_program_id, 
				hant.han_hospital_admission_date::date AS facility_date,
				CASE
					WHEN hant.treatment_facility_name != 'Other' THEN hant.treatment_facility_name
					ELSE hant.other_treatment_facility_name
				END AS facility
			FROM hospital_admission_notification_template AS hant
			WHERE hant.treatment_facility_name is not null
			ORDER BY hant.patient_program_id, hant.han_hospital_admission_date DESC),*/
		followup_facility AS (
			SELECT
				DISTINCT on (ft.patient_program_id) ft.patient_program_id, 
				ft.followup_visit_date::date AS facility_date,
				CASE
					WHEN ft.treatment_facility_name != 'Other' THEN ft.treatment_facility_name
					ELSE ft.other_treatment_facility_name
				END AS facility
			FROM followup_template AS ft
			WHERE ft.treatment_facility_name is not null
			ORDER BY ft.patient_program_id, ft.followup_visit_date DESC)
		SELECT * FROM treatment_initiation_facility
		UNION
		/*SELECT * FROM hospital_admission_facililty
		UNION*/ 
		SELECT * FROM followup_facility
		ORDER BY patient_program_id, facility_date DESC),
	enrollment_facility AS (
		SELECT
			ppdd.patient_program_id,
			ppdd.date_enrolled::date AS facility_date,
			pa."Registration_Facility" AS facility
		FROM program_attributes AS pa
		LEFT OUTER JOIN patient_program_data_default AS ppdd
			ON pa.patient_program_id = ppdd.patient_program_id
		ORDER BY ppdd.patient_program_id, ppdd.date_enrolled DESC)		
	SELECT 
		ef.patient_program_id,
		CASE
			WHEN lf.facility IS NOT NULL THEN lf.facility
			ELSE ef.facility
		END AS last_facility
	FROM enrollment_facility AS ef
	LEFT OUTER JOIN (
		SELECT 
			DISTINCT ON (patient_program_id) patient_program_id, 
			facility 
		FROM last_facility_sub 
		WHERE facility IS NOT NULL 
		ORDER BY patient_program_id, facility_date DESC) AS lf
	ON ef.patient_program_id = lf.patient_program_id),
		
/*SUB-TABLE: The dlm_start_date table selects the earliest start date for dlm.*/
dlm_start_date AS (
    SELECT 
        DISTINCT ON (mdd.patient_program_id) mdd.start_date AS dlm_start_date,
		mdd.patient_program_id AS patient_program_id
    FROM medication_data_default AS mdd
	WHERE mdd.coded_drug_name = 'Delamanid (Dlm)'
	ORDER BY mdd.patient_program_id, mdd.start_date),

/*SUB-TABLE: The bdq_start_date table selects the earliest start date for bdq.*/
bdq_start_date AS (
    SELECT 
        DISTINCT ON (mdd.patient_program_id) mdd.start_date AS bdq_start_date,
		mdd.patient_program_id AS patient_program_id
    FROM medication_data_default AS mdd
    WHERE mdd.coded_drug_name = 'Bedaquiline (Bdq)'
    ORDER BY mdd.patient_program_id, mdd.start_date),
    
/*SUB-TABLE: The initial_drug_regimen table indicates which drugs were started within 1 week of the treatment initiation start date.*/
initial_drug_regimen AS (
	SELECT
		idr.patient_program_id,
		MAX (CASE WHEN idr.drug = 'Isoniazid (H)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "H",
		MAX (CASE WHEN idr.drug = 'Rifampicin (R)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "R",
		MAX (CASE WHEN idr.drug = 'Ethambutol (E)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "E",
		MAX (CASE WHEN idr.drug = 'Pyrazinamide (Z)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Z",
		MAX (CASE WHEN idr.drug = 'Streptomycin (S)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "S",
		MAX (CASE WHEN idr.drug = 'Amikacin (Am)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Am",
		MAX (CASE WHEN idr.drug = 'Kanamycin (Km)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Km",
		MAX (CASE WHEN idr.drug = 'Capreomycin (Cm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Cm",
		MAX (CASE WHEN idr.drug = 'Levofloxacin (Lfx)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Lfx",
		MAX (CASE WHEN idr.drug = 'Moxifloxacin (Mfx)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Mfx",
		MAX (CASE WHEN idr.drug = 'Prothionamide (Pto)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Pto",
		MAX (CASE WHEN idr.drug = 'Ethionamide (Eto)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Eto",
		MAX (CASE WHEN idr.drug = 'Cycloserine (Cs)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Cs",
		MAX (CASE WHEN idr.drug = 'Terizidone (Trd)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Trd",
		MAX (CASE WHEN idr.drug = 'Para-aminosalicylic acid (PAS)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "PAS",
		MAX (CASE WHEN idr.drug = 'Para-aminosalicylic acid - sodium (PAS-Na)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "PAS-Na",
		MAX (CASE WHEN idr.drug = 'Bedaquiline (Bdq)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Bdq",
		MAX (CASE WHEN idr.drug = 'Delamanid (Dlm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Dlm",
		MAX (CASE WHEN idr.drug = 'Linezolid (Lzd)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Lzd",
		MAX (CASE WHEN idr.drug = 'Clofazimine (Cfz)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Cfz",
		MAX (CASE WHEN idr.drug = 'Imipenem/Cilastatin (Imp/Cln)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Imp/Cln",
		MAX (CASE WHEN idr.drug = 'Amoxicillin/Clavulanate (Amx/Clv)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Amx/Clv",
		MAX (CASE WHEN idr.drug = 'Meropenem (Mpm)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Mpm",
		MAX (CASE WHEN idr.drug = 'Pretomanid (Pa)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Pa",
		MAX (CASE WHEN idr.drug = 'Rifapentine (Rpt or P)' THEN concat(idr.dose,' ',idr.dose_unit) ELSE NULL END) AS "Rpt or P"
	FROM (SELECT 
			mdd.patient_program_id AS patient_program_id,
			mdd.coded_drug_name AS drug,
			mdd.dose AS dose,
			mdd.dose_units AS dose_unit
		FROM medication_data_default AS mdd
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON mdd.patient_program_id = ti.patient_program_id
		WHERE mdd.start_date BETWEEN (ti.tuberculosis_drug_treatment_start_date - 7) and (ti.tuberculosis_drug_treatment_start_date + 7) and mdd.coded_drug_name IS NOT NULL
		GROUP BY mdd.coded_drug_name, mdd.patient_program_id, mdd.dose, mdd.dose_units) AS idr
	GROUP BY idr.patient_program_id),

/*SUB-TABLE: The latest_hiv_result table selects the most recent complete HIV results from the serology lab results form.*/
latest_hiv_result AS (
	SELECT 
        distinct on (lrs.patient_program_id) lrs.lab_hiv_test_result AS hiv_result,
        lrs.patient_program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hiv_test_result is not NULL
    ORDER BY lrs.patient_program_id, lrs.specimen_collection_date), 

/*SUB-TABLE: The latest_hep_b_result table selects the most recent complete Hep B results from the serology lab results form.*/
latest_hep_b_result AS (
    SELECT 
        distinct on (lrs.patient_program_id) lrs.lab_hepatitis_b_antigen_test_result AS hep_b_result,
        lrs.patient_program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hepatitis_b_antigen_test_result is not NULL
    ORDER BY lrs.patient_program_id, lrs.specimen_collection_date),

/*SUB-TABLE: The latest_hep_c_result table selects the most recent complete Hep C results from the serology lab results form.*/
latest_hep_c_result AS (
    SELECT 
        distinct on (lrs.patient_program_id) lrs.lab_hepatitis_c_antibody_test_result AS Hep_C_result,
        lrs.patient_program_id
    FROM lab_results_serology_template AS lrs
    WHERE lrs.lab_hepatitis_c_antibody_test_result is not NULL
    ORDER BY lrs.patient_program_id, lrs.specimen_collection_date),

/*SUB-TABLE: The latest_followup table provides the next planned visit date from the most recent follow-up form.*/  
latest_followup AS (
    SELECT 
        DISTINCT ON (patient_program_id) patient_program_id,
        return_visit_date
    FROM followup_template
    GROUP by patient_program_id, return_visit_date, followup_visit_date
    ORDER by patient_program_id, followup_visit_date),

/*SUB-TABLE: The baseline_positive table provides a list of patients who have a positive result reported 3 months prior to or 2 weeks after the treatement initiation date.*/
baseline_pos AS (
		WITH pos_culture_baseline_fuzzy AS (
			WITH culture_positive AS (
				SELECT 
					bcrd.patient_program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis')
			SELECT
				ti.patient_program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cp.specimen_collection_date,
				cp.bacteriology_culture_results,
				(cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS "day_diff_pos"
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_positive AS cp
				ON ti.patient_program_id = cp.patient_program_id
			WHERE (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
			AND (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cp.patient_program_id, (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date)),
		neg_culture_baseline_fuzzy AS (
			WITH culture_negative AS (
				SELECT 
					bcrd.patient_program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Negative for M. tuberculosis')
			SELECT
				ti.patient_program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cn.specimen_collection_date,
				cn.bacteriology_culture_results,
				(cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS "day_diff_neg"
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_negative AS cn
				ON ti.patient_program_id = cn.patient_program_id
			WHERE (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
				AND (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cn.patient_program_id, (cn.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date))	
		SELECT
			DISTINCT ON (pcb.patient_program_id) pcb.patient_program_id,
			pcb.tuberculosis_drug_treatment_start_date,
			pcb.specimen_collection_date AS closest_positive,
			ncb.specimen_collection_date AS closest_negative,
			CASE
				WHEN (pcb.tuberculosis_drug_treatment_start_date - pcb.specimen_collection_date) > (pcb.tuberculosis_drug_treatment_start_date - ncb.specimen_collection_date) THEN 'Y'
				ELSE NULL 
			END AS closer_negative
		FROM (SELECT 
			ROW_NUMBER() OVER (PARTITION BY pcbf.patient_program_id ORDER BY abs(pcbf.day_diff_pos - 0) ASC) AS pos_bl,
			pcbf.*
		FROM pos_culture_baseline_fuzzy AS pcbf) AS pcb
		LEFT OUTER JOIN (SELECT 
			ROW_NUMBER() OVER (PARTITION BY ncbf.patient_program_id ORDER BY abs(ncbf.day_diff_neg - 0) ASC) AS neg_bl,
			ncbf.*
		FROM neg_culture_baseline_fuzzy AS ncbf) ncb
			ON pcb.patient_program_id = ncb.patient_program_id
		WHERE pcb.pos_bl = 1),
    
/*SUB-TABLE: The initial_cc table provdies information regarding the date of initial culture conversion.*/
initial_cc AS (
	WITH baseline_pos AS (
		WITH pos_culture_baseline_fuzzy AS (
			WITH culture_positive AS (
				SELECT 
					bcrd.patient_program_id,
					bcs.specimen_collection_date,
					bcrd.bacteriology_culture_results
				FROM bacteriology_culture_results_details AS bcrd
				LEFT OUTER JOIN bacteriology_concept_set AS bcs
					ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
				WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis')
			SELECT
				ti.patient_program_id,
				ti.tuberculosis_drug_treatment_start_date,
				cp.specimen_collection_date,
				cp.bacteriology_culture_results,
				(cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) AS "day_diff_pos"
			FROM treatment_initiation_template AS ti
			LEFT OUTER JOIN culture_positive AS cp
				ON ti.patient_program_id = cp.patient_program_id
			WHERE (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) >= -90 
			AND (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date) <= 15
			ORDER BY cp.patient_program_id, (cp.specimen_collection_date - ti.tuberculosis_drug_treatment_start_date))	
		SELECT
			pcb.patient_program_id,
			pcb.tuberculosis_drug_treatment_start_date,
			pcb.specimen_collection_date AS closest_positive
		FROM (SELECT 
			ROW_NUMBER() OVER (PARTITION BY pcbf.patient_program_id ORDER BY abs(pcbf.day_diff_pos - 0) ASC) AS pos_bl,
			pcbf.*
		FROM pos_culture_baseline_fuzzy AS pcbf) AS pcb
		WHERE pcb.pos_bl = 1),
	negative_results AS (
		SELECT
			bcrd.patient_program_id AS "patient_program_id", 
			bcs.specimen_collection_date::date AS "collection_date"
		FROM bacteriology_culture_results_details AS bcrd
		LEFT OUTER JOIN bacteriology_concept_set AS bcs
			ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON bcrd.patient_program_id = ti.patient_program_id
		WHERE bcrd.bacteriology_culture_results = 'Negative for M. tuberculosis' AND bcs.specimen_collection_date >= ti.tuberculosis_drug_treatment_start_date),
	positive_results AS (
		SELECT
			bcrd.patient_program_id AS "patient_program_id",
			bcs.specimen_collection_date::date AS "collection_date"
		FROM bacteriology_culture_results_details AS bcrd
		LEFT OUTER JOIN bacteriology_concept_set AS bcs
			ON bcrd.id_bacteriology_concept_set = bcs.id_bacteriology_concept_set
		LEFT OUTER JOIN treatment_initiation_template AS ti
			ON bcrd.patient_program_id = ti.patient_program_id
		WHERE bcrd.bacteriology_culture_results = 'Positive for M. tuberculosis' AND bcs.specimen_collection_date >= ti.tuberculosis_drug_treatment_start_date),
	cc_date AS (	
		SELECT
			nr.patient_program_id,
			nr.collection_date AS collection_date1,
			nr2.collection_date AS collection_date2,
			(nr2.collection_date - nr.collection_date) AS days_between,
			SUM(CASE 
				WHEN nr.patient_program_id = bp.patient_program_id THEN 1
				ELSE 0
			END) AS pos_baseline,
			SUM(CASE
				WHEN pr.collection_date BETWEEN nr.collection_date AND nr2.collection_date THEN 1
				WHEN pr.collection_date = nr.collection_date THEN 1
				WHEN pr.collection_date = nr2.collection_date THEN 1
				ELSE 0
			END) AS pos_between,
			SUM(CASE
				WHEN bp.closest_positive < nr.collection_date THEN 1
				ELSE 0
			END) AS cc_after_pos,
			SUM(CASE
				WHEN pr.collection_date > nr2.collection_date THEN 1
				ELSE 0
			END) AS pos_after_cc
		FROM negative_results AS nr
		INNER JOIN negative_results AS nr2
			ON nr.patient_program_id = nr2.patient_program_id AND nr.collection_date != nr2.collection_date
		LEFT OUTER JOIN positive_results AS pr
			ON nr.patient_program_id = pr.patient_program_id
		LEFT OUTER JOIN baseline_pos AS bp
			ON nr.patient_program_id = bp.patient_program_id
		WHERE (nr2.collection_date - nr.collection_date) > 15
		GROUP BY nr.patient_program_id, nr.collection_date, nr2.collection_date
		ORDER BY nr.patient_program_id, nr.collection_date)
	SELECT
		DISTINCT ON (cd.patient_program_id) cd.patient_program_id,
		cd.collection_date1 AS initial_cc_date,
		CASE
			WHEN cd.pos_after_cc > 0 THEN 'Y'
			ELSE NULL 
		END AS revert_after_cc
	FROM cc_date AS cd 
	WHERE cd.pos_between = 0 AND cd.pos_baseline != 0 AND cd.cc_after_pos != 0
	ORDER BY cd.patient_program_id, cd.collection_date1)

/*Below is the main data table with all basic register variables. Only patients enrolled into a TB program and have a treatment start date documented on the treatment initiation form are part of the register.*/
SELECT
	pa."Registration_Number" AS "01_Registration_Number",
	ppv.program_name AS "02_Register",
	pi."Patient_Identifier" AS "03_EMR_ID",
	pta."patientDistrict" AS "04_District",
	ppv.gender AS "05_Sex",
	ppv.age_at_program AS "06_Age_at_Registration",
	ppv.age_group_at_program AS "07_Age_Group_at_Registration",
	bl.baseline_who_registration_group AS "08_WHO_Registration_Group",
	bl.category_iv_tuberculosis_classification AS "09_History_of_Previously_Treated",
	blds.disease_site AS "10_Baseline_Disease_Site",
	bl.baseline_mdr_tb_diagnosis_method AS "11_MTB_Confirmed",
	bl.baseline_drug_resistance AS "12_Drug_Resistance_Profile",
	bl.baseline_subclassification_for_confimed_drug_resistant_case AS "13_Sub-class_of_Drug_Resistance_Profile",
	ti.tuberculosis_drug_treatment_start_date::date AS "14_Treatment_Start_Date",
	date_part('year',ti.tuberculosis_drug_treatment_start_date::date) AS "15_Treatment_Start_Year",
	concat(date_part('year',ti.tuberculosis_drug_treatment_start_date::date),' - Q',date_part('quarter',ti.tuberculosis_drug_treatment_start_date::date)) AS "16_Treatment_Start_Quarter",
	ROUND((CASE 
		WHEN eot.tuberculosis_treatment_end_date is not null then (DATE_PART('day',(eot.tuberculosis_treatment_end_date::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12
		ELSE (DATE_PART('day',(now()::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12
	END)::NUMERIC,1) AS "17_Treatment_Duration_(months)",
	CASE
		WHEN ti.ti_type_of_treatment_regimen = 'PNG Regimen List 1' then 'Short Regimen'
		WHEN ti.ti_type_of_treatment_regimen = 'PNG Regimen List 2' then 'Conventional Regimen without new drugs'
		WHEN ti.ti_type_of_treatment_regimen = 'PNG Regimen List 3' then 'Regimen including new drugs'
		ELSE ti.ti_type_of_treatment_regimen 
	END AS "18_Type_of_Regimen",
	ti.ti_second_line_regimen_drug_type AS "19_Second_Line_Treatment_Type",
	dsd.dlm_start_date::date AS "20_Start_Date_(Dlm)",
	bsd.bdq_start_date::date AS "21_Start_Date_(Bdq)",
	LEAST(dsd.dlm_start_date,bsd.bdq_start_date)::date AS "22_Start_Date_(Dlm_or_Bdq)",
	CASE
		WHEN dsd.dlm_start_date BETWEEN (bsd.bdq_start_date::date - 7) AND (bsd.bdq_start_date::date + 7) THEN LEAST(dsd.dlm_start_date,bsd.bdq_start_date)::date
		ELSE null
	END AS "23_Start_Date_(Dlm_and_Bdq)",
	lf.last_facility AS "24_Last_Facility",
	idr."H" AS "25_Initial_Drug_H",
	idr."R" AS "26_Initial_Drug_R",
	idr."E" AS "27_Initial_Drug_E",
	idr."Z" AS "28_Initial_Drug_Z",
	idr."S" AS "29_Initial_Drug_S",
	idr."Am" AS "30_Initial_Drug_Am",
	idr."Km" AS "31_Initial_Drug_Km",
	idr."Cm" AS "32_Initial_Drug_Cm",
	idr."Lfx" AS "33_Initial_Drug_Lfx",
	idr."Mfx" AS "34_Initial_Drug_Mfx",
	idr."Pto" AS "35_Initial_Drug_Pto",
	idr."Eto" AS "36_Initial_Drug_Eto",
	idr."Cs" AS "37_Initial_Drug_Cs",
	idr."Trd" AS "38_Initial_Drug_Trd",
	idr."PAS" AS "39_Initial_Drug_PAS",
	idr."PAS-Na" AS "40_Initial_Drug_PAS-Na",
	idr."Bdq" AS "41_Initial_Drug_Bdq",
	idr."Dlm" AS "42_Initial_Drug_Dlm",
	idr."Lzd" AS "43_Initial_Drug_Lzd",
	idr."Cfz" AS "44_Initial_Drug_Cfz",
	idr."Imp/Cln" AS "45_Initial_Drug_Imp/Clm",
	idr."Amx/Clv" AS "46_Initial_Drug_Amx/Clv",
	idr."Mpm" AS "47_Initial_Drug_Mpm",
	idr."Pa" AS "48_Initial_Drug_Pa",
	idr."Rpt or P" AS "49_Initial_Drug_Rpt_or_P",
	bl.baseline_hiv_serostatus_result AS "50_HIV_Baseline",
	lhr.hiv_result AS "51_HIV_Lab",
	CASE
		WHEN lhr.hiv_result =  'Positive' THEN 'HIV Positive'
		WHEN lhr.hiv_result = 'Negative' THEN 'HIV Negative'
		ELSE (CASE
			WHEN bl.baseline_hiv_serostatus_result = 'Positive' THEN 'HIV Positive'
			WHEN bl.baseline_hiv_serostatus_result = 'Negative' THEN 'HIV Negative'
			ELSE null
		END)
	END AS "52_HIV_Status",
	CASE
		WHEN bl.baseline_hepatitis_b = 'False' THEN 'Negative'
		WHEN bl.baseline_hepatitis_b = 'True' THEN 'Positive'
		WHEN bl.baseline_hepatitis_b = 'Unknown' THEN 'Unknown'
		ELSE null
	END AS "53_Hep_B_Baseline",
	lhbr.hep_b_result AS "54_Hep_B_Lab",
	CASE
		WHEN lhbr.hep_b_result = 'Non-reactive' THEN 'Hep B Negative'
		WHEN lhbr.hep_b_result = 'Reactive' THEN 'Hep B Positive'
		ElSE (CASE
			WHEN bl.baseline_hepatitis_b = 'True' THEN 'Hep B Positive' 
			WHEN bl.baseline_hepatitis_b = 'False' THEN 'Hep B Negative'
			ELSE null
		END)
	END AS "55_Hep_B_Status",
	CASE
		WHEN bl.baseline_hepatitis_c = 'True' THEN 'Positive'
		WHEN bl.baseline_hepatitis_c = 'False'THEN 'Negative'
		ELSE null
	END AS "56_Hep_C_Baseline",
	lhcr.hep_c_result AS "57_Hep_C_Lab",
	CASE
		WHEN lhcr.hep_c_result = 'Non-reactive' THEN 'Hep C Negative'
		WHEN lhcr.hep_c_result = 'Reactive' THEN 'Hep C Positive'
		ElSE (CASE
			WHEN bl.baseline_hepatitis_c = 'True' THEN 'Hep C Positive' 
			WHEN bl.baseline_hepatitis_c = 'False' THEN 'Hep C Negative'
			ELSE null
		END)
	END AS "58_Hep_C_Status",
	CASE
		WHEN bl.diabetes_mellitus = 'False' THEN 'Negative'
		WHEN bl.diabetes_mellitus = 'True' THEN 'Positive'
		ELSE null
	END AS "59_Diabetes_Baseline",
	CASE
		WHEN bpos.patient_program_id IS NOT NULL THEN 'Y'
		ELSE null
	END AS "60_Positive_Culture_at_Baseline",
	bpos.closer_negative AS "61_Negative_Culture_Closer_to_Baseline_Than_Positive_Culture",
	CASE
		WHEN icc.patient_program_id IS NOT NULL THEN 'Y'
		ELSE null
	END AS "62_Culture_Conversion_(for_positive_at_baseline_only)",
	icc.initial_cc_date AS "63_Initial_Culture_Conversion_Date",
	ROUND(((DATE_PART('day',(icc.initial_cc_date::timestamp)-(ti.tuberculosis_drug_treatment_start_date::timestamp)))/365*12)::NUMERIC,1) AS "64_Months_to_Initial_Culture_Conversion",
	icc.revert_after_cc AS "65_Reconversion_after_Initial_Culture_Conversion",
	eot.eot_outcome AS "66_Outcome",
	eot.tuberculosis_treatment_end_date AS "67_End_of_Treatment_Date",
	CASE
		WHEN lfu.return_visit_date IS NOT NULL THEN lfu.return_visit_date
		ELSE bl.return_visit_date
	END AS "68_Next_Visit",
	CASE
		WHEN eot.tuberculosis_treatment_end_date IS NULL THEN 'Yes'
		ELSE null
	END AS "69_Active_Treatment",	
	1 as "99_background_sum",
	'x' as "99_background_count"
FROM patient_program_view AS ppv 
LEFT OUTER JOIN treatment_initiation_template AS ti
	ON ppv.patient_program_id = ti.patient_program_id
LEFT OUTER JOIN patient_identifier AS pi
	ON ppv.patient_id = pi.patient_id
LEFT OUTER JOIN person_attributes AS pta
	ON ppv.patient_id = pta.person_id
LEFT OUTER JOIN program_attributes AS pa
	ON ppv.patient_program_id = pa.patient_program_id
LEFT OUTER JOIN baseline_template AS bl
	ON ppv.patient_program_id = bl.patient_program_id
LEFT OUTER JOIN baseline_disease_site_agg AS blds
	ON ppv.patient_program_id = blds.patient_program_id												  
LEFT OUTER JOIN dlm_start_date AS dsd
	ON ppv.patient_program_id = dsd.patient_program_id
LEFT OUTER JOIN bdq_start_date AS bsd
	ON ppv.patient_program_id = bsd.patient_program_id										  
LEFT OUTER JOIN last_facility AS lf 
	ON ppv.patient_program_id = lf.patient_program_id
LEFT OUTER JOIN initial_drug_regimen AS idr
	ON ppv.patient_program_id = idr.patient_program_id
LEFT OUTER JOIN latest_hiv_result AS lhr 
	ON ppv.patient_program_id = lhr.patient_program_id
LEFT OUTER JOIN latest_hep_b_result AS lhbr 
	ON ppv.patient_program_id = lhbr.patient_program_id
LEFT OUTER JOIN latest_hep_c_result AS lhcr
	ON ppv.patient_program_id = lhcr.patient_program_id
LEFT OUTER JOIN baseline_pos AS bpos
	ON ppv.patient_program_id = bpos.patient_program_id
LEFT OUTER JOIN initial_cc AS icc
	ON ppv.patient_program_id = icc.patient_program_id
LEFT OUTER JOIN outcome_end_of_treatment_template AS eot
	ON ppv.patient_program_id = eot.patient_program_id
LEFT OUTER JOIN latest_followup AS lfu
	ON ppv.patient_program_id = lfu.patient_program_id
WHERE ti.tuberculosis_drug_treatment_start_date IS NOT NULL
ORDER BY pi.patient_id